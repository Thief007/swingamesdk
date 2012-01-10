//=============================================================================
// sgShared.pas
//=============================================================================
//
// The shared data and functions that are private to the SwinGame SDK. This
// unit should not be accessed directly by games.
//
// Change History:
//
// Version 3:
// - 2009-12-21: Andrew : Write exceptions to stderr if not raised.
// - 2009-12-18: Andrew : Added screen rect cache
// - 2009-12-10: Andrew : Added iter free - fixed memory leak
// - 2009-11-11: Andrew : Switched to release rather than drain on the autorelease pool
// - 2009-11-06: Andrew : Added resource management type code
// - 2009-10-16: Andrew : Added the free notifier, so it can be called from many places
// - 2009-07-29: Andrew : Added flag for opened audio.
// - 2009-07-27: Andrew : Added code to cycle auto release pool for Objective C
//                      : Fixed possible double release of AutoRelease pool
// - 2009-07-10: Andrew : Added initialisation code
// - 2009-06-23: Clinton: Comment/format cleanup/tweaks
//                      : Slight optimization to NewSDLRect (redundant code)
//                      : Renamed scr to screen
// - 2009-06-05: Andrew : Created to contain all globals that are hidden from
//                        the library
//=============================================================================

{$I sgTrace.inc}
unit sgShared;
  
//=============================================================================
interface
  uses 
    SDL, SDL_Image,     //SDL
    stringhash,   // libsrc
    sgInputBackend, sgTypes;
//=============================================================================
  
  type
    // The resource contain is an object to hold the resource for the 
    // hash table
    TResourceContainer = class(tObject)
    private
      resource_val : Pointer;
    public
      constructor Create(data: Pointer);
      
      property Resource: Pointer read resource_val;
    end;
        
    // The resource contain is an object to hold the resource for the 
    // hash table
    TIntegerContainer = class(tObject)
    private
      val : Longint;
    public
      constructor Create(data: Longint);

      property Value: Longint read val;
    end;

    // Used by release all
    ReleaseFunction = procedure(name: String);
    IntProc = procedure(val: Longword);

  // Immediate if
  function iif(pred: Boolean; trueVal, falseVal: Single): Single; overload;
  
  // Calls Release on all of the resources in the tbl hash table.
  procedure ReleaseAll(tbl: TStringHash; releaser: ReleaseFunction);

  // Takes a 4-byte (32bit) unsigned integer representing colour in the
  // current SDL pixel format and returns a nice `TSDL_Color` record with
  // simple red, green and blue components
  function ToSDLColor(color: Longword): TSDL_Color;

  // Converts the passed in SwinGame `Color` format (which changes as does
  // SDL to whatever the current SDL environment uses) to the Color structure
  // used by SDL_GFX (which is always 4 bytes in RGBA order).
  function ToGfxColor(val: Color): Color;

  // Returns a new `SDL_Rect` at the specified position (``x`` and ``y``) and
  // size (``w`` and ``h``). SDL_Rect are used internally by SDL to specify
  // the part of a source or destination image to be used in clipping and
  // blitting operations.
  function NewSDLRect(x, y, w, h: Longint): SDL_Rect; overload;
  function NewSDLRect(const r: Rectangle): SDL_Rect; overload;

  // Rounds `x` up... 1.1 -> 2
  function Ceiling(x: Single): Longint;

  // Used by SwinGame units to register event "handlers" what will be called
  // when SDL events have occured. Events such as mouse movement, button
  // clicking and key changes (up/down). See sgInput.
 // procedure RegisterEventProcessor(handle: EventProcessPtr; handle2: EventStartProcessPtr);
  
  // All SwinGame initialisation code must call this before performing any processing...
  procedure InitialiseSwinGame();
  
  procedure RaiseException(message: String);
  procedure RaiseWarning(message: String);
  
  {$ifdef DARWIN}
  procedure CyclePool();
  {$endif}
  
  procedure SetNonAlphaPixels(bmp: Bitmap; surface: PSDL_Surface);
  function GetPixel32(surface: PSDL_Surface; x, y: Longint): Color;
  
  function RoundUByte(val: Double): UInt8;
  function RoundInt(val: Double): LongInt;
  function RoundUShort(val: Double): UInt16;
  function RoundShort(val: Double): SInt16;
  function StrToSingle(val: String): Single;
  function StrToUByte(val: String): UInt8;
  
  /// Called when ANY resource is freed to inform other languages to remove from
  /// their caches.
  procedure CallFreeNotifier(p: Pointer);
    
  // Global variables that can be shared.
  var
    // The base path to the program's executable.
    applicationPath: String = '';
    
    // This `Bitmap` wraps the an SDL image (and its double-buffered nature)
    // which is used to contain the current "screen" rendered to the window.
    screen: Bitmap = nil;
    
    // Used for on screen tests...
    screenRect: Rectangle;
    
    // The singleton instance manager used to check events and called
    // registered "handlers". See `RegisterEventProcessor`.
    //sdlManager: TSDLManager = nil;

    // The name of the icon file shown.
    // TODO: Does this work? Full path or just resource/filename?
    iconFile: String = '';

    // This "base" surface is used to get standard pixel format information
    // for any other surfaces that need to be created and to support colour.
    baseSurface: PSDL_Surface = nil;

    // Contains the last error message that has occured if `HasException` is
    // true. If multiple error messages have occured, only the last is stored.
    // Used only by the generated library code.
    ErrorMessage: String = '';

    // This flag is set to true if an error message has occured. Used only by
    // the generated library code.
    HasException: Boolean = False;
    
    // This flag indicates if the audio has been opened.
    AudioOpen: Boolean = False;
    
    // The function pointer to call into the other language to tell them something was freed
    _FreeNotifier: FreeNotifier = nil;
    
    // Timing details related to calculating FPS
    _lastUpdateTime: Longword = 0;
    
    // The pointer to the screen's surface
    _screen: PSDL_Surface = nil;
    
    _UpdateFPSData: IntProc = nil;
    
    UseExceptions: Boolean = True;
  const
    DLL_VERSION = 'TEST BUILD';
    {$ifndef FPC}
    LineEnding = #13#10; // Delphi Windows \r\n pair
    {$endif}


//=============================================================================
implementation
  uses 
    SysUtils, Math, Classes, StrUtils,
    sgTrace, sgGraphics, 
    SDL_gfx;
//=============================================================================
  
  var
    is_initialised: Boolean = False;
//----------------------------------------------------------------------------
// Global variables for Mac OS Autorelease Pool
//----------------------------------------------------------------------------
    {$ifdef DARWIN}
      NSAutoreleasePool: Pointer;
      //drain: Pointer;
      pool: Pointer = nil;
    {$endif}
    
//---------------------------------------------------------------------------
// OS X dylib external link
//---------------------------------------------------------------------------
  
  {$ifdef DARWIN}
    {$linklib libobjc.dylib}
    procedure NSApplicationLoad(); cdecl; external 'Cocoa'; {$EXTERNALSYM NSApplicationLoad}
    function objc_getClass(name: PChar): Pointer; cdecl; external 'libobjc.dylib'; {$EXTERNALSYM objc_getClass}
    function sel_registerName(name: PChar): Pointer; cdecl; external 'libobjc.dylib'; {$EXTERNALSYM sel_registerName}
    function class_respondsToSelector(cls, sel: Pointer): boolean; cdecl; external 'libobjc.dylib'; {$EXTERNALSYM class_respondsToSelector}
    function objc_msgSend(self, cmd: Pointer): Pointer; cdecl; external 'libobjc.dylib'; {$EXTERNALSYM objc_msgSend}
  {$endif}
  
  constructor TResourceContainer.create(data: Pointer);
  begin
    inherited create;
    resource_val := data;
  end;
  
  constructor TIntegerContainer.create(data: Longint);
  begin
    inherited create;
    val := data;
  end;
  
  procedure InitialiseSwinGame();
  begin
    if is_initialised then exit;
    is_initialised := True;
    
    {$IFDEF TRACE}
      TraceEnter('sgShared', 'InitialiseSwinGame', '');
    {$ENDIF}
    
    Randomize();
    
    {$ifdef DARWIN}
      {$IFDEF Trace}
        TraceIf(tlInfo, 'sgShared', 'INFO', 'InitialiseSwinGame', 'Loading Mac version');
      {$ENDIF}
      
      //FIX: Error with Mac and FPC 2.2.2
      SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
      
      NSAutoreleasePool := objc_getClass('NSAutoreleasePool');
      pool := objc_msgSend(NSAutoreleasePool, sel_registerName('alloc'));
      pool := objc_msgSend(pool, sel_registerName('init'));
      NSApplicationLoad();
    {$endif}
    
    {$IFDEF Trace}
      TraceIf(tlInfo, 'sgShared', 'INFO', 'InitialiseSwinGame', 'About to initialise SDL');
    {$ENDIF}
    
    if SDL_INIT(SDL_INIT_VIDEO or SDL_INIT_AUDIO) = -1 then
    begin
      {$IFDEF Trace}
      TraceIf(tlError, 'sgShared', 'ERROR', 'InitialiseSwinGame', SDL_GetError());
      {$ENDIF}
      RaiseException('Error loading sdl... ' + SDL_GetError());
      exit;
    end;
    
    {$IFDEF Trace}
      TraceIf(tlInfo, 'sgShared', 'INFO', 'InitialiseSwinGame', 'About to enable unicode');
    {$ENDIF}
    
    //Unicode required by input manager.
    SDL_EnableUNICODE(SDL_ENABLE);
    
    //Initialise colors... assuming ARGB -  will be recalculated when window is opened
    ColorWhite        := $FFFFFFFF;
    ColorGreen        := $FF00FF00;
    ColorBlue         := $FF0000FF;
    ColorBlack        := $FF000000;
    ColorRed          := $FFFF0000;
    ColorYellow       := $FFFFFF00;
    ColorPink         := $FFFF1493;
    ColorTurquoise    := $FF00CED1;
    ColorGrey         := $FF808080;
    ColorMagenta      := $FF00FFFF;
    ColorTransparent  := $00000000;
    ColorLightGrey    := $FFC8C8C8;
    
    {$IFDEF TRACE}
      TraceExit('sgShared', 'InitialiseSwinGame');
    {$ENDIF}
  end;
  
  {$ifdef DARWIN}
  procedure CyclePool();
  begin
    if class_respondsToSelector(NSAutoreleasePool, sel_registerName('drain')) then
    begin
      //Drain the pool - releases it
      objc_msgSend(pool, sel_registerName('drain'));
      // WriteLn('Drain');
      //Create a new pool
      pool := objc_msgSend(NSAutoreleasePool, sel_registerName('alloc'));
      pool := objc_msgSend(pool, sel_registerName('init'));
    end;
  end;
  {$endif}

  function ToSDLColor(color: Longword): TSDL_Color;
  begin
    if (baseSurface = nil) or (baseSurface^.format = nil) then
    begin
      RaiseWarning('Estimating color as screen is not created.');
      result.r := color and $00FF0000 shr 16;
      result.g := color and $0000FF00 shr 8;
      result.b := color and $000000FF;
      exit;
    end;

    SDL_GetRGB(color, baseSurface^.format, @result.r, @result.g, @result.b);
  end;

  function ToGfxColor(val: Color): Color;
  var
    r, g, b, a: Byte;
  begin
    ColorComponents(val, r, g, b, a);
    result := (r shl 24) or (g shl 16) or (b shl 8) or a;
  end;
  
  //procedure RegisterEventProcessor(handle: EventProcessPtr; handle2: EventStartProcessPtr);
  //begin
  //  if sdlManager = nil then
  //  begin
  //    sdlManager := TSDLManager.Create();
  //  end;
  //  
  //  sdlManager.RegisterEventProcessor(handle, handle2);
  //end;
  
  function NewSDLRect(const r: Rectangle): SDL_Rect; overload;
  begin
    result := NewSDLRect(Round(r.x), Round(r.y), r.width, r.height);
  end;
  
  function NewSDLRect(x, y, w, h: Longint): SDL_Rect; overload;
  begin
    if w < 0 then
    begin
      x += w;
      w := -w;
    end;
    if h < 0 then
    begin
      y += h;
      h := -h;
    end;
    
    result.x := x;
    result.y := y;
    result.w := Word(w);
    result.h := Word(h);
  end;
  
  function Ceiling(x: Single): Longint;
  begin
    result := Round(x);
    if result < x then result := result + 1;
  end;
  
  procedure SetNonAlphaPixels(bmp: Bitmap; surface: PSDL_Surface);
  var
    r, c: Longint;
    hasAlpha: Boolean;
  begin
    SetLength(bmp^.nonTransparentPixels, bmp^.width, bmp^.height);
    hasAlpha := surface^.format^.BytesPerPixel = 4;

    for c := 0 to bmp^.width - 1 do
    begin
      for r := 0 to bmp^.height - 1 do
      begin
        bmp^.nonTransparentPixels[c, r] := (not hasAlpha) or ((GetPixel32(surface, c, r) and SDL_Swap32($000000FF)) > 0);
      end;
    end;
  end;
  
  function GetPixel32(surface: PSDL_Surface; x, y: Longint): Color;
  var
    pixel, pixels: PUint32;
    offset: Longword;
  {$IFDEF FPC}
    pixelAddress: PUint32;
  {$ELSE}
    pixelAddress: Longword;
  {$ENDIF}
  begin
    //Convert the pixels to 32 bit
    pixels := surface^.pixels;

    //Get the requested pixel
    offset := (( y * surface^.w ) + x) * surface^.format^.BytesPerPixel;

    {$IFDEF FPC}
      pixelAddress := pixels + (offset div 4);
      pixel := PUint32(pixelAddress);
    {$ELSE}
      pixelAddress := Longword(pixels) + offset;
      pixel := Ptr(pixelAddress);
    {$ENDIF}

    {$IF SDL_BYTEORDER = SDL_BIG_ENDIAN }
    case surface^.format^.BytesPerPixel of
      1: result := pixel^ and $000000ff;
      2: result := pixel^ and $0000ffff;
      3: result := pixel^ and $00ffffff;
      4: result := pixel^;
    else
      RaiseException('Unsuported bit format...');
      exit;
    end;
    {$ELSE}
    case surface^.format^.BytesPerPixel of
      1: result := pixel^ and $ff000000;
      2: result := pixel^ and $ffff0000;
      3: result := pixel^ and $ffffff00;
      4: result := pixel^;
    else
      raise Exception.Create('Unsuported bit format...')
    end;
    {$IFEND}
  end;
  
  procedure RaiseException(message: String);
  begin
    HasException := True;
    ErrorMessage := message;
    
    {$IFDEF TRACE}
      TraceIf(tlError, '', 'EXCP', '** Exception Raised **', message);
      TraceExit('', 'Ended with exception');
    {$ENDIF}
    
    if UseExceptions then raise Exception.Create(message)
    else WriteLn(stderr, message);
  end;

  procedure RaiseWarning(message: String);
  begin
    WriteLn(stderr, message);
    
    {$IFDEF Trace}
      TraceIf(tlWarning, '', 'WARN', '** Warning Raised **', message);
    {$ENDIF}
  end;

  //---------------------------------------------------------------------------

  procedure CallFreeNotifier(p: Pointer);
  begin
    if Assigned(_FreeNotifier) then
    begin
      try
        _FreeNotifier(p);
      except
        ErrorMessage := 'Error calling free notifier';
        HasException := True;
      end;
    end;
  end;
  
  //----------------------------------------------------------------------------
  
  procedure ReleaseAll(tbl: TStringHash; releaser: ReleaseFunction);
  var
    iter: TStrHashIterator;
    names: array of String;
    i: Integer;
  begin
    if tbl.count = 0 then exit;
    
    SetLength(names, tbl.count);
    iter := tbl.getIterator();
    i := 0;
    
    while i < Length(names) do
    begin
      names[i] := iter.key;
      i := i + 1;
      iter.next;
    end;

    for i := Low(names) to High(names) do
    begin
      releaser(names[i]);
    end;
    
    iter.Free();
    tbl.deleteAll();
  end;

function iif(pred: Boolean; trueVal, falseVal: Single): Single; overload;
begin
  if pred then result := trueVal
  else result := falseVal;
end;

function RoundUByte(val: Double): UInt8;
begin
  result := UInt8(Round(val));
end;

function RoundInt(val: Double): LongInt;
begin
  result := LongInt(Round(val));
end;

function RoundUShort(val: Double): UInt16;
begin
  result := UInt16(Round(val));
end;

function RoundShort(val: Double): SInt16;
begin
  result := SInt16(Round(val));
end;

function StrToSingle(val: String): Single;
begin
  result := Single(StrToFloat(val));
end;

function StrToUByte(val: String): UInt8;
begin
  result := UInt8(StrToInt(val));
end;

//=============================================================================

  initialization
  begin
    InitialiseSwinGame();
  end;

//=============================================================================

  finalization
  begin
    {$ifdef DARWIN}
    if not assigned(pool) then
    begin
      pool := objc_msgSend(NSAutoreleasePool, sel_registerName('alloc'));
      pool := objc_msgSend(pool, sel_registerName('init'));
    end;
    {$endif}
    
    //if sdlManager <> nil then
    //begin
    //  sdlManager.Free();
    //  sdlManager := nil;
    //end;
    
    if screen <> nil then
    begin
      if screen^.surface <> nil then 
        SDL_FreeSurface(screen^.surface);
    
      Dispose(screen);
      screen := nil;
      //scr and baseSurface are now the same!
      baseSurface := nil;
    end;
    
    SDL_Quit();
    
    {$ifdef DARWIN}
      // last pool will self drain...
      if assigned(pool) then
      begin
        objc_msgSend(pool, sel_registerName('drain'));
      end;
      pool := nil;
      NSAutoreleasePool := nil;
    {$endif}
  end;

end.