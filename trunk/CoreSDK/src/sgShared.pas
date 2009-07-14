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
// - 2009-07-10: Andrew : Added initialisation code
// - 2009-06-23: Clinton: Comment/format cleanup/tweaks
//                      : Slight optimization to NewSDLRect (redundant code)
//                      : Renamed scr to screen
// - 2009-06-05: Andrew : Created to contain all globals that are hidden from
//                        the library
//=============================================================================

unit sgShared;

//=============================================================================
interface
//=============================================================================

  uses SDL, SDL_Image, sgEventProcessing, sgCore, sgTypes;

  // Takes a 4-byte (32bit) unsigned integer representing colour in the
  // current SDL pixel format and returns a nice `TSDL_Color` record with
  // simple red, green and blue components
  function ToSDLColor(color: UInt32): TSDL_Color;

  // Converts the passed in SwinGame `Color` format (which changes as does
  // SDL to whatever the current SDL environment uses) to the Color structure
  // used by SDL_GFX (which is always 4 bytes in RGBA order).
  function ToGfxColor(val: Color): Color;

  // Returns a new `SDL_Rect` at the specified position (``x`` and ``y``) and
  // size (``w`` and ``h``). SDL_Rect are used internally by SDL to specify
  // the part of a source or destination image to be used in clipping and
  // blitting operations.
  function NewSDLRect(x, y, w, h: LongInt): SDL_Rect;

  // Rounds `x` up... 1.1 -> 2
  function Ceiling(x: Single): LongInt;

  // Used by SwinGame units to register event "handlers" what will be called
  // when SDL events have occured. Events such as mouse movement, button
  // clicking and key changes (up/down). See sgInput.
  procedure RegisterEventProcessor(handle: EventProcessPtr; handle2: EventStartProcessPtr);
  
  // All SwinGame initialisation code must call this before performing any processing...
  procedure InitialiseSwinGame();
  
  procedure SetNonAlphaPixels(bmp: Bitmap; surface: PSDL_Surface);
  function GetPixel32(surface: PSDL_Surface; x, y: LongInt): Color;

  // Global variables that can be shared.
  var
    // This `Bitmap` wraps the an SDL image (and its double-buffered nature)
    // which is used to contain the current "screen" rendered to the window.
    screen: Bitmap;

    // The singleton instance manager used to check events and called
    // registered "handlers". See `RegisterEventProcessor`.
    sdlManager: TSDLManager;

    // The name of the icon file shown.
    // TODO: Does this work? Full path or just resource/filename?
    iconFile: String;

    // This "base" surface is used to get standard pixel format information
    // for any other surfaces that need to be created and to support colour.
    baseSurface: PSDL_Surface;

    // Contains the last error message that has occured if `HasException` is
    // true. If multiple error messages have occured, only the last is stored.
    // Used only by the generated library code.
    ErrorMessage: String;

    // This flag is set to true if an error message has occured. Used only by
    // the generated library code.
    HasException: Boolean;

  const
    DLL_VERSION = 300000;
    {$ifndef FPC}
    LineEnding = #13#10; // Delphi Windows \r\n pair
    {$endif}


//=============================================================================
implementation
//=============================================================================

  uses SysUtils, Math, Classes, sgTrace, SDL_gfx;
  
  var
    is_initialised: Boolean = False;
    //----------------------------------------------------------------------------
    // Global variables for Mac OS Autorelease Pool
    //----------------------------------------------------------------------------
    {$ifdef DARWIN}
      NSAutoreleasePool: LongInt;
      pool: LongInt;
    {$endif}
    
  //---------------------------------------------------------------------------
  // OS X dylib external link
  //---------------------------------------------------------------------------
  
  {$ifdef DARWIN}
    {$linklib libobjc.dylib}
    procedure NSApplicationLoad(); cdecl; external 'Cocoa'; {$EXTERNALSYM NSApplicationLoad}
    function objc_getClass(name: PChar): LongInt; cdecl; external 'libobjc.dylib'; {$EXTERNALSYM objc_getClass}
    function sel_registerName(name: PChar): LongInt; cdecl; external 'libobjc.dylib'; {$EXTERNALSYM sel_registerName}
    function objc_msgSend(self, cmd: LongInt): LongInt; cdecl; external 'libobjc.dylib'; {$EXTERNALSYM objc_msgSend}
  {$endif}
  
  procedure InitialiseSwinGame();
  begin
    if is_initialised then exit;
    is_initialised := True;
    
    {$ifdef DARWIN}
      {$IFDEF Trace}
        TraceIf(tlInfo, 'sgCore', 'Info', 'initialization', 'Loading Mac version');
      {$ENDIF}

      //FIX: Error with Mac and FPC 2.2.2
      SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

      NSAutoreleasePool := objc_getClass('NSAutoreleasePool');
      pool := objc_msgSend(NSAutoreleasePool, sel_registerName('new'));

      objc_msgSend(pool, sel_registerName('init'));
      NSApplicationLoad();
    {$endif}

    if SDL_Init(SDL_INIT_EVERYTHING) = -1 then
    begin
      {$IFDEF Trace}
      TraceIf(tlError, 'sgCore', 'Error Loading SDL', 'initialization', SDL_GetError());
      {$ENDIF}
      raise Exception.Create('Error loading sdl... ' + SDL_GetError());
    end;
    
    //Unicode required by input manager.
    SDL_EnableUNICODE(SDL_ENABLE);
    
    screen := nil;
    baseSurface := nil;
  end;
  

  function ToSDLColor(color: UInt32): TSDL_Color;
  begin
    if (baseSurface = nil) or (baseSurface^.format = nil) then
    begin
      raise Exception.Create('Unable to get color as screen is not created.');
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
  
  procedure RegisterEventProcessor(handle: EventProcessPtr; handle2: EventStartProcessPtr);
  begin
    if sdlManager = nil then
    begin
      sdlManager := TSDLManager.Create();
    end;
    
    sdlManager.RegisterEventProcessor(handle, handle2);
  end;
  
  function NewSDLRect(x, y, w, h: LongInt): SDL_Rect;
  begin
    if (w < 0) or (h < 0) then
      raise Exception.Create('Width and height of a rectangle must be larger than 0');
    // TODO: This can probably be removed now.
    //   
    // if w < 0 then
    // begin
    //   result.x := x + w;
    //   w := -w;
    // end
    // else result.x := x;
    // 
    // if h < 0 then
    // begin
    //   result.y := y + h;
    //   h := -h;
    // end
    // else result.y := y;
    result.x := x;
    result.y := y;
    result.w := Word(w);
    result.h := Word(h);
  end;
  
  function Ceiling(x: Single): LongInt;
  begin
    result := Round(x);
    if result < x then result := result + 1;
  end;
  
  procedure SetNonAlphaPixels(bmp: Bitmap; surface: PSDL_Surface);
  var
    r, c: LongInt;
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
  
  function GetPixel32(surface: PSDL_Surface; x, y: LongInt): Color;
  var
    pixel, pixels: PUint32;
    offset: Uint32;
  {$IFDEF FPC}
    pixelAddress: PUint32;
  {$ELSE}
    pixelAddress: UInt32;
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
      pixelAddress := UInt32(pixels) + offset;
      pixel := Ptr(pixelAddress);
    {$ENDIF}

    {$IF SDL_BYTEORDER = SDL_BIG_ENDIAN }
    case surface^.format^.BytesPerPixel of
      1: result := pixel^ and $000000ff;
      2: result := pixel^ and $0000ffff;
      3: result := pixel^ and $00ffffff;
      4: result := pixel^;
    else
      raise Exception.Create('Unsuported bit format...');
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
  


//=============================================================================

  initialization
  begin
    InitialiseSwinGame();
  end;

//=============================================================================

  finalization
  begin
    if sdlManager <> nil then
    begin
      sdlManager.Free();
      sdlManager := nil;
    end;
  
    {$ifdef DARWIN}
      objc_msgSend(pool, sel_registerName('drain'));
    {$endif}
  
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
  end;

end.