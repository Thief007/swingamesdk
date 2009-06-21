//---------------------------------------------------/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
//        SDLEventProcessing.pas
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// This unit handles the processing of events and reading
// of text for SwinGames.
//
// An object of the TSDLManager is created and managed in
// sgCore. Only one instance should be used. This is
// available in the sdlManager global variable.
//
// Change History:
//
// Version 2.0.0:
// - 2008-12-17: Andrew: Moved all integers to LongInt
// - 2008-12-16: Andrew: Added any key press
//                       Added SetText
//
// Version 1.1.5:
// - 2008-04-18: Andrew: Added EndTextRead
//
//  Version 1.0:
//  - Various

unit SDLEventProcessing;

interface
uses Classes, SDL, SDL_Mixer, SysUtils, SDL_TTF;

type EventProcessPtr = procedure(event: PSDL_Event);
type EventStartProcessPtr = procedure();

type TSDLManager = class (TObject)
  private
    _quit: Boolean;
    _keyPressed: Boolean;
    _readingString: Boolean;
    _tempString: String;
    _textSurface: PSDL_Surface;
    _maxStringLen: LongInt;
    _font: PTTF_Font;
    _foreColor: TSDL_Color;
    _x, _y: LongInt;
    _KeyTyped: Array of LongInt;
    _EventProcessors: Array of EventProcessPtr;
    _EventStartProcessors: Array of EventStartProcessPtr;
  public
    procedure ProcessEvents();

    constructor Create();
    destructor Destroy(); override;

    function HasQuit(): Boolean;

    property MaxStringLength: LongInt read _maxStringLen write _maxStringLen;
    property EnteredString: String read _tempString write _tempString;
    property IsReading: Boolean read _readingString;

    procedure DrawCollectedText(dest: PSDL_Surface);
    procedure SetText(text: String);
    procedure StartReadingText(textColor: TSDL_Color; maxLength: LongInt; theFont: PTTF_Font; x, y: LongInt);
    function  EndReadingText(): String;

    function WasKeyTyped(keyCode: LongInt): Boolean;

    procedure RegisterEventProcessor(handle: EventProcessPtr; handle2: EventStartProcessPtr);

    function IsKeyPressed(virtKeyCode : LongInt): Boolean;
    function WasAKeyPressed(): Boolean;
  private
    procedure DoQuit();
    procedure CheckQuit();
    procedure HandleEvent(event: PSDL_Event);
    procedure HandleKeydownEvent(event: PSDL_Event);
end;

implementation
  
  function TSDLManager.WasAKeyPressed(): Boolean;
  begin
    result := _keyPressed;
  end;
  
  function TSDLManager.IsKeyPressed(virtKeyCode : LongInt): Boolean;
  var
    keys: PUint8;
    {$IFNDEF FPC}
    indexAddress: uint32;
    intPtr: ^UInt8;
    {$ENDIF}
  begin
    keys := SDL_GetKeyState(nil);
  
    if keys <> nil then
    begin
      {indexAddress := uint32(keys) + uint32(virtKeyCode);

      {- $ IFDEF FPC
        intPtr := PUInt8(indexAddress);
      {- $ ELSE
        intPtr := Ptr(indexAddress);
      {- $ ENDIF
      result := intPtr^ = 1;}

      {$IFDEF FPC}
        result := (keys + virtKeyCode)^ = 1;
      {$ELSE}
        indexAddress := uint32(keys) + uint32(virtKeyCode);
        intPtr := Ptr(indexAddress);
        result := intPtr^ = 1;
      {$ENDIF}

      {$IFDEF FPC}
      if not result then
      begin
        if virtKeyCode = SDLK_LSUPER then result := (keys + SDLK_LMETA)^ = 1
        else if virtKeyCode = SDLK_RSUPER then result := (keys + SDLK_RMETA)^ = 1;
      end;
      {$ENDIF}
    end
    else
    begin
      result := false;
    end;
  end;

  procedure TSDLManager.CheckQuit();
  {$IFDEF FPC}
  var
    keys: PUInt8;
  {$ENDIF}
  begin
    {$IFDEF FPC}
    keys := SDL_GetKeyState(nil);
    if ((keys + SDLK_LMETA)^ + (keys + SDLK_Q)^ = 2) or
      ((keys + SDLK_RMETA)^ + (keys + SDLK_Q)^ = 2) or
       ((keys + SDLK_LALT)^ + (keys + SDLK_F4)^ = 2) or
       ((keys + SDLK_RALT)^ + (keys + SDLK_F4)^ = 2)
      then DoQuit()
    {$ELSE}
    if IsKeyPressed(SDLK_LALT) and IsKeyPressed(SDLK_F4)
      then DoQuit()
    {$ENDIF}
  end;

  procedure TSDLManager.ProcessEvents();
  var
    event: TSDL_EVENT;
    i: LongInt;
  begin
    _keyPressed := false;
    SetLength(_KeyTyped, 0);

    for i := 0 to High(_EventProcessors) do
    begin
      _EventStartProcessors[i]();
    end;  
  
    SDL_PumpEvents();

    while SDL_PollEvent(@event) > 0 do
    begin
      HandleEvent(@event);
    end;

    CheckQuit();
  end;
  
  procedure TSDLManager.HandleEvent(event: PSDL_Event);
  var i : LongInt;
  begin
    case event.type_ of
      SDL_QUITEV: DoQuit();
      SDL_KEYDOWN: HandleKeydownEvent(event);
    end;
  
    for i := 0 to High(_EventProcessors) do
    begin
      _EventProcessors[i](event);
    end;
  end;
  
  procedure TSDLManager.HandleKeydownEvent(event: PSDL_Event);
  var
    oldStr: String;
  begin
    _keyPressed := true;
    SetLength(_KeyTyped, Length(_KeyTyped) + 1);
    _KeyTyped[High(_KeyTyped)] := event.key.keysym.sym;

    if _readingString then
    begin
      oldStr := _tempString;

      //If the key is not a control character
      if (event.key.keysym.sym = SDLK_BACKSPACE) and (Length(_tempString) > 0)then
      begin
         _tempString := Copy(_tempString, 1, Length(_tempString) - 1);
      end
      else if event.key.keysym.sym = SDLK_RETURN then
      begin
        _readingString := false;
      end
      else if event.key.keysym.sym = SDLK_ESCAPE then
      begin
        _tempString := '';
        _readingString := false;
      end
      else if Length(_tempString) < _maxStringLen then
      begin
        case event.key.keysym.unicode of
          //Skip non printable characters
          0: ;
          128..159: ;
          7..10, 13: ;
          27: ;
          else //Append the character
            _tempString := _tempString + Char(event.key.keysym.unicode);
        end;
      end;

      //If the string was change
      if oldStr <> _tempString then
      begin
         //Free the old surface
        if _textSurface <> nil then SDL_FreeSurface( _textSurface );

         //Render a new text surface
         if Length(_tempString) > 0 then
           _textSurface := TTF_RenderText_Blended(_font, PChar(_tempString + '|'), _foreColor)
         else
          _textSurface := nil;
      end;
    end;
  end;
  
  procedure TSDLManager.SetText(text: String);
  begin
    _tempString := text;
    
     //Free the old surface
    if _textSurface <> nil then SDL_FreeSurface( _textSurface );

     //Render a new text surface
     if Length(_tempString) > 0 then
       _textSurface := TTF_RenderText_Blended(_font, PChar(_tempString + '|'), _foreColor)
     else
      _textSurface := nil;
  end;
  
  function TSDLManager.EndReadingText(): String;
  begin
    _readingString := false;
    result := _tempString;
  end;
  
  procedure TSDLManager.DrawCollectedText(dest: PSDL_Surface);
  var
    drect: SDL_Rect;
  begin
    if _readingString and (_textSurface <> nil) then
    begin
      drect.x := _x;
      drect.y := _y;
    
      SDL_BlitSurface(_textSurface, nil, dest, @drect);
    end;
  end;
  
  procedure TSDLManager.StartReadingText(textColor: TSDL_Color; maxLength: LongInt; theFont: PTTF_Font; x: LongInt; y: LongInt);
  begin
    if _textSurface <> nil then
    begin
      //Free the old surface
      SDL_FreeSurface( _textSurface );
      _textSurface := nil;
    end;
  
    _readingString := true;
    _tempString := '';
    _maxStringLen := maxLength;
    _foreColor := textColor;
    _font := theFont;
    _x := x;
    _y := y;
  end;
  
  constructor TSDLManager.Create();
  begin
    _quit := false;
    _keyPressed := false;
    _textSurface := nil;
    _readingString := false;
  end;
  
  destructor TSDLManager.Destroy();
  begin
    SDL_FreeSurface(_textSurface);
    inherited Destroy();
  end;
  
  procedure TSDLManager.DoQuit();
  begin
    _quit := true;
  end;
  
  function TSDLManager.HasQuit(): Boolean;
  begin
    result := _quit;
  end;
  
  function TSDLManager.WasKeyTyped(keyCode: LongInt): Boolean;
  var i : LongInt;
  begin
    result := false;
  
    for i := 0 to High(_KeyTyped) do
    begin
      if _KeyTyped[i] = keyCode then
      begin
        result := true;
        exit;
      end;
    end;
  end;
  
  procedure TSDLManager.RegisterEventProcessor(handle: EventProcessPtr; handle2: EventStartProcessPtr);
  begin
    //if handle^ <> nil then
    begin
      SetLength(_EventProcessors, Length(_EventProcessors) + 1);
      _EventProcessors[High(_EventProcessors)] := handle;
    end;
  
    //if handle2^ <> nil then
    begin
      SetLength(_EventStartProcessors, Length(_EventStartProcessors) + 1);
      _EventStartProcessors[High(_EventStartProcessors)] := handle2;
    end;  
  end;
end.