//=============================================================================
// sgEventProcessing.pas
//=============================================================================
//
// This unit handles the processing of events, including the reading of text
// for SwinGames. This unit and its code is not directly used by typical games.
//
// An object of the TSDLManager is created and managed in sgCore. Only one
// instance should be used (essentially a "singleton" pattern), which is
// available in the sdlManager as a global variable.
//
// Change History:
// Version 3:
// - 2010-02-02: Andrew : Added the ability to define a region for text entry
// - 2010-01-13: Cooldave: Fixed error with Bar in SetText.
// - 2009-06-23: Clinton: Renamed file/unit, comment/format cleanup/tweaks
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
//=============================================================================

unit sgEventProcessing;

//=============================================================================
interface
//=============================================================================

  uses 
    Classes, SDL, SDL_Mixer, SysUtils, SDL_TTF;

  type

    EventProcessPtr = procedure(event: PSDL_Event);

    EventStartProcessPtr = procedure();

    TSDLManager = class(TObject)
    private
      // private data
      _quit:                  Boolean;
      _keyPressed:            Boolean;
      _readingString:         Boolean;
      _textCancelled:         Boolean;
      _tempString:            String;
      _textSurface:           PSDL_Surface;
      _cursorSurface:         PSDL_Surface;
      _maxStringLen:          LongInt;
      _font:                  PTTF_Font;
      _foreColor:             TSDL_Color;
      _area:                  SDL_Rect;
      _KeyTyped:              Array of LongInt;
      _EventProcessors:       Array of EventProcessPtr;
      _EventStartProcessors:  Array of EventStartProcessPtr;
      
      // private methods
      procedure DoQuit();
      procedure CheckQuit();
      procedure HandleEvent(event: PSDL_Event);
      procedure HandleKeydownEvent(event: PSDL_Event);
    public
      // text reading/draw collection
      property MaxStringLength: LongInt read _maxStringLen write _maxStringLen;
      property EnteredString: String read _tempString write _tempString;
      property IsReading: Boolean read _readingString;
      property TextEntryWasCancelled: Boolean read _textCancelled;
      procedure DrawCollectedText(dest: PSDL_Surface);
      procedure SetText(text: String);
      procedure StartReadingText(textColor: TSDL_Color; maxLength: LongInt; theFont: PTTF_Font; area: SDL_Rect);
      function  EndReadingText(): String;
      // key pressed/typed tests
      function WasKeyTyped(keyCode: LongInt): Boolean;
      function IsKeyPressed(virtKeyCode: LongInt): Boolean;
      function WasAKeyPressed(): Boolean;
      // event register/process Quit
      procedure RegisterEventProcessor(handle: EventProcessPtr; handle2: EventStartProcessPtr);
      procedure ProcessEvents();
      function HasQuit(): Boolean;
      // create/destroy
      constructor Create();
      destructor Destroy(); override;
  end;

//=============================================================================
implementation
//=============================================================================

  function TSDLManager.WasAKeyPressed(): Boolean;
  begin
    result := _keyPressed;
  end;

  function TSDLManager.IsKeyPressed(virtKeyCode: LongInt): Boolean;
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
  var i: LongInt;
  begin
    case event^.type_ of
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
    oldStr, newStr: String;
  begin
    _keyPressed := true;
    SetLength(_KeyTyped, Length(_KeyTyped) + 1);
    _KeyTyped[High(_KeyTyped)] := event^.key.keysym.sym;

    if _readingString then
    begin
      oldStr := _tempString;

      //If the key is not a control character
      if (event^.key.keysym.sym = SDLK_BACKSPACE) and (Length(_tempString) > 0)then
      begin
         _tempString := Copy(_tempString, 1, Length(_tempString) - 1);
      end
      else if (event^.key.keysym.sym = SDLK_RETURN) or (event^.key.keysym.sym = SDLK_KP_ENTER) then
      begin
        _readingString := false;
      end
      else if event^.key.keysym.sym = SDLK_ESCAPE then
      begin
        _tempString := '';
        _readingString := false;
        _textCancelled := true;
      end
      else if Length(_tempString) < _maxStringLen then
      begin
        case event^.key.keysym.unicode of
          //Skip non printable characters
          0..31: ;
          127..High(UInt16): ;
          else //Append the character
            _tempString := _tempString + Char(event^.key.keysym.unicode);
        end;
      end;

      //If the string was change
      if oldStr <> _tempString then
      begin
         //Free the old surface
        if (_textSurface <> nil) and (_textSurface <> _cursorSurface) then 
          SDL_FreeSurface( _textSurface );
        
        //Render a new text surface
        if Length(_tempString) > 0 then
        begin
         newStr := _tempString + '|';
         _textSurface := TTF_RenderText_Blended(_font, PChar(newStr), _foreColor)
        end
        else 
          _textSurface := _cursorSurface;
      end;
    end;
  end;
  
  procedure TSDLManager.SetText(text: String);
  var
    outStr: String;
  begin
    _tempString := text;
    
     //Free the old surface
    if (_textSurface <> nil) and (_textSurface <> _cursorSurface) then 
      SDL_FreeSurface( _textSurface );
    
    //Render a new text surface
    if Length(_tempString) > 0 then
    begin
      outStr := _tempString + '|';
      _textSurface := TTF_RenderText_Blended(_font, PChar(outStr), _foreColor)
    end
    else
      _textSurface := _cursorSurface;
  end;
  
  function TSDLManager.EndReadingText(): String;
  begin
    _readingString := false;
    result := _tempString;
  end;
  
  procedure TSDLManager.DrawCollectedText(dest: PSDL_Surface);
  var
    srect, drect: SDL_Rect;
    textWidth: LongInt;
  begin
    if not assigned(dest) then exit;
    
    if _readingString and (_textSurface <> nil) then
    begin
      textWidth := _textSurface^.w;
      
      if textWidth > _area.w then
        srect.x := textWidth - _area.w
      else
        srect.x := 0;
      srect.y := 0;
      
      srect.w := _area.w;
      srect.h := _area.h;
      
      drect := _area;
      
      SDL_BlitSurface(_textSurface, @srect, dest, @drect);
    end;
  end;
  
  procedure TSDLManager.StartReadingText(textColor: TSDL_Color; maxLength: LongInt; theFont: PTTF_Font; area: SDL_Rect);
  var
    newStr: String;
  begin
    if _textSurface <> nil then
    begin
      //Free the old surface
      SDL_FreeSurface( _textSurface );
      _textSurface := nil;
    end;
  
    _readingString := true;
    _textCancelled := false;
    _tempString := '';
    _maxStringLen := maxLength;
    _foreColor := textColor;
    _font := theFont;
    _area := area;
    
    newStr := '|';
    
    if _cursorSurface <> nil then SDL_FreeSurface(_cursorSurface);
    _cursorSurface := TTF_RenderText_Blended(_font, PChar(newStr), _foreColor);
    _textSurface := _cursorSurface;
  end;
  
  constructor TSDLManager.Create();
  begin
    _quit := false;
    _keyPressed := false;
    _textSurface := nil;
    _cursorSurface := nil;
    _readingString := false;
    _textCancelled := false;
    
    SetLength(_EventProcessors, 0);
    SetLength(_EventStartProcessors, 0);
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
  var i: LongInt;
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