unit sgDriverInputSDL;

interface
  uses sdl, sdl_ttf;
  procedure LoadSDLInputDriver();

    var 
      _textSurface:           PSDL_Surface;
      _cursorSurface:         PSDL_Surface;
      _maxStringLen:          Longint;
      _font:                  PTTF_Font;
      _foreColor:             TSDL_Color;
      _area:                  SDL_Rect;
      
implementation
  uses sgDriverInput, sgInputBackend, sgInput, sgShared, sgTypes ;
  
  Procedure InitializeGlobalVars();
  begin
    _textSurface := nil;
    _cursorSurface := nil;
  end;
  
  function IsKeyPressedProcedure(virtKeyCode : LongInt) : Boolean;
  
  var
    keys: PUint8;
    {$IFNDEF FPC}
    indexAddress: Longword;
    intPtr: ^UInt8;
    {$ENDIF}
  begin
    keys := SDL_GetKeyState(nil);
    if keys <> nil then
    begin
      {$IFDEF FPC}
        result := (keys + virtKeyCode)^ = 1;
      {$ELSE}
        indexAddress := Longword(keys) + Longword(virtKeyCode);
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
      WriteLn(VirtKeyCode);
      result := false;
    end;
  end;
  
  function CheckQuitProcedure() : Boolean;
    {$IFDEF FPC}
    var
      keys: PUInt8;
    {$ENDIF} 
  begin
    {$IFDEF FPC}
    keys := SDL_GetKeyState(nil);
    result := (((keys + SDLK_LMETA)^ + (keys + SDLK_Q)^ = 2) or
      ((keys + SDLK_RMETA)^ + (keys + SDLK_Q)^ = 2) or
       ((keys + SDLK_LALT)^ + (keys + SDLK_F4)^ = 2) or
       ((keys + SDLK_RALT)^ + (keys + SDLK_F4)^ = 2));

    {$ELSE}
    result := (IsKeyPressed(SDLK_LALT) and IsKeyPressed(SDLK_F4));
    {$ENDIF}
  end;
  
  procedure ProcessEventsProcedure(); 
  var
    event: TSDL_EVENT;
  begin
     //http://sdl.beuc.net/sdl.wiki/SDL_PumpEvents
     //SDL_PumpEvents();

     while SDL_PollEvent(@event) > 0 do
     begin
       case event.type_ of
         SDL_QUITEV:   DoQuit();
         SDL_KEYDOWN:  HandleKeydownEvent(event.key.keysym.sym,event.key.keysym.unicode);
         SDL_KEYUP:    HandleKeyupEvent(event.key.keysym.sym);
         SDL_MOUSEBUTTONUP: ProcessMouseEvent(event.button.button); 
       end;
     end;
  end;
  
  procedure DrawCollectedTextProcedure(dest : Bitmap);
  var
    srect, drect: SDL_Rect;
    textWidth: Longint;
  begin
    if not assigned(dest^.Surface) then exit;

    if _readingString then
      if (_textSurface <> nil) then
        begin
          textWidth := _textSurface^.w;

          if textWidth > _area.w then
            srect.x := SInt16(textWidth - _area.w)
          else
            srect.x := 0;
          srect.y := 0;

          srect.w := _area.w;
          srect.h := _area.h;

          drect := _area;

          SDL_BlitSurface(_textSurface, @srect, dest^.Surface, @drect);
        end;
  end;
  
  function ShiftDownProcedure(kycode : LongInt) : Boolean;
  begin
    result := ((kyCode = SDLK_LSHIFT) or (kyCode = SDLK_RSHIFT));
  end;
  
  procedure FreeOldSurfaceProcedure();
  begin
    if assigned(_textSurface) and (_textSurface <> _cursorSurface) then 
      SDL_FreeSurface( _textSurface );
  end;
  
  procedure RenderTextSurfaceProcedure(text : String);
  var
    outStr: String;
  begin
    if Length(text) > 0 then
    begin
      outStr := text + '|';
      _textSurface := TTF_RenderText_Blended(_font, PChar(outStr), _foreColor)
    end
    else
      _textSurface := _cursorSurface;  
  end;
  
  procedure StartReadingTextProcedure(textColor: Color; maxLength: Longint; theFont: Font; area: Rectangle; var tempString : String);
  var
    newStr: String;
  begin
    if assigned(_textSurface) and (_textSurface <> _cursorSurface) then
    begin
      //Free the old surface
      SDL_FreeSurface( _textSurface );
      _textSurface := nil;
    end;

    _readingString := true;
    _textCancelled := false;
    tempString := '';
    _maxStringLen := maxLength;
    _foreColor := ToSDLColor(textColor);

    _font := theFont^.fptr;
    _area := NewSDLRect(area);

    newStr := '|';

    if assigned(_cursorSurface) then SDL_FreeSurface(_cursorSurface);
    _cursorSurface := TTF_RenderText_Blended(_font, PChar(newStr), _foreColor);
    _textSurface := _cursorSurface;
  end;  
  
  
  procedure DestroyProcedure();
  begin
    if assigned(_textSurface) and (_textSurface <> _cursorSurface) then SDL_FreeSurface(_textSurface);
    if assigned(_cursorSurface) then SDL_FreeSurface(_cursorSurface);
  end;

  procedure LoadSDLInputDriver(); 
  begin
    InputDriver.IsKeyPressed := @IsKeyPressedProcedure;
    InputDriver.CheckQuit := @CheckQuitProcedure;
    InputDriver.ProcessEvents := @ProcessEventsProcedure;
    InputDriver.DrawCollectedText := @DrawCollectedTextProcedure;
    InputDriver.ShiftDown := @ShiftDownProcedure;
    InputDriver.FreeOldSurface := @FreeOldSurfaceProcedure;
    InputDriver.RenderTextSurface := @RenderTextSurfaceProcedure;
    InputDriver.StartReadingText := @StartReadingTextProcedure;
    InputDriver.Destroy := @DestroyProcedure;
  end;
  
end.