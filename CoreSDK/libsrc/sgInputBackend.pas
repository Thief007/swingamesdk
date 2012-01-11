unit sgInputBackend;

interface
  uses sgTypes;
    procedure DoQuit();
    procedure CheckQuit();
    procedure HandleKeydownEvent(kyCode, kyChar : LongInt);
    procedure AddKeyData(kyCode, kyChar: Longint);
    procedure HandleKeyupEvent(kyCode: LongInt);
    procedure ProcessKeyPress(kyCode, kyChar: Longint);
    procedure CheckKeyRepeat();
    // text reading/draw collection
    procedure DrawCollectedText(dest: Bitmap);
    procedure SetText(text: String);
    procedure InputBackendStartReadingText(textColor: Color; maxLength: Longint; theFont: Font; area: Rectangle);
    function  InputBackendEndReadingText(): String;
    // key pressed/typed tests
    function WasKeyTyped(kyCode: Longint): Boolean;
    function WasAKeyPressed(): Boolean;
    // event register/process Quit
    procedure InputBackendProcessEvents();
    procedure ProcessMouseEvent(button: Byte);
    function HasQuit(): Boolean;
    function isReading() : Boolean;
    function EnteredString: String; 
    function TextEntryWasCancelled: boolean; 
    
    
    
    
    
  type
    KeyDownData = record
      downAt:   Longint;
      code:     Longint;
      keyChar:  Longint;
    end;
     
  var
    _quit:                  Boolean;
    _keyPressed:            Boolean;
    _textCancelled:         Boolean;
    _tempString:            String;

    _lastKeyRepeat:         Longint;
    _KeyDown:               Array of KeyDownData;
    _KeyTyped:              Array of Longint;
    _maxStringLen:          LongInt;
    _textBitmap:            Bitmap;
    _cursorBitmap:          Bitmap;
    _font:                  Font;
    _foreColor:             Color;
    _area:                  Rectangle;
    _readingString:         Boolean;
    _ButtonsClicked: Array [MouseButton] of Boolean;
implementation
  uses sgDriverInput, sgDriverTimer, sgSharedUtils, sgDriverImages, sgImages, sgText, sgShared;
  
  procedure _InitGlobalVars(); 
  begin
    _quit := false;
    _keyPressed := false;
    _textBitmap := nil;
    _cursorBitmap := nil;
    _readingString := false;
    _textCancelled := false;
    _lastKeyRepeat := 0;
    
    SetLength(_KeyDown, 0);
  end;
  
  function EnteredString: String; 
  begin
    result := _tempString;
  end;
  
  
  
  function TextEntryWasCancelled: boolean; 
  begin
    result := _textCancelled;
  end;
  
  procedure CheckQuit();
  begin
    if (InputDriver.CheckQuit()) then
      DoQuit()
  end;
  
  procedure CheckKeyRepeat();
  var
    nowTime, timeDown: Longint;
  begin
    if Length(_KeyDown) <> 1 then exit;
    
    nowTime := TimerDriver.GetTicks();
    
    timeDown := nowTime - _KeyDown[0].downAt;
    
    // 300 is the key repeat delay - hard coded for the moment...
    if timeDown > 300 then
    begin
      ProcessKeyPress(_KeyDown[0].code, _KeyDown[0].keyChar);
      
      // 40 is the key repeat delap - hard coded for the moment...
      _KeyDown[0].downAt := _KeyDown[0].downAt + 30;
    end;
  end;
  
  procedure ResetMouseState();
  var
    b: MouseButton;
  begin
    for b := LeftButton to MouseX2Button do
      _ButtonsClicked[b] := false;
  end;
  
  procedure InputBackendProcessEvents();
  begin
    _keyPressed := false;
    SetLength(_KeyTyped, 0);
    ResetMouseState();
  
    InputDriver.ProcessEvents();

    CheckKeyRepeat();
    CheckQuit();
  end;
  
  function isReading() : Boolean;
  begin
    result := _readingString;
  end;
  
  procedure HandleKeyupEvent(kyCode: LongInt);
  var
    i, keyAt: Integer;
  begin
    keyAt := -1;
    for i := 0 to High(_KeyDown) do
    begin
      if _KeyDown[i].code = kyCode then
      begin
        keyAt := i;
        break;
      end;
    end;
    
    if keyAt = -1 then exit;
    for i := keyAt to High(_KeyDown) -1 do
    begin
      _KeyDown[i] := _KeyDown[i + 1];
    end;
    SetLength(_KeyDown, Length(_KeyDown) - 1);
  end;
  
  procedure AddKeyData(kyCode, kyChar: Longint);
  var
    i: Integer;
  begin
    if (kyCode = LongInt(vk_LSHIFT)) or (kyCode = LongInt(vk_RSHIFT)) then exit;
    
    // WriteLn(kyCode, ' ', kyChar);
    for i := 0 to High(_KeyDown) do
    begin
      if _KeyDown[i].code = kyCode then exit;
    end;
    
    SetLength(_KeyDown, Length(_KeyDown) + 1);
    
    with _KeyDown[High(_KeyDown)] do
    begin
      downAt  := TimerDriver.GetTicks();
      code    := kyCode;
      keyChar := kyChar;
    end;
  end;
  
  
  procedure HandleKeydownEvent(kyCode, kyChar : LongInt);
  begin
    _keyPressed := true;
    
    SetLength(_KeyTyped, Length(_KeyTyped) + 1);
    _KeyTyped[High(_KeyTyped)] := kyCode;
    
    AddKeyData(kyCode, kyChar);
    ProcessKeyPress(kyCode, kyChar);
  end;
  
  procedure ProcessKeyPress(kyCode, kyChar: Longint);
  var
    oldStr : String;
    
  begin
    if _readingString then
    begin
      oldStr := _tempString;
      
      //If the key is not a control character
      if (kyCode = LongInt(vk_BACKSPACE)) and (Length(_tempString) > 0) then
      begin
         _tempString := Copy(_tempString, 1, Length(_tempString) - 1);
      end
      else if (kyCode = LongInt(vk_RETURN)) or (kyCode = LongInt(vk_KP_ENTER)) then
      begin
        _readingString := false;
      end
      else if kyCode = LongInt(vk_ESCAPE) then
      begin
        _tempString := '';
        _readingString := false;
        _textCancelled := true;
      end
      else if Length(_tempString) < _maxStringLen then
      begin
        case kyChar of
          //Skip non printable characters
          0..31: ;
          127..High(Byte): ;
          else //Append the character
            _tempString := _tempString + Char(kyChar);
        end;
      end;
      
      //If the string was change
      if oldStr <> _tempString then
      begin
        SetText(_tempString);
      end;
    end;
  end;
  
  
  procedure ProcessMouseEvent(button: Byte);
  begin
    if( WithinRange(length(_ButtonsClicked), button )) then
      begin
        _ButtonsClicked[MouseButton(button)] := true;
      end;
  end;
  
  
  procedure FreeOldSurface();
  begin
    if imagesDriver.SurfaceExists(_textBitmap) and (not imagesDriver.SameBitmap(_textBitmap, _cursorBitmap)) then 
      FreeBitmap(_textBitmap);
  end;
  
  procedure RenderTextSurface(text : String);
  var
    outStr: String;
  begin
    if Length(text) > 0 then
    begin
      outStr := text + '|';
      _textBitmap := DrawTextTo(_font, outStr, _forecolor);
    end
    else
      _textBitmap := _cursorBitmap;  
  end;
  
  procedure InputBackendStartReadingText(textColor: Color; maxLength: Longint; theFont: Font; area: Rectangle);
  var
    newStr: String;
  begin
    if imagesDriver.SurfaceExists(_textBitmap) and (not imagesDriver.SameBitmap(_textBitmap, _cursorBitmap)) then
    begin
      //Free the old surface
      imagesDriver.FreeSurface(_textBitmap );
    end;

    _readingString := true;
    _textCancelled := false;
    _tempString := '';
    _maxStringLen := maxLength;
    _foreColor := textColor;

    _font := theFont;
    _area := area;

    newStr := '|';

    if imagesDriver.SurfaceExists(_cursorBitmap) then FreeBitmap(_cursorBitmap);
    _cursorBitmap := DrawTextTo(_font, newStr, _foreColor);
    _textBitmap := _cursorBitmap;
  end;
  
  procedure DrawCollectedText(dest : Bitmap);
  var
    srect, drect: Rectangle;
    textWidth: Longint;
  begin
    if (not imagesDriver.SurfaceExists(dest)) then exit;

    if _readingString then
      if (imagesDriver.SurfaceExists(_textBitmap)) then
        begin
          textWidth := _textBitmap^.width;

          if textWidth > _area.width then
            srect.x := SmallInt(textWidth - _area.width)
          else
            srect.x := 0;
          srect.y := 0;

          srect.width := _area.width;
          srect.height := _area.height;

          drect := _area;

          imagesDriver.BlitSurface(_textBitmap,  dest,@srect, @drect);
        end;
  end;

  
  
  procedure SetText(text: String);
  begin
    _tempString := text;
    
     //Free the old surface
    FreeOldSurface();
    
    //Render a new text surface
    RenderTextSurface(_tempString);
  end;
  
  function InputBackendEndReadingText(): String;
  begin
    _readingString := false;
    result := _tempString;
  end;
  
  function WasAKeyPressed(): Boolean;
  begin
    result := _keyPressed;
  end;
  
  procedure DoQuit();
  begin
    _quit := true;
  end;
  
  function HasQuit(): Boolean;
  begin
    result := _quit;
  end;
  
  function WasKeyTyped(kyCode: Longint): Boolean;
  var i: Longint;
  begin
    result := false;
  
    for i := 0 to High(_KeyTyped) do
    begin
      if _KeyTyped[i] = kyCode then
      begin
        result := true;
        exit;
      end;
    end;
  end;
  

  
  procedure Destroy();
  begin
    if(Assigned(_textBitmap)) then FreeBitmap(_textBitmap);
    if(Assigned(_cursorBitmap)) then FreeBitmap(_cursorBitmap);
    if(Assigned(_font)) then FreeFont(_font);
  end;
  
  initialization


  begin
  
    _InitGlobalVars();
  
  
  end;
finalization
  Destroy();


end.