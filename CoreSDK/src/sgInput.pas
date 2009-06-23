//=============================================================================
//          sgInput.pas
//=============================================================================
//
// This unit is responsible for input event processing for mouse visibility, 
// movement and button clicks (including the scroll wheel as button clicks) and 
// keyboard events for text input and key state checking.
//
// Change History:
//
// Version 3.0:
// - 2009-06-15: Clinton: renamed+removed Is/Was and placed Key/Mouse first 
//                        moved and added meta comments, tweaked formatting.
// - 2009-06-05: Andrew: Using sgShared
//
// Version 2.2.2:
// - 2008-12-17: Andrew: Moved all integers to LongInt
// - 2008-12-16: Andrew: Added WasAKeyPressed
//
// Version 1.1.5:
// - 2008-04-18: Andrew: Added EndTextRead
//
// Version 1.1:
// - 2008-02-13: James: changed MoveMouse so it dosnt generate mouse movemnt event
// - 2008-01-25: Stephen: Fixed IsMouseShown
// - 2008-01-25: Andrew: Fixed compiler hints
// - 2008-01-22: James: changed MoveMouse to Point2D
// - 2008-01-17: Aki + Andrew: Refactor
//  
// Version 1.0:
// - Various
//=============================================================================

{$I sgTrace.inc}

/// @module Input
/// @static
unit sgInput;

//=============================================================================
interface
//=============================================================================

  uses SDL, sgTypes;
  
  /// @returns The current window position of the mouse as a `Vector`
  /// @lib
  function GetMousePositionAsVector(): Vector;
  
  /// @returns The current window position of the mouse as a `Point2D` 
  /// @lib
  function GetMousePosition(): Point2D;
  
  /// @returns The amount of accumulated mouse movement, since the last time 
  ///          `ProcessEvents` was called, as a `Vector`. 
  /// @lib
  function GetMouseMovement(): Vector;
  
  /// @param button The specific `MouseButton` to check
  /// @returns `true` if the specified button is currently pressed down
  /// @lib
  function MouseDown(button: MouseButton): Boolean;
  
  /// @param button The specific `MouseButton` to check
  /// @returns `true` if the specified button is currently up
  /// @lib
  function MouseUp(button: MouseButton): Boolean;

  /// @param button The specific `MouseButton` to check
  /// @returns true if the specified button was clicked since the last time
  ///          `ProcessEvents` was called
  /// @lib
  function MouseClicked(button: MouseButton): Boolean;
  
  /// Moves the mouse cursor to the specified screen location
  ///
  /// @lib
  procedure MoveMouse(x, y : UInt16);overload;
    
  /// Moves the mouse cursor to the specified screen location
  ///
  /// @lib MoveMouseToPoint
  procedure MoveMouse(point: Point2D);overload;
  
  /// Tells the mouse cursor to be visible if it was previously hidden with 
  /// by a `HideMouse` or `SetMouseVisible(False)` call.
  ///
  /// @lib
  procedure ShowMouse(); overload;
    
  /// Used to explicitly set the mouse cursors visible state (if it is showing
  /// in the window or not) based on the `show` parameter.
  ///
  /// @param show If the mouse should be set to visible (showing) or not
  /// @lib SetMouseVisible
  procedure ShowMouse(show : Boolean); overload;
    
  /// Tells the mouse cursor to hide (no longer visible) if it is currently 
  /// showing. Use `ShowMouse` to make the mouse cursor visible again.
  ///
  /// @lib
  procedure HideMouse();
    
  /// @returns `true` if the mouse is currently visible, `false` if not.
  /// @lib
  function MouseShown(): Boolean;
  
  /// Starts the reading of a string of characters from the user. Entry is 
  /// completed when the user presses ENTER, and aborted with ESCAPE.
  /// If the user aborts entry the result is an empty string. Text entry is
  /// updated during `ProcessEvents`, and text is drawn to the screen as part 
  /// of the `RefreshScreen` call.
  ///
  /// @param textColor The color of the text entered by the user
  /// @param maxLength The maximum length of the string the user can enter
  /// @param theFont   The font used to draw the text entered
  /// @param x         Screen x location at which to draw the text
  /// @param y         Screen y location at which to draw the text
  ///
  /// @see StartReadingTextWithText, EndReadingText, IsReadingText
  /// @lib
  procedure StartReadingText(textColor: Color; maxLength: LongInt; theFont: Font; x, y: LongInt);
  
  /// The same as `StartReadingText' but with an additional `text` parameter
  /// that is displayed as default text to the user.  
  ///
  /// @like StartReadingText
  /// @param text A default text string to display which the user can change 
  /// @lib
  procedure StartReadingTextWithText(text: String; textColor: Color; maxLength: LongInt; theFont: Font; x, y: LongInt);
  
  /// @returns the string that has been read since `StartReadingText` or 
  ///          `StartReadingTextWithText` was called
  /// @lib
  function EndReadingText(): String;
  
  /// IsReadingText indicates if the API is currently reading text from the
  /// user. Calling StartReadingText will set this to true, and it becomes
  /// false when the user presses enter or escape. At this point you can
  /// read the string entered as either ASCII or Unicode.
  ///
  /// @returns True while the API is reading text from the user
  /// @lib
  function IsReadingText(): Boolean;
  
  /// TextReadAsASCII allows you to read the value of the string entered by the
  /// user as ASCII. See TextReasAsUNICODE, StartReadingText and IsReadingText
  /// for more details.
  ///
  /// @returns The string entered by the user
  /// @lib
  function TextReadAsASCII(): String;
  
  /// Returns true when the key requested is being held down. This is updated
  /// as part of the `ProcessEvents` call. Use the key codes from `KeyCodes`
  /// to specify the key to be checked.
  ///
  /// @param key A `KeyCode` to indicate the what key to check 
  /// @returns `true` if the key specified is currently being held down
  /// @lib
  function KeyDown(key: KeyCode): Boolean;

  /// Returns true if the specified key was pressed down since the last time 
  /// `ProcessEvents` was called. This occurs only once for the key that is 
  /// pressed and will not return true again until the key is released and
  /// pressed down again.
  ///
  /// @param key A `KeyCode` to indicate the what key to check 
  /// @returns `true` if the key was pressed down since the last `ProcessEvents` call
  /// @lib
  function KeyTyped(key: KeyCode): Boolean;

  /// Checks to see if any key has been pressed since the last time 
  /// `ProcessEvents` was called.
  ///
  /// @returns `true` if any key has been pressed.
  /// @lib
  function AnyKeyPressed(): Boolean;
  
  /// The KeyName function returns a string name for a given `KeyCode`. For 
  /// example, vk_Comma returns the string 'Comma'. This function could be used
  /// to display more meaningful key names for configuring game controls, etc.
  ///
  /// @param key The enumerated key code value used to identify a specific key
  /// @returns a string version of the key name
  /// @lib
  function KeyName(key: KeyCode): String;

//=============================================================================
implementation
//=============================================================================

  uses SysUtils, Classes, sgPhysics, SwinGameTrace, sgShared,
       sgCore, sgText, sgShapes;
  
  //---------------------------------------------------------------------------
  
  function KeyTyped(key: KeyCode): Boolean;
  begin
    result := sdlManager.WasKeyTyped(LongInt(key));
  end;

  function KeyDown(key : keyCode): Boolean;
  begin
    result := sdlManager.IsKeyPressed(LongInt(key));
  end;
  
  function AnyKeyPressed(): Boolean;
  begin
   result := sdlManager.WasAKeyPressed();
  end;
  
  //---------------------------------------------------------------------------
  
  procedure StartReadingText(textColor: Color; maxLength: LongInt; theFont: Font; x, y: LongInt);
  begin
    if theFont = nil then raise Exception.Create('The specified font to start reading text is nil');
    if maxLength <= 0 then raise Exception.Create('Minimum length to start reading text is 1');
    if IsReadingText() then raise Exception.Create('Already reading text, cannot start reading text again.');
    
    sdlManager.StartReadingText(ToSDLColor(textColor), maxLength, theFont, x, y);
  end;
  
  procedure StartReadingTextWithText(text: String; textColor: Color; maxLength: LongInt; theFont: Font; x, y: LongInt);
  begin
    StartReadingText(textColor, maxLength, theFont, x, y);
    sdlManager.SetText(text);
  end;
  
  function IsReadingText(): Boolean;
  begin
    result := sdlManager.IsReading;
  end;
  
  function EndReadingText(): String;
  begin
    result := sdlManager.EndReadingText();
  end;
  
  function TextReadAsASCII(): String;
  begin
    result := String(sdlManager.EnteredString);
  end;
  
  var _ButtonsClicked: Array [MouseButton] of Boolean;
  
  //---------------------------------------------------------------------------
  
  function GetMousePositionAsVector(): Vector;
  var
    x, y: LongInt;
  begin
    x := 0; y := 0;
    SDL_GetMouseState(x, y);
    result := CreateVector(x, y);
  end;
  
  procedure ShowMouse(); overload;
  begin
    ShowMouse(true);
  end;
  
  procedure HideMouse();
  begin
    ShowMouse(false);
  end;
  
  procedure ShowMouse(show : Boolean); overload;
  begin
    try
      if show then SDL_ShowCursor(1)
      else SDL_ShowCursor(0);
    except
      raise Exception.Create('Unable to show or hide mouse');
    end;
  end;
  
  procedure MoveMouse(x, y : UInt16);overload;
  begin
    SDL_WarpMouse(x,y);
    GetMouseMovement();
  end;
  
  procedure MoveMouse(point : Point2d);overload;
  begin
    SDL_WarpMouse(Round(point.x), Round(point.y));
    GetMouseMovement();
  end;
  
  function MouseShown(): Boolean;
  begin
    result := SDL_ShowCursor(-1) = 1;
  end;
  
  function GetMousePosition(): Point2D;
  var
    x, y: LongInt;
  begin
    x := 0; y := 0;
    SDL_GetMouseState(x, y);
    result := PointFrom(x, y);    
  end;
  
  function GetMouseMovement(): Vector;
  var
    x, y: LongInt;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgInput', 'GetMouseMovement');
    {$ENDIF}
    
    x := 0; 
    y := 0;
    SDL_GetRelativeMouseState(x, y);
    result := CreateVector(x, y);
    
    {$IFDEF TRACE}
      TraceExit('sgInput', 'GetMouseMovement');
    {$ENDIF}
  end;
  
  function MouseDown(button: MouseButton): Boolean;
  var
    x, y: LongInt;
  begin
    x := 0; y := 0;
    result := (SDL_GetMouseState(x, y) and SDL_BUTTON(LongInt(button))) > 0;
  end;
  
  function MouseUp(button: MouseButton): Boolean;
  begin
    result := not MouseDown(button);
  end;
  
  function MouseClicked(button: MouseButton): Boolean;
  begin
    result := _ButtonsClicked[button];
  end;
  
  procedure ProcessMouseEvent(event: PSDL_Event; first: Boolean);
  begin
    if event = nil then exit;
    
    if event^.type_ = SDL_MOUSEBUTTONUP then
    begin
      _ButtonsClicked[MouseButton(event^.button.button)] := true;
    end;
  end;
  
  procedure StartProcessMouseEvents();
  var
    b: MouseButton;
  begin
    for b := LeftButton to MouseX2Button do
      _ButtonsClicked[b] := false;
  end;

  function KeyName(key: KeyCode): String;
  begin
    case key of
      vk_Unknown: result := 'Unknown';
      vk_BACKSPACE: result := 'Backspace';
      vk_TAB: result := 'Tab';
      vk_CLEAR: result := 'Clear';
      vk_RETURN: result := 'Return';
      vk_PAUSE: result := 'Pause';
      vk_ESCAPE: result := 'Escape';
      vk_SPACE: result := 'Space';
      vk_EXCLAIM: result := 'Exclaim';
      vk_QUOTEDBL: result := 'Double Quote';
      vk_HASH: result := 'Hash';
      vk_DOLLAR: result := 'Dollar';
      vk_AMPERSAND: result := 'Ampersand';
      vk_QUOTE: result := 'Quote';
      vk_LEFTPAREN: result := 'Left Parenthesis';
      vk_RIGHTPAREN: result := 'Right Parenthesis';
      vk_ASTERISK: result := 'Asterisk';
      vk_PLUS: result := 'Plus';
      vk_COMMA: result := 'Comma';
      vk_MINUS: result := 'Minus';
      vk_PERIOD: result := 'Period';
      vk_SLASH: result := 'Slash';
      vk_0: result := '0';
      vk_1: result := '1';
      vk_2: result := '2';
      vk_3: result := '3';
      vk_4: result := '4';
      vk_5: result := '5';
      vk_6: result := '6';
      vk_7: result := '7';
      vk_8: result := '8';
      vk_9: result := '9';
      vk_COLON: result := 'Colon';
      vk_SEMICOLON: result := 'Semicolon';
      vk_LESS: result := 'Less';
      vk_EQUALS: result := 'Equals';
      vk_GREATER: result := 'Greater';
      vk_QUESTION: result := 'Question';
      vk_AT: result := 'At';

      // Skip uppercase letters 

      vk_LEFTBRACKET: result := 'Left Bracket';
      vk_BACKSLASH: result := 'Backslash';
      vk_RIGHTBRACKET: result := 'Right Bracket';
      vk_CARET: result := 'Caret';
      vk_UNDERSCORE: result := 'Underscore';
      vk_BACKQUOTE: result := 'Back Quote';
      vk_a: result := 'a';
      vk_b: result := 'b';
      vk_c: result := 'c';
      vk_d: result := 'd';
      vk_e: result := 'e';
      vk_f: result := 'f';
      vk_g: result := 'g';
      vk_h: result := 'h';
      vk_i: result := 'i';
      vk_j: result := 'j';
      vk_k: result := 'k';
      vk_l: result := 'l';
      vk_m: result := 'm';
      vk_n: result := 'n';
      vk_o: result := 'o';
      vk_p: result := 'p';
      vk_q: result := 'q';
      vk_r: result := 'r';
      vk_s: result := 's';
      vk_t: result := 't';
      vk_u: result := 'u';
      vk_v: result := 'v';
      vk_w: result := 'w';
      vk_x: result := 'x';
      vk_y: result := 'y';
      vk_z: result := 'z';
      vk_DELETE: result := 'Delete';
      // End of ASCII mapped keysyms

      // International keyboard syms
      vk_WORLD_0: result := 'World 0';
      vk_WORLD_1: result := 'World 1';
      vk_WORLD_2: result := 'World 2';
      vk_WORLD_3: result := 'World 3';
      vk_WORLD_4: result := 'World 4';
      vk_WORLD_5: result := 'World 5';
      vk_WORLD_6: result := 'World 6';
      vk_WORLD_7: result := 'World 7';
      vk_WORLD_8: result := 'World 8';
      vk_WORLD_9: result := 'World 9';
      vk_WORLD_10: result := 'World 10';
      vk_WORLD_11: result := 'World 11';
      vk_WORLD_12: result := 'World 12';
      vk_WORLD_13: result := 'World 13';
      vk_WORLD_14: result := 'World 14';
      vk_WORLD_15: result := 'World 15';
      vk_WORLD_16: result := 'World 16';
      vk_WORLD_17: result := 'World 17';
      vk_WORLD_18: result := 'World 18';
      vk_WORLD_19: result := 'World 19';
      vk_WORLD_20: result := 'World 20';
      vk_WORLD_21: result := 'World 21';
      vk_WORLD_22: result := 'World 22';
      vk_WORLD_23: result := 'World 23';
      vk_WORLD_24: result := 'World 24';
      vk_WORLD_25: result := 'World 25';
      vk_WORLD_26: result := 'World 26';
      vk_WORLD_27: result := 'World 27';
      vk_WORLD_28: result := 'World 28';
      vk_WORLD_29: result := 'World 29';
      vk_WORLD_30: result := 'World 30';
      vk_WORLD_31: result := 'World 31';
      vk_WORLD_32: result := 'World 32';
      vk_WORLD_33: result := 'World 33';
      vk_WORLD_34: result := 'World 34';
      vk_WORLD_35: result := 'World 35';
      vk_WORLD_36: result := 'World 36';
      vk_WORLD_37: result := 'World 37';
      vk_WORLD_38: result := 'World 38';
      vk_WORLD_39: result := 'World 39';
      vk_WORLD_40: result := 'World 40';
      vk_WORLD_41: result := 'World 41';
      vk_WORLD_42: result := 'World 42';
      vk_WORLD_43: result := 'World 43';
      vk_WORLD_44: result := 'World 44';
      vk_WORLD_45: result := 'World 45';
      vk_WORLD_46: result := 'World 46';
      vk_WORLD_47: result := 'World 47';
      vk_WORLD_48: result := 'World 48';
      vk_WORLD_49: result := 'World 49';
      vk_WORLD_50: result := 'World 50';
      vk_WORLD_51: result := 'World 51';
      vk_WORLD_52: result := 'World 52';
      vk_WORLD_53: result := 'World 53';
      vk_WORLD_54: result := 'World 54';
      vk_WORLD_55: result := 'World 55';
      vk_WORLD_56: result := 'World 56';
      vk_WORLD_57: result := 'World 57';
      vk_WORLD_58: result := 'World 58';
      vk_WORLD_59: result := 'World 59';
      vk_WORLD_60: result := 'World 60';
      vk_WORLD_61: result := 'World 61';
      vk_WORLD_62: result := 'World 62';
      vk_WORLD_63: result := 'World 63';
      vk_WORLD_64: result := 'World 64';
      vk_WORLD_65: result := 'World 65';
      vk_WORLD_66: result := 'World 66';
      vk_WORLD_67: result := 'World 67';
      vk_WORLD_68: result := 'World 68';
      vk_WORLD_69: result := 'World 69';
      vk_WORLD_70: result := 'World 70';
      vk_WORLD_71: result := 'World 71';
      vk_WORLD_72: result := 'World 72';
      vk_WORLD_73: result := 'World 73';
      vk_WORLD_74: result := 'World 74';
      vk_WORLD_75: result := 'World 75';
      vk_WORLD_76: result := 'World 76';
      vk_WORLD_77: result := 'World 77';
      vk_WORLD_78: result := 'World 78';
      vk_WORLD_79: result := 'World 79';
      vk_WORLD_80: result := 'World 80';
      vk_WORLD_81: result := 'World 81';
      vk_WORLD_82: result := 'World 82';
      vk_WORLD_83: result := 'World 83';
      vk_WORLD_84: result := 'World 84';
      vk_WORLD_85: result := 'World 85';
      vk_WORLD_86: result := 'World 86';
      vk_WORLD_87: result := 'World 87';
      vk_WORLD_88: result := 'World 88';
      vk_WORLD_89: result := 'World 89';
      vk_WORLD_90: result := 'World 90';
      vk_WORLD_91: result := 'World 91';
      vk_WORLD_92: result := 'World 92';
      vk_WORLD_93: result := 'World 93';
      vk_WORLD_94: result := 'World 94';
      vk_WORLD_95: result := 'World 95';

      // Numeric keypad
      vk_KP0: result := 'Keypad 0';
      vk_KP1: result := 'Keypad 1';
      vk_KP2: result := 'Keypad 2';
      vk_KP3: result := 'Keypad 3';
      vk_KP4: result := 'Keypad 4';
      vk_KP5: result := 'Keypad 5';
      vk_KP6: result := 'Keypad 6';
      vk_KP7: result := 'Keypad 7';
      vk_KP8: result := 'Keypad 8';
      vk_KP9: result := 'Keypad 9';
      vk_KP_PERIOD: result := 'Keypad Period';
      vk_KP_DIVIDE: result := 'Keypad Divide';
      vk_KP_MULTIPLY: result := 'Keypad Multiply';
      vk_KP_MINUS: result := 'Keypad Minus';
      vk_KP_PLUS: result := 'Keypad Plus';
      vk_KP_ENTER: result := 'Keypad Enter';
      vk_KP_EQUALS: result := 'Keypad Equals';

      // Arrows + Home/End pad
      vk_UP: result := 'Up';
      vk_DOWN: result := 'Down';
      vk_RIGHT: result := 'Right';
      vk_LEFT: result := 'Left';
      vk_INSERT: result := 'Insert';
      vk_HOME: result := 'Home';
      vk_END: result := 'End';
      vk_PAGEUP: result := 'Page Up';
      vk_PAGEDOWN: result := 'Page Down';

      // Function keys
      vk_F1: result := 'F1';
      vk_F2: result := 'F2';
      vk_F3: result := 'F3';
      vk_F4: result := 'F4';
      vk_F5: result := 'F5';
      vk_F6: result := 'F6';
      vk_F7: result := 'F7';
      vk_F8: result := 'F8';
      vk_F9: result := 'F9';
      vk_F10: result := 'F10';
      vk_F11: result := 'F11';
      vk_F12: result := 'F12';
      vk_F13: result := 'F13';
      vk_F14: result := 'F14';
      vk_F15: result := 'F15';

      // Key state modifier keys
      vk_NUMLOCK: result := 'Numlock';
      vk_CAPSLOCK: result := 'Caps lock';
      vk_SCROLLOCK: result := 'Scroll Lock';
      vk_RSHIFT: result := 'Right Shift';
      vk_LSHIFT: result := 'Left Shift';
      vk_RCTRL: result := 'Right Ctrl';
      vk_LCTRL: result := 'Left Ctrl';
      vk_RALT: result := 'Right Alt';
      vk_LALT: result := 'Left Alt';
      vk_RMETA: result := 'Right Meta';
      vk_LMETA: result := 'Left Meta';
      vk_LSUPER: result := 'Left Super';
      vk_RSUPER: result := 'Right Super';
      vk_MODE: result := 'Mode';
      vk_COMPOSE: result := 'Compose';
      // Miscellaneous function keys
      vk_HELP: result := 'Help';
      vk_PRINT: result := 'Print';
      vk_SYSREQ: result := 'Sys Req';
      vk_BREAK: result := 'Break';
      vk_MENU: result := 'Menu';
      vk_POWER: result := 'Power';
      vk_EURO: result := 'Euro';
    end;

  end;


//=============================================================================

initialization
begin
  RegisterEventProcessor(@ProcessMouseEvent, @StartProcessMouseEvents);
end;

end.