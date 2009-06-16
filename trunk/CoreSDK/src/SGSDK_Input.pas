//=============================================================================
//          SGSDK_Input.pas
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
// - 2009-06-05: Andrew: Using sg_Shared
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

{$I SwinGame.inc}

/// @module Input
/// @static
unit SGSDK_Input;

//=============================================================================
interface
//=============================================================================

  uses SDL, SGSDK_Core, SGSDK_Font, SGSDK_Shapes, SGSDK_KeyCodes;
  
  type
    
    /// A mouse can have many different types of buttons. Most people know 
    /// about the simple Left and Right buttons, but there is also a Middle
    /// button (sometimes part of a scoll wheel). Scroll wheel movement is also
    /// treated as mouse button "clicks" of either the wheel "up" or "down" 
    /// buttons. 
    ///
    /// @enum MouseButton
    MouseButton = (
      LeftButton = 1,
      MiddleButton = 2,
      RightButton = 3,
      WheelUpButton = 4,
      WheelDownButton = 5,
      MouseX1Button = 6,
      MouseX2Button = 7
    );
  
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
  

//=============================================================================
implementation
//=============================================================================

  uses SysUtils, Classes, SGSDK_Physics, SwinGameTrace, sg_Shared;
  
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
    result := CreatePoint(x, y);    
  end;
  
  function GetMouseMovement(): Vector;
  var
    x, y: LongInt;
  begin
    {$IFDEF TRACE}
      TraceEnter('SGSDK_Input', 'GetMouseMovement');
    {$ENDIF}
    
    x := 0; 
    y := 0;
    SDL_GetRelativeMouseState(x, y);
    result := CreateVector(x, y);
    
    {$IFDEF TRACE}
      TraceExit('SGSDK_Input', 'GetMouseMovement');
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

//=============================================================================

initialization
begin
  RegisterEventProcessor(@ProcessMouseEvent, @StartProcessMouseEvents);
end;

end.