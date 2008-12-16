//-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
// 					SGSDK_Input.pas
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// The Input unit is responsible for input event
// processing for mouse and keyboard actions.
//
// Change History:
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

{$I SwinGame.inc}

unit SGSDK_Input;

interface
	uses SDL, SGSDK_Core, SGSDK_Font, SGSDK_Shapes;

  	//
	//
	// Input handling routines.
	//
	//*****
	//
	// These routines are used to handle input from the user.
	//

	type
		MouseButton = (
				LeftButton = SDL_BUTTON_LEFT,
				MiddleButton = SDL_BUTTON_MIDDLE,
				RightButton = SDL_BUTTON_RIGHT,
				WheelUpButton = SDL_BUTTON_WHEELUP,
				WheelDownButton = SDL_BUTTON_WHEELDOWN,
				MouseX1Button = SDL_BUTTON_X1,
				MouseX2Button = SDL_BUTTON_X2
			);
	
	function GetMousePositionAsVector(): Vector;
	function GetMousePosition(): Point2D;
	function GetMouseMovement(): Vector;
	function IsMouseDown(button: MouseButton): Boolean;
	function IsMouseUp(button: MouseButton): Boolean;
	function MouseWasClicked(button: MouseButton): Boolean;
	
	procedure StartReadingText(textColor: Colour; maxLength: LongInt; theFont: Font; x, y: LongInt);
  procedure StartReadingTextWithText(text: String; textColor: Colour; maxLength: LongInt; theFont: Font; x, y: LongInt);
	function 	EndReadingText(): String;
	
	function IsReadingText(): Boolean;
	function TextReadAsASCII(): String;
	function IsKeyPressed(virtKeyCode : LongInt): Boolean;
	function WasKeyTyped(virtKeyCode: LongInt): Boolean;
	function AKeyWasPressed(): Boolean;
	
	procedure MoveMouse(x, y : UInt16);overload;
	procedure MoveMouse(point: Point2D);overload;
	
	procedure ShowMouse(); overload;
	procedure ShowMouse(show : Boolean); overload;
	procedure HideMouse();
	function IsMouseShown(): Boolean;

implementation
	uses SysUtils, Classes, SGSDK_Physics, SwinGameTrace;
	
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
	
	function IsMouseShown(): Boolean;
	begin
		result := SDL_ShowCursor(-1) = 1;
	end;

	/// Returns true when a key is typed. This occurs when the key is pressed on the 
	/// keyboard, and will not reoccur until it is released and pressed again. This
	/// needs to be checked each ProcessEvents loop.
	///
	/// @param:	virtKeyCode			the code of the key to check
	/// @returns:					True if the key is pressed
	function WasKeyTyped(virtKeyCode: LongInt): Boolean;
	begin
		result := sdlManager.WasKeyTyped(virtKeyCode);
	end;

	/// Returns true when the key requested is being held down. This is updated
	///	as part of the ProcessEvents call. Use the key codes from the KeyCodes
	///	unit.
	///
	///	@returns:	 True if the key is currently being held down
	function IsKeyPressed(virtKeyCode : LongInt): Boolean;
	begin
		result := sdlManager.IsKeyPressed(virtKeyCode);
	end;
	
	function AKeyWasPressed(): Boolean;
	begin
	 result := sdlManager.WasAKeyPressed();
	end;

	/// StartReadingText start the API reading a string values from the user.
	///	Entry is completed when the user presses enter, and aborted with escape.
	///	If the user aborts entry the result is an empty string. Text entry is
	///	updated as part of ProcessEvents, and is drawn to the screen as part of
	///	the RefreshScreen call.
	///
	///	@param textColor:	The color of the text entered by the user
	///	@param maxLength:	The maximum length of the string the user can enter
	///	@param theFont:		The font used to draw the text entered
	///	@param x, y:			 The location at which to draw the text entered
	procedure StartReadingText(textColor: Colour; maxLength: LongInt; 
                             theFont: Font; x, y: LongInt);
	begin
		if theFont = nil then raise Exception.Create('The specified font to start reading text is nil');
		if maxLength <= 0 then raise Exception.Create('Minimum length to start reading text is 1');
		if IsReadingText() then raise Exception.Create('Already reading text, cannot start reading text again.');
		
		sdlManager.StartReadingText(ToSDLColor(textColor), maxLength, theFont, x, y);
	end;
	
  procedure StartReadingTextWithText(text: String; textColor: Colour; maxLength: LongInt; theFont: Font; x, y: LongInt);
	begin
	  StartReadingText(textColor, maxLength, theFont, x, y);
	  sdlManager.SetText(text);
	end;

	/// IsReadingText indicates if the API is currently reading text from the
	///	user. Calling StartReadingText will set this to true, and it becomes
	///	false when the user presses enter or escape. At this point you can
	///	read the string entered as either ASCII or Unicode.
	///
	///	@returns: True while the API is reading text from the user
	function IsReadingText(): Boolean;
	begin
		result := sdlManager.IsReading;
	end;
	
	function EndReadingText(): String;
	begin
		result := sdlManager.EndReadingText();
	end;

 	/// TextReadAsASCII allows you to read the value of the string entered by the
	///	user as ASCII. See TextReasAsUNICODE, StartReadingText and IsReadingText
	///	for more details.
	///
	///	@return:	 The string entered by the user
	function TextReadAsASCII(): String;
	begin
		result := String(sdlManager.EnteredString);
	end;
	
	var _ButtonsClicked: Array [MouseButton] of Boolean;
	
	function GetMousePositionAsVector(): Vector;
	var
		x, y: LongInt;
	begin
		x := 0; y := 0;
		SDL_GetMouseState(x, y);
		result := CreateVector(x, y);
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
		result := CreateVector(Single(x), Single(y));
		
	  {$IFDEF TRACE}
			TraceExit('SGSDK_Input', 'GetMouseMovement');
		{$ENDIF}
	end;
	
	function IsMouseDown(button: MouseButton): Boolean;
	var
		x, y: LongInt;
	begin
		x := 0; y := 0;
		result := (SDL_GetMouseState(x, y) and SDL_BUTTON(LongInt(button))) > 0;
	end;
	
	function IsMouseUp(button: MouseButton): Boolean;
	begin
		result := not IsMouseDown(button);
	end;
	
	function MouseWasClicked(button: MouseButton): Boolean;
	begin
		result := _ButtonsClicked[button];
	end;
	
	procedure ProcessEvent(event: PSDL_Event; first: Boolean);
	begin
		if event = nil then exit;
		
		if event^.type_ = SDL_MOUSEBUTTONUP then
		begin
			_ButtonsClicked[MouseButton(event^.button.button)] := true;
		end;
	end;
	
	procedure StartProcessEvents();
	var
		b: MouseButton;
	begin
		for b := LeftButton to MouseX2Button do
			_ButtonsClicked[b] := false;
			
		//_ButtonsClicked[LeftButton] := false;
		//_ButtonsClicked[RightButton] := false;
	end;

initialization
begin
	RegisterEventProcessor(@ProcessEvent, @StartProcessEvents);
end;
end.