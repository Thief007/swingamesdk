unit SGSDK_Input;

interface
	uses	SDL_Mixer, SDL, SDL_Image, SDL_TTF, SDLEventProcessing, SGSDK_Core, SGSDK_Font, SGSDK_Physics;

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
				RightButton = SDL_BUTTON_RIGHT
			);
	
	function GetMousePosition(): Vector;
	function GetMouseMovement(): Vector;
	function IsMouseDown(button: MouseButton): Boolean;
	function IsMouseUp(button: MouseButton): Boolean;
	function MouseWasClicked(button: MouseButton): Boolean;


	procedure StartReadingText(textColor: Colour; maxLength: Integer;
							   theFont: Font; x, y: Integer);

	function IsReadingText(): Boolean;
	function TextReadAsASCII(): String;
	//function TextReadAsUNICODE(): WideString;
	function IsKeyPressed(virtKeyCode : Integer): Boolean;
	function WasKeyTyped(virtKeyCode: Integer): Boolean;

implementation
	uses SysUtils, Math, Classes;

	/// Returns true when a key is typed. This occurs when the key is pressed on the 
	/// keyboard, and will not reoccur until it is released and pressed again. This
	/// needs to be checked each ProcessEvents loop.
	///
	/// @param:	virtKeyCode			the code of the key to check
	/// @returns:					True if the key is pressed
	function WasKeyTyped(virtKeyCode: Integer): Boolean;
	begin
		try
			result := sdlManager.WasKeyTyped(virtKeyCode);
		except
			RaiseSGSDKException('Unable to check if the specified key was typed');
      result := false;
		end;
	end;

	/// Returns true when the key requested is being held down. This is updated
	///	as part of the ProcessEvents call. Use the key codes from the KeyCodes
	///	unit.
	///
	///	@returns:	 True if the key is currently being held down
	function IsKeyPressed(virtKeyCode : Integer): Boolean;
	var
		keys: PUint8;
		indexAddress: uint32;
		intPtr: ^UInt8;
	begin
		keys := SDL_GetKeyState(nil);

		if keys <> nil then
		begin
			indexAddress := uint32(keys) + uint32(virtKeyCode);

			{$IFDEF FPC}
				intPtr := PUInt8(indexAddress);
			{$ELSE}
				intPtr := Ptr(indexAddress);
			{$ENDIF}
			result := intPtr^ = 1;
		end
		else
		begin
			result := false;
		end;
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
	procedure StartReadingText(textColor: Colour; maxLength: Integer; 
                             theFont: Font; x, y: Integer);
	begin
		if theFont = nil then RaiseSGSDKException('The specified font to start reading text is nil');
		if maxLength <= 0 then RaiseSGSDKException('Minimum length to start reading text is 1');
		try
			sdlManager.StartReadingText(ToSDLColor(textColor), maxLength, theFont, x, y);
		except
			RaiseSGSDKException('Unable to start reading text');
		end;
	end;

	/// IsReadingText indicates if the API is currently reading text from the
	///	user. Calling StartReadingText will set this to true, and it becomes
	///	false when the user presses enter or escape. At this point you can
	///	read the string entered as either ASCII or Unicode.
	///
	///	@returns: True while the API is reading text from the user
	function IsReadingText(): Boolean;
	begin
		try
			result := sdlManager.IsReading;
		except
			RaiseSGSDKException('Unable to check if the program is reading text');
      result := false;
		end;
	end;

 	/// TextReadAsASCII allows you to read the value of the string entered by the
	///	user as ASCII. See TextReasAsUNICODE, StartReadingText and IsReadingText
	///	for more details.
	///
	///	@return:	 The string entered by the user
	function TextReadAsASCII(): String;
	begin
		try
			result := String(sdlManager.EnteredString);
		except
			RaiseSGSDKException('Unable to get the text entered as ASCII');
		end;
	end;

  	/// TextReadAsUNICODE returns the string entered by the user as UNICODE. See
	///	TextReadAsASCII, StartReadingText, and IsReadingText for more details.
	///
	///	@returns:	The string entered by the user
	{function TextReadAsUNICODE(): WideString;
	begin
		try
			result := sdlManager.EnteredString;
		except
			RaiseSGSDKException('Unable to get the text entered as Unicode');
		end;
	end;}

	var _ButtonsClicked: Array [MouseButton] of Boolean;
	
	function GetMousePosition(): Vector;
	var
		x, y: Integer;
	begin
		try
			SDL_GetMouseState(x, y);
			result := CreateVector(x, y);
		except
			RaiseSGSDKException('Failed to get the mouse position');
		end;
	end;

	function GetMouseMovement(): Vector;
	var
		x, y: Integer;
	begin
		try
			SDL_GetRelativeMouseState(x, y);
			result := CreateVector(x, y);
		except
			RaiseSGSDKException('Failed to get the mouse movement vector');
		end;
	end;
	
	function IsMouseDown(button: MouseButton): Boolean;
	var
		x, y: Integer;
	begin
		try
			result := (SDL_GetMouseState(x, y) and 
			SDL_BUTTON(Integer(button))) > 0;
		except
			RaiseSGSDKException('Failed to check if the mouse button is down');
      result := false;
		end;
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
		if event^.type_ = SDL_MOUSEBUTTONUP then
		begin
			_ButtonsClicked[MouseButton(event^.button.button)] := true;
		end;
	end;
	
	procedure StartProcessEvents();
	begin
		_ButtonsClicked[LeftButton] := false;
		_ButtonsClicked[RightButton] := false;
	end;

initialization
	begin
		RegisterEventProcessor(@ProcessEvent, @StartProcessEvents);
	end;

end.