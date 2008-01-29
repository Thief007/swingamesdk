///-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
// 					SGSDK.pas
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// This is the library interface to SwinGame SDK. This is
// used by the non-pascal languages to interact with the
// API.
//
// cdecl is used to ensure consistent calling contentions.
// exports is required by windows, option in Mac, and not
//     needed for unix.
// Arrays are passed as pointers with the associated 
//     length, then reconstructed here.
// Errors are caught and the HasException must be set. See
//     the TrapException routine.
// Booleans are passed using Integer with -1 as true, any
//     other value is false.
//
// Change History:
//  
//  Version 1.1:
//  - 2008-01-25: Stephen: Fixed IsMouseShown
//  - 2008-01-23: Andrew: ToGameCoordinates - changed to use Point2D
//					  	Added MoveSpriteItself
//						Added Shapes code
//						Removed lots of Vector functions
//  - 2008-01-22: Andrew: Fixes for version 1.1
//                Removed PlaySoundEffect, can use PlaySoundEffectLoop
//						Removed MoveVisualAreaWithVector, can use MoveVisualArea
//						Removed Sin, Cos, Tan using system functions.
//						Added Timer function (7)
//  - 2008-01-17: Aki + Andrew: Refactor, fix error handling
//  - 2008-01-16: James: Added Mouse input for MoveMouse + ShowMouse
//  - 2008-01-16: Andrew Cain: Modified exception handling.
//
//  Version 1.0:
//  - Various

{$PACKENUM 4}

library SGSDK;

uses 
	SGSDK_Core, SGSDK_Input, SGSDK_Audio, SGSDK_Font, SGSDK_Physics, SGSDK_Graphics,
	SDL_Mixer, SDL, SDL_Image, SDL_TTF, SGSDK_Camera, SGSDK_Shapes, 
	SGSDK_MappyLoader, SysUtils;

	type
		//IntPtr used to receive arrays
		IntPtr = ^Integer;
		//BitmapPtr used to receive Bitmap arrays
		BitmapPtr = ^Bitmap;
		Matrix2DPtr = ^Matrix2D;
		LineSegPtr = ^LineSegment;

		IntArray = Array of Integer;
		BitmapArray = Array of Bitmap;


	///-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
	//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
	// 					Helper Methods
	//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
	//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\

	/// Copies the data from an array passed as a pointer
	/// into a Pascal array
	procedure PopulateIntArray(data: IntPtr; len: Integer; out arr: IntArray);
	var
		i: Integer;
	begin
		if len < 0 then raise Exception.Create('Length of array cannot be negative');
			
		SetLength(arr, len);

		for i := 0 to len - 1 do
		begin
			arr[i] := (data + i)^;
		end;
	end;
	
	procedure PopulateBitmapArray(data: BitmapPtr; len: Integer; out arr: BitmapArray);
	var
		i: Integer;
	begin
		if len < 0 then raise Exception.Create('Length of array cannot be negative');

		SetLength(arr, len);

		for i := 0 to len - 1 do
		begin
			arr[i] := (data + i)^;
		end;
	end;

	procedure PopulateLineSegmentArray(data: LineSegPtr; len: Integer; out arr: LinesArray);
	var
		i: Integer;
	begin
		if len < 0 then raise Exception.Create('Length of array cannot be negative');

		SetLength(arr, len);

		for i := 0 to len - 1 do
		begin
			arr[i] := (data + i)^;
		end;
	end;

	///-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
	//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
	// 					Exception
	//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
	//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\

	var
		ErrorMessage: String;
		HasException: Boolean;		
	
	function GetExceptionMessage(): PChar; cdecl; export;
	begin
		result := PChar(ErrorMessage);
	end;
	
	function ExceptionOccured(): Integer; cdecl; export;
	begin
		if HasException then result := -1
		else result := 0;
	end;
	
	procedure TrapException(exc: Exception);
	begin
		HasException := true;
		ErrorMessage := exc.Message;
	end;
	
	///-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
	//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
	// 					Core
	//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
	//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
	
	procedure ProcessEvents(); cdecl; export;
	begin
		try
			SGSDK_Core.ProcessEvents();
		Except on exc: Exception do TrapException(exc);
		end;		
	end;

	procedure OpenGraphicsWindow(caption : PChar; width : Integer; height : Integer); cdecl; export;
	begin
		ErrorMessage := '';
		HasException := false;
		
		Try
			SGSDK_Core.OpenGraphicsWindow(caption, width, height);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function WindowCloseRequested(): Integer; cdecl; export;
	begin
		result := -1;
		try
			if SGSDK_Core.WindowCloseRequested() then
			begin
				result:= -1
			end
			else
			begin
				result:= 0
			end;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure SetIcon(iconFilename: PChar); cdecl; export;
	begin
		Try
			SGSDK_Core.SetIcon(iconFilename);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure ChangeScreenSize(width, height: Integer); cdecl; export;
	begin
		Try
			SGSDK_Core.ChangeScreenSize(width, height);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure ToggleFullScreen(); cdecl; export;
	begin
		Try
			SGSDK_Core.ToggleFullScreen();
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure RefreshScreenWithFrame(TargetFPS : Integer); cdecl; export;
	begin
		Try
			SGSDK_Core.RefreshScreen(TargetFPS);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure RefreshScreen(); cdecl; export;
	begin
		Try
			SGSDK_Core.RefreshScreen();
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure TakeScreenshot(basename: PChar); cdecl; export;
	begin
		Try
			SGSDK_Core.TakeScreenshot(basename);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function  ScreenWidth(): Integer; cdecl; export;
	begin
		Try
			result := SGSDK_Core.ScreenWidth();
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	function  ScreenHeight(): Integer; cdecl; export;
	begin
		Try
			result := SGSDK_Core.ScreenHeight();
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	function ToSDLColor(color: UInt32): TSDL_Color; cdecl; export;
	begin		
		Try
			result := SGSDK_Core.ToSDLColor(color);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		
		result.r := 0;
		result.g := 0;
		result.b := 0;		
	end;
	
	function GetColourBitmap(forBitmap: Bitmap; apiColor: Color): Colour; cdecl; export;
	begin
		Try
			result := SGSDK_Core.GetColour(forBitmap, apiColor);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := ColorWhite;
	end;
	
	function GetColourRGBA(red, green, blue, alpha: Byte) : Colour; cdecl; export;
	begin
		Try
			result := SGSDK_Core.GetColour(red, green, blue, alpha);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := ColorWhite;
	end;
	
	function GetFramerate(): Integer; cdecl; export;
	begin
		Try
			result := SGSDK_Core.GetFramerate();
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	function GetTicks(): UInt32; cdecl; export;
	begin
		Try
			result := SGSDK_Core.GetTicks();
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;
	end;
	
	procedure Sleep(time : UInt32); cdecl; export;
	begin
		Try
			SGSDK_Core.Sleep(time);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function GetPathToResourceWithKind(filename: PChar; kind: ResourceKind) : PChar; cdecl; export;
	begin
		Try
			result := PChar(SGSDK_Core.GetPathToResource(filename, kind));
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	function GetPathToResource(filename: PChar): PChar; cdecl; export;
	begin
		Try
			result := PChar(SGSDK_Core.GetPathToResource(filename));
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := '';
	end;

	function GetPathToResourceWithBaseAndKind(path, filename: PChar; kind: ResourceKind) : PChar; cdecl; export;
	begin
		try
			result := PChar(SGSDK_Core.GetPathToResourceWithBase(path, filename, kind));
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := '';
	end;

	function GetPathToResourceWithBase(path, filename: PChar) : PChar; cdecl; export;
	begin
		try
			result := PChar(SGSDK_Core.GetPathToResourceWithBase(path, filename));
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := '';
	end;

{	function Cos(angle: Single): Single; cdecl; export;
	begin
		Try
			result := SGSDK_Core.Cos(angle);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	function Sin(angle: Single): Single; cdecl; export;
	begin
		Try
			result := SGSDK_Core.Sin(angle);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	function Tan(angle: Single): Single; cdecl; export;
	begin
		Try
			result := SGSDK_Core.Tan(angle);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;}	
	
	function CreateTimer() : Timer; cdecl; export;
	begin
		Try
			result := SGSDK_Core.CreateTimer();
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := nil;		
	end;
	
	procedure FreeTimer(var toFree: Timer); cdecl; export;
	begin
		Try
			SGSDK_Core.FreeTimer(toFree);
		Except on exc: Exception do TrapException(exc);
		end;		
	end;
	
	procedure StartTimer(toStart : Timer); cdecl; export;
	begin
		Try
			SGSDK_Core.StartTimer(toStart);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure StopTimer(toStop : Timer); cdecl; export;
	begin
		Try
			SGSDK_Core.StopTimer(toStop);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure PauseTimer(toPause : Timer); cdecl; export;
	begin
		Try
			SGSDK_Core.PauseTimer(toPause);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure UnpauseTimer(toUnpause : Timer); cdecl; export;
	begin
		Try
			SGSDK_Core.UnpauseTimer(toUnpause);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function GetTimerTicks(toGet : Timer) : UInt32; cdecl; export;
	begin
		Try
			result := SGSDK_Core.GetTimerTicks(toGet);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;						
	end;

	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Input
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	procedure ShowMouse(show : Integer); cdecl; export;
	begin
		try
			SGSDK_Input.ShowMouse(show = -1);
		Except on exc: Exception  do TrapException(exc);
		end;
	end;
	
	function IsMouseShown(): Integer ; cdecl; export;
	begin
		try
			if SGSDK_Input.IsMouseShown() then result := -1
			else result := 0;
			exit;
		Except on exc: Exception  do TrapException(exc);
		end;
		result := -1;
	end;
	
	procedure MoveMouse(x : UInt16; y : UInt16); cdecl; export;
	begin
		try
			SGSDK_Input.MoveMouse(x,y);
		Except on exc: Exception  do TrapException(exc);
		end;
	end;

	function GetMousePositionAsVector(): Vector; cdecl; export;
	begin
		Try
			result := SGSDK_Input.GetMousePositionAsVector();
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result.x := 0; result.y := 0;
	end;

	function GetMousePosition(): Point2D; cdecl; export;
	begin
		Try
			result := SGSDK_Input.GetMousePosition();
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result.x := 0; result.y := 0;
	end;
	
	function GetMouseMovement(): Vector; cdecl; export;
	begin
		Try
			result := SGSDK_Input.GetMouseMovement();
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result.x := 0; result.y := 0;
	end;
	
	function IsMouseDown(button: MouseButton): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Input.IsMouseDown(button) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	function IsMouseUp(button: MouseButton): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Input.IsMouseUp(button) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	function MouseWasClicked(button: MouseButton): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Input.MouseWasClicked(button) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;


	procedure StartReadingText(textColor: Colour; maxLength: Integer;
										theFont: Font; x, y: Integer); cdecl; export;
	begin
		Try
			SGSDK_Input.StartReadingText(textColor, maxLength, theFont, x, y);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	function IsReadingText(): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Input.IsReadingText() then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;
	end;
	
	procedure TextReadAsASCII(out txt: PChar); cdecl; export;
	var
		temp: String;
	begin
		temp := '';
		Try
			temp := SGSDK_Input.TextReadAsASCII();
		Except on exc: Exception do TrapException(exc);
		end;
		txt := PChar(temp);
		//len := Length(temp);
	end;
		
	function IsKeyPressed(virtKeyCode : Integer): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Input.IsKeyPressed(virtKeyCode) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;
	end;
	
	function WasKeyTyped(virtKeyCode: Integer): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Input.WasKeyTyped(virtKeyCode) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;
	end;
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Audio
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	
	
	procedure OpenAudio(); cdecl; export;
	begin
		Try
			SGSDK_Audio.OpenAudio();
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure CloseAudio(); cdecl; export;
	begin
		Try
			SGSDK_Audio.CloseAudio();
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function LoadSoundEffect(path: PChar): SoundEffect; cdecl; export;
	begin
		Try
			result := SGSDK_Audio.LoadSoundEffect(path);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := nil;
	end;
	
	function LoadMusic(path: PChar): Music; cdecl; export;
	begin
		Try
			result := SGSDK_Audio.LoadMusic(path);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := nil;
	end;

	procedure FreeMusic(var mus: Music); cdecl; export;
	begin
		Try
			SGSDK_Audio.FreeMusic(mus);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	procedure FreeSoundEffect(var effect: SoundEffect); cdecl; export;
	begin
		Try
			SGSDK_Audio.FreeSoundEffect(effect);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

{	procedure PlaySoundEffect(effect: SoundEffect); cdecl; export;
	begin
		Try
			SGSDK_Audio.PlaySoundEffect(effect);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
}	
	procedure PlaySoundEffectLoop(effect: SoundEffect; loops: Integer); cdecl; export;
	begin
		Try
			SGSDK_Audio.PlaySoundEffect(effect, loops);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	procedure PlayMusic(mus: Music; loops: Integer); cdecl; export;
	begin
		Try
			SGSDK_Audio.PlayMusic(mus, loops);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	function IsMusicPlaying(): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Audio.IsMusicPlaying() then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;
	end;

	function IsSoundEffectPlaying(effect: SoundEffect): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Audio.IsSoundEffectPlaying(effect) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;
	end;

	procedure StopSoundEffect(effect: SoundEffect); cdecl; export;
	begin
		Try
			SGSDK_Audio.StopSoundEffect(effect);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	procedure StopMusic(); cdecl; export;
	begin
		Try
			SGSDK_Audio.StopMusic();
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Font
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	
	function LoadFont(fontName: PChar; size: Integer): Font; cdecl; export;
	begin
		Try
			result := SGSDK_Font.LoadFont(fontName, size);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := nil;
	end;

	procedure SetFontStyle(font: Font; style: FontStyle); cdecl; export;
	begin
		Try
			SGSDK_Font.SetFontStyle(font, style);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	procedure FreeFont(var fontToFree: Font); cdecl; export;
	begin
		Try
			SGSDK_Font.FreeFont(fontToFree);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure DrawText(theText: PChar; textColor: Colour;
					 theFont: Font; x, y: Single); cdecl; export;
	begin
		Try
			SGSDK_Font.DrawText(theText, textColor, theFont, x, y);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	procedure DrawTextLines(theText: PChar; textColor, backColor: Colour;
							theFont: Font; align: FontAlignment;
							x, y: Single; w, h: Integer); cdecl; export;
	begin
		Try
			SGSDK_Font.DrawTextLines(theText, textColor, backColor, theFont, align, x, y, w, h);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure DrawTextOnScreen(theText: PChar; textColor: Colour;
					 theFont: Font; x, y: Integer); cdecl; export;
	begin
		Try
			SGSDK_Font.DrawTextOnScreen(theText, textColor, theFont, x, y);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	procedure DrawTextLinesOnScreen(theText: PChar; textColor, backColor: Colour;
							theFont: Font; align: FontAlignment;
							x, y, w, h: Integer); cdecl; export;
	begin
		Try
			SGSDK_Font.DrawTextLinesOnScreen(theText, textColor, backColor, theFont, align, x, y, w, h);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
							
	procedure DrawTextOnBitmap(dest: Bitmap; theText: PChar; textColor: Colour;
					theFont: Font; x, y: Integer); cdecl; export;
	begin
		Try
			SGSDK_Font.DrawText(dest, theText, textColor, theFont, x, y);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	procedure DrawTextLinesOnBitmap(dest: Bitmap; theText: PChar;
							textColor, backColor: Colour;
							theFont: Font; align: FontAlignment;
							x, y, w, h: Integer); cdecl; export;
	begin
		Try
			SGSDK_Font.DrawTextLines(dest, theText, textColor, backColor, theFont, align, x, y, w, h);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	function TextWidth(theText: PChar; theFont: Font): Integer; cdecl; export;
	begin
		Try
			result :=SGSDK_Font.TextWidth(theText, theFont);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;

	function TextHeight(theText: PChar; theFont: Font): Integer; cdecl; export;
	begin
		Try
			result := SGSDK_Font.TextHeight(theText, theFont);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;

	procedure DrawFramerate(x, y: Integer; font: Font); cdecl; export;
	begin
		Try
			SGSDK_Font.DrawFramerate(x, y, font);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Physics
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	
	function RectangleHasCollidedWithLine(rect: Rectangle; line: LineSegment): integer; cdecl; export;
	begin
		Try
			if SGSDK_Physics.RectangleHasCollidedWithLine(rect, line) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;
	end;
	
	function IsSpriteOnScreenAt(theSprite: Sprite; x, y: Integer): integer; cdecl; export;
	begin
		Try
			if SGSDK_Physics.IsSpriteOnScreenAt(theSprite, x, y) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;
	end;
	
	function CircleHasCollidedWithLine(p1: Sprite; line: LineSegment): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Physics.CircleHasCollidedWithLine(p1, line) then result := 1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;
	end;
	
	function HasSpriteCollidedX(theSprite : Sprite; x : Integer;
								 range : CollisionDetectionRange):  Integer; cdecl; export;
	begin
		Try
			if SGSDK_Physics.HasSpriteCollidedX(theSprite, x, range) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;
	end;

	function HasSpriteCollidedY(theSprite : Sprite; y : Integer;
								 range : CollisionDetectionRange): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Physics.HasSpriteCollidedY(theSprite, y, range) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;
	end;

	function HasSpriteCollidedWithRect(theSprite : Sprite; x, y : Single;
		width, height : Integer): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Physics.HasSpriteCollidedWithRect(theSprite, x, y, width, height) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;
	end;
		
	function HaveSpritesCollided(sprite1, sprite2 : Sprite): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Physics.HaveSpritesCollided(sprite1, sprite2) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;
	end;
	
	function HasSpriteCollidedWithBitmap(theSprite: Sprite; theBitmap: Bitmap; x, y: Single; bounded: Integer): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Physics.HasSpriteCollidedWithBitmap(theSprite, theBitmap, x, y, bounded = -1) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;		
	end;
	
	function HasSpriteCollidedWithBitmapPart(theSprite: Sprite; theBitmap: Bitmap; pt: Point2D; src: Rectangle; bounded: Integer): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Physics.HasSpriteCollidedWithBitmap(theSprite, theBitmap, pt, src, bounded = -1) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;		
	end;
	
	function HaveBitmapsCollided(image1: Bitmap; x1, y1: Integer; bounded1: Integer; image2: Bitmap; x2, y2: Integer; bounded2: Integer): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Physics.HaveBitmapsCollided(image1, x1, y1, bounded1 = -1, image2, x2, y2, bounded2 = -1) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;
	end;
	
	function HaveBitmapPartsCollided(image1: Bitmap; pt1: Point2D; src1: Rectangle; bounded1: Integer; image2: Bitmap; pt2: Point2D; src2: Rectangle; bounded2: Integer): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Physics.HaveBitmapsCollided(image1, pt1, src1, bounded1 = -1, image2, pt2, src2, bounded2 = -1) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;
	end;

{	function CreateVector(x,y : Single; invertY : Integer): Vector;  cdecl; export;
	begin
		Try
			result :=SGSDK_Physics.CreateVector(x, y, invertY = -1);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result.x := 0; result.y := 0;
	end;

	function AddVectors(v1, v2 : Vector): Vector; cdecl; export;
	begin
		Try
			result :=SGSDK_Physics.AddVectors(v1, v2);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result.x := 0; result.y := 0;
	end;

	function SubtractVectors(v1, v2 : Vector): Vector; cdecl; export;
	begin
		Try
			result :=SGSDK_Physics.SubtractVectors(v1, v2);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result.x := 0; result.y := 0;
	end;

	function InvertVector(v : Vector): Vector; cdecl; export;
	begin
		Try
			result :=SGSDK_Physics.InvertVector(v);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result.x := 0; result.y := 0;
	end;
}
	function LimitMagnitude(theVector: Vector; maxMagnitude: Single): Vector; cdecl; export;
	begin
		Try
			result := SGSDK_Physics.LimitMagnitude(theVector, maxMagnitude);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result.x := 0; result.y := 0;
	end;

	function GetUnitVector(theVector : Vector): Vector; cdecl; export;
	begin
		Try
			result :=SGSDK_Physics.GetUnitVector(theVector);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result.x := 0; result.y := 0;
	end;

{	function IsZeroVector(theVector : Vector): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Physics.IsZeroVector(theVector) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;
	end;

	function Magnitude(theVector : Vector): Single; cdecl; export;
	begin
		Try
			result :=SGSDK_Physics.Magnitude(theVector);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	function DotProduct(v1, v2: Vector): Single; cdecl; export;
	begin
		Try
			result :=SGSDK_Physics.DotProduct(v1, v2);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	function MultiplyVector(v1: Vector; s1: Single): Vector; cdecl; export;
	begin
		Try
			result :=SGSDK_Physics.MultiplyVector(v1, s1);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result.x := 0; result.y := 0;
	end;
}
 	function CalculateAngle(x1, y1, x2, y2: Single): Single;  cdecl; export;
	begin
		Try
			result :=SGSDK_Physics.CalculateAngle(x1, y1, x2, y2);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
{	function CalculateAngleSprite(sprite1, sprite2: Sprite): Single;  cdecl; export;
	begin
		Try
			result :=SGSDK_Physics.CalculateAngle(sprite1, sprite2);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	function GetVectorFromAngle(angle, magnitude: Single): Vector; cdecl; export;
	begin
		Try
			result := SGSDK_Physics.GetVectorFromAngle(angle, magnitude);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result.x := 0; result.y := 0;
	end;
}
	function TranslationMatrix(dx, dy: Single): Matrix2DPtr; cdecl; export;
	var
		p: Matrix2DPtr;
	begin
		Try
			new(p);
			p^ := SGSDK_Physics.TranslationMatrix(dx, dy);
			result := p;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := nil;
	end;
	
	function ScaleMatrix(scale: Single): Matrix2DPtr; cdecl; export;
	var
		p: Matrix2DPtr;
	begin
		Try
			new(p);
			p^ := SGSDK_Physics.ScaleMatrix(scale);
			result := p;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := nil;
	end;
	
	function RotationMatrix(deg: Single): Matrix2DPtr; cdecl; export;
	var
		p: Matrix2DPtr;
	begin
		Try	
			new(p);
			p^ := SGSDK_Physics.RotationMatrix(deg);
			result := p;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := nil;
	end;
	
	function MultiplyMatrix2D(m1, m2: Matrix2DPtr): Matrix2DPtr; cdecl; export;
	var
		p: Matrix2DPtr;
	begin
		Try
			new(p);
			p^ := SGSDK_Physics.Multiply(m1^, m2^);
			result := p;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := nil;
	end;
	
	function MultiplyMatrix2DAndVector(m: Matrix2DPtr; v: Vector): Vector; cdecl; export;
	begin
		Try
			result := SGSDK_Physics.Multiply(m^, v);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result.x := 0; result.y := 0;
	end;
	
	procedure VectorCollision(p1, p2 : Sprite); cdecl; export;
	begin
		Try
			SGSDK_Physics.VectorCollision(p1, p2);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure CircleCollisionWithLine(p1: Sprite; line: LineSegment); cdecl; export;
	begin
		Try
			SGSDK_Physics.CircleCollisionWithLine(p1, line);
		Except on exc: Exception do TrapException(exc);
		end;		
	end;

	procedure CircularCollision(p1, p2: Sprite); cdecl; export;
	begin
		Try
			SGSDK_Physics.CircularCollision(p1, p2);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	function GetMatrix2DElement(matrix: Matrix2DPtr; x, y: Integer): Single; cdecl; export;
	begin
		Try
			result := matrix^[x,y];
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	procedure SetMatrix2DElement(matrix: Matrix2DPtr; x, y: Integer; val: Single); cdecl; export;
	begin
		Try
			matrix^[x,y] := val;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure FreeMatrix2D(var matrix: Matrix2DPtr); cdecl; export;
	begin
		Try
			if Assigned(matrix) then Dispose(matrix);
			matrix := nil;
		Except on exc: Exception do TrapException(exc);
		end;
	end;	

	function VectorOutOfCircleFromPoint(pnt, center: Point2D; radius: Single; movement: Vector): Vector; cdecl; export;
	begin
		Try
			result := SGSDK_Physics.VectorOutOfCircleFromPoint(pnt, center, radius, movement);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := CreateVector(0,0);		
	end;
	
	function VectorOutOfCircleFromCircle(pnt: Point2D; radius: Single; center: Point2D; radius2: Single; movement: Vector): Vector; cdecl; export;
	begin
		Try
			result := SGSDK_Physics.VectorOutOfCircleFromCircle(pnt, radius, center, radius2, movement);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := CreateVector(0,0);				
	end;
	
	function VectorOutOfRectFromPoint(pnt: Point2D; rect: Rectangle; movement: Vector): Vector; cdecl; export;
	begin
		Try
			result := SGSDK_Physics.VectorOutOfRectFromPoint(pnt, rect, movement);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := CreateVector(0,0);
	end;
	
	function VectorOutOfRectFromRect(const srcRect, targetRect: Rectangle; const movement: Vector) : Vector; cdecl; export;
	begin
		Try
			result := SGSDK_Physics.VectorOutOfRectFromRect(srcRect, targetRect, movement);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := CreateVector(0,0);
	end;

	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Graphics
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	
	function GetSpriteBitmap(surface: Sprite; id: Integer): Bitmap; cdecl; export;
	begin
		Try
			result := surface.bitmaps[id];
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := nil;
	end;
	
	function GetSpriteX(surface: Sprite): Single; cdecl; export;
	begin
		Try
			result := surface.x;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	procedure SetSpriteX(surface: Sprite; val: Single); cdecl; export;
	begin
		Try
			surface.x := val;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function GetSpriteY(surface: Sprite): Single; cdecl; export;
	begin
		Try
			result := surface.y;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	procedure SetSpriteY(surface: Sprite; val: Single); cdecl; export;
	begin
		Try
			surface.y := val;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function GetSpriteCurrentFrame(surface: Sprite): Integer; cdecl; export;
	begin
		Try
			result := surface.currentFrame;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	procedure SetSpriteCurrentFrame(surface: Sprite; val: Integer); cdecl; export;
	begin
		Try
			surface.currentFrame := val;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function GetSpriteUsePixelCollision(surface: Sprite): Integer; cdecl; export;
	begin
		Try
			if surface.usePixelCollision then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;
	end;
	
	procedure SetSpriteUsePixelCollision(surface: Sprite; val: Integer); cdecl; export;
	begin
		Try
			surface.usePixelCollision := val = -1;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
		
	function CreateBitmap(width, height: Integer): Bitmap; cdecl; export;
	begin
		Try
			result := SGSDK_Graphics.CreateBitmap(width, height);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := nil;
	end;
	
	procedure OptimiseBitmap(surface: Bitmap); cdecl; export;
	begin
		Try
			SGSDK_Graphics.OptimiseBitmap(surface);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function LoadBitmapWithTransparentColor(pathToBitmap: PChar; transparent: Integer; transparentColor: Colour): Bitmap; cdecl; export;
	begin
		Try
			result := SGSDK_Graphics.LoadBitmap(pathToBitmap, transparent = -1, transparentColor);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := nil;
	end;
	
	function LoadTransparentBitmap(pathToBitmap : PChar; transparentColor : Colour): Bitmap; cdecl; export;
	begin
		Try
			result := SGSDK_Graphics.LoadTransparentBitmap(pathToBitmap, transparentColor);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := nil;
	end;
	
	procedure FreeBitmap(var bitmapToFree : Bitmap); cdecl; export;
	begin
		Try
			SGSDK_Graphics.FreeBitmap(bitmapToFree);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function GetBitmapWidth(targetbitmap : Bitmap): Integer ; cdecl; export;
	begin
		Try
			result := targetbitmap.width;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	function GetBitmapHeight(targetbitmap : Bitmap): Integer; cdecl; export;
	begin
		Try
			result := targetbitmap.height;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	procedure ClearSurfaceWithColor(dest: Bitmap; toColour: Colour); cdecl; export;
	begin
		Try
			SGSDK_Graphics.ClearSurface(dest, toColour);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure DrawBitmapWithDestination(dest: Bitmap; bitmapToDraw: Bitmap; x, y : Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawBitmap(dest, bitmapToDraw, x, y);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure DrawBitmapPartWithDestination(dest: Bitmap; bitmapToDraw: Bitmap;
							srcX, srcY, srcW, srcH, x, y : Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawBitmapPart(dest, bitmapToDraw, srcX, srcY, srcW, srcH, x, y);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure DrawPixelWithDestination(dest: Bitmap; theColour: Colour; x, y: Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawPixel(dest, theColour, x, y);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure DrawRectangleWithDestination(dest: Bitmap; theColour : Colour; filled : Integer;
							xPos, yPos, width, height : Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawRectangle(dest, theColour, filled = -1, xPos, yPos, width, height);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure FillRectangleWithDestination(dest: Bitmap; theColour : Colour; xPos, yPos,
							width, height : Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.FillRectangle(dest, theColour, xPos, yPos, width, height);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	procedure DrawLineWithDestination(dest: Bitmap; theColour: Colour; xPosStart, yPosStart,
					 xPosEnd, yPosEnd: Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawLine(dest, theColour, xPosStart, yPosStart, xPosEnd, yPosEnd);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	procedure DrawHorizontalLineWithDestination(dest: Bitmap; theColor: Color;
								 y, x1, x2: Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawHorizontalLine(dest, theColor, y, x1, x2);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	procedure DrawVerticalLineWithDestination(dest: Bitmap; theColor: Color;
							 x, y1, y2: Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawVerticalLine(dest, theColor, x, y1, y2);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	procedure DrawCircleWithDestination(dest: Bitmap; theColour: Colour; filled: Integer;
							 xc, yc, radius: Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawCircle(dest, theColour, filled = -1, xc, yc, radius);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	procedure DrawEllipseWithDestination(dest: Bitmap; theColour: Colour; filled: Integer;
							xPos, yPos, width, height: Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawEllipse(dest, theColour, filled = -1, xPos, yPos, width, height);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	procedure ClearScreen(toColour : Colour); cdecl; export;
	begin
		Try
			SGSDK_Graphics.ClearScreen(toColour);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	procedure DrawBitmap(bitmapToDraw : Bitmap; x, y : Single); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawBitmap(bitmapToDraw, x, y);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	procedure DrawBitmapPart(bitmapToDraw : Bitmap;
							srcX, srcY, srcW, srcH : Integer; x, y : Single); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawBitmapPart(bitmapToDraw, srcX, srcY, srcW, srcH, x, y);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure DrawPixel(theColour: Colour; x, y: Single); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawPixel(theColour, x, y);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	procedure DrawRectangle(theColour : Colour; filled : Integer;
							xPos, yPos: Single; width, height : Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawRectangle(theColour, filled = -1, xPos, yPos, width, height);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	procedure DrawLine(theColour: Colour; xPosStart, yPosStart,
					 xPosEnd, yPosEnd: Single); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawLine(theColour, xPosStart, yPosStart, xPosEnd, yPosEnd);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	procedure DrawHorizontalLine(theColor: Color; y, x1, x2: Single); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawHorizontalLine(theColor, y, x1, x2);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	procedure DrawVerticalLine(theColor: Color; x, y1, y2: Single); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawVerticalLine(theColor, x, y1, y2);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	procedure DrawCircle(theColour: Colour; filled: Integer;
						 xc, yc: Single; radius: Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawCircle(theColour, filled = -1, xc, yc, radius);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	procedure DrawEllipse(theColour: Colour; filled: Integer;
						xPos, yPos: Single; width, height: Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawEllipse(theColour, filled = -1, xPos, yPos, width, height);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// 								Animated Sprite Additions
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	function CreateSprite(startBitmap : Bitmap): Sprite; cdecl; export;
	begin
		Try
			result := SGSDK_Graphics.CreateSprite(startBitmap);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := nil;
	end;
	
	function CreateSpriteMultiFPC(image: Bitmap; framesPerCell, frames, width, height: Integer): Sprite; cdecl; export;
	begin
		Try
			result := SGSDK_Graphics.CreateSprite(image,framesPerCell, frames, width, height);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := nil;
	end;
	
	function CreateSpriteArrayFPC(bitLength: Integer; bitmaps: BitmapPtr; framesPerCell, frames: Integer): Sprite; cdecl; export;
	var
		bmps: BitmapArray;
	begin
		Try
			PopulateBitmapArray(bitmaps, bitLength, bmps);
			
			result := SGSDK_Graphics.CreateSprite(bmps, framesPerCell, frames);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := nil;
	end;
	
	function CreateSpriteMultiEnding(image : Bitmap; isMulti, length: Integer; framesPerCell: IntPtr; endingAction : SpriteEndingAction; width : Integer; height : Integer): Sprite; cdecl; export;
	var
		fpc: IntArray;
	begin
		Try	
			PopulateIntArray(framesPerCell, length, fpc);

			result := SGSDK_Graphics.CreateSprite(image, isMulti = -1, fpc, endingAction, width, height);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := nil;
	end;
	
	function CreateSpriteMulti(image : Bitmap; isMulti, length : Integer; framesPerCell : IntPtr; width, height : Integer): Sprite; cdecl; export;
	var
		fpc: IntArray;
	begin
		Try
			PopulateIntArray(framesPerCell, length, fpc);
			
			result := SGSDK_Graphics.CreateSprite(image, isMulti = -1, fpc, width, height);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := nil;
	end;

	function CreateSpriteArrayEnding(bitLength: Integer; bitmaps: BitmapPtr; length: Integer; framesPerCell: IntPtr; endingAction: SpriteEndingAction): Sprite; cdecl; export;
	var
		fpc: IntArray;
		bmps: BitmapArray;
	begin
		Try
			PopulateBitmapArray(bitmaps, bitLength, bmps);
			PopulateIntArray(framesPerCell, length, fpc);
			
			result := SGSDK_Graphics.CreateSprite(bmps, fpc, endingAction);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := nil;
	end;
	
	function CreateSpriteArray(bitlength: Integer; bitmaps: BitmapPtr; length: Integer; framesPerCell: IntPtr): Sprite; cdecl; export;
	var
		fpc: IntArray;
		bmps: BitmapArray;
	begin
		Try
			PopulateBitmapArray(bitmaps, bitLength, bmps);
			PopulateIntArray(framesPerCell, length, fpc);
			
			result := SGSDK_Graphics.CreateSprite(bmps, fpc);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := nil;
	end;
	
	procedure UpdateSpriteAnimation(spriteToDraw : Sprite); cdecl; export;
	begin
		Try
			SGSDK_Graphics.UpdateSpriteAnimation(spriteToDraw);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure UpdateSprite(spriteToDraw : Sprite); cdecl; export;
	begin
		Try
			SGSDK_Graphics.UpdateSprite(spriteToDraw);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function GetSpriteKind(surface : Sprite): Integer; cdecl; export;
	begin
		Try
			result := Integer(surface.spriteKind);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	procedure SetSpriteKind(surface : Sprite; kind : SpriteKind); cdecl; export;
	begin
		Try
			surface.SpriteKind := kind;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure SetSpriteFramesPerCell(surface: Sprite; framesPerCell: IntPtr; length: Integer); cdecl; export;
	var
		fpc: IntArray;
	begin
		Try
			PopulateIntArray(framesPerCell, length, fpc);
			
			surface.framesPerCell := fpc;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function GetSpriteFramesPerCell(surface : Sprite; ind : Integer): Integer; cdecl; export;
	begin
		Try
			result := surface.framesPerCell[ind];
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	function GetSpriteCols(surface : Sprite) : Integer; cdecl; export;
	begin
		Try
			result := surface.cols;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	function GetSpriteRow(surface : Sprite) : Integer; cdecl; export;
	begin
		Try
			result := surface.row;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	function GetSpriteFrameCount(surface : Sprite) : Integer; cdecl; export;
	begin
		Try
			result := Length(surface.framesperCell);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	function GetSpriteEndingAction(surface : Sprite) : Integer; cdecl; export;
	begin
		Try
			result := Integer(surface.endingAction);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	procedure SetSpriteendingAction(surface : Sprite; endingAction : SpriteEndingAction); cdecl; export;
	begin
		Try
			surface.endingAction := endingAction;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function GetSpritehasEnded(surface : Sprite) : Integer; cdecl; export;
	begin
		Try
			if surface.hasEnded then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	function GetSpriteReverse(surface : Sprite) : Integer; cdecl; export;
	begin
		Try
			if surface.reverse then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1
	end;
	
	function GetSpriteMass(surface : Sprite) : Single; cdecl; export;
	begin
		Try
			result := surface.mass;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	function GetSpriteMovement(surface : Sprite) : Vector; cdecl; export;
	begin
		Try
			result := surface.movement;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result.x := 0; result.y := 0;
	end;
	
	procedure SetSpriteMass(surface : Sprite; mass : Single); cdecl; export;
	begin
		Try
			surface.mass := mass;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure SetSpriteMovement(surface: Sprite; v : Vector); cdecl; export;
	begin
		Try
			surface^.movement := v;
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	procedure FreeSprite(var spriteToFree : Sprite); cdecl; export;
	begin
		Try
			SGSDK_Graphics.FreeSprite(spriteToFree);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	function AddBitmapToSprite(spriteToAddTo : Sprite;
							   bitmapToAdd : Bitmap): Integer; cdecl; export;
	begin
		Try
			result := SGSDK_Graphics.AddBitmapToSprite(spriteToAddTo, bitmapToAdd);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;

	function CurrentHeight(sprite: Sprite): Integer; cdecl; export;
	begin
		Try
			result := SGSDK_Graphics.CurrentHeight(sprite);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;

	function CurrentWidth(sprite: Sprite): Integer; cdecl; export;
	begin
		Try
			result := SGSDK_Graphics.CurrentWidth(sprite);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;

	procedure DrawSprite(spriteToDraw : Sprite); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawSprite(spriteToDraw);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure DrawSpriteOffSet(spriteToDraw : Sprite; xOffset, yOffset: Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawSprite(spriteToDraw, xOffset, yOffset);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	procedure MoveSpriteItself(sprite: Sprite); cdecl; export;
	begin
		Try
			SGSDK_Graphics.MoveSprite(sprite);
		Except on exc: Exception do TrapException(exc);
		end;		
	end;

	procedure MoveSprite(spriteToMove : Sprite; movementVector : Vector); cdecl; export;
	begin
		Try
			SGSDK_Graphics.MoveSprite(spriteToMove, movementVector);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	procedure MoveSpriteTo(spriteToMove : Sprite; x,y : Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.MoveSpriteTo(SpriteToMove, x, y);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	function IsSpriteOffscreen(theSprite : Sprite): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Graphics.IsSpriteOffscreen(theSprite) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	procedure ReplayAnimation(theSprite : Sprite);cdecl; export;
	begin
		Try
			SGSDK_Graphics.ReplayAnimation(theSprite);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure DrawBitmapPartOnScreen(bitmapToDraw : Bitmap; srcX, srcY, srcW, srcH, x, y : Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawBitmapPartOnScreen(bitmapToDraw, srcX, srcY, srcW, srcW, x ,y);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure DrawBitmapOnScreen(bitmapToDraw : Bitmap; x, y : Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawBitmapOnScreen(bitmapToDraw, x ,y );
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure DrawPixelOnScreen(theColour: Colour; x, y: Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawPixelOnScreen(theColour, x ,y);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure DrawRectangleOnScreen(theColour : Colour; filled : Integer;
							xPos, yPos, width, height : Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawRectangleOnScreen(theColour, filled = -1, xPos, yPos, width, height);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	procedure DrawLineOnScreen(theColour: Colour; xPosStart, yPosStart,
					 xPosEnd, yPosEnd: Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawLineOnScreen(theColour, xPosStart, yPosStart, xPosEnd, yPosEnd);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure DrawHorizontalLineOnScreen(theColor: Color; y, x1, x2: Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawHorizontalLineOnScreen(theColor, y, x1, x2);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure DrawVerticalLineOnScreen(theColor: Color; x, y1, y2: Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawVerticalLineOnScreen(theColor, x, y1, y2);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure DrawCircleOnScreen(theColour: Colour; filled: Integer;
						 xc, yc, radius: Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawCircleOnScreen(theColour, filled = -1, xc, yc, radius);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure DrawEllipseOnScreen(theColour: Colour; filled: Integer;
						xPos, yPos, width, height: Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawEllipseOnScreen(theColour, filled = -1, xPos, yPos, width, height);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function XOffset(): Integer; cdecl; export;
	begin
		Try
			result := SGSDK_Camera.XOffset();
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;
	end;
	
	function YOffset(): Integer; cdecl; export;
	begin
		Try
			result := SGSDK_Camera.YOffset();
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;
	end;
	
	function ScreenX(x: Single): Integer; cdecl; export;
	begin
		Try
			result := SGSDK_Camera.ScreenX(x);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;
	end;
	
	function ScreenY(y: Single): Integer; cdecl; export;
	begin
		Try
			result := SGSDK_Camera.ScreenY(y);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;
	end;
	
	function GameX(x: Integer) : Single; cdecl; export;
	begin
		Try
			result := SGSDK_Camera.GameX(x);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;
	end;
	
	function GameY(y: Integer) : Single; cdecl; export;
	begin
		Try
			result := SGSDK_Camera.GameY(y);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;
	end;
	
	function ToGameCoordinates(screenPoint: Point2D): Point2D; cdecl; export;
	begin
		Try
			result := SGSDK_Camera.ToGameCoordinates(screenPoint);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result.x := 0; result.y := 0;
	end;
	
{	procedure MoveVisualAreaWithVector(v: Vector); cdecl; export;
	begin
		Try
			SGSDK_Camera.MoveVisualArea(v);
		Except on exc: Exception do TrapException(exc);
		end;
	end;}	
	
	procedure MoveVisualArea(dx, dy: Single); cdecl; export;
	begin
		Try
			SGSDK_Camera.MoveVisualArea(dx, dy);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure SetScreenOffset(x, y: Single); cdecl; export;
	begin
		Try
			SGSDK_Camera.SetScreenOffset(x, y);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure FollowSprite(spr : Sprite; xOffset, yOffset : Integer); cdecl; export;
	begin
		Try
			SGSDK_Camera.FollowSprite(spr, xOffset, yOffset);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure SetClip(bmp: Bitmap; x, y, w, h: Integer); cdecl; export;
	begin
		Try
			if bmp <> nil then SGSDK_Graphics.SetClip(bmp, x, y, w, h)
			else SGSDK_Graphics.SetClip(x, y, w, h);
		Except on exc: Exception do TrapException(exc);
		end;		
	end;

	procedure ResetClip(bmp: Bitmap); cdecl; export;
	begin
		Try
			if bmp <> nil then SGSDK_Graphics.ResetClip(bmp)
			else SGSDK_Graphics.ResetClip();
		Except on exc: Exception do TrapException(exc);
		end;		
	end;
		
	///-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
	//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
	// 					MappyLoader
	//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
	//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
	
	function LoadMap(mapFile, imgFile : PChar): Map; cdecl; export;
	begin
		Try
			result := SGSDK_MappyLoader.LoadMapFiles(mapFile, imgFile);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := nil;
	end;
	
	procedure DrawMap(m : Map); cdecl; export;
	begin
		Try
			SGSDK_MappyLoader.DrawMap(m);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function CollisionWithMapVector(m : Map; spr : Sprite; vec: Vector): CollisionSide; cdecl; export;
	begin
		Try
			result := SGSDK_MappyLoader.CollisionWithMap(m, spr,vec);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := CollisionSide(-1);
	end;
	
	function EventCount(m : Map; event : Event): Integer; cdecl; export;
	begin
		Try
			result := SGSDK_MappyLoader.EventCount(m, event);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	function EventPositionX(m : Map; event : Event; eventnumber : Integer): Integer; cdecl; export;
	begin
		Try
			result := SGSDK_MappyLoader.EventPositionX(m, event, eventnumber);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	function EventPositionY(m : Map; event : Event; eventnumber : Integer): Integer; cdecl; export;
	begin
		Try
			result := SGSDK_MappyLoader.EventPositionY(m, event, eventnumber);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	procedure FreeMap(var m : Map); cdecl; export;
	begin
		Try
			SGSDK_MappyLoader.FreeMap(m);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function SpriteHasCollidedWithMapTile(m : Map; spr : Sprite; out collidedX, collidedY : Integer): Integer; cdecl; export;
	begin
		Try
			if SGSDK_MappyLoader.SpriteHasCollidedWithMapTile(m, spr, collidedX, collidedY) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function WillCollideOnSide(m: Map; spr : Sprite): CollisionSide; cdecl; export;
	begin
		Try
			result := SGSDK_MappyLoader.WillCollideOnSide(m, spr);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure MoveSpriteOutOfTile(m: Map; spr : Sprite; x, y : Integer); cdecl; export;
	begin
		Try
			SGSDK_Mappyloader.MoveSpriteOutOfTile(m, spr, x, y);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	///-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
	//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
	// 					Shapes
	//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
	//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\

	function DistancePointToLine(x, y: Single; line: LineSegment): Single; cdecl; export;
	begin
		Try
			result := SGSDK_Shapes.DistancePointToLine(x, y, line);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;	
		result := -1;	
	end;
	
	function ClosestPointOnLine(x, y: Single; const line: LineSegment): Point2D; cdecl; export;
	begin
		Try
			result := SGSDK_Shapes.ClosestPointOnLine(x, y, line);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;	
		result.x := -1;			
		result.y := -1;
	end;
	
	function CenterPoint(sprt: Sprite): Point2D; cdecl; export;
	begin
		Try
			result := SGSDK_Shapes.CenterPoint(sprt);
			exit;
		Except on exc: Exception do TrapException(exc);
		end;	
		result.x := -1;			
		result.y := -1;		
	end;
	
	function IsPointOnLine(pnt: Point2D; line: LineSegment): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Shapes.IsPointOnLine(pnt, line) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;	
		result := 0;		
	end;
	
	function GetLineIntersectionPoint(line1, line2: LineSegment; out pnt: Point2D) : integer; cdecl; export;
	begin
		Try
			if SGSDK_Shapes.GetLineIntersectionPoint(line1, line2, pnt) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;	
		result := 0;
	end;
	
	function LineIntersectsWithLines(target: LineSegment; len: Integer; data: LineSegPtr): integer; cdecl; export;
	var
		arr: LinesArray;
	begin
		Try
			PopulateLineSegmentArray(data, len, arr);
			
			if SGSDK_Shapes.LineIntersectsWithLines(target, arr) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;	
		result := 0;
	end;
	
	function HasBitmapCollidedWithRect(image: Bitmap; x, y, rectX, rectY, rectWidth, rectHeight: Integer): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Physics.HasBitmapCollidedWithRect(image, x, y, rectX, rectY, rectWidth, rectHeight) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;
	end;

{$ifdef UNIX}
	{$ifndef DARWIN}
end.
	{$endif}
{$endif}

exports

	///-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
	//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
	// 					Core
	//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
	//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
	
	OpenGraphicsWindow,
	WindowCloseRequested,
	ProcessEvents,
	SetIcon,
	ChangeScreenSize,
	ToggleFullScreen,
	RefreshScreenWithFrame,
	RefreshScreen,
	TakeScreenShot,
	ScreenWidth,
	ScreenHeight,
	ToSDLColor,
	GetColourBitmap,	
	GetColourRGBA,
	GetFramerate,	
	GetTicks,	
	Sleep,	
	GetPathToResourceWithKind,
	GetPathToResource,
	GetPathToResourceWithBase,
	GetPathToResourceWithBaseAndKind,
	RegisterEventProcessor,	
{	Cos,
	Sin,
	Tan,}	
	CreateTimer, {1.1}
	FreeTimer, {1.1}
	StartTimer, {1.1}
	StopTimer, {1.1}
	PauseTimer, {1.1}
	UnpauseTimer, {1.1}
	GetTimerTicks, {1.1}
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Input
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************

	GetMousePositionAsVector,
	GetMousePosition,
	GetMouseMovement,
	IsMouseDown,
	IsMouseUp,
	MouseWasClicked,
	StartReadingText,
	IsReadingText,
	TextReadAsASCII,
	IsKeyPressed,
	WasKeyTyped,
	ShowMouse,
	MoveMouse,
	IsMouseShown,
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Audio
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	
	OpenAudio,
	CloseAudio,
	LoadSoundEffect,
	LoadMusic,
	FreeMusic,
	FreeSoundEffect,
	PlayMusic,
	PlaySoundEffectLoop,
	IsMusicPlaying,
	IsSoundEffectPlaying,
	StopSoundEffect,
	StopMusic,
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Font
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	
	LoadFont,
	SetFontStyle,
	FreeFont,
	DrawText,
	DrawTextLines,
	DrawTextOnBitmap,
	DrawTextLinesOnBitmap,
	DrawTextOnScreen,
	DrawTextLinesOnScreen,
	TextWidth,
	TextHeight,
	DrawFramerate,
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Physics
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	HasSpriteCollidedX,
	HasSpriteCollidedY,
	HasSpriteCollidedWithRect,
	HaveSpritesCollided,
	HasSpriteCollidedWithBitmap, {1.1}
	HasSpriteCollidedWithBitmapPart, {1.1}
	HaveBitmapsCollided,
	HaveBitmapPartsCollided,
{	CreateVector,
	AddVectors,
	SubtractVectors,
	InvertVector,}
	CircleHasCollidedWithLine,
	RectangleHasCollidedWithLine,
	VectorOutOfRectFromPoint,	
	VectorOutOfRectFromRect,
	LimitMagnitude,
	GetUnitVector,
	//IsZeroVector,
	//Magnitude,
	//DotProduct,
	//MultiplyVector,
	CalculateAngle,
	//CalculateAngleSprite,
	TranslationMatrix,
	ScaleMatrix,
	RotationMatrix,
	MultiplyMatrix2D,
	MultiplyMatrix2DAndVector,
	VectorCollision,
	GetMatrix2DElement,
	SetMatrix2DElement,
	FreeMatrix2D,
	//GetVectorFromAngle,
	VectorOutOfCircleFromPoint, {1.1}
	VectorOutOfCircleFromCircle, {1.1}
	CircleCollisionWithLine, {1.1}
	CircularCollision, {1.1}
	HasBitmapCollidedWithRect,
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Graphics
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	GetSpriteBitmap,
	GetSpriteX,
	GetSpriteY,
	GetSpriteCurrentFrame,
	GetSpriteUsePixelCollision,
	SetSpriteX,
	SetSpriteY,
	SetSpriteCurrentFrame,
	SetSpriteUsePixelCollision,
	GetSpriteMass,
	GetSpriteMovement,
	SetSpriteMass,
	SetSpriteMovement,
	CreateBitmap,
	OptimiseBitmap,
	LoadBitmapWithTransparentColor,
	LoadTransparentBitmap,
	FreeBitmap,
	GetBitmapWidth,
	GetBitmapHeight,
	ClearSurfaceWithColor,
	DrawBitmapWithDestination,
	DrawBitmapPartWithDestination,
	DrawPixelWithDestination,
	DrawRectangleWithDestination,
	DrawLineWithDestination,
	DrawHorizontalLineWithDestination,
	DrawVerticalLineWithDestination,
	DrawCircleWithDestination,
	DrawEllipseWithDestination,
	ClearScreen,
	DrawBitmap,
	DrawBitmapPart,
	DrawPixel,
	DrawRectangle,
	DrawLine,
	DrawHorizontalLine,
	DrawVerticalLine,
	DrawCircle,
	DrawEllipse,
	CreateSprite,
	CreateSpriteMulti,
	CreateSpriteMultiEnding,
	CreateSpriteArray,
	CreateSpriteArrayEnding,
	CreateSpriteMultiFPC,
	CreateSpriteArrayFPC,
	UpdateSpriteAnimation,
	UpdateSprite,
	GetSpriteKind,	
	GetSpriteFramesPerCell,
	GetSpriteCols,
	GetSpriteRow,
	GetSpriteFrameCount,
	GetSpriteEndingAction,
	GetSpritehasEnded,
	GetSpriteReverse,
	ReplayAnimation,
	SetSpriteKind,
	SetSpriteFramesPerCell,
	SetSpriteendingAction,
	FreeSprite,
	AddBitmapToSprite,
	CurrentHeight,
	CurrentWidth,
	DrawSprite,
	DrawSpriteOffset,
	MoveSpriteItself, {1.0 - missing added 1.1}
	MoveSprite,
	MoveSpriteTo,
	IsSpriteOffscreen,
	IsSpriteOnScreenAt,
	DrawBitmapPartOnScreen,
	DrawBitmapOnScreen,
	DrawPixelOnScreen,
	DrawRectangleOnScreen,
	DrawLineOnScreen,
	DrawHorizontalLineOnScreen,
	DrawVerticalLineOnScreen,
	DrawCircleOnScreen,
	DrawEllipseOnScreen,
	XOffset,
	YOffset,
	ScreenX,
	ScreenY,
	GameX,
	GameY,
	ToGameCoordinates,
	//MoveVisualAreaWithVector,
	MoveVisualArea,
	SetScreenOffset,
	FollowSprite,
	SetClip,
	ResetClip,
	
	///-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
	//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
	// 					MappyLoader
	//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
	//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
	LoadMap,
	DrawMap,
	CollisionWithMapVector,
	EventCount,
	EventPositionX,
	EventPositionY,
	FreeMap,
	//v1.1
	SpriteHasCollidedWithMapTile,
	WillCollideOnSide,
	MoveSpriteOutOfTile,

	///-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
	//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
	// 					Shapes
	//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
	//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\

	DistancePointToLine, {1.1}
	ClosestPointOnLine, {1.1}
	CenterPoint, {1.1}
	IsPointOnLine, {1.1}
	GetLineIntersectionPoint, {1.1}
	LineIntersectsWithLines, {1.1}
	

	///-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
	//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
	// 					Error Handling
	//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
	//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
	GetExceptionMessage,
	ExceptionOccured;
end.

