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
// Version 2.0:
// - 2008-12-17: Andrew: Moved "new" methods to the bottom... makes it easier to identify new functionality for other code (C#).
//                       Added new functions and procedures, to map to new functionality
//                       Replaced all platform specific integers to LongInt (i.e. int32)
//                       Remove const parameters - they were not passed by reference in calling code!
// - 2008-12-12: Andrew: Added simple string drawing
// - 2008-12-10: Andrew: Changed Triangle to Array.
//  
//  Version 1.1.6 - in progress
//	 - 2008-05-09: Andrew: Added debug tracing code
//   - 2008-04-19: Aki: Added DrawTriangle method
//   - 2008-04-19: Aki: Added methods for new mappy loader and shapes methods
//  Version 1.1.5 - released
//   - 2008-04-18: Andrew: 	Added version number
//													Added EndReadingText
//
//  Pre Version 1.1.5...
//  - 2008-04-02: Andrew: Fixed error reporting, 
//                        Fixed returning strings - now uses buffer
//												Fixed calles to Free
//  - 2008-03-10: Andrew: Fixed case of TakeScreenshot
//  - 2008-03-09: Andrew: Added extra exception handling data
//                       Added DrawSprite offsets
//  - 2008-02-22: Andrew: Changed to GetMouseXY
//  - 2008-02-16: Andrew: Added GetPixel and GetPixelFromScreen
//  Version 1.1:
//  - 2008-01-31: Stephen: Fixed SetSpriteEndingAction
//	- 2008-01-30: James: CircleHasCollidedWithLine changed to return -1
//  - 2008-01-30: Andrew: Fixed String Marshalling and Free
//  - 2008-01-29: Andrew: Removed var from the Free routines
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

{$I SwinGame.inc}

library SGSDK;

uses 
	SGSDK_Core, SGSDK_Input, SGSDK_Audio, SGSDK_Font, SGSDK_Physics, SGSDK_Graphics,
	SDL_Mixer, SDL, SDL_Image, SDL_TTF, SGSDK_Camera, SGSDK_Shapes, 
	SGSDK_MappyLoader, SysUtils, Strings, SwinGameTrace;

	type
		//IntPtr used to receive arrays
		IntPtr = ^LongInt;
		//BitmapPtr used to receive Bitmap arrays
		BitmapPtr = ^Bitmap;
		Point2DPtr = ^Point2D;
		
		Matrix2DData = record
				data: Matrix2D;
				free: Boolean;
			end;
		
		Matrix2DPtr = ^Matrix2DData;
		LineSegPtr = ^LineSegment;

		IntArray = Array of LongInt;
		BitmapArray = Array of Bitmap;


	///-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
	//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
	// 					Helper Methods
	//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
	//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\


	//Version number: MMmmrr
	// Major = MM
	// Minor = mm
	// Revision = rr
	
	function DLLVersion(): LongInt; cdecl; export;
	begin
		result := 20000;
	end;
	

	/// Copies the data from an array passed as a pointer
	/// into a Pascal array
	procedure PopulateIntArray(data: IntPtr; len: LongInt; out arr: IntArray);
	var
		i: LongInt;
	begin
		if len < 0 then raise Exception.Create('Length of array cannot be negative');
			
		SetLength(arr, len);

		for i := 0 to len - 1 do
		begin
			arr[i] := (data + i)^;
		end;
	end;
	
	procedure PopulateBitmapArray(data: BitmapPtr; len: LongInt; out arr: BitmapArray);
	var
		i: LongInt;
	begin
		if len < 0 then raise Exception.Create('Length of array cannot be negative');

		SetLength(arr, len);

		for i := 0 to len - 1 do
		begin
			arr[i] := (data + i)^;
		end;
	end;

	procedure PopulateLineSegmentArray(data: LineSegPtr; len: LongInt; out arr: LinesArray);
	var
		i: LongInt;
	begin
		if len < 0 then raise Exception.Create('Length of array cannot be negative');

		SetLength(arr, len);

		for i := 0 to len - 1 do
		begin
			arr[i] := (data + i)^;
		end;
	end;
	
	procedure PopulateTriangle(data: Point2DPtr; out tri: Triangle);
	var
	  i: LongInt;
	begin
    for i := 0 to 2 do
    begin
      tri[i] := (data + i)^;
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
	
	//Special comments = //##????| o = out, a = array, _ = normal parameter position
	
	//##o|
	procedure GetExceptionMessage(result: PChar); cdecl; export;
	begin
		StrCopy(result, PChar(ErrorMessage));
	end;
	
	function ExceptionOccured(): LongInt; cdecl; export;
	begin
		if HasException then result := -1
		else result := 0;
	end;
	
	procedure TrapException(exc: Exception; fromMethod: String);
	begin
		HasException := true;
		
		if Assigned(exc) then
	  		ErrorMessage := 'Error from ' + fromMethod + ' - ' + exc.Message
		else
	  		ErrorMessage := 'Unknown error from ' + fromMethod;

	  	{$IFDEF TRACE}
			Trace('SGSDK.dll', 'Error', fromMethod, ErrorMessage);
		{$ENDIF}
	end;
	
	///-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
	//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
	// 					Core
	//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
	//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
	
	procedure ProcessEvents(); cdecl; export;
	begin
	  	{$IFDEF TRACE}
			TraceEnter('SGSDK.dll', 'ProcessEvents');
		{$ENDIF}

		try
			SGSDK_Core.ProcessEvents();
		Except on exc: Exception do TrapException(exc, 'ProcessEvents');
		end;	
			
	  	{$IFDEF TRACE}
			TraceExit('SGSDK.dll', 'ProcessEvents');
		{$ENDIF}
	end;

	procedure OpenGraphicsWindow(caption : PChar; width : LongInt; height : LongInt); cdecl; export;
	begin
	  {$IFDEF TRACE}
			TraceEnter('SGSDK.dll', 'OpenGraphicsWindow');
			Trace('SGSDK.dll', 'parameters', 'OpenGraphicsWindow', String(caption) + ': W' + IntToStr(width) + ': H' + IntToStr(height));
		{$ENDIF}
		ErrorMessage := '';
		HasException := false;
		
		Try
			SGSDK_Core.OpenGraphicsWindow(caption, width, height);
		Except on exc: Exception do TrapException(exc, 'OpenGraphicsWindow');
		end;
		
	  	{$IFDEF TRACE}
			TraceExit('SGSDK.dll', 'OpenGraphicsWindow');
		{$ENDIF}		
	end;
	
	function WindowCloseRequested(): LongInt; cdecl; export;
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
		Except on exc: Exception do TrapException(exc, 'WindowCloseRequested');
		end;
	end;
	
	procedure SetIcon(iconFilename: PChar); cdecl; export;
	begin
		Try
			SGSDK_Core.SetIcon(iconFilename);
		Except on exc: Exception do TrapException(exc, 'SetIcon');
		end;
	end;
	
	procedure ChangeScreenSize(width, height: LongInt); cdecl; export;
	begin
		Try
			SGSDK_Core.ChangeScreenSize(width, height);
		Except on exc: Exception do TrapException(exc, 'ChangeScreenSize');
		end;
	end;
	
	procedure ToggleFullScreen(); cdecl; export;
	begin
		Try
			SGSDK_Core.ToggleFullScreen();
		Except on exc: Exception do TrapException(exc, 'ToggleFullScreen');
		end;
	end;
	
	procedure RefreshScreenWithFrame(TargetFPS : LongInt); cdecl; export;
	begin
		Try
			SGSDK_Core.RefreshScreen(TargetFPS);
		Except on exc: Exception do TrapException(exc, 'RefreshScreen');
		end;
	end;
	
	procedure RefreshScreen(); cdecl; export;
	begin
		Try
			SGSDK_Core.RefreshScreen();
		Except on exc: Exception do TrapException(exc, 'RefreshScreen');
		end;
	end;
	
	procedure TakeScreenShot(basename: PChar); cdecl; export;
	begin
		Try
			SGSDK_Core.TakeScreenshot(basename);
		Except on exc: Exception do TrapException(exc, 'TakeScreenshot');
		end;
	end;
	
	function  ScreenWidth(): LongInt; cdecl; export;
	begin
		Try
			result := SGSDK_Core.ScreenWidth();
			exit;
		Except on exc: Exception do TrapException(exc, 'ScreenWidth');
		end;
		result := -1;
	end;
	
	function  ScreenHeight(): LongInt; cdecl; export;
	begin
		Try
			result := SGSDK_Core.ScreenHeight();
			exit;
		Except on exc: Exception do TrapException(exc, 'ScreenHeight');
		end;
		result := -1;
	end;
	
	function ToSDLColor(color: UInt32): TSDL_Color; cdecl; export;
	begin		
		Try
			result := SGSDK_Core.ToSDLColor(color);
			exit;
		Except on exc: Exception do TrapException(exc, 'ToSDLColor');
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
		Except on exc: Exception do TrapException(exc, 'GetColour - from bitmap');
		end;
		result := ColorWhite;
	end;
	
	function GetColourRGBA(red, green, blue, alpha: Byte) : Colour; cdecl; export;
	begin
		Try
			result := SGSDK_Core.GetColour(red, green, blue, alpha);
			exit;
		Except on exc: Exception do TrapException(exc, 'GetColor - RGBA');
		end;
		result := ColorWhite;
	end;
	
	function GetFramerate(): LongInt; cdecl; export;
	begin
		Try
			result := SGSDK_Core.GetFramerate();
			exit;
		Except on exc: Exception do TrapException(exc, 'GetFramerate');
		end;
		result := -1;
	end;
	
	function GetTicks(): UInt32; cdecl; export;
	begin
		Try
			result := SGSDK_Core.GetTicks();
			exit;
		Except on exc: Exception do TrapException(exc, 'GetTicks');
		end;
		result := 0;
	end;
	
	procedure Sleep(time : UInt32); cdecl; export;
	begin
		Try
			SGSDK_Core.Sleep(time);
		Except on exc: Exception do TrapException(exc, 'Sleep');
		end;
	end;
	
	{
	function GetPathToResourceWithKind(filename: PChar; kind: ResourceKind) : PChar; cdecl; export;
	begin
		Try
			result := PChar(SGSDK_Core.GetPathToResource(filename, kind));
			exit;
		Except on exc: Exception do TrapException(exc, 'GetPathToResource - with kind');
		end;
		result := PChar('');
	end;
	
	function GetPathToResource(filename: PChar): PChar; cdecl; export;
	begin
		Try
			result := PChar(SGSDK_Core.GetPathToResource(filename));
			exit;
		Except on exc: Exception do TrapException(exc, 'GetPathToResource');
		end;
		result := PChar('');
	end;
	}
	
	//##___o|
	procedure GetPathToResourceWithBaseAndKind(path, filename: PChar; kind: ResourceKind; result: PChar); cdecl; export;
	var
		temp: String;
		pTemp: PChar;
	begin
		try
			//WriteLn('GetPathToResourceWithBaseAndKind: ', path, ' - ', Integer(kind));
			temp := SGSDK_Core.GetPathToResourceWithBase(path, filename, kind);
			//WriteLn(temp);
			pTemp := PChar(temp);
			//WriteLn(pTemp);
			
			StrCopy(result, pTemp);
			//WriteLn(result);
			
			//WriteLn(' RETURN ', Integer(Pointer(result)), result);
			
			exit;
		Except on exc: Exception do TrapException(exc, 'GetPathToResource with kind and base');
		end;
		StrCopy(result, PChar(''));
	end;

	{function GetPathToResourceWithBaseAndKind(path, filename: PChar; kind: ResourceKind) : PChar; cdecl; export;
	var
		temp: String;
	begin
		try
			//Write('GetPathToResourceWithBaseAndKind: ', path, ' - ', Integer(kind));
			temp := SGSDK_Core.GetPathToResourceWithBase(path, filename, kind);
			result := PChar(temp);
			WriteLn(' RETURN ', Integer(Pointer(result)), result);
			
			exit;
		Except on exc: Exception do TrapException(exc, 'GetPathToResource with kind and base');
		end;
		result := PChar('');
	end;}

	{procedure GetPathToResourceWithBase(path, filename, result: PChar); cdecl; export;
	var
		temp: String;
		pTemp: PChar;
	begin
		try
			temp := SGSDK_Core.GetPathToResourceWithBase(path, filename);
			pTemp := PChar(temp);
			
			StrCopy(result, pTemp);
			exit;
		Except on exc: Exception do TrapException(exc, 'GetPathToResource with base');
		end;
		result := PChar('');
	end;}

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
		Except on exc: Exception do TrapException(exc, 'CreateTimer');
		end;
		result := nil;		
	end;
	
	procedure FreeTimer(toFree: Timer); cdecl; export;
	begin
		Try
			SGSDK_Core.FreeTimer(toFree);
		Except on exc: Exception do TrapException(exc, 'FreeTimer');
		end;		
	end;
	
	procedure StartTimer(toStart : Timer); cdecl; export;
	begin
		Try
			SGSDK_Core.StartTimer(toStart);
		Except on exc: Exception do TrapException(exc, 'StartTimer');
		end;
	end;
	
	procedure StopTimer(toStop : Timer); cdecl; export;
	begin
		Try
			SGSDK_Core.StopTimer(toStop);
		Except on exc: Exception do TrapException(exc, 'StopTimer');
		end;
	end;
	
	procedure PauseTimer(toPause : Timer); cdecl; export;
	begin
		Try
			SGSDK_Core.PauseTimer(toPause);
		Except on exc: Exception do TrapException(exc, 'PauseTimer');
		end;
	end;
	
	procedure UnpauseTimer(toUnpause : Timer); cdecl; export;
	begin
		Try
			SGSDK_Core.UnpauseTimer(toUnpause);
		Except on exc: Exception do TrapException(exc, 'UnpauseTimer');
		end;
	end;
	
	function GetTimerTicks(toGet : Timer) : UInt32; cdecl; export;
	begin
		Try
			result := SGSDK_Core.GetTimerTicks(toGet);
			exit;
		Except on exc: Exception do TrapException(exc, 'GetTimerTicks');
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
	procedure ShowMouse(show : LongInt); cdecl; export;
	begin
		try
			SGSDK_Input.ShowMouse(show = -1);
		Except on exc: Exception  do TrapException(exc, 'ShowMouse');
		end;
	end;
	
	function IsMouseShown(): LongInt ; cdecl; export;
	begin
		try
			if SGSDK_Input.IsMouseShown() then result := -1
			else result := 0;
			exit;
		Except on exc: Exception  do TrapException(exc, 'IsMouseShown');
		end;
		result := -1;
	end;
	
	procedure MoveMouse(x : UInt16; y : UInt16); cdecl; export;
	begin
		try
			SGSDK_Input.MoveMouse(x,y);
		Except on exc: Exception  do TrapException(exc, 'MoveMouse');
		end;
	end;

	procedure GetMouseXY(out x: Single; out y: Single); cdecl; export;
	var
		myPt: Point2D;
	begin
		Try
			myPt := SGSDK_Input.GetMousePosition();
			
			x := myPt.x;
			y := myPt.y;
			
			exit;
		Except on exc: Exception do TrapException(exc, 'GetMouseXY');
		end;
		
		x := 0; 
		y := 0;
	end;

{
	function GetMousePositionAsVector(): Vector; cdecl; export;
	begin
		WriteLn('GetMousePositionAsVector dll');
		Try
			result := SGSDK_Input.GetMousePositionAsVector();
			WriteLn('End GetMousePositionAsVector dll');
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result.x := 0; result.y := 0;
		WriteLn('Failed GetMousePositionAsVector dll');
	end;

	function GetMousePosition(): Point2D; cdecl; export;
	begin
		WriteLn('GetMousePosition dll');
		Try
			result := SGSDK_Input.GetMousePosition();
			WriteLn('End GetMousePosition dll');
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result.x := 0; result.y := 0;
		WriteLn('Failed GetMousePosition dll');
	end;
}	
	function GetMouseMovement(): Vector; cdecl; export;
	var
	  vec: Vector;
	begin
	  {$IFDEF TRACE}
			TraceEnter('SGSDK.dll', 'GetMouseMovement');
		{$ENDIF}

		Try
		  vec := SGSDK_Input.GetMouseMovement();
			result.x := vec.x; result.y := vec.y;
			exit;
		Except on exc: Exception do TrapException(exc, 'GetMouseMovement');
		end;
		result.x := 0; result.y := 0;
	  	{$IFDEF TRACE}
			TraceExit('SGSDK.dll', 'GetMouseMovement');
		{$ENDIF}

	end;
	
	function IsMouseDown(button: MouseButton): LongInt; cdecl; export;
	begin
		Try
			if SGSDK_Input.IsMouseDown(button) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'IsMouseDown');
		end;
		result := -1;
	end;
	
	function IsMouseUp(button: MouseButton): LongInt; cdecl; export;
	begin
		Try
			if SGSDK_Input.IsMouseUp(button) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'IsMouseUp');
		end;
		result := -1;
	end;
	
	function MouseWasClicked(button: MouseButton): LongInt; cdecl; export;
	begin
		Try
			if SGSDK_Input.MouseWasClicked(button) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'WasMouseClicked');
		end;
		result := -1;
	end;

	procedure StartReadingText(textColor: Colour; maxLength: LongInt; theFont: Font; x, y: LongInt); cdecl; export;
	begin
		Try
			SGSDK_Input.StartReadingText(textColor, maxLength, theFont, x, y);
		Except on exc: Exception do TrapException(exc, 'StartReadingText');
		end;
	end;

	function IsReadingText(): LongInt; cdecl; export;
	begin
		Try
			if SGSDK_Input.IsReadingText() then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'IsReadingText');
		end;
		result := 0;
	end;
	
	//##o|
	procedure EndReadingText(result: PChar); cdecl; export;
	var
		temp: String;
	begin
			temp := '';
			Try
				temp := SGSDK_Input.EndReadingText();
				StrCopy(result, PChar(temp));
				exit;
			Except on exc: Exception do TrapException(exc, 'EndReadingText');
			end;
			StrCopy(result, PChar(''));
	end;
	
	//##o|
	procedure TextReadAsASCII(result: PChar); cdecl; export;
	var
		temp: String;
	begin
		temp := '';
		Try
			temp := SGSDK_Input.TextReadAsASCII();
			StrCopy(result, PChar(temp));
			exit;
		Except on exc: Exception do TrapException(exc, 'TextReadAsASCII');
		end;
		StrCopy(result, PChar(''));
	end;
		
	function IsKeyPressed(virtKeyCode : LongInt): LongInt; cdecl; export;
	begin
		Try
			if SGSDK_Input.IsKeyPressed(virtKeyCode) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'IsKeyPressed');
		end;
		result := 0;
	end;
	
	function WasKeyTyped(virtKeyCode: LongInt): LongInt; cdecl; export;
	begin
		Try
			if SGSDK_Input.WasKeyTyped(virtKeyCode) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'WasKeyTyped');
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
		Except on exc: Exception do TrapException(exc, 'OpenAudio');
		end;
	end;
	
	procedure CloseAudio(); cdecl; export;
	begin
		Try
			SGSDK_Audio.CloseAudio();
		Except on exc: Exception do TrapException(exc, 'CloseAudio');
		end;
	end;
	
	function LoadSoundEffect(path: PChar): SoundEffect; cdecl; export;
	begin
		//WriteLn('Loading ', path);
		Try
			result := SGSDK_Audio.LoadSoundEffect(path);
			//WriteLn('Ending load sound effect');
			exit;
		Except on exc: Exception do TrapException(exc, 'LoadSoundEffect');
		end;

		//WriteLn('Failed load sound effect');
		result := nil;
	end;
	
	function LoadMusic(path: PChar): Music; cdecl; export;
	begin
		Try
			result := SGSDK_Audio.LoadMusic(path);
			exit;
		Except on exc: Exception do TrapException(exc, 'LoadMusic');
		end;
		result := nil;
	end;

	procedure FreeMusic(mus: Music); cdecl; export;
	begin
		Try
			SGSDK_Audio.FreeMusic(mus);
		Except on exc: Exception do TrapException(exc, 'FreeMusic');
		end;
	end;

	procedure FreeSoundEffect(effect: SoundEffect); cdecl; export;
	begin
		Try
			SGSDK_Audio.FreeSoundEffect(effect);
		Except on exc: Exception do TrapException(exc, 'FreeSoundEffect');
		end;
	end;

	procedure PlaySoundEffect(effect: SoundEffect); cdecl; export;
	begin
		Try
			SGSDK_Audio.PlaySoundEffect(effect);
		Except on exc: Exception do TrapException(exc, 'PlaySoundEffect');
		end;
	end;
	
	procedure PlaySoundEffectLoop(effect: SoundEffect; loops: LongInt); cdecl; export;
	begin
		Try
			SGSDK_Audio.PlaySoundEffect(effect, loops);
		Except on exc: Exception do TrapException(exc, 'PlaySoundEffect loop');
		end;
	end;

	procedure PlayMusic(mus: Music; loops: LongInt); cdecl; export;
	begin
		Try
			SGSDK_Audio.PlayMusic(mus, loops);
		Except on exc: Exception do TrapException(exc, 'PlayMusic');
		end;
	end;


	function IsMusicPlaying(): LongInt; cdecl; export;
	begin
		Try
			if SGSDK_Audio.IsMusicPlaying() then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'IsMusicPlaying');
		end;
		result := 0;
	end;

	function IsSoundEffectPlaying(effect: SoundEffect): LongInt; cdecl; export;
	begin
		Try
			if SGSDK_Audio.IsSoundEffectPlaying(effect) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'IsSoundEffectPlaying');
		end;
		result := 0;
	end;

	procedure StopSoundEffect(effect: SoundEffect); cdecl; export;
	begin
		Try
			SGSDK_Audio.StopSoundEffect(effect);
		Except on exc: Exception do TrapException(exc, 'StopSoundEffect');
		end;
	end;

	procedure StopMusic(); cdecl; export;
	begin
		Try
			SGSDK_Audio.StopMusic();
		Except on exc: Exception do TrapException(exc, 'StopMusic');
		end;
	end;
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Font
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	
	function LoadFont(fontName: PChar; size: LongInt): Font; cdecl; export;
	begin
		Try
			result := SGSDK_Font.LoadFont(fontName, size);
			exit;
		Except on exc: Exception do TrapException(exc, 'LoadFont');
		end;
		result := nil;
	end;

	procedure SetFontStyle(font: Font; style: FontStyle); cdecl; export;
	begin
		Try
			SGSDK_Font.SetFontStyle(font, style);
		Except on exc: Exception do TrapException(exc, 'SetFontStyle');
		end;
	end;

	procedure FreeFont(fontToFree: Font); cdecl; export;
	begin
		Try
			SGSDK_Font.FreeFont(fontToFree);
		Except on exc: Exception do TrapException(exc, 'FreeFont');
		end;
	end;
		
	procedure DrawText(theText: PChar; textColor: Colour; theFont: Font; x, y: Single); cdecl; export;
	begin
		Try
			SGSDK_Font.DrawText(theText, textColor, theFont, x, y);
		Except on exc: Exception do TrapException(exc, 'DrawText');
		end;
	end;

	procedure DrawTextLines(theText: PChar; textColor, backColor: Colour;
							theFont: Font; align: FontAlignment;
							x, y: Single; w, h: LongInt); cdecl; export;
	begin
		Try
			SGSDK_Font.DrawTextLines(theText, textColor, backColor, theFont, align, x, y, w, h);
		Except on exc: Exception do TrapException(exc, 'DrawTextLines');
		end;
	end;
	
	procedure DrawTextOnScreen(theText: PChar; textColor: Colour;
					 theFont: Font; x, y: LongInt); cdecl; export;
	begin
		Try
			SGSDK_Font.DrawTextOnScreen(theText, textColor, theFont, x, y);
		Except on exc: Exception do TrapException(exc, 'DrawTextOnScreen');
		end;
	end;

	procedure DrawTextLinesOnScreen(theText: PChar; textColor, backColor: Colour;
							theFont: Font; align: FontAlignment;
							x, y, w, h: LongInt); cdecl; export;
	begin
		Try
			SGSDK_Font.DrawTextLinesOnScreen(theText, textColor, backColor, theFont, align, x, y, w, h);
		Except on exc: Exception do TrapException(exc, 'DrawTextLinesOnScreen');
		end;
	end;
							
	procedure DrawTextOnBitmap(dest: Bitmap; theText: PChar; textColor: Colour;
					theFont: Font; x, y: LongInt); cdecl; export;
	begin
		Try
			SGSDK_Font.DrawText(dest, theText, textColor, theFont, x, y);
		Except on exc: Exception do TrapException(exc, 'DrawTextOnBitmap');
		end;
	end;

	procedure DrawTextLinesOnBitmap(dest: Bitmap; theText: PChar;
							textColor, backColor: Colour;
							theFont: Font; align: FontAlignment;
							x, y, w, h: LongInt); cdecl; export;
	begin
		Try
			SGSDK_Font.DrawTextLines(dest, theText, textColor, backColor, theFont, align, x, y, w, h);
		Except on exc: Exception do TrapException(exc, 'DrawTextLinesOnBitmap');
		end;
	end;

	function TextWidth(theText: PChar; theFont: Font): LongInt; cdecl; export;
	begin
		Try
			result :=SGSDK_Font.TextWidth(theText, theFont);
			exit;
		Except on exc: Exception do TrapException(exc, 'TextWidth');
		end;
		result := -1;
	end;

	function TextHeight(theText: PChar; theFont: Font): LongInt; cdecl; export;
	begin
		Try
			result := SGSDK_Font.TextHeight(theText, theFont);
			exit;
		Except on exc: Exception do TrapException(exc, 'TextHeight');
		end;
		result := -1;
	end;

	procedure DrawFramerate(x, y: LongInt; font: Font); cdecl; export;
	begin
		Try
			SGSDK_Font.DrawFramerate(x, y, font);
		Except on exc: Exception do TrapException(exc, 'DrawFramerate');
		end;
	end;
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Physics
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	
	function RectangleHasCollidedWithLine(rect: Rectangle; line: LineSegment): LongInt; cdecl; export;
	begin
		Try
			if SGSDK_Physics.RectangleHasCollidedWithLine(rect, line) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'RectangleHasCollidedWithLine');
		end;
		result := 0;
	end;
	
	function IsSpriteOnScreenAt(theSprite: Sprite; x, y: LongInt): LongInt; cdecl; export;
	begin
		Try
			if SGSDK_Physics.IsSpriteOnScreenAt(theSprite, x, y) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'IsSpriteOnScreenAt');
		end;
		result := 0;
	end;
	
	function CircleHasCollidedWithLine(p1: Sprite; line: LineSegment): LongInt; cdecl; export;
	begin
		Try
			if SGSDK_Physics.CircleHasCollidedWithLine(p1, line) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'CircleHasCollidedWithLine');
		end;
		result := 0;
	end;
	
	function HasSpriteCollidedX(theSprite : Sprite; x : LongInt;
								 range : CollisionDetectionRange):  LongInt; cdecl; export;
	begin
		Try
			if SGSDK_Physics.HasSpriteCollidedX(theSprite, x, range) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'HasSpriteCollidedX');
		end;
		result := 0;
	end;

	function HasSpriteCollidedY(theSprite : Sprite; y : LongInt;
								 range : CollisionDetectionRange): LongInt; cdecl; export;
	begin
		Try
			if SGSDK_Physics.HasSpriteCollidedY(theSprite, y, range) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'HasSpriteCollidedY');
		end;
		result := 0;
	end;

	function HasSpriteCollidedWithRect(theSprite : Sprite; x, y : Single;
		width, height : LongInt): LongInt; cdecl; export;
	begin
		Try
			if SGSDK_Physics.HasSpriteCollidedWithRect(theSprite, x, y, width, height) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'HasSpriteCollidedWithRect');
		end;
		result := 0;
	end;
		
	function HaveSpritesCollided(sprite1, sprite2 : Sprite): LongInt; cdecl; export;
	begin
		Try
			if SGSDK_Physics.HaveSpritesCollided(sprite1, sprite2) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'HaveSpritesCollided');
		end;
		result := 0;
	end;
	
	function HasSpriteCollidedWithBitmap(theSprite: Sprite; theBitmap: Bitmap; x, y: Single; bounded: LongInt): LongInt; cdecl; export;
	begin
		Try
			if SGSDK_Physics.HasSpriteCollidedWithBitmap(theSprite, theBitmap, x, y, bounded = -1) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'HasSpriteCollidedWithBitmap');
		end;
		result := 0;		
	end;
	
	function HasSpriteCollidedWithBitmapPart(theSprite: Sprite; theBitmap: Bitmap; pt: Point2D; src: Rectangle; bounded: LongInt): LongInt; cdecl; export;
	begin
		Try
			if SGSDK_Physics.HasSpriteCollidedWithBitmap(theSprite, theBitmap, pt, src, bounded = -1) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'HasSpriteCollidedWithBitmapPart');
		end;
		result := 0;		
	end;
	
	function HaveBitmapsCollided(image1: Bitmap; x1, y1: LongInt; bounded1: LongInt; image2: Bitmap; x2, y2: LongInt; bounded2: LongInt): LongInt; cdecl; export;
	begin
		Try
			if SGSDK_Physics.HaveBitmapsCollided(image1, x1, y1, bounded1 = -1, image2, x2, y2, bounded2 = -1) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'HaveBitmapsCollided');
		end;
		result := 0;
	end;
	
	function HaveBitmapPartsCollided(image1: Bitmap; pt1: Point2D; src1: Rectangle; bounded1: LongInt; image2: Bitmap; pt2: Point2D; src2: Rectangle; bounded2: LongInt): LongInt; cdecl; export;
	begin
		Try
			if SGSDK_Physics.HaveBitmapsCollided(image1, pt1, src1, bounded1 = -1, image2, pt2, src2, bounded2 = -1) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'HaveBitmapPartsCollided');
		end;
		result := 0;
	end;

{	function CreateVector(x,y : Single; invertY : LongInt): Vector;  cdecl; export;
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
		Except on exc: Exception do TrapException(exc, 'LimitMagnitude');
		end;
		result.x := 0; result.y := 0;
	end;

	function GetUnitVector(theVector : Vector): Vector; cdecl; export;
	begin
		Try
			result :=SGSDK_Physics.GetUnitVector(theVector);
			exit;
		Except on exc: Exception do TrapException(exc, 'GetUnitVector');
		end;
		result.x := 0; result.y := 0;
	end;

{	function IsZeroVector(theVector : Vector): LongInt; cdecl; export;
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
		Except on exc: Exception do TrapException(exc, 'CalculateAngle');
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

	procedure NewMatrix(out p: Matrix2DPtr);
	begin
		new(p);
		p^.Free := false;
	end;
	
	procedure CheckMatrix(m: Matrix2DPtr);
	begin
		if not Assigned(m) then raise Exception.Create('Matrix is not valid');
		if m^.free then raise Exception.Create('Using a freed matrix'); 
	end;

	function TranslationMatrix(dx, dy: Single): Matrix2DPtr; cdecl; export;
	var
		p: Matrix2DPtr;
	begin
		Try
			NewMatrix(p);
			p^.data := SGSDK_Physics.TranslationMatrix(dx, dy);
			result := p;
			exit;
		Except on exc: Exception do TrapException(exc, 'TranslationMatrix');
		end;
		result := nil;
	end;
	
	function ScaleMatrix(scale: Single): Matrix2DPtr; cdecl; export;
	var
		p: Matrix2DPtr;
	begin
		Try
			NewMatrix(p);
			p^.Data := SGSDK_Physics.ScaleMatrix(scale);
			result := p;
			exit;
		Except on exc: Exception do TrapException(exc, 'ScaleMatrix');
		end;
		result := nil;
	end;
	
	function RotationMatrix(deg: Single): Matrix2DPtr; cdecl; export;
	var
		p: Matrix2DPtr;
	begin
		Try	
			NewMatrix(p);
			p^.Data := SGSDK_Physics.RotationMatrix(deg);
			result := p;
			exit;
		Except on exc: Exception do TrapException(exc, 'RotationMatrix');
		end;
		result := nil;
	end;
	
	function MultiplyMatrix2D(m1, m2: Matrix2DPtr): Pointer; cdecl; export;
	var
		p: Matrix2DPtr;
	begin
		Try
			NewMatrix(p);
			p^.Data := SGSDK_Physics.Multiply(m1^.Data, m2^.Data);
			result := p;
			exit;
		Except on exc: Exception do TrapException(exc, 'MultiplyMatrix2DAndVector');
		end;
		result := nil;
	end;
	
	function MultiplyMatrix2DAndVector(m: Matrix2DPtr; v: Vector): Vector; cdecl; export;
	begin
		Try
			CheckMatrix(m);
				
			result := SGSDK_Physics.Multiply(m^.Data, v);
			exit;
		Except on exc: Exception do TrapException(exc, 'MultiplyMatrix2DAndVector');
		end;
		result.x := 0; result.y := 0;
	end;
	
	procedure VectorCollision(p1, p2 : Sprite); cdecl; export;
	begin
		Try
			SGSDK_Physics.VectorCollision(p1, p2);
		Except on exc: Exception do TrapException(exc, 'VectorCollision');
		end;
	end;
	
	procedure CircleCollisionWithLine(p1: Sprite; line: LineSegment); cdecl; export;
	begin
		Try
			SGSDK_Physics.CircleCollisionWithLine(p1, line);
		Except on exc: Exception do TrapException(exc, 'CircleCollisionWithLine');
		end;		
	end;

	procedure CircularCollision(p1, p2: Sprite); cdecl; export;
	begin
		Try
			SGSDK_Physics.CircularCollision(p1, p2);
		Except on exc: Exception do TrapException(exc, 'CircularCollision');
		end;
	end;

	function GetMatrix2DElement(matrix: Matrix2DPtr; x, y: LongInt): Single; cdecl; export;
	begin
		Try
			CheckMatrix(matrix);
			
			result := matrix^.Data[x, y];
			exit;
		Except on exc: Exception do TrapException(exc, 'GetMatrix2DElement');
		end;
		result := -1;
	end;
	
	procedure SetMatrix2DElement(matrix: Matrix2DPtr; x, y: LongInt; val: Single); cdecl; export;
	begin
		Try
			CheckMatrix(matrix);
			
			matrix^.Data[x,y] := val;
		Except on exc: Exception do TrapException(exc, 'SetMatrix2DElement');
		end;
	end;
	
	procedure FreeMatrix2D(matrix: Matrix2DPtr); cdecl; export;
	begin
		Try
			CheckMatrix(matrix);
			Dispose(matrix);
		Except on exc: Exception do TrapException(exc, 'FreeMatrix2D');
		end;
	end;	

	function VectorOutOfCircleFromPoint(pnt, center: Point2D; radius: Single; movement: Vector): Vector; cdecl; export;
	begin
		Try
			result := SGSDK_Physics.VectorOutOfCircleFromPoint(pnt, center, radius, movement);
			exit;
		Except on exc: Exception do TrapException(exc, 'VectorOutOfCircleFromPoint');
		end;
		result := CreateVector(0,0);		
	end;
	
	function VectorOutOfCircleFromCircle(pnt: Point2D; radius: Single; center: Point2D; radius2: Single; movement: Vector): Vector; cdecl; export;
	begin
		Try
			result := SGSDK_Physics.VectorOutOfCircleFromCircle(pnt, radius, center, radius2, movement);
			exit;
		Except on exc: Exception do TrapException(exc, 'VectorOutOfCircleFromCircle');
		end;
		result := CreateVector(0,0);				
	end;
	
	function VectorOutOfRectFromPoint(pnt: Point2D; rect: Rectangle; movement: Vector): Vector; cdecl; export;
	begin
		Try
			result := SGSDK_Physics.VectorOutOfRectFromPoint(pnt, rect, movement);
			exit;
		Except on exc: Exception do TrapException(exc, 'VectorOutOfRectFromPoint');
		end;
		result := CreateVector(0,0);
	end;
	
	function VectorOutOfRectFromRect(srcRect, targetRect: Rectangle; movement: Vector) : Vector; cdecl; export;
	begin
		Try
			result := SGSDK_Physics.VectorOutOfRectFromRect(srcRect, targetRect, movement);
			exit;
		Except on exc: Exception do TrapException(exc, 'VectorOutOfRectFromRect');
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
	
	//
	// Version 1.1.1
	//
	
	function GetPixel(bmp: Bitmap; x, y: LongInt): Colour; cdecl; export;
	begin
		Try
			result := SGSDK_Graphics.GetPixel(bmp, x, y);
			exit;
		Except on exc: Exception do TrapException(exc, 'GetPixel');
		end;
		result := 0;
	end;

	function GetPixelFromScreen(x, y: LongInt): Colour; cdecl; export;
	begin
		Try
			result := SGSDK_Graphics.GetPixelFromScreen(x, y);
			exit;
		Except on exc: Exception do TrapException(exc, 'GetPixelFromScreen');
		end;
		result := 0;
	end;

	
	//
	// Version 1 and 1.1
	//
	function GetSpriteBitmap(surface: Sprite; id: LongInt): Bitmap; cdecl; export;
	begin
		Try
			result := surface.bitmaps[id];
			exit;
		Except on exc: Exception do TrapException(exc, 'GetSpriteBitmap');
		end;
		result := nil;
	end;
	
	function GetSpriteX(surface: Sprite): Single; cdecl; export;
	begin
		Try
			result := surface.x;
			exit;
		Except on exc: Exception do TrapException(exc, 'GetSpriteX');
		end;
		result := -1;
	end;
	
	procedure SetSpriteX(surface: Sprite; val: Single); cdecl; export;
	begin
		Try
			surface.x := val;
		Except on exc: Exception do TrapException(exc, 'SetSpriteX');
		end;
	end;
	
	function GetSpriteY(surface: Sprite): Single; cdecl; export;
	begin
		Try
			result := surface.y;
			exit;
		Except on exc: Exception do TrapException(exc, 'GetSpriteY');
		end;
		result := -1;
	end;
	
	procedure SetSpriteY(surface: Sprite; val: Single); cdecl; export;
	begin
		Try
			surface.y := val;
		Except on exc: Exception do TrapException(exc, 'SetSpriteY');
		end;
	end;
	
	function GetSpriteCurrentFrame(surface: Sprite): LongInt; cdecl; export;
	begin
		Try
			result := surface.currentFrame;
			exit;
		Except on exc: Exception do TrapException(exc, 'GetSpriteCurrentFrame');
		end;
		result := -1;
	end;
	
	procedure SetSpriteCurrentFrame(surface: Sprite; val: LongInt); cdecl; export;
	begin
		Try
			surface.currentFrame := val;
		Except on exc: Exception do TrapException(exc, 'SetSpriteCurrentFrame');
		end;
	end;
	
	function GetSpriteUsePixelCollision(surface: Sprite): LongInt; cdecl; export;
	begin
		Try
			if surface.usePixelCollision then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'GetSpriteUsePixelCollision');
		end;
		result := 0;
	end;
	
	procedure SetSpriteUsePixelCollision(surface: Sprite; val: LongInt); cdecl; export;
	begin
		Try
			surface.usePixelCollision := val = -1;
		Except on exc: Exception do TrapException(exc, 'SetSpriteUsePixelCollision');
		end;
	end;
		
	function CreateBitmap(width, height: LongInt): Bitmap; cdecl; export;
	begin
		Try
			result := SGSDK_Graphics.CreateBitmap(width, height);
			exit;
		Except on exc: Exception do TrapException(exc, 'CreateBitmap');
		end;
		result := nil;
	end;
	
	procedure OptimiseBitmap(surface: Bitmap); cdecl; export;
	begin
		Try
			SGSDK_Graphics.OptimiseBitmap(surface);
		Except on exc: Exception do TrapException(exc, 'OptimiseBitmap');
		end;
	end;
	
	function LoadBitmapWithTransparentColor(pathToBitmap: PChar; transparent: LongInt; transparentColor: Colour): Bitmap; cdecl; export;
	begin
		Try
			result := SGSDK_Graphics.LoadBitmap(pathToBitmap, transparent = -1, transparentColor);
			exit;
		Except on exc: Exception do TrapException(exc, 'LoadBitmapWithTransparentColor');
		end;
		result := nil;
	end;
	
	function LoadTransparentBitmap(pathToBitmap : PChar; transparentColor : Colour): Bitmap; cdecl; export;
	begin
		Try
			result := SGSDK_Graphics.LoadTransparentBitmap(pathToBitmap, transparentColor);
			exit;
		Except on exc: Exception do TrapException(exc, 'LoadTransparentBitmap');
		end;
		result := nil;
	end;
	
	procedure FreeBitmap(bitmapToFree : Bitmap); cdecl; export;
	begin
		Try
			SGSDK_Graphics.FreeBitmap(bitmapToFree);
		Except on exc: Exception do TrapException(exc, 'FreeBitmap');
		end;
	end;
	
	function GetBitmapWidth(targetbitmap : Bitmap): LongInt ; cdecl; export;
	begin
		Try
			result := targetbitmap.width;
			exit;
		Except on exc: Exception do TrapException(exc, 'GetBitmapWidth');
		end;
		result := -1;
	end;
	
	function GetBitmapHeight(targetbitmap : Bitmap): LongInt; cdecl; export;
	begin
		Try
			result := targetbitmap.height;
			exit;
		Except on exc: Exception do TrapException(exc, 'GetBitmapHeight');
		end;
		result := -1;
	end;
	
	procedure ClearSurfaceWithColor(dest: Bitmap; toColour: Colour); cdecl; export;
	begin
		Try
			SGSDK_Graphics.ClearSurface(dest, toColour);
		Except on exc: Exception do TrapException(exc, 'ClearSurfaceWithColor');
		end;
	end;
	
	procedure DrawBitmapWithDestination(dest: Bitmap; bitmapToDraw: Bitmap; x, y : LongInt); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawBitmap(dest, bitmapToDraw, x, y);
		Except on exc: Exception do TrapException(exc, 'DrawBitmapWithDestination');
		end;
	end;
	
	procedure DrawBitmapPartWithDestination(dest: Bitmap; bitmapToDraw: Bitmap;
							srcX, srcY, srcW, srcH, x, y : LongInt); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawBitmapPart(dest, bitmapToDraw, srcX, srcY, srcW, srcH, x, y);
		Except on exc: Exception do TrapException(exc, 'DrawBitmapPartWithDestination');
		end;
	end;
	
	procedure DrawPixelWithDestination(dest: Bitmap; theColour: Colour; x, y: LongInt); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawPixel(dest, theColour, x, y);
		Except on exc: Exception do TrapException(exc, 'DrawPixelWithDestination');
		end;
	end;
	
	procedure DrawRectangleWithDestination(dest: Bitmap; theColour : Colour; filled : LongInt;
							xPos, yPos, width, height : LongInt); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawRectangle(dest, theColour, filled = -1, xPos, yPos, width, height);
		Except on exc: Exception do TrapException(exc, 'DrawRectangleWithDestination');
		end;
	end;
	
	procedure FillRectangleWithDestination(dest: Bitmap; theColour : Colour; xPos, yPos,
							width, height : LongInt); cdecl; export;
	begin
		Try
			SGSDK_Graphics.FillRectangle(dest, theColour, xPos, yPos, width, height);
		Except on exc: Exception do TrapException(exc, 'FillRectangleWithDestination');
		end;
	end;

	procedure DrawLineWithDestination(dest: Bitmap; theColour: Colour; xPosStart, yPosStart,
					 xPosEnd, yPosEnd: LongInt); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawLine(dest, theColour, xPosStart, yPosStart, xPosEnd, yPosEnd);
		Except on exc: Exception do TrapException(exc, 'DrawLineWithDestination');
		end;
	end;

	procedure DrawHorizontalLineWithDestination(dest: Bitmap; theColor: Color;
								 y, x1, x2: LongInt); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawHorizontalLine(dest, theColor, y, x1, x2);
		Except on exc: Exception do TrapException(exc, 'DrawHorizontalLineWithDestination');
		end;
	end;

	procedure DrawVerticalLineWithDestination(dest: Bitmap; theColor: Color;
							 x, y1, y2: LongInt); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawVerticalLine(dest, theColor, x, y1, y2);
		Except on exc: Exception do TrapException(exc, 'DrawVerticalLineWithDestination');
		end;
	end;

	procedure DrawCircleWithDestination(dest: Bitmap; theColour: Colour; filled: LongInt;
							 xc, yc, radius: LongInt); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawCircle(dest, theColour, filled = -1, xc, yc, radius);
		Except on exc: Exception do TrapException(exc, 'DrawCircleWithDestination');
		end;
	end;

	procedure DrawEllipseWithDestination(dest: Bitmap; theColour: Colour; filled: LongInt;
							xPos, yPos, width, height: LongInt); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawEllipse(dest, theColour, filled = -1, xPos, yPos, width, height);
		Except on exc: Exception do TrapException(exc, 'DrawEllipseWithDestination');
		end;
	end;

	procedure ClearScreen(toColour : Colour); cdecl; export;
	begin
		Try
			SGSDK_Graphics.ClearScreen(toColour);
		Except on exc: Exception do TrapException(exc, 'ClearScreen');
		end;
	end;

	procedure DrawBitmap(bitmapToDraw : Bitmap; x, y : Single); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawBitmap(bitmapToDraw, x, y);
		Except on exc: Exception do TrapException(exc, 'DrawBitmap');
		end;
	end;

	procedure DrawBitmapPart(bitmapToDraw : Bitmap;
							srcX, srcY, srcW, srcH : LongInt; x, y : Single); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawBitmapPart(bitmapToDraw, srcX, srcY, srcW, srcH, x, y);
		Except on exc: Exception do TrapException(exc, 'DrawBitmapPart');
		end;
	end;
	
	procedure DrawPixel(theColour: Colour; x, y: Single); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawPixel(theColour, x, y);
		Except on exc: Exception do TrapException(exc, 'DrawPixel');
		end;
	end;

	procedure DrawRectangle(theColour : Colour; filled : LongInt; xPos, yPos: Single; width, height : LongInt); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawRectangle(theColour, filled = -1, xPos, yPos, width, height);
		Except on exc: Exception do TrapException(exc, 'DrawRectangle');
		end;
	end;

	procedure DrawLine(theColour: Colour; xPosStart, yPosStart, xPosEnd, yPosEnd: Single); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawLine(theColour, xPosStart, yPosStart, xPosEnd, yPosEnd);
		Except on exc: Exception do TrapException(exc, 'DrawLine');
		end;
	end;

	procedure DrawHorizontalLine(theColor: Color; y, x1, x2: Single); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawHorizontalLine(theColor, y, x1, x2);
		Except on exc: Exception do TrapException(exc, 'DrawHorizontalLine');
		end;
	end;

	procedure DrawVerticalLine(theColor: Color; x, y1, y2: Single); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawVerticalLine(theColor, x, y1, y2);
		Except on exc: Exception do TrapException(exc, 'DrawVerticalLine');
		end;
	end;

	procedure DrawCircle(theColour: Colour; filled: LongInt;
						 xc, yc: Single; radius: LongInt); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawCircle(theColour, filled = -1, xc, yc, radius);
		Except on exc: Exception do TrapException(exc, 'DrawCircle');
		end;
	end;

	procedure DrawEllipse(theColour: Colour; filled: LongInt;
						xPos, yPos: Single; width, height: LongInt); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawEllipse(theColour, filled = -1, xPos, yPos, width, height);
		Except on exc: Exception do TrapException(exc, 'DrawEllipse');
		end;
	end;
	
	//Version 1.1.6
	
	//##_a|
	procedure DrawTriangle(theColour: Colour; firstPoint: Point2DPtr); cdecl; export;
	var
	  tri: Triangle;
	begin
		Try
		  PopulateTriangle(firstPoint, tri);
			SGSDK_Graphics.DrawTriangle(theColour, tri);
		Except on exc: Exception do TrapException(exc, 'DrawTriangle');
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
		Except on exc: Exception do TrapException(exc, 'CreateSprite');
		end;
		result := nil;
	end;
	
	function CreateSpriteMultiFPC(image: Bitmap; framesPerCell, frames, width, height: LongInt): Sprite; cdecl; export;
	begin
		Try
			result := SGSDK_Graphics.CreateSprite(image,framesPerCell, frames, width, height);
			exit;
		Except on exc: Exception do TrapException(exc, 'CreateSprite');
		end;
		result := nil;
	end;
	
	//##_a__|
	function CreateSpriteArrayFPC(bitLength: LongInt; bitmaps: BitmapPtr; framesPerCell, frames: LongInt): Sprite; cdecl; export;
	var
		bmps: BitmapArray;
	begin
		Try
			PopulateBitmapArray(bitmaps, bitLength, bmps);
			
			result := SGSDK_Graphics.CreateSprite(bmps, framesPerCell, frames);
			exit;
		Except on exc: Exception do TrapException(exc, 'CreateSpriteArrayFPC');
		end;
		result := nil;
	end;
	
	//##___a___|
	function CreateSpriteMultiEnding(image : Bitmap; isMulti, length: LongInt; framesPerCell: IntPtr; endingAction : SpriteEndingAction; width : LongInt; height : LongInt): Sprite; cdecl; export;
	var
		fpc: IntArray;
	begin
		Try	
			PopulateIntArray(framesPerCell, length, fpc);

			result := SGSDK_Graphics.CreateSprite(image, isMulti = -1, fpc, endingAction, width, height);
			exit;
		Except on exc: Exception do TrapException(exc, 'CreateSpriteMultiEnding');
		end;
		result := nil;
	end;
	
	//##___a__|
	function CreateSpriteMulti(image : Bitmap; isMulti, length : LongInt; framesPerCell : IntPtr; width, height : LongInt): Sprite; cdecl; export;
	var
		fpc: IntArray;
	begin
		Try
			PopulateIntArray(framesPerCell, length, fpc);
			
			result := SGSDK_Graphics.CreateSprite(image, isMulti = -1, fpc, width, height);
			exit;
		Except on exc: Exception do TrapException(exc, 'CreateSpriteMulti');
		end;
		result := nil;
	end;

  //##_a_a_|
	function CreateSpriteArrayEnding(bitLength: LongInt; bitmaps: BitmapPtr; length: LongInt; framesPerCell: IntPtr; endingAction: SpriteEndingAction): Sprite; cdecl; export;
	var
		fpc: IntArray;
		bmps: BitmapArray;
	begin
		Try
			PopulateBitmapArray(bitmaps, bitLength, bmps);
			PopulateIntArray(framesPerCell, length, fpc);
			
			result := SGSDK_Graphics.CreateSprite(bmps, fpc, endingAction);
			exit;
		Except on exc: Exception do TrapException(exc, 'CreateSpriteArrayEnding');
		end;
		result := nil;
	end;
	
	//##_a_a|
	function CreateSpriteArray(bitlength: LongInt; bitmaps: BitmapPtr; length: LongInt; framesPerCell: IntPtr): Sprite; cdecl; export;
	var
		fpc: IntArray;
		bmps: BitmapArray;
	begin
		Try
			PopulateBitmapArray(bitmaps, bitLength, bmps);
			PopulateIntArray(framesPerCell, length, fpc);
			
			result := SGSDK_Graphics.CreateSprite(bmps, fpc);
			exit;
		Except on exc: Exception do TrapException(exc, 'CreateSpriteArray');
		end;
		result := nil;
	end;
	
	procedure UpdateSpriteAnimation(spriteToDraw : Sprite); cdecl; export;
	begin
		Try
			SGSDK_Graphics.UpdateSpriteAnimation(spriteToDraw);
		Except on exc: Exception do TrapException(exc, 'UpdateSpriteAnimation');
		end;
	end;
	
	procedure UpdateSprite(spriteToDraw : Sprite); cdecl; export;
	begin
		Try
			SGSDK_Graphics.UpdateSprite(spriteToDraw);
		Except on exc: Exception do TrapException(exc, 'UpdateSprite');
		end;
	end;
	
	function GetSpriteKind(surface : Sprite): LongInt; cdecl; export;
	begin
		Try
			result := LongInt(surface.spriteKind);
			exit;
		Except on exc: Exception do TrapException(exc, 'GetSpriteKind');
		end;
		result := -1;
	end;
	
	procedure SetSpriteKind(surface : Sprite; kind : SpriteKind); cdecl; export;
	begin
		Try
			surface.SpriteKind := kind;
		Except on exc: Exception do TrapException(exc, 'SetSpriteKind');
		end;
	end;
	
	//##_a_|
	procedure SetSpriteFramesPerCell(surface: Sprite; framesPerCell: IntPtr; length: LongInt); cdecl; export;
	var
		fpc: IntArray;
	begin
		Try
			PopulateIntArray(framesPerCell, length, fpc);
			
			surface.framesPerCell := fpc;
		Except on exc: Exception do TrapException(exc, 'SetSpriteFramesPerCell');
		end;
	end;
	
	function GetSpriteFramesPerCell(surface : Sprite; ind : LongInt): LongInt; cdecl; export;
	begin
		Try
			result := surface.framesPerCell[ind];
			exit;
		Except on exc: Exception do TrapException(exc, 'GetSpriteFramesPerCell');
		end;
		result := -1;
	end;
	
	function GetSpriteCols(surface : Sprite) : LongInt; cdecl; export;
	begin
		Try
			result := surface.cols;
			exit;
		Except on exc: Exception do TrapException(exc, 'GetSpriteCols');
		end;
		result := -1;
	end;
	
	function GetSpriteRow(surface : Sprite) : LongInt; cdecl; export;
	begin
		Try
			result := surface.row;
			exit;
		Except on exc: Exception do TrapException(exc, 'GetSpriteRow');
		end;
		result := -1;
	end;
	
	function GetSpriteFrameCount(surface : Sprite) : LongInt; cdecl; export;
	begin
		Try
			result := Length(surface.framesperCell);
			exit;
		Except on exc: Exception do TrapException(exc, 'GetSpriteFrameCount');
		end;
		result := -1;
	end;
	
	function GetSpriteEndingAction(surface : Sprite) : LongInt; cdecl; export;
	begin
		Try
			result := LongInt(surface.endingAction);
			exit;
		Except on exc: Exception do TrapException(exc, 'GetSpriteEndingAction');
		end;
		result := -1;
	end;
	
	procedure SetSpriteEndingAction(surface : Sprite; endingAction : SpriteEndingAction); cdecl; export;
	begin
		Try
			surface.endingAction := endingAction;
		Except on exc: Exception do TrapException(exc, 'SetSpriteEndingAction');
		end;
	end;
	
	function GetSpritehasEnded(surface : Sprite) : LongInt; cdecl; export;
	begin
		Try
			if surface.hasEnded then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'GetSpriteHasEnded');
		end;
		result := -1;
	end;
	
	function GetSpriteReverse(surface : Sprite) : LongInt; cdecl; export;
	begin
		Try
			if surface.reverse then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'GetSpriteReverse');
		end;
		result := -1
	end;
	
	function GetSpriteMass(surface : Sprite) : Single; cdecl; export;
	begin
		Try
			result := surface.mass;
			exit;
		Except on exc: Exception do TrapException(exc, 'GetSpriteMass');
		end;
		result := -1;
	end;
	
	function GetSpriteMovement(surface : Sprite) : Vector; cdecl; export;
	begin
		Try
			result := surface.movement;
			exit;
		Except on exc: Exception do TrapException(exc, 'GetSpriteMovement');
		end;
		result.x := 0; result.y := 0;
	end;
	
	procedure SetSpriteMass(surface : Sprite; mass : Single); cdecl; export;
	begin
		Try
			surface.mass := mass;
		Except on exc: Exception do TrapException(exc, 'SetSpriteMass');
		end;
	end;
	
	procedure SetSpriteMovement(surface: Sprite; v : Vector); cdecl; export;
	begin
		Try
			surface^.movement := v;
		Except on exc: Exception do TrapException(exc, 'SetSpriteMovement');
		end;
	end;

	procedure FreeSprite(spriteToFree : Sprite); cdecl; export;
	begin
		Try
			SGSDK_Graphics.FreeSprite(spriteToFree);
		Except on exc: Exception do TrapException(exc, 'FreeSprite');
		end;
	end;

	function AddBitmapToSprite(spriteToAddTo : Sprite;
							   bitmapToAdd : Bitmap): LongInt; cdecl; export;
	begin
		Try
			result := SGSDK_Graphics.AddBitmapToSprite(spriteToAddTo, bitmapToAdd);
			exit;
		Except on exc: Exception do TrapException(exc, 'AddBitmapToSprite');
		end;
		result := -1;
	end;

	function CurrentHeight(sprite: Sprite): LongInt; cdecl; export;
	begin
		Try
			result := SGSDK_Graphics.CurrentHeight(sprite);
			exit;
		Except on exc: Exception do TrapException(exc, 'CurrentHeight');
		end;
		result := -1;
	end;

	function CurrentWidth(sprite: Sprite): LongInt; cdecl; export;
	begin
		Try
			result := SGSDK_Graphics.CurrentWidth(sprite);
			exit;
		Except on exc: Exception do TrapException(exc, 'CurrentWidth');
		end;
		result := -1;
	end;

	procedure DrawSprite(spriteToDraw: Sprite; xOffset, yOffset: LongInt); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawSprite(spriteToDraw, xOffset, yOffset);
		Except on exc: Exception do TrapException(exc, 'DrawSprite');
		end;
	end;
	
	{procedure DrawSpriteOffset(spriteToDraw : Sprite; xOffset, yOffset: LongInt); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawSprite(spriteToDraw, xOffset, yOffset);
		Except on exc: Exception do TrapException(exc, 'DrawSpriteOffset');
		end;
	end;}
	
	procedure MoveSpriteItself(sprite: Sprite); cdecl; export;
	begin
		Try
			SGSDK_Graphics.MoveSprite(sprite);
		Except on exc: Exception do TrapException(exc, 'MoveSprite');
		end;		
	end;

	procedure MoveSprite(spriteToMove : Sprite; movementVector : Vector); cdecl; export;
	begin
		Try
			SGSDK_Graphics.MoveSprite(spriteToMove, movementVector);
		Except on exc: Exception do TrapException(exc, 'MoveSprite');
		end;
	end;

	procedure MoveSpriteTo(spriteToMove : Sprite; x,y : LongInt); cdecl; export;
	begin
		Try
			SGSDK_Graphics.MoveSpriteTo(SpriteToMove, x, y);
		Except on exc: Exception do TrapException(exc, 'MoveSpriteTo');
		end;
	end;

	function IsSpriteOffscreen(theSprite : Sprite): LongInt; cdecl; export;
	begin
		Try
			if SGSDK_Graphics.IsSpriteOffscreen(theSprite) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'IsSpriteOffscreen');
		end;
		result := -1;
	end;
	
	procedure ReplayAnimation(theSprite : Sprite);cdecl; export;
	begin
		Try
			SGSDK_Graphics.ReplayAnimation(theSprite);
		Except on exc: Exception do TrapException(exc, 'ReplayAnimation');
		end;
	end;
	
	procedure DrawBitmapPartOnScreen(bitmapToDraw : Bitmap; srcX, srcY, srcW, srcH, x, y : LongInt); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawBitmapPartOnScreen(bitmapToDraw, srcX, srcY, srcW, srcH, x ,y);
		Except on exc: Exception do TrapException(exc, 'DrawBitmapPartOnScreen');
		end;
	end;
	
	procedure DrawBitmapOnScreen(bitmapToDraw : Bitmap; x, y : LongInt); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawBitmapOnScreen(bitmapToDraw, x ,y );
		Except on exc: Exception do TrapException(exc, 'DrawBitmapOnScreen');
		end;
	end;
	
	procedure DrawPixelOnScreen(theColour: Colour; x, y: LongInt); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawPixelOnScreen(theColour, x ,y);
		Except on exc: Exception do TrapException(exc, 'DrawPixelOnScreen');
		end;
	end;
	
	procedure DrawRectangleOnScreen(theColour : Colour; filled : LongInt;
							xPos, yPos, width, height : LongInt); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawRectangleOnScreen(theColour, filled = -1, xPos, yPos, width, height);
		Except on exc: Exception do TrapException(exc, 'DrawRectangleOnScreen');
		end;
	end;

	procedure DrawLineOnScreen(theColour: Colour; xPosStart, yPosStart,
					 xPosEnd, yPosEnd: LongInt); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawLineOnScreen(theColour, xPosStart, yPosStart, xPosEnd, yPosEnd);
		Except on exc: Exception do TrapException(exc, 'DrawLineOnScreen');
		end;
	end;
	
	procedure DrawHorizontalLineOnScreen(theColor: Color; y, x1, x2: LongInt); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawHorizontalLineOnScreen(theColor, y, x1, x2);
		Except on exc: Exception do TrapException(exc, 'DrawHorizontalLineOnScreen');
		end;
	end;
	
	procedure DrawVerticalLineOnScreen(theColor: Color; x, y1, y2: LongInt); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawVerticalLineOnScreen(theColor, x, y1, y2);
		Except on exc: Exception do TrapException(exc, 'DrawVerticalLineOnScreen');
		end;
	end;
	
	procedure DrawCircleOnScreen(theColour: Colour; filled: LongInt;
						 xc, yc, radius: LongInt); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawCircleOnScreen(theColour, filled = -1, xc, yc, radius);
		Except on exc: Exception do TrapException(exc, 'DrawCircleOnScreen');
		end;
	end;
	
	procedure DrawEllipseOnScreen(theColour: Colour; filled: LongInt;
						xPos, yPos, width, height: LongInt); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawEllipseOnScreen(theColour, filled = -1, xPos, yPos, width, height);
		Except on exc: Exception do TrapException(exc, 'DrawEllipseOnScreen');
		end;
	end;
	
	function XOffset(): LongInt; cdecl; export;
	begin
		Try
			result := SGSDK_Camera.XOffset();
			exit;
		Except on exc: Exception do TrapException(exc, 'XOffset');
		end;
		result := 0;
	end;
	
	function YOffset(): LongInt; cdecl; export;
	begin
		Try
			result := SGSDK_Camera.YOffset();
			exit;
		Except on exc: Exception do TrapException(exc, 'YOffset');
		end;
		result := 0;
	end;
	
	function ScreenX(x: Single): LongInt; cdecl; export;
	begin
		Try
			result := SGSDK_Camera.ScreenX(x);
			exit;
		Except on exc: Exception do TrapException(exc, 'ScreenX');
		end;
		result := 0;
	end;
	
	function ScreenY(y: Single): LongInt; cdecl; export;
	begin
		Try
			result := SGSDK_Camera.ScreenY(y);
			exit;
		Except on exc: Exception do TrapException(exc, 'ScreenY');
		end;
		result := 0;
	end;
	
	function GameX(x: LongInt) : Single; cdecl; export;
	begin
		Try
			result := SGSDK_Camera.GameX(x);
			exit;
		Except on exc: Exception do TrapException(exc, 'GameX');
		end;
		result := 0;
	end;
	
	function GameY(y: LongInt) : Single; cdecl; export;
	begin
		Try
			result := SGSDK_Camera.GameY(y);
			exit;
		Except on exc: Exception do TrapException(exc, 'GameY');
		end;
		result := 0;
	end;
	
	function ToGameCoordinates(screenPoint: Point2D): Point2D; cdecl; export;
	begin
		Try
			result := SGSDK_Camera.ToGameCoordinates(screenPoint);
			exit;
		Except on exc: Exception do TrapException(exc, 'ToGameCoordinates');
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
		Except on exc: Exception do TrapException(exc, 'MoveVisualArea');
		end;
	end;
	
	procedure SetScreenOffset(x, y: Single); cdecl; export;
	begin
		Try
			SGSDK_Camera.SetScreenOffset(x, y);
		Except on exc: Exception do TrapException(exc, 'SetScreenOffset');
		end;
	end;
	
	procedure FollowSprite(spr : Sprite; xOffset, yOffset : LongInt); cdecl; export;
	begin
		Try
			SGSDK_Camera.FollowSprite(spr, xOffset, yOffset);
		Except on exc: Exception do TrapException(exc, 'FollowSprite');
		end;
	end;
	
	procedure SetClip(bmp: Bitmap; x, y, w, h: LongInt); cdecl; export;
	begin
		Try
			if bmp <> nil then SGSDK_Graphics.SetClip(bmp, x, y, w, h)
			else SGSDK_Graphics.SetClip(x, y, w, h);
		Except on exc: Exception do TrapException(exc, 'SetClip');
		end;		
	end;

	procedure ResetClip(bmp: Bitmap); cdecl; export;
	begin
		Try
			if bmp <> nil then SGSDK_Graphics.ResetClip(bmp)
			else SGSDK_Graphics.ResetClip();
		Except on exc: Exception do TrapException(exc, 'ResetClip');
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
		Except on exc: Exception do TrapException(exc, 'LoadMap');
		end;
		result := nil;
	end;
	
	procedure DrawMap(m : Map); cdecl; export;
	begin
		Try
			SGSDK_MappyLoader.DrawMap(m);
		Except on exc: Exception do TrapException(exc, 'DrawMap');
		end;
	end;
	
	function CollisionWithMapVector(m : Map; spr : Sprite; vec: Vector): CollisionSide; cdecl; export;
	begin
		Try
			result := SGSDK_MappyLoader.CollisionWithMap(m, spr,vec);
			exit;
		Except on exc: Exception do TrapException(exc, 'CollisionWithMap');
		end;
		result := CollisionSide(-1);
	end;
	
	function EventCount(m : Map; anEvent: Event): LongInt; cdecl; export;
	begin
		Try
			result := SGSDK_MappyLoader.EventCount(m, anEvent);
			exit;
		Except on exc: Exception do TrapException(exc, 'EventCount');
		end;
		result := -1;
	end;
	
	function EventPositionX(m : Map; anEvent : Event; eventnumber : LongInt): LongInt; cdecl; export;
	begin
		Try
			result := SGSDK_MappyLoader.EventPositionX(m, anEvent, eventnumber);
			exit;
		Except on exc: Exception do TrapException(exc, 'EventPositionX');
		end;
		result := -1;
	end;
	
	function EventPositionY(m : Map; anEvent : Event; eventnumber : LongInt): LongInt; cdecl; export;
	begin
		Try
			result := SGSDK_MappyLoader.EventPositionY(m, anEvent, eventnumber);
			exit;
		Except on exc: Exception do TrapException(exc, 'EventPositionY');
		end;
		result := -1;
	end;
	
	procedure FreeMap(m : Map); cdecl; export;
	begin
		Try
			SGSDK_MappyLoader.FreeMap(m);
		Except on exc: Exception do TrapException(exc, 'FreeMap');
		end;
	end;
	
	function SpriteHasCollidedWithMapTile(m : Map; spr : Sprite; out collidedX, collidedY : LongInt): LongInt; cdecl; export;
	begin
		Try
			if SGSDK_MappyLoader.SpriteHasCollidedWithMapTile(m, spr, collidedX, collidedY) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'SpriteHasCollidedWithMapTile');
		end;
	end;
	
	function WillCollideOnSide(m: Map; spr : Sprite): CollisionSide; cdecl; export;
	begin
		Try
			result := SGSDK_MappyLoader.WillCollideOnSide(m, spr);
		Except on exc: Exception do TrapException(exc, 'WillCollideOnSide');
		end;
	end;
	
	procedure MoveSpriteOutOfTile(m: Map; spr : Sprite; x, y : LongInt); cdecl; export;
	begin
		Try
			SGSDK_Mappyloader.MoveSpriteOutOfTile(m, spr, x, y);
		Except on exc: Exception do TrapException(exc, 'MoveSpriteOutOfTile');
		end;
	end;
	
	//New!
	function MapWidth(m : Map): LongInt; cdecl; export;
	begin
		Try
			result := SGSDK_Mappyloader.MapWidth(m);
			exit;
		Except on exc: Exception do TrapException(exc, 'MapWidth');
		end;
		result := -1;
	end;
	
	function MapHeight(m : Map): LongInt; cdecl; export;
	begin
		Try
			result := SGSDK_Mappyloader.MapHeight(m);
			exit;
		Except on exc: Exception do TrapException(exc, 'MapHeight');
		end;
		result := -1;
	end;
	
	function BlockWidth(m : Map): LongInt; cdecl; export;
	begin
		Try
			result := SGSDK_Mappyloader.BlockWidth(m);
			exit;
		Except on exc: Exception do TrapException(exc, 'BlockWidth');
		end;
		result := 01;
	end;
	
	function BlockHeight(m : Map): LongInt; cdecl; export;
	begin
		Try
			result := SGSDK_Mappyloader.BlockHeight(m);
			exit;
		Except on exc: Exception do TrapException(exc, 'BlockHeight');
		end;
		result := -1;
	end;
	
	function GapX(m : Map): LongInt; cdecl; export;
	begin
		Try
			result := SGSDK_Mappyloader.GapX(m);
			exit;
		Except on exc: Exception do TrapException(exc, 'GapX');
		end;
		result := -1;
	end;
	
	function GapY(m : Map): LongInt; cdecl; export;
	begin
		Try
			result := SGSDK_Mappyloader.GapY(m);
			exit;
		Except on exc: Exception do TrapException(exc, 'GapY');
		end;
		result := -1;
	end;
	
	function StaggerX(m : Map): LongInt; cdecl; export;
	begin
		Try
			result := SGSDK_Mappyloader.StaggerX(m);
			exit;
		Except on exc: Exception do TrapException(exc, 'StaggerX');
		end;
		result := -1;
	end;
	
	function StaggerY(m : Map): LongInt; cdecl; export;
	begin
		Try
			result := SGSDK_Mappyloader.StaggerY(m);
			exit;
		Except on exc: Exception do TrapException(exc, 'StaggerY');
		end;
		result := -1;
	end;
	
	function GetTileFromPoint(point: Point2D; m: Map): Tile; cdecl; export;
	begin
		Try
			result := SGSDK_Mappyloader.GetTileFromPoint(point, m);
			exit;
		Except on exc: Exception do TrapException(exc, 'GetTileFromPoint');
		end;
		result.xIndex := -1;
		result.yIndex := -1;
		result.topCorner := SGSDK_Shapes.CreatePoint(0,0);
		result.PointA := SGSDK_Shapes.CreatePoint(0,0);
		result.PointB := SGSDK_Shapes.CreatePoint(0,0);
		result.PointC := SGSDK_Shapes.CreatePoint(0,0);
		result.PointD := SGSDK_Shapes.CreatePoint(0,0);
	end;
	
	function GetEventAtTile(m : Map; xIndex, yIndex: LongInt): LongInt; cdecl; export;
	begin
		Try
			result := LongInt(SGSDK_Mappyloader.GetEventAtTile(m, xIndex, yIndex));
			exit;
		Except on exc: Exception do TrapException(exc, 'GetEventAtTile');
		end;
		result := -1;
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
		Except on exc: Exception do TrapException(exc, 'DistancePointToLine');
		end;	
		result := -1;	
	end;
	
	function ClosestPointOnLine(x, y: Single; line: LineSegment): Point2D; cdecl; export;
	begin
		Try
			result := SGSDK_Shapes.ClosestPointOnLine(x, y, line);
			exit;
		Except on exc: Exception do TrapException(exc, 'ClosestPointOnLine');
		end;	
		result.x := -1;			
		result.y := -1;
	end;
	
	function CenterPoint(sprt: Sprite): Point2D; cdecl; export;
	begin
		Try
			result := SGSDK_Shapes.CenterPoint(sprt);
			exit;
		Except on exc: Exception do TrapException(exc, 'CenterPoint');
		end;	
		result.x := -1;			
		result.y := -1;		
	end;
	
	function IsPointOnLine(pnt: Point2D; line: LineSegment): LongInt; cdecl; export;
	begin
		Try
			if SGSDK_Shapes.IsPointOnLine(pnt, line) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'IsPointOnLine');
		end;	
		result := 0;		
	end;
	
	function GetLineIntersectionPoint(line1, line2: LineSegment; out pnt: Point2D) : LongInt; cdecl; export;
	begin
		Try
			if SGSDK_Shapes.GetLineIntersectionPoint(line1, line2, pnt) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'GetLineIntersectionPoint');
		end;	
		result := 0;
	end;
	
	//##__a|
	function LineIntersectsWithLines(target: LineSegment; len: LongInt; data: LineSegPtr): LongInt; cdecl; export;
	var
		arr: LinesArray;
	begin
		Try
			PopulateLineSegmentArray(data, len, arr);
			
			if SGSDK_Shapes.LineIntersectsWithLines(target, arr) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'LineIntersectsWithLines');
		end;	
		result := 0;
	end;
	
	function HasBitmapCollidedWithRect(image: Bitmap; x, y, rectX, rectY, rectWidth, rectHeight: LongInt): LongInt; cdecl; export;
	begin
		Try
			if SGSDK_Physics.HasBitmapCollidedWithRect(image, x, y, rectX, rectY, rectWidth, rectHeight) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'HasBitmapCollidedWithRect');
		end;
		result := 0;
	end;
	
{
	function CreateTriangle(ax, ay, bx, by, cx, cy: Single): Triangle; cdecl; export;
	begin
		Try
			result := SGSDK_Shapes.CreateTriangle(ax, ay, bx, by, cx, cy);
		Except on exc: Exception do TrapException(exc, 'CreateTriangle');
		end;
	end;
}	

  //##_a|
	function IsPointInTriangle(point: Point2D; inTriangle: Point2DPtr): LongInt; cdecl; export;
	var
	  tri: Triangle;
	begin
	  PopulateTriangle(inTriangle, tri);
	  
		Try
			if SGSDK_Shapes.IsPointInTriangle(point, tri) then result := -1
			else result := 0;
			exit;
		Except on exc: Exception do TrapException(exc, 'IsPointInTriangle');
		end;	
		result := 0;
	end;


//***
//
// New in verson 2
//
//***

procedure StartReadingTextWithText(text: PChar; textColor: Colour; maxLength: LongInt; theFont: Font; x, y: LongInt); cdecl; export;
begin
	Try
		SGSDK_Input.StartReadingTextWithText(text, textColor, maxLength, theFont, x, y);
	Except on exc: Exception do TrapException(exc, 'StartReadingTextWithText');
	end;
end;

procedure DrawSimpleText(theText: PChar; textColor: Color; x, y: Single); cdecl; export;
begin
	Try
		SGSDK_Font.DrawText(theText, textColor, x, y);
	Except on exc: Exception do TrapException(exc, 'DrawSimpleText');
	end;
end;

procedure DrawSimpleFramerate(x, y: Single); cdecl; export;
begin
	Try
		SGSDK_Font.DrawFramerate(x, y);
	Except on exc: Exception do TrapException(exc, 'DrawSimpleFramerate');
	end;
end;


procedure DrawSimpleTextOnScreen(theText: PChar; textColor: Color; x, y: Single); cdecl; export;
begin
	Try
		SGSDK_Font.DrawTextOnScreen(theText, textColor, x, y);
	Except on exc: Exception do TrapException(exc, 'DrawSimpleTextOnScreen');
	end;    
end;

procedure DrawSimpleTextOn(dest: Bitmap; theText: PChar; textColor: Color; x, y: Single); cdecl; export;
begin
	Try
		SGSDK_Font.DrawText(dest, theText, textColor, x, y);
	Except on exc: Exception do TrapException(exc, 'DrawSimpleTextOn');
	end;    	 
end;

//##_a|
procedure DrawTriangleOnScreen(theColour: Colour; firstPoint: Point2DPtr); cdecl; export;
var
  tri: Triangle;
begin
	Try
	  PopulateTriangle(firstPoint, tri);
		SGSDK_Graphics.DrawTriangleOnScreen(theColour, tri);
	Except on exc: Exception do TrapException(exc, 'DrawTriangleOnScreen');
	end;
end;

//##__a|
procedure DrawTriangleWithDestination(dest: Bitmap; theColour: Colour; firstPoint: Point2DPtr); cdecl; export;
var
  tri: Triangle;
begin
	Try
	  PopulateTriangle(firstPoint, tri);
		SGSDK_Graphics.DrawTriangle(dest, theColour, tri);
	Except on exc: Exception do TrapException(exc, 'DrawTriangleWithDestination');
	end;
end;

//##_a|
procedure FillTriangle(theColour: Colour; firstPoint: Point2DPtr); cdecl; export;
var
  tri: Triangle;
begin
	Try
	  PopulateTriangle(firstPoint, tri);
		SGSDK_Graphics.FillTriangle(theColour, tri);
	Except on exc: Exception do TrapException(exc, 'FillTriangle');
	end;
end;

//##_a|
procedure FillTriangleOnScreen(theColour: Colour; firstPoint: Point2DPtr); cdecl; export;
var
  tri: Triangle;
begin
	Try
	  PopulateTriangle(firstPoint, tri);
		SGSDK_Graphics.FillTriangleOnScreen(theColour, tri);
	Except on exc: Exception do TrapException(exc, 'FillTriangleOnScreen');
	end;
end;

//##__a|
procedure FillTriangleWithDestination(dest: Bitmap; theColour: Colour; firstPoint: Point2DPtr); cdecl; export;
var
  tri: Triangle;
begin
	Try
	  PopulateTriangle(firstPoint, tri);
		SGSDK_Graphics.FillTriangle(dest, theColour, tri);
	Except on exc: Exception do TrapException(exc, 'FillTriangleWithDestination');
	end;
end;

procedure UpdateSpriteAnimationPct(spriteToDraw: Sprite; pct: Single); cdecl; export;
begin
	Try
		SGSDK_Graphics.UpdateSpriteAnimation(spriteToDraw, pct);
	Except on exc: Exception do TrapException(exc, 'UpdateSpriteAnimationPct');
	end;
end;

procedure UpdateSpritePct(spriteToDraw: Sprite; pct: Single); cdecl; export;
begin
	Try
		SGSDK_Graphics.UpdateSprite(spriteToDraw, pct);
	Except on exc: Exception do TrapException(exc, 'UpdateSpritePct');
	end;
end;

procedure PlaySoundEffectLoopVolume(effect: SoundEffect; loops: LongInt; vol: Single); cdecl; export;
begin
	Try
		SGSDK_Audio.PlaySoundEffect(effect, loops, vol);
	Except on exc: Exception do TrapException(exc, 'PlaySoundEffectLoopVolume');
	end;
end;

procedure SetMusicVolume(vol: Single); cdecl; export;
begin
	Try
		SGSDK_Audio.SetMusicVolume(vol);
	Except on exc: Exception do TrapException(exc, 'SetMusicVolume');
	end;  
end;

function MusicVolume(): Single; cdecl; export;
begin
	Try
		result := SGSDK_Audio.MusicVolume();
	Except on exc: Exception do TrapException(exc, 'MusicVolume');
	end;    
end;

procedure MakeOpaque(bmp: Bitmap); cdecl; export;
begin
	Try
		SGSDK_Graphics.MakeOpaque(bmp);
	Except on exc: Exception do TrapException(exc, 'MakeOpaque');
	end;      
end;

procedure MakeTransparent(bmp: Bitmap); cdecl; export;
begin
	Try
		SGSDK_Graphics.MakeTransparent(bmp);
	Except on exc: Exception do TrapException(exc, 'MakeTransparent');
	end;        
end;

function RotateZoomBitmap(src: Bitmap; degRot, zoom: Single): Bitmap; cdecl; export;
begin
	Try
		result := SGSDK_Graphics.RotateZoomBitmap(src, degRot, zoom);
	Except on exc: Exception do TrapException(exc, 'RotateZoomBitmap');
	end;          
end;

procedure SetupBitmapForCollisions(src: Bitmap); cdecl; export;
begin
  Try
		SGSDK_Graphics.SetupBitmapForCollisions(src);
	Except on exc: Exception do TrapException(exc, 'SetupBitmapForCollisions');
	end;
end;

function AKeyWasPressed(): LongInt; cdecl; export;
begin
  Try
		if SGSDK_Input.AKeyWasPressed() then result := 1
		else result := 0;
	Except on exc: Exception do TrapException(exc, 'AKeyWasPressed');
	end;  
end;

//##a|
function TriangleBarycenter(firstPoint: Point2DPtr): Point2D; cdecl; export;
var
  tri: Triangle;
begin
  Try
    PopulateTriangle(firstPoint, tri);
    result := SGSDK_Shapes.TriangleBarycenter(tri);
  Except on exc: Exception do TrapException(exc, 'TriangleBarycenter');
  end;  
end;

//##_aooo|
procedure ApplyMatrixToTriangle(m: Matrix2DPtr; pnt: Point2DPtr; out pA, pB, pC: Point2D); cdecl; export;
var
  tri: Triangle;
begin
  Try
    PopulateTriangle(pnt, tri);
    SGSDK_Shapes.ApplyMatrix(m.data, tri);
    pA := tri[0];
    pB := tri[1];
    pC := tri[2];
  Except on exc: Exception do TrapException(exc, 'TriangleBarycenter');
  end;    
end;

procedure SetSpriteRotation(s: Sprite; rot: Single); cdecl; export;
begin
  s.rotation := rot;
end;

function GetSpriteRotation(s: Sprite): Single; cdecl; export;
begin
  result := s.rotation;
end;

procedure SetSpriteZoom(s: Sprite; zoom: Single); cdecl; export;
begin
  s.zoom := zoom;
end;

function GetSpriteZoom(s: Sprite): Single; cdecl; export;
begin
  result := s.zoom;
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
	
	OpenGraphicsWindow name 'OpenGraphicsWindow',
	WindowCloseRequested name 'WindowCloseRequested',
	ProcessEvents name 'ProcessEvents',
	SetIcon name 'SetIcon',
	ChangeScreenSize name 'ChangeScreenSize',
	ToggleFullScreen name 'ToggleFullScreen',
	RefreshScreenWithFrame name 'RefreshScreenWithFrame',
	RefreshScreen name 'RefreshScreen',
	TakeScreenShot name 'TakeScreenShot',
	ScreenWidth name 'ScreenWidth',
	ScreenHeight name 'ScreenHeight',
	ToSDLColor name 'ToSDLColor',
	GetColourBitmap name 'GetColourBitmap',	
	GetColourRGBA name 'GetColourRGBA',
	GetFramerate name 'GetFramerate',	
	GetTicks name 'GetTicks',	
	Sleep name 'Sleep',	
	//GetPathToResourceWithKind,
	GetPathToResourceWithBaseAndKind name 'GetPathToResourceWithBaseAndKind',
	//GetPathToResource,
	//GetPathToResourceWithBase,
	//GetPathToResourceWithBaseAndKind,
	RegisterEventProcessor name 'RegisterEventProcessor',	
{	Cos,
	Sin,
	Tan,}	
	CreateTimer name 'CreateTimer', {1.1}
	FreeTimer name 'FreeTimer', {1.1}
	StartTimer name 'StartTimer', {1.1}
	StopTimer name 'StopTimer', {1.1}
	PauseTimer name 'PauseTimer', {1.1}
	UnpauseTimer name 'UnpauseTimer', {1.1}
	GetTimerTicks name 'GetTimerTicks', {1.1}
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Input
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************

	//GetMousePositionAsVector,
	//GetMousePosition,
	GetMouseXY name 'GetMouseXY',
	GetMouseMovement name 'GetMouseMovement',
	IsMouseDown name 'IsMouseDown',
	IsMouseUp name 'IsMouseUp',
	MouseWasClicked name 'MouseWasClicked',
	StartReadingText name 'StartReadingText',
	EndReadingText name 'EndReadingText', {1.1.5}
	IsReadingText name 'IsReadingText',
	TextReadAsASCII name 'TextReadAsASCII',
	IsKeyPressed name 'IsKeyPressed',
	WasKeyTyped name 'WasKeyTyped',
	ShowMouse name 'ShowMouse',
	MoveMouse name 'MoveMouse',
	IsMouseShown name 'IsMouseShown',
	StartReadingTextWithText name 'StartReadingTextWithText', //2
	AKeyWasPressed name 'AKeyWasPressed', //2
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Audio
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	
	OpenAudio name 'OpenAudio',
	CloseAudio name 'CloseAudio',
	LoadSoundEffect name 'LoadSoundEffect',
	LoadMusic name 'LoadMusic',
	FreeMusic name 'FreeMusic',
	FreeSoundEffect name 'FreeSoundEffect',
	PlayMusic name 'PlayMusic',
	PlaySoundEffect name 'PlaySoundEffect',
	PlaySoundEffectLoop name 'PlaySoundEffectLoop',
	PlaySoundEffectLoopVolume name 'PlaySoundEffectLoopVolume', // new in 2
	IsMusicPlaying name 'IsMusicPlaying',
	IsSoundEffectPlaying name 'IsSoundEffectPlaying',
	StopSoundEffect name 'StopSoundEffect',
	StopMusic name 'StopMusic',
	SetMusicVolume name 'SetMusicVolume', // new in 2
	MusicVolume name 'MusicVolume', // new in 2
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Font
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	
	LoadFont name 'LoadFont',
	SetFontStyle name 'SetFontStyle',
	FreeFont name 'FreeFont',
	DrawText name 'DrawText',
	DrawTextLines name 'DrawTextLines',
	DrawTextOnBitmap name 'DrawTextOnBitmap',
	DrawTextLinesOnBitmap name 'DrawTextLinesOnBitmap',
	DrawTextOnScreen name 'DrawTextOnScreen',
	DrawTextLinesOnScreen name 'DrawTextLinesOnScreen',
	TextWidth name 'TextWidth',
	TextHeight name 'TextHeight',
	DrawFramerate name 'DrawFramerate',
	DrawSimpleText name 'DrawSimpleText', // new in 2
	DrawSimpleTextOnScreen name 'DrawSimpleTextOnScreen', // new in 2
	DrawSimpleTextOn name 'DrawSimpleTextOn', // new in 2
	DrawSimpleFramerate name 'DrawSimpleFramerate', //new in 2
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Physics
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	HasSpriteCollidedX name 'HasSpriteCollidedX',
	HasSpriteCollidedY name 'HasSpriteCollidedY',
	HasSpriteCollidedWithRect name 'HasSpriteCollidedWithRect',
	HaveSpritesCollided name 'HaveSpritesCollided',
	HasSpriteCollidedWithBitmap name 'HasSpriteCollidedWithBitmap', {1.1}
	HasSpriteCollidedWithBitmapPart name 'HasSpriteCollidedWithBitmapPart', {1.1}
	HaveBitmapsCollided name 'HaveBitmapsCollided',
	HaveBitmapPartsCollided name 'HaveBitmapPartsCollided',
{	CreateVector,
	AddVectors,
	SubtractVectors,
	InvertVector,}
	CircleHasCollidedWithLine name 'CircleHasCollidedWithLine',
	RectangleHasCollidedWithLine name 'RectangleHasCollidedWithLine',
	VectorOutOfRectFromPoint name 'VectorOutOfRectFromPoint',	
	VectorOutOfRectFromRect name 'VectorOutOfRectFromRect',
	LimitMagnitude name 'LimitMagnitude',
	GetUnitVector name 'GetUnitVector',
	//IsZeroVector,
	//Magnitude,
	//DotProduct,
	//MultiplyVector,
	CalculateAngle name 'CalculateAngle',
	//CalculateAngleSprite,
	TranslationMatrix name 'TranslationMatrix',
	ScaleMatrix name 'ScaleMatrix',
	RotationMatrix name 'RotationMatrix',
	MultiplyMatrix2D name 'MultiplyMatrix2D',
	MultiplyMatrix2DAndVector name 'MultiplyMatrix2DAndVector',
	VectorCollision name 'VectorCollision',
	GetMatrix2DElement name 'GetMatrix2DElement',
	SetMatrix2DElement name 'SetMatrix2DElement',
	FreeMatrix2D name 'FreeMatrix2D',
	//GetVectorFromAngle,
	VectorOutOfCircleFromPoint name 'VectorOutOfCircleFromPoint', {1.1}
	VectorOutOfCircleFromCircle name 'VectorOutOfCircleFromCircle', {1.1}
	CircleCollisionWithLine name 'CircleCollisionWithLine', {1.1}
	CircularCollision name 'CircularCollision', {1.1}
	HasBitmapCollidedWithRect name 'HasBitmapCollidedWithRect',
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Graphics
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	GetPixelFromScreen name 'GetPixelFromScreen',
	GetPixel name 'GetPixel',
	GetSpriteBitmap name 'GetSpriteBitmap',
	GetSpriteX name 'GetSpriteX',
	GetSpriteY name 'GetSpriteY',
	GetSpriteCurrentFrame name 'GetSpriteCurrentFrame',
	GetSpriteUsePixelCollision name 'GetSpriteUsePixelCollision',
	SetSpriteX name 'SetSpriteX',
	SetSpriteY name 'SetSpriteY',
	SetSpriteCurrentFrame name 'SetSpriteCurrentFrame',
	SetSpriteUsePixelCollision name 'SetSpriteUsePixelCollision',
	GetSpriteMass name 'GetSpriteMass',
	GetSpriteMovement name 'GetSpriteMovement',
	SetSpriteMass name 'SetSpriteMass',
	SetSpriteMovement name 'SetSpriteMovement',
	CreateBitmap name 'CreateBitmap',
	OptimiseBitmap name 'OptimiseBitmap',
	LoadBitmapWithTransparentColor name 'LoadBitmapWithTransparentColor',
	LoadTransparentBitmap name 'LoadTransparentBitmap',
	FreeBitmap name 'FreeBitmap',
	GetBitmapWidth name 'GetBitmapWidth',
	GetBitmapHeight name 'GetBitmapHeight',
	ClearSurfaceWithColor name 'ClearSurfaceWithColor',
	DrawBitmapWithDestination name 'DrawBitmapWithDestination',
	DrawBitmapPartWithDestination name 'DrawBitmapPartWithDestination',
	DrawPixelWithDestination name 'DrawPixelWithDestination',
	DrawRectangleWithDestination name 'DrawRectangleWithDestination',
	DrawLineWithDestination name 'DrawLineWithDestination',
	DrawHorizontalLineWithDestination name 'DrawHorizontalLineWithDestination',
	DrawVerticalLineWithDestination name 'DrawVerticalLineWithDestination',
	DrawCircleWithDestination name 'DrawCircleWithDestination',
	DrawEllipseWithDestination name 'DrawEllipseWithDestination',
	FillRectangleWithDestination name 'FillRectangleWithDestination',
	ClearScreen name 'ClearScreen',
	DrawBitmap name 'DrawBitmap',
	DrawBitmapPart name 'DrawBitmapPart',
	DrawPixel name 'DrawPixel',
	DrawRectangle name 'DrawRectangle',
	DrawLine name 'DrawLine',
	DrawHorizontalLine name 'DrawHorizontalLine',
	DrawVerticalLine name 'DrawVerticalLine',
	DrawCircle name 'DrawCircle',
	DrawEllipse name 'DrawEllipse',
	CreateSprite name 'CreateSprite',
	CreateSpriteMulti name 'CreateSpriteMulti',
	CreateSpriteMultiEnding name 'CreateSpriteMultiEnding',
	CreateSpriteArray name 'CreateSpriteArray',
	CreateSpriteArrayEnding name 'CreateSpriteArrayEnding',
	CreateSpriteMultiFPC name 'CreateSpriteMultiFPC',
	CreateSpriteArrayFPC name 'CreateSpriteArrayFPC',
	UpdateSpriteAnimation name 'UpdateSpriteAnimation',
	UpdateSprite name 'UpdateSprite',
	GetSpriteKind name 'GetSpriteKind',	
	GetSpriteFramesPerCell name 'GetSpriteFramesPerCell',
	GetSpriteCols name 'GetSpriteCols',
	GetSpriteRow name 'GetSpriteRow',
	GetSpriteFrameCount name 'GetSpriteFrameCount',
	GetSpriteEndingAction name 'GetSpriteEndingAction',
	GetSpritehasEnded name 'GetSpritehasEnded',
	GetSpriteReverse name 'GetSpriteReverse',
	ReplayAnimation name 'ReplayAnimation',
	SetSpriteKind name 'SetSpriteKind',
	SetSpriteFramesPerCell name 'SetSpriteFramesPerCell',
	SetSpriteEndingAction name 'SetSpriteEndingAction',
	FreeSprite name 'FreeSprite',
	AddBitmapToSprite name 'AddBitmapToSprite',
	CurrentHeight name 'CurrentHeight',
	CurrentWidth name 'CurrentWidth',
	DrawSprite name 'DrawSprite',
	//DrawSpriteOffset,
	MoveSpriteItself name 'MoveSpriteItself', {1.0 - missing added 1.1}
	MoveSprite name 'MoveSprite',
	MoveSpriteTo name 'MoveSpriteTo',
	IsSpriteOffscreen name 'IsSpriteOffscreen',
	IsSpriteOnScreenAt name 'IsSpriteOnScreenAt',
	DrawBitmapPartOnScreen name 'DrawBitmapPartOnScreen',
	DrawBitmapOnScreen name 'DrawBitmapOnScreen',
	DrawPixelOnScreen name 'DrawPixelOnScreen',
	DrawRectangleOnScreen name 'DrawRectangleOnScreen',
	DrawLineOnScreen name 'DrawLineOnScreen',
	DrawHorizontalLineOnScreen name 'DrawHorizontalLineOnScreen',
	DrawVerticalLineOnScreen name 'DrawVerticalLineOnScreen',
	DrawCircleOnScreen name 'DrawCircleOnScreen',
	DrawEllipseOnScreen name 'DrawEllipseOnScreen',
	XOffset name 'XOffset',
	YOffset name 'YOffset',
	ScreenX name 'ScreenX',
	ScreenY name 'ScreenY',
	GameX name 'GameX',
	GameY name 'GameY',
	ToGameCoordinates name 'ToGameCoordinates',
	//MoveVisualAreaWithVector,
	MoveVisualArea name 'MoveVisualArea',
	SetScreenOffset name 'SetScreenOffset',
	FollowSprite name 'FollowSprite',
	SetClip name 'SetClip',
	ResetClip name 'ResetClip',
	DrawTriangle name 'DrawTriangle', {1.1.5}
	DrawTriangleWithDestination name 'DrawTriangleWithDestination', // new in 2
	DrawTriangleOnScreen name 'DrawTriangleOnScreen', // new in 2
	FillTriangle name 'FillTriangle', // new in 2
	FillTriangleWithDestination name 'FillTriangleWithDestination', // new in 2
	FillTriangleOnScreen name 'FillTriangleOnScreen', // new in 2
	MakeTransparent name 'MakeTransparent', // new in 2
	MakeOpaque name 'MakeOpaque', //new in 2
	SetSpriteRotation name 'SetSpriteRotation', //2
	GetSpriteRotation name 'GetSpriteRotation', //2
	SetSpriteZoom name 'SetSpriteZoom', //2
	GetSpriteZoom name 'GetSpriteZoom', //2
	RotateZoomBitmap name 'RotateZoomBitmap', //2
	SetupBitmapForCollisions name 'SetupBitmapForCollisions', //2
	UpdateSpriteAnimationPct name 'UpdateSpriteAnimationPct', //2
	UpdateSpritePct name 'UpdateSpritePct', //2

	
	///-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
	//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
	// 					MappyLoader
	//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
	//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
	LoadMap name 'LoadMap',
	DrawMap name 'DrawMap',
	CollisionWithMapVector name 'CollisionWithMapVector',
	EventCount name 'EventCount',
	EventPositionX name 'EventPositionX',
	EventPositionY name 'EventPositionY',
	FreeMap name 'FreeMap',
	//v1.1
	SpriteHasCollidedWithMapTile name 'SpriteHasCollidedWithMapTile',
	WillCollideOnSide name 'WillCollideOnSide',
	MoveSpriteOutOfTile name 'MoveSpriteOutOfTile',
	//v1.1.5
	MapWidth name 'MapWidth',
	MapHeight name 'MapHeight',
	BlockWidth name 'BlockWidth',
	BlockHeight name 'BlockHeight',
	GapX name 'GapX',
	GapY name 'GapY',
	StaggerX name 'StaggerX',
	StaggerY name 'StaggerY',
	GetTileFromPoint name 'GetTileFromPoint',
	GetEventAtTile name 'GetEventAtTile',

	///-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
	//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
	// 					Shapes
	//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
	//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\

	DistancePointToLine name 'DistancePointToLine', {1.1}
	ClosestPointOnLine name 'ClosestPointOnLine', {1.1}
	CenterPoint name 'CenterPoint', {1.1}
	IsPointOnLine name 'IsPointOnLine', {1.1}
	GetLineIntersectionPoint name 'GetLineIntersectionPoint', {1.1}
	LineIntersectsWithLines name 'LineIntersectsWithLines', {1.1}
	//CreateTriangle, {1.1.5}
	IsPointInTriangle name 'IsPointInTriangle', {1.1.5}
	TriangleBarycenter name 'TriangleBarycenter', //2
	ApplyMatrixToTriangle name 'ApplyMatrixToTriangle', //2
	

	///-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
	//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
	// 					Error Handling
	//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
	//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
	DLLVersion name 'DLLVersion',
	GetExceptionMessage name 'GetExceptionMessage',
	ExceptionOccured name 'ExceptionOccured';
end.

