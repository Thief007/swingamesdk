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
//  - 2008-01-16: Andrew Cain: Modified exception handling.

library SGSDK;

uses 
	SGSDK_Core, SGSDK_Input, SGSDK_Audio, SGSDK_Font, SGSDK_Physics, SGSDK_Graphics,
	SDL_Mixer, SDL, SDL_Image, SDL_TTF, SDLEventProcessing, SGSDK_Camera, 
	SGSDK_MappyLoader, SysUtils;

	type
		//IntPtr used to receive arrays
		IntPtr = ^Integer;
		//BitmapPtr used to receive Bitmap arrays
		BitmapPtr = ^Bitmap;
		
		IntArray = Array of Integer;
		BitmapArray = Array of Bitmap;
		Matrix2DPtr = ^Matrix2D;
		MapPtr = ^Map;


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
		SGSDK_Core.ProcessEvents();
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
			
			exit;
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

{	procedure RegisterEventProcessor(handle: EventProcessPtr; handle2: EventStartProcessPtr); cdecl; export;
	begin
		Try
			SGSDK_Core.RegisterEventProcessor(handle, handle2);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
}	
	function Cos(angle: Single): Single; cdecl; export;
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
	end;
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Input
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	procedure ShowMouse();
	begin
		try
			SGSDK_Input.ShowMouse();
		Except on exc: Exception  do TrapException(exc);
		end;
	end;
	
	procedure HideMouse();
	begin
		try
			SGSDK_Input.HideMouse();
		Except on exc: Exception  do TrapException(exc);
		end;
	end;
	
	procedure MoveMouse(x : UInt16; y : UInt16);
	begin
		try
			SGSDK_Input.MoveMouse(x,y);
		Except on exc: Exception  do TrapException(exc);
		end;
	end;
	function GetMousePosition(): Vector; cdecl; export;
	begin
		Try
			result := SGSDK_Input.GetMousePosition();
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := CreateVector(0,0);
	end;
	
	function GetMouseMovement(): Vector; cdecl; export;
	begin
		Try
			result := SGSDK_Input.GetMouseMovement();
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := CreateVector(0,0);
	end;
	
	function IsMouseDown(button: MouseButton): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Input.IsMouseDown(button) then
			begin
				result:= -1
			end
			else
			begin
				result:= 0
			end;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	function IsMouseUp(button: MouseButton): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Input.IsMouseUp(button) then
			begin
				result:= -1
			end
			else
			begin
				result:= 0
			end;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := -1;
	end;
	
	function MouseWasClicked(button: MouseButton): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Input.MouseWasClicked(button) then
			begin
				result:= -1
			end
			else
			begin
				result:= 0
			end;
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
			if SGSDK_Input.IsReadingText() then
			begin
				result:= -1
			end
			else
			begin
				result:= 0
			end;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;
	end;
	
	function TextReadAsASCII(): PChar; cdecl; export;
	begin
		Try
			result := PChar(SGSDK_Input.TextReadAsASCII());
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := PChar('');
	end;
		
	function IsKeyPressed(virtKeyCode : Integer): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Input.IsKeyPressed(virtKeyCode) then
			begin
				result:= -1
			end
			else
			begin
				result:= 0
			end;
			exit;
		Except on exc: Exception do TrapException(exc);
		end;
		result := 0;
	end;
	
	function WasKeyTyped(virtKeyCode: Integer): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Input.WasKeyTyped(virtKeyCode) then
			begin
				result:= -1
			end
			else
			begin
				result:= 0
			end;
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
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function LoadMusic(path: PChar): Music; cdecl; export;
	begin
		Try
			result := SGSDK_Audio.LoadMusic(path);
		Except on exc: Exception do TrapException(exc);
		end;
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

	procedure PlaySoundEffect(effect: SoundEffect); cdecl; export;
	begin
		Try
			SGSDK_Audio.PlaySoundEffect(effect);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
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

	//procedure PlayMusic1(mus: Music); cdecl; export;
	//begin
		//SGSDK_Audio.PlayMusic(mus);
	//end;

	function IsMusicPlaying(): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Audio.IsMusicPlaying() then
			begin
				result:= -1
			end
			else
			begin
				result:= 0
			end
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	function IsSoundEffectPlaying(effect: SoundEffect): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Audio.IsSoundEffectPlaying(effect) then
			begin
				result:= -1
			end
			else
			begin
				result:= 0
			end
		Except on exc: Exception do TrapException(exc);
		end;
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
		Except on exc: Exception do TrapException(exc);
		end;
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
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	function TextHeight(theText: PChar; theFont: Font): Integer; cdecl; export;
	begin
		Try
			result := SGSDK_Font.TextHeight(theText, theFont);
		Except on exc: Exception do TrapException(exc);
		end;
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
	
	function HasSpriteCollidedX(theSprite : Sprite; x : Integer;
								 range : CollisionDetectionRange):  Integer; cdecl; export;
	begin
		Try
			if SGSDK_Physics.HasSpriteCollidedX(theSprite, x, range) then
			begin
				result:= -1
			end
			else
			begin
				result:= 0
			end
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	function HasSpriteCollidedY(theSprite : Sprite; y : Integer;
								 range : CollisionDetectionRange): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Physics.HasSpriteCollidedY(theSprite, y, range) then
			begin
				result:= -1
			end
			else
			begin
				result:= 0
			end
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	function HasSpriteCollidedWithRect(theSprite : Sprite; x, y : Single;
		width, height : Integer): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Physics.HasSpriteCollidedWithRect(theSprite, x, y, width, height) then
			begin
				result:= -1
			end
			else
			begin
				result:= 0
			end
		Except on exc: Exception do TrapException(exc);
		end;
	end;
		
	function HaveSpritesCollided(sprite1, sprite2 : Sprite): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Physics.HaveSpritesCollided(sprite1, sprite2) then
			begin
				result:= -1
			end
			else
			begin
				result:= 0
			end
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	{function HasSpriteCollidedWithBitmap(theSprite: Sprite; theBitmap: Bitmap;
		x, y: Integer; bounded: Boolean;
		vwPrtX, vwPrtY: Integer)
		: Integer; cdecl; export;
	begin
		if SGSDK_Physics.HasSpriteCollidedWithBitmap(theSprite, theBitmap, x, y, bounded, vwPrtX, vwPrtY) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;

	function HasSpriteCollidedWithBitmap1(theSprite: Sprite; theBitmap: Bitmap;
								x, y: Integer; bounded: Boolean)
								: Integer; cdecl; export;
	begin
		if SGSDK_Physics.HasSpriteCollidedWithBitmap(theSprite, theBitmap, x, y, bounded) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;

	function HasSpriteCollidedWithBitmap2(theSprite: Sprite; theBitmap: Bitmap;
								x, y, vwPrtX, vwPrtY: Integer)
								: Integer; cdecl; export;
	begin
		if SGSDK_Physics.HasSpriteCollidedWithBitmap(theSprite, theBitmap, x, y, vwPrtX, vwPrtY) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;

	function HasSpriteCollidedWithBitmap3(theSprite: Sprite; theBitmap: Bitmap;
								x, y: Integer)
								: Integer; cdecl; export;
	begin
		if SGSDK_Physics.HasSpriteCollidedWithBitmap(theSprite, theBitmap, x, y) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;

	function HaveBitmapsCollided(image1: Bitmap; x1, y1: Integer; image2 : Bitmap;
								 x2, y2: Integer): Integer; cdecl; export;
	begin
		if SGSDK_Physics.HaveBitmapsCollided(image1, x1, y1, image2, x2, y2) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;}

	function HaveBitmapsCollided(image1: Bitmap; x1, y1: Integer;
		bounded1: Integer; image2: Bitmap;
		x2, y2: Integer; bounded2: Integer)
		: Integer; cdecl; export;
	begin
		Try
			if SGSDK_Physics.HaveBitmapsCollided(image1, x1, y1, bounded1 = -1, image2, x2, y2, bounded2 = -1) then
			begin
				result:= -1
			end
			else
			begin
				result:= 0
			end
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	{function CollisionWithinBitmapImages(image1: Bitmap; x1, y1: Integer;
		image2: Bitmap; x2, y2: Integer)
		: Integer; cdecl; export;
	begin
		if SGSDK_Physics.CollisionWithinBitmapImages(image1, x1, y1, image2, x2, y2) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;}

	function CollisionWithinBitmapImages(image1: Bitmap; x1, y1: Integer;
		bounded1: Integer; image2: Bitmap;
		x2, y2: Integer; bounded2: Integer)
		: Integer; cdecl; export;
	begin
		Try
			if SGSDK_Physics.CollisionWithinBitmapImages(image1, x1, y1, bounded1 = -1, image2, x2, y2, bounded2 = -1) then
			begin
				result:= -1
			end
			else
			begin
				result:= 0
			end
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function CreateVector(x,y : Single; invertY : Integer): Vector;  cdecl; export;
	begin
		Try
			result :=SGSDK_Physics.CreateVector(x, y, invertY = -1);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	{function CreateVector1(x,y : Single): Vector;  cdecl; export;
	begin
		result :=SGSDK_Physics.CreateVector(x, y);
	end;}

	function AddVectors(v1, v2 : Vector): Vector; cdecl; export;
	begin
		Try
			result :=SGSDK_Physics.AddVectors(v1, v2);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	function SubtractVectors(v1, v2 : Vector): Vector; cdecl; export;
	begin
		Try
			result :=SGSDK_Physics.SubtractVectors(v1, v2);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	function InvertVector(v : Vector): Vector; cdecl; export;
	begin
		Try
			result :=SGSDK_Physics.InvertVector(v);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	function ChopVector(theVector : Vector; minX, maxX, minY, maxY : Integer): Vector; cdecl; export;
	begin
		Try
			result :=SGSDK_Physics.ChopVector(theVector, minX, maxX, minY, maxY);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	function LimitVector(theVector: Vector; maxMagnitude: Single): Vector; cdecl; export;
	begin
		Try
			result :=SGSDK_Physics.LimitVector(theVector, maxMagnitude);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	function GetUnitVector(theVector : Vector): Vector; cdecl; export;
	begin
		Try
			result :=SGSDK_Physics.GetUnitVector(theVector);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	function IsZeroVector(theVector : Vector): Integer; cdecl; export;
	begin
		Try
			if SGSDK_Physics.IsZeroVector(theVector) then
			begin
				result:= -1
			end
			else
			begin
				result:= 0
			end
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	function Magnitude(theVector : Vector): Single; cdecl; export;
	begin
		Try
			result :=SGSDK_Physics.Magnitude(theVector);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function DotProduct(v1, v2: Vector): Single; cdecl; export;
	begin
		Try
			result :=SGSDK_Physics.DotProduct(v1, v2);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function MultiplyVector(v1: Vector; s1: Single): Vector; cdecl; export;
	begin
		Try
			result :=SGSDK_Physics.MultiplyVector(v1, s1);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

 	function CalculateAngleNumber(x1, y1, x2, y2: Single): Single;  cdecl; export;
	begin
		Try
			result :=SGSDK_Physics.CalculateAngle(x1, y1, x2, y2);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function CalculateAngleSprite(sprite1, sprite2: Sprite): Single;  cdecl; export;
	begin
		Try
			result :=SGSDK_Physics.CalculateAngle(sprite1, sprite2);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function GetVectorFromAngle(angle, magnitude: Single): Vector; cdecl; export;
	begin
		Try
			result := SGSDK_Physics.GetVectorFromAngle(angle, magnitude);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	///////////////////////////////////////////////////////////////////////////////////////////////

	function TranslationMatrix(dx, dy: Single): Matrix2DPtr; cdecl; export;
	var
		p: Matrix2DPtr;
	begin
		Try
			new(p);
			p^ := SGSDK_Physics.TranslationMatrix(dx, dy);
			result := p;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function ScaleMatrix(scale: Single): Matrix2DPtr; cdecl; export;
	var
		p: Matrix2DPtr;
	begin
		Try
			new(p);
			p^ := SGSDK_Physics.ScaleMatrix(scale);
			result := p;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function RotationMatrix(deg: Single): Matrix2DPtr; cdecl; export;
	var
		p: Matrix2DPtr;
	begin
		Try	
			new(p);
			p^ := SGSDK_Physics.RotationMatrix(deg);
			result := p;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function MultiplyMatrix2D(m1, m2: Matrix2DPtr): Matrix2DPtr; cdecl; export;
	var
		p: Matrix2DPtr;
	begin
		Try
			new(p);
			p^ := SGSDK_Physics.Multiply(m1^, m2^);
			result := p;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function MultiplyMatrix2DAndVector(m: Matrix2DPtr; v: Vector): Vector; cdecl; export;
	begin
		Try
			result := SGSDK_Physics.Multiply(m^, v);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	///////////////////////////////////////////////////////////////////////////////////////////////
	
	procedure VectorCollision(p1, p2 : Sprite); cdecl; export;
	begin
		Try
			SGSDK_Physics.VectorCollision(p1, p2);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function GetMatrix2DElement(matrix: Matrix2DPtr; x, y: Integer): Single; cdecl; export;
	begin
		Try
			result := matrix^[x,y];
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure SetMatrix2DElement(matrix: Matrix2DPtr; x, y: Integer; val: Single); cdecl; export;
	begin
		Try
			matrix^[x,y] := val;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure FreeMatrix2D(matrix: Matrix2DPtr); cdecl; export;
	begin
		Try
			Dispose(matrix);
			matrix := nil;
		Except on exc: Exception do TrapException(exc);
		end;
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
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function GetSpriteX(surface: Sprite): Single; cdecl; export;
	begin
		Try
			result := surface.xPos;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure SetSpriteX(surface: Sprite; val: Single); cdecl; export;
	begin
		Try
			surface.xPos := val;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function GetSpriteY(surface: Sprite): Single; cdecl; export;
	begin
		Try
			result := surface.yPos;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure SetSpriteY(surface: Sprite; val: Single); cdecl; export;
	begin
		Try
			surface.yPos := val;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function GetSpriteCurrentFrame(surface: Sprite): Integer; cdecl; export;
	begin
		Try
			result := surface.currentFrame;
		Except on exc: Exception do TrapException(exc);
		end;
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
			if surface.usePixelCollision then
			begin
				result := -1
			end
			else
			begin
				result := 0
			end;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure SetSpriteUsePixelCollision(surface: Sprite; val: Integer); cdecl; export;
	begin
		Try
			surface.usePixelCollision := val = -1;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function NewSDLRect(x, y, w, h: Integer): SDL_Rect; cdecl; export;
	begin
		Try
			result := SGSDK_Graphics.NewSDLRect(x, y, w, h);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function CreateBitmap(width, height: Integer): Bitmap; cdecl; export;
	begin
		Try
			result := SGSDK_Graphics.CreateBitmap(width, height);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure OptimiseBitmap(surface: Bitmap); cdecl; export;
	begin
		Try
			SGSDK_Graphics.OptimiseBitmap(surface);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function LoadBitmapWithTransparentColor(pathToBitmap: PChar; transparent: Integer;
								transparentColor: Colour): Bitmap; cdecl; export;
	begin
		Try
			result := SGSDK_Graphics.LoadBitmap(pathToBitmap, transparent = -1, transparentColor);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	{function LoadBitmap(pathToBitmap : PChar): Bitmap; cdecl; export;
	begin
		result := SGSDK_Graphics.LoadBitmap(pathToBitmap);
	end;}
	
	function LoadTransparentBitmap(pathToBitmap : PChar;
								transparentColor : Colour): Bitmap; cdecl; export;
	begin
		Try
			result := SGSDK_Graphics.LoadTransparentBitmap(pathToBitmap, transparentColor);
		Except on exc: Exception do TrapException(exc);
		end;
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
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function GetBitmapHeight(targetbitmap : Bitmap): Integer; cdecl; export;
	begin
		Try
			result := targetbitmap.height;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure ClearSurfaceWithColor(dest: Bitmap; toColour: Colour); cdecl; export;
	begin
		Try
			SGSDK_Graphics.ClearSurface(dest, toColour);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	{procedure ClearSurface(dest: Bitmap); cdecl; export;
	begin
		SGSDK_Graphics.ClearSurface(dest);
	end;}
	
	procedure DrawBitmapWithDestination(dest: Bitmap; bitmapToDraw: Bitmap; x, y : Integer);
		cdecl; export;
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
	
	procedure DrawPixelWithDestination(dest: Bitmap; theColour: Colour; x, y: Integer);
		cdecl; export;
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
	
	{procedure DrawRectangle3(dest: Bitmap; theColour : Colour; xPos, yPos,
							width, height : Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawRectangle(dest, theColour, xPos, yPos, width, height);
	end;}
	
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

	{procedure DrawCircle3(dest: Bitmap; theColour: Colour;
							 xc, yc, radius: Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawCircle(dest, theColour, xc, yc, radius);
	end;}

	{procedure FillCircle2(dest: Bitmap; theColour: Colour;
							 xc, yc, radius: Integer); cdecl; export;
	begin
		SGSDK_Graphics.FillCircle(dest, theColour, xc, yc, radius);
	end;}

	procedure DrawEllipseWithDestination(dest: Bitmap; theColour: Colour; filled: Integer;
							xPos, yPos, width, height: Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawEllipse(dest, theColour, filled = -1, xPos, yPos, width, height);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	{procedure DrawEllipse3(dest: Bitmap; theColour: Colour;
							xPos, yPos, width, height: Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawEllipse(dest, theColour, xPos, yPos, width, height);
	end;}
	
	{procedure FillEllipse2(dest: Bitmap; theColour: Colour;
							xPos, yPos, width, height: Integer); cdecl; export;
	begin
		SGSDK_Graphics.FillEllipse(dest, theColour, xPos, yPos, width, height);
	end;}
	
	// Draw to screen
	
	procedure ClearScreen(toColour : Colour); cdecl; export;
	begin
		Try
			SGSDK_Graphics.ClearScreen(toColour);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	{procedure ClearScreen1(); cdecl; export;
	begin
		SGSDK_Graphics.ClearScreen();
	end;}

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

	{procedure DrawRectangle1(theColour : Colour; xPos, yPos,
							width, height : Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawRectangle(theColour, xPos, yPos, width, height);
	end;}

	{procedure FillRectangle1(theColour : Colour; xPos, yPos,
							width, height : Integer); cdecl; export;
	begin
		SGSDK_Graphics.FillRectangle(theColour, xPos, yPos, width, height);
	end;}

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

	{procedure DrawCircle1(theColour: Colour; xc, yc, radius: Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawCircle(theColour, xc, yc, radius);
	end;}

	{procedure FillCircle1(theColour: Colour; xc, yc, radius: Integer); cdecl; export;
	begin
		SGSDK_Graphics.FillCircle(theColour, xc, yc, radius);
	end;}

	procedure DrawEllipse(theColour: Colour; filled: Integer;
						xPos, yPos: Single; width, height: Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawEllipse(theColour, filled = -1, xPos, yPos, width, height);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	{procedure DrawEllipse1(theColour: Colour;
						xPos, yPos, width, height: Integer); cdecl; export;
	begin
		SGSDK_Graphics.DrawEllipse(theColour, xPos, yPos, width, height);
	end;}

	{procedure FillEllipse1(theColour: Colour;
						xPos, yPos, width, height: Integer); cdecl; export;
	begin
		SGSDK_Graphics.FillEllipse(theColour, xPos, yPos, width, height);
	end;}

	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// 								Animated Sprite Additions
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	function CreateSprite(startBitmap : Bitmap): Sprite; cdecl; export;
	begin
		Try
			result := SGSDK_Graphics.CreateSprite(startBitmap);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function CreateSpriteMultiFPC(image: Bitmap; framesPerCell, frames, width, height: Integer): Sprite; cdecl; export;
	begin
		Try
			result := SGSDK_Graphics.CreateSprite(image,framesPerCell, frames, width, height);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function CreateSpriteArrayFPC(bitLength: Integer; bitmaps: BitmapPtr; framesPerCell, frames: Integer): Sprite; cdecl; export;
	var
		bmps: BitmapArray;
	begin
		Try
			PopulateBitmapArray(bitmaps, bitLength, bmps);
			
			result := SGSDK_Graphics.CreateSprite(bmps, framesPerCell, frames);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function CreateSpriteMultiEnding(image : Bitmap; isMulti, length: Integer; framesPerCell: IntPtr; endingAction : SpriteEndingAction; width : Integer; height : Integer): Sprite; cdecl; export;
	var
		fpc: IntArray;
	begin
		Try	
			PopulateIntArray(framesPerCell, length, fpc);

			result := SGSDK_Graphics.CreateSprite(image, isMulti = -1, fpc, endingAction, width, height);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function CreateSpriteMulti(image : Bitmap; isMulti, length : Integer; framesPerCell : IntPtr; width, height : Integer): Sprite; cdecl; export;
	var
		fpc: IntArray;
	begin
		Try
			PopulateIntArray(framesPerCell, length, fpc);
			
			result := SGSDK_Graphics.CreateSprite(image, isMulti = -1, fpc, width, height);
		Except on exc: Exception do TrapException(exc);
		end;
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
		Except on exc: Exception do TrapException(exc);
		end;
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
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	// Update Sprite
	
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
	
	// Sprite Properties
	
	function GetSpriteKind(surface : Sprite): Integer; cdecl; export;
	begin
		Try
			result := Integer(surface.spriteKind);
		Except on exc: Exception do TrapException(exc);
		end;
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
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function GetSpriteCols(surface : Sprite) : Integer; cdecl; export;
	begin
		Try
			result := surface.cols;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function GetSpriteRow(surface : Sprite) : Integer; cdecl; export;
	begin
		Try
			result := surface.row;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function GetSpriteFrameCount(surface : Sprite) : Integer; cdecl; export;
	begin
		//result := surface.frameCount;
		Try
			result := Length(surface.framesperCell);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function GetSpriteendingAction(surface : Sprite) : Integer; cdecl; export;
	begin
		Try
			result := Integer(surface.endingAction);
		Except on exc: Exception do TrapException(exc);
		end;
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
			if surface.hasEnded then
				result := -1
			else
				result := 0;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function GetSpriteReverse(surface : Sprite) : Integer; cdecl; export;
	begin
		Try
			if surface.reverse then
				result := -1
			else
				result := 0;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function GetSpriteMass(surface : Sprite) : Single; cdecl; export;
	begin
		Try
			result := surface.mass;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function GetSpriteMovement(surface : Sprite) : Vector; cdecl; export;
	begin
		Try
			result := surface.movement;
		Except on exc: Exception do TrapException(exc);
		end;
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
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	function CurrentHeight(sprite: Sprite): Integer; cdecl; export;
	begin
		Try
			result := SGSDK_Graphics.CurrentHeight(sprite);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	function CurrentWidth(sprite: Sprite): Integer; cdecl; export;
	begin
		Try
			result := SGSDK_Graphics.CurrentWidth(sprite);
		Except on exc: Exception do TrapException(exc);
		end;
	end;

	{procedure DrawSprite1(spriteToDraw : Sprite); cdecl; export;
	begin
		SGSDK_Graphics.DrawSprite(spriteToDraw);
	end;}

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
			if SGSDK_Graphics.IsSpriteOffscreen(theSprite) then
			begin
				result:= -1
			end
			else
			begin
				result:= 0
			end
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure ReplayAnimation(theSprite : Sprite);cdecl; export;
	begin
		Try
			SGSDK_Graphics.ReplayAnimation(theSprite);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	{
	function IsSpriteOffscreenWithViewPort(theSprite : Sprite; vwPrtX, vwPrtY,
															vwPrtWidth, vwPrtHeight : Integer) : Integer; cdecl; export;
	begin
		if SGSDK_Graphics.IsSpriteOffscreen(theSprite, vwPrtX, vwPrtY, vwPrtWidth, vwPrtHeight) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;
	}
	
	/// Draw to Screen Stuff
	
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

	{
	procedure FillRectangleOnScreen(theColour : Colour; xPos, yPos,
							width, height : Integer); cdecl; export;
	begin
		SGSDK_Graphics.FillRectangleOnScreen(theColour, xPos, yPos, width, height);
	end;
	}
	
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
	
	{
	procedure FillCircleOnScreen(theColour: Colour; xc, yc, radius: Integer); cdecl; export;
	begin
		SGSDK_Graphics.FillCircleOnScreen(theColour, xc, yc, radius);
	end;
	}
	
	procedure DrawEllipseOnScreen(theColour: Colour; filled: Integer;
						xPos, yPos, width, height: Integer); cdecl; export;
	begin
		Try
			SGSDK_Graphics.DrawEllipseOnScreen(theColour, filled = -1, xPos, yPos, width, height);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	{
	procedure FillEllipseOnScreen(theColour: Colour;
						xPos, yPos, width, height: Integer); cdecl; export;
	begin
		SGSDK_Graphics.FillEllipseOnScreen(theColour, xPos, yPos, width, height);
	end;
	}
	
	// Screen Functions
	
	
	function XOffset(): Integer; cdecl; export;
	begin
		Try
			result := SGSDK_Camera.XOffset();
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function YOffset(): Integer; cdecl; export;
	begin
		Try
			result := SGSDK_Camera.YOffset();
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function ScreenX(x: Single): Integer; cdecl; export;
	begin
		Try
			result := SGSDK_Camera.ScreenX(x);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function ScreenY(y: Single): Integer; cdecl; export;
	begin
		Try
			result := SGSDK_Camera.ScreenY(y);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function GameX(x: Integer) : Single; cdecl; export;
	begin
		Try
			result := SGSDK_Camera.GameX(x);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function GameY(y: Integer) : Single; cdecl; export;
	begin
		Try
			result := SGSDK_Camera.GameY(y);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function ToGameCoordinates(screenVector: Vector): Vector; cdecl; export;
	begin
		Try
			result := SGSDK_Camera.ToGameCoordinates(screenVector);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure MoveVisualAreaWithVector(v: Vector); cdecl; export;
	begin
		Try
			SGSDK_Camera.MoveVisualArea(v);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
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
	
	///-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
	//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
	// 					MappyLoader
	//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
	//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
	
	function LoadMap(mapFile, imgFile : PChar): MapPtr; cdecl; export;
	var
		p : MapPtr;
	begin
		Try
			new(p);
			p^ := SGSDK_MappyLoader.LoadMapFiles(mapFile, imgFile);
			result := p;
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure DrawMap(m : MapPtr); cdecl; export;
	begin
		Try
			//WriteLn('DrawMAp');
			SGSDK_MappyLoader.DrawMap(m^);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function CollisionWithMapVector(m : MapPtr; spr : Sprite; vec: Vector): CollisionSide; cdecl; export;
	begin
		Try
			result := SGSDK_MappyLoader.CollisionWithMap(m^, spr, vec);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function EventCount(m : MapPtr; event : Event): Integer; cdecl; export;
	begin
		Try
			result := SGSDK_MappyLoader.EventCount(m^, event);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function EventPositionX(m : MapPtr; event : Event; eventnumber : Integer): Integer; cdecl; export;
	begin
		Try
			result := SGSDK_MappyLoader.EventPositionX(m^, event, eventnumber);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	function EventPositionY(m : MapPtr; event : Event; eventnumber : Integer): Integer; cdecl; export;
	begin
		Try
			result := SGSDK_MappyLoader.EventPositionY(m^, event, eventnumber);
		Except on exc: Exception do TrapException(exc);
		end;
	end;
	
	procedure FreeMap(m : MapPtr);
	begin
		Try
			SGSDK_MappyLoader.FreeMap(m^);
			dispose(m);
			m := nil;
		Except on exc: Exception do TrapException(exc);
		end;
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
	Cos,
	Sin,
	Tan,
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	//                       Input
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	
	GetMousePosition,
	GetMouseMovement,
	IsMouseDown,
	IsMouseUp,
	MouseWasClicked,
	StartReadingText,
	IsReadingText,
	TextReadAsASCII,
	//TextReadAsUNICODE,
	IsKeyPressed,
	WasKeyTyped,
	ShowMouse,
	HideMouse,
	MoveMouse,
	
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
	PlaySoundEffect,
	PlayMusic,
	PlaySoundEffectLoop,
	//PlayMusic1,
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
	//HasSpriteCollidedWithBitmap,
	//HasSpriteCollidedWithBitmap1,
	//HasSpriteCollidedWithBitmap2,
	//HasSpriteCollidedWithBitmap3,
	HaveBitmapsCollided,
	//HaveBitmapsCollided1,
	CollisionWithinBitmapImages,
	//CollisionWithinBitmapImages1,
	CreateVector,
	//CreateVector1,
	AddVectors,
	SubtractVectors,
	InvertVector,
	ChopVector,
	LimitVector,
	GetUnitVector,
	IsZeroVector,
	Magnitude,
	DotProduct,
	MultiplyVector,
	CalculateAngleNumber,
	CalculateAngleSprite,
	TranslationMatrix,
	ScaleMatrix,
	RotationMatrix,
	MultiplyMatrix2D,
	MultiplyMatrix2DAndVector,
	VectorCollision,
	
	GetMatrix2DElement,
	SetMatrix2DElement,
	FreeMatrix2D,
	
	GetVectorFromAngle,
	
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
	
	NewSDLRect,
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
	//DrawRectangle3,
	//FillRectangleWithDestination,
	DrawLineWithDestination,
	DrawHorizontalLineWithDestination,
	DrawVerticalLineWithDestination,
	DrawCircleWithDestination,
	//DrawCircle3,
	//FillCircle2,
	DrawEllipseWithDestination,
	//DrawEllipse3,
	//FillEllipse2,
	
	ClearScreen,
	//ClearScreen1,
	DrawBitmap,
	DrawBitmapPart,
	DrawPixel,
	DrawRectangle,
	//DrawRectangle1,
	//FillRectangle1,
	DrawLine,
	DrawHorizontalLine,
	DrawVerticalLine,
	DrawCircle,
	//DrawCircle1,
	//FillCircle1,
	DrawEllipse,
	//DrawEllipse1,
	//FillEllipse1,
	
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
	GetSpriteendingAction,
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
	MoveSprite,
	MoveSpriteTo,
	IsSpriteOffscreen,
	
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
	MoveVisualAreaWithVector,
	MoveVisualArea,
	SetScreenOffset,
	FollowSprite,
	
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
	
	GetExceptionMessage,
	ExceptionOccured

	;
end.

