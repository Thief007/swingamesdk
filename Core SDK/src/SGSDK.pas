library SGSDK;

uses SGSDK_Core, SGSDK_Input, SGSDK_Audio, SGSDK_Font,
	SDL_Mixer, SDL, SDL_Image, SDL_TTF, SDLEventProcessing;

	///-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
	//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
	// 					Core
	//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
	//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
	procedure ProcessEvents(); cdecl; export;
	begin
		SGSDK_Core.ProcessEvents();
	end;

	procedure OpenGraphicsWindow(caption : String; width : Integer; height : Integer); cdecl; export;
	begin
		SGSDK_Core.OpenGraphicsWindow(caption, width, height);
	end;
	
	function WindowCloseRequested(): Integer; cdecl; export;
	begin
		if SGSDK_Core.WindowCloseRequested() then
		begin
			//WriteLn('Close...');
			result:= -1
		end
		else
		begin
			//WriteLn('Not Close...');
			result:= 0
		end
	end;
	
	procedure SetIcon(iconFilename: String); cdecl; export;
	begin
		SGSDK_Core.SetIcon(iconFilename);
	end;
	
	procedure ChangeScreenSize(width, height: Integer); cdecl; export;
	begin
		SGSDK_Core.ChangeScreenSize(width, height);
	end;
	
	procedure ToggleFullScreen(); cdecl; export;
	begin
		SGSDK_Core.ToggleFullScreen();
	end;
	
	procedure RefreshScreen(); cdecl; export;
	begin
		SGSDK_Core.RefreshScreen();
	end;
	
	procedure TakeScreenshot(basename: String); cdecl; export;
	begin
		SGSDK_Core.TakeScreenshot(basename);
	end;
	
	function  ScreenWidth(): Integer; cdecl; export;
	begin
		result := SGSDK_Core.ScreenWidth();
	end;
	
	function  ScreenHeight(): Integer; cdecl; export;
	begin
		result := SGSDK_Core.ScreenHeight();
	end;
	
	function	ToSDLColor(color: UInt32): TSDL_Color; cdecl; export;
	begin
		result := SGSDK_Core.ToSDLColor(color);
	end;
	
	function	GetColour1(forBitmap: Bitmap; apiColor: Color): Colour; overload; cdecl; export;
	begin
		result := SGSDK_Core.GetColour(forBitmap, apiColor);
	end;
	
	function	GetColour2(red, green, blue, alpha: Byte) : Colour; cdecl; export;
	begin
		result := SGSDK_Core.GetColour(red, green, blue, alpha);
	end;
	
	function	GetColour3(red, green, blue : Byte) : Colour; overload; cdecl; export;
	begin
		result := SGSDK_Core.GetColour(red, green, blue);	
	end;
	
	function	GetFramerate(): Integer; cdecl; export;
	begin
		result := SGSDK_Core.GetFramerate();
	end;
	
	function	GetTicks(): UInt32; cdecl; export;
	begin
		result := SGSDK_Core.GetTicks();
	end;
	
	procedure Sleep(time : UInt32); cdecl; export;
	begin
		SGSDK_Core.Sleep(time);
	end;
	
	function GetPathToResource1(filename: String; kind: ResourceKind) : String; overload; cdecl; export;
	begin
		result := SGSDK_Core.GetPathToResource(filename, kind);
	end;
	
	function GetPathToResource2(filename: String): String; overload; cdecl; export;
	begin
		result := SGSDK_Core.GetPathToResource(filename);
	end;
	
	procedure RegisterEventProcessor(handle: EventProcessPtr; handle2: EventStartProcessPtr); cdecl; export;
	begin
		SGSDK_Core.RegisterEventProcessor(handle, handle2);
	end;
	
	function Cos(angle: Single): Single; cdecl; export;
	begin
		result := SGSDK_Core.Cos(angle);
	end;
	
	function Sin(angle: Single): Single; cdecl; export;
	begin
		result := SGSDK_Core.Sin(angle);
	end;
	
	function Tan(angle: Single): Single; cdecl; export;
	begin
		result := SGSDK_Core.Tan(angle);
	end;
	
	//INPUT
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//**************************************************
	//                       Audio
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	
	function	LoadSoundEffect(path: String): SoundEffect; cdecl; export;
	begin
		result := SGSDK_Audio.LoadsoundEffect(path);
	end;
	
	function	LoadMusic(path: String): Music; cdecl; export;
	begin
		result := SGSDK_Audio.LoadMusic(path);
	end;

	procedure FreeMusic(var mus: Music); cdecl; export;
	begin
		SGSDK_Audio.FreeMusic(mus);
	end;

	procedure FreeSoundEffect(var effect: SoundEffect); cdecl; export;
	begin
		SGSDK_Audio.FreeSoundEffect(effect);
	end;

	procedure PlaySoundEffect(effect: SoundEffect); cdecl; export;
	begin
		SGSDK_Audio.PlaySoundEffect(effect);
	end;

	procedure PlaySoundEffect1(effect: SoundEffect; loops: Integer); cdecl; export;
	begin
		SGSDK_Audio.PlaySoundEffect(effect, loops);
	end;

	procedure PlayMusic(mus: Music; loops: Integer); cdecl; export;
	begin
		SGSDK_Audio.PlayMusic(mus, loops);
	end;

	procedure PlayMusic1(mus: Music); cdecl; export;
	begin
		SGSDK_Audio.PlayMusic(mus);
	end;

	function IsMusicPlaying(mus: Music): Integer; cdecl; export;
	begin
		if SGSDK_Audio.IsMusicPlaying(mus) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;

	function IsSoundEffectPlaying(effect: SoundEffect): Integer; cdecl; export;
	begin
		if SGSDK_Audio.IsSoundEffectPlaying(effect) then
		begin
			result:= -1
		end
		else
		begin
			result:= 0
		end
	end;

	procedure StopSoundEffect(effect: SoundEffect); cdecl; export;
	begin
		SGSDK_Audio.StopSoundEffect(effect);
	end;

	procedure StopMusic(); cdecl; export;
	begin
		SGSDK_Audio.StopMusic();
	end;
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//**************************************************
	//                       Font
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	
	function LoadFont(fontName: String; size: Integer): Font; cdecl; export;
	begin
		result := SGSDK_Font.LoadFont(fontName, size)
	end;

	procedure SetFontStyle(font: Font; style: FontStyle); cdecl; export;
	begin
		SGSDK_Font.SetFontStyle(font, style);
	end;

	procedure FreeFont(var fontToFree: Font); cdecl; export;
	begin
		SGSDK_Font.FreeFont(fontToFree);
	end;
	
	procedure DrawText(theText: String; textColor: Colour;
					 theFont: Font; x, y: Integer); cdecl; export;
	begin
		SGSDK_Font.DrawText(theText, textColor, theFont, x, y);
	end;

	procedure DrawTextLines(theText: String; textColor, backColor: Colour;
							theFont: Font; align: FontAlignment;
							x, y, w, h: Integer); cdecl; export;
	begin
		SGSDK_Font.DrawTextLines(theText, textColor, backColor, theFont, align, x, y, w, h);
	end;
							
	procedure DrawText1(dest: Bitmap; theText: String; textColor: Colour;
					theFont: Font; x, y: Integer); cdecl; export;
	begin
		SGSDK_Font.DrawText(dest, theText, textColor, theFont, x, y);
	end;

	procedure DrawTextLines1(dest: Bitmap; theText: String;
							textColor, backColor: Colour;
							theFont: Font; align: FontAlignment;
							x, y, w, h: Integer); cdecl; export;
	begin
		SGSDK_Font.DrawTextLines(dest, theText, textColor, backColor, theFont, align, x, y, w, h);
	end;

	function TextWidth(theText: String; theFont: Font): Integer; cdecl; export;
	begin
		result :=SGSDK_Font.TextWidth(theText, theFont);
	end;

	function TextHeight(theText: String; theFont: Font): Integer; cdecl; export;
	begin
		result := SGSDK_Font.TextHeight(theText, theFont);
	end;

	procedure DrawFramerate(x, y: Integer; font: Font); cdecl; export;
	begin
		SGSDK_Font.DrawFramerate(x, y, font);
	end;
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
	RefreshScreen,
	TakeScreenShot,
	ScreenWidth,
	ScreenHeight,
	ToSDLColor,
	GetColour1,	
	GetColour2,
	GetColour3,
	GetFramerate,	
	GetTicks,	
	Sleep,	
	GetPathToResource1,
	GetPathToResource2,
	RegisterEventProcessor,	
	Cos,
	Sin,
	Tan,
	
	
	
	//INPUT
	
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//**************************************************
	//                       Audio
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	
	LoadSoundEffect,
	LoadMusic,
	FreeMusic,
	FreeSoundEffect,
	PlaySoundEffect,
	PlayMusic,
	PlaySoundEffect1,
	PlayMusic1,
	IsMusicPlaying,
	IsSoundEffectPlaying,
	StopSoundEffect,
	StopMusic,
	
	
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//**************************************************
	//                       Font
	//***************************************************
	//* * * * * * * * * * * * * * * * * * * * * * * * * *
	//***************************************************
	LoadFont,
	SetFontStyle,
	FreeFont,
	DrawText,
	DrawTextLines,
	DrawText1,
	DrawTextLines1,
	TextWidth,
	TextHeight,
	DrawFramerate
	;
end.