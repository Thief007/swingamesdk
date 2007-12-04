unit GameResources;
 
interface
	uses SGSDK_Core, SGSDK_Font, SGSDK_Audio, SGSDK_Graphics, SGSDK_Input, SGSDK_Physics;

	type 
		GameFonts = (
			ArialLarge,
			Courier
		 );
		
		GameImages = (
			NoImages
		 );
		 
		 GameSounds = (
			NoSounds
		 );
		 
		 GameMusicResources = (
			NoMusic
		 );

	procedure LoadResources();
	procedure FreeResources();
		
	function GameFont(font: GameFonts): Font;
	function GameImage(image: GameImages): Bitmap;
 	function GameSound(sound: GameSounds): SoundEffect;
 	function GameMusic(music: GameMusicResources): Music;
 	 
implementation
	var
		_Images: Array [GameImages] of Bitmap;
		_Fonts: Array [GameFonts] of Font;
		_Sounds: Array [GameSounds] of SoundEffect;
		_Music: Array [GameMusicResources] of Music;

		_Background: Bitmap;
		_Animation: Sprite;
		_LoadingFont: Font;
 		_StartSound: SoundEffect;
 	
 	procedure PlaySwinGameIntro();
 	var
 		i : Integer;
 	begin
        Sleep(100);
		
		PlaySoundEffect(_StartSound);
		for i:= 0 to 13 do
		begin
			DrawBitmap(_Background, 0, 0);
			
			DrawSprite(_Animation);
			UpdateSprite(_Animation);
			
			Sleep(67);
			
			RefreshScreen();
			ProcessEvents();			
		end;
		Sleep(1000);
 	end;
 	
	procedure ShowLoadingScreen();
	var
		i: Integer;
		tempInt: Array of Integer;
		
	begin
		_Background := LoadBitmap(GetPathToResource('SplashBack.png', ImageResource));
		DrawBitmap(_Background, 0, 0);
		RefreshScreen();
		ProcessEvents();
		
		SetLength(tempInt, 14);
		
		for i := 0 to 13 do
		begin
			tempInt[i] := 0;
		end;
		_Animation := CreateSprite(LoadBitmap(GetPathToResource('SwinGameAni.png', ImageResource)), true, tempInt, ReverseLoop, 712, 184);
		_Animation.xPos := 41;
		_Animation.yPos := 242;
		_LoadingFont := LoadFont(GetPathToResource('cour.ttf', FontResource), 18);
		_StartSound := LoadSoundEffect(GetPathToResource('SwinGameStart.ogg', SoundResource));
	
		PlaySwinGameIntro();
	end;
	
	procedure ShowMessage(message: String; number: Integer);
	begin
		DrawText(message, ColorRed, _LoadingFont, 240, 20 + (25 * number));
		RefreshScreen();
		ProcessEvents();
	end;
	
	procedure EndLoadingScreen();
	begin
		ClearScreen();
		RefreshScreen();
		FreeFont(_LoadingFont);
		FreeBitmap(_Background);
		FreeSprite(_Animation);
		FreeSoundEffect(_StartSound);
	end;
 
	 procedure LoadFonts();
	 begin
		_Fonts[ArialLarge] := LoadFont(GetPathToResource('arial.ttf', FontResource), 80);
		_Fonts[Courier] := LoadFont(GetPathToResource('cour.ttf', FontResource), 16);
	 end;
	 
	 procedure FreeFonts();
	 var
		 i: GameFonts;
	 begin
		 for i := ArialLarge to Courier do
		 begin
			 FreeFont(_Fonts[i]);
		 end;
	 end;
	 
	 procedure LoadImages();
	 begin
		 //_Images[NoImages] := LoadBitmap(GetPathToResource('Ufo.png', ImageResource));		
	 end;
	 
	 procedure FreeImages();
	 //var
	 //	 i: GameImages;
	 begin
		 {for i := Low(_Images) to High(_Images[i]) do
		 begin
			 FreeBitmap(_Images[i]);
		 end;}
	 end;
	 
	procedure LoadSounds();
	begin
		//_Sounds[NoSound] := LoadSoundEffect(GetPathToResource('sound.ogg', SoundResource));
	end;
	 
	procedure FreeSounds();
	//var
	//	 i: GameSounds;
	begin
		{for i := Low(_Sounds) to High(_Sounds) do
		begin
			FreeSoundEffect(_Sounds[i]);
		end;}
	end;
	
	procedure LoadMusic();
	begin
		//_Music[NoMusic] := LoadMusic(GetPathToResource('sound.mp3', SoundResource));
	end;
	 
	procedure FreeMusic();
	//var
	//	 i: GameMusicResources;
	begin
		{for i := Low(_Music) to High(_Music) do
		begin
			FreeMusic(_Music[i]);
		end;}
	end;
 
	procedure LoadResources();
	var
		oldW, oldH: Integer;
	begin
		oldW := ScreenWidth();
		oldH := ScreenHeight();
		
		ChangeScreenSize(800, 600);
		
		//Remove sleeps once "real" game resources are being loaded
		ShowLoadingScreen();
		ShowMessage('Loading fonts...', 0); 
		LoadFonts();
		Sleep(500);
		
		ShowMessage('Loading images...', 1);
		LoadImages();
		Sleep(500);
		
		ShowMessage('Loading sounds...', 2);
		LoadSounds();
		Sleep(500);
		
		ShowMessage('Loading music...', 3);
		LoadMusic();
		Sleep(500);
		
		//Add game level loading here...
		
		Sleep(500);
		ShowMessage('Game loaded...', 4);
		Sleep(500);
		EndLoadingScreen();
		
		ChangeScreenSize(oldW, oldH);
	end;
	
	procedure FreeResources();
	begin
		FreeFonts();
		FreeImages();
		FreeMusic();
		FreeSounds();
	end;
	
	function GameFont(font: GameFonts): Font; 
	begin
		result := _Fonts[font];
	end;
	
	function GameImage(image: GameImages): Bitmap;
	begin
		result := _Images[image];
	end;
	
	function GameSound(sound: GameSounds): SoundEffect; 
	begin
		result := _Sounds[sound];
	end;
	
	function GameMusic(music: GameMusicResources): Music;
	begin
		result := _Music[music];
	end;

end.