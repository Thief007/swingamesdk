unit GameResources;
 
interface
	uses SGSDK_Core, SGSDK_Graphics, SGSDK_Audio, SGSDK_Font;

	type 
		GameFonts = (
			ArialLarge,
			WelcomeFont,
			Courier,
			CourierHuge
		 );
		
		GameImages = (
			 UFO,
			 Earth,
			 Saturn,
			 Uranus,
			 Asteroid,
			 SmallAsteroid,
			 Background,
			 EditorStatus,
			 MidBack_Galaxy1,
			 MidBack_Galaxy2,
			 MidBack_Galaxy3,
			 MidBack_Galaxy4,
			 WarpHoleImg,
			 ExplosionImg,
			StarImg,
			StarImgEdit,
			FuelPackImg,
			FuelPackImgEdit,
			WarpImg,
			HUDImg,
			ShieldStrengthImg,
			FuelLevelImg,
			BatteryImg,
			BatteryEditImg
		 );
		 
		 GameSounds = (
			CollectStarSound,
			CollectFuelSound,
			DestroySound,
			WarpStartSound,
			SirenSound
		 );
		 
		 GameMusicResources = (
			IntroMusic,
			Loop1Music
		 );

	procedure LoadResources();
	procedure FreeResources();
		
	function GameFont(font: GameFonts): Font;
	function GameImage(image: GameImages): Bitmap;
 	function GameSound(sound: GameSounds): SoundEffect;
 	function GameMusic(music: GameMusicResources): Music;
 	
 	function ImageID(bmp: Bitmap): GameImages;
 
implementation
	var
		_Images: Array [GameImages] of Bitmap;
		_Fonts: Array [GameFonts] of Font;
		_Sounds: Array [GameSounds] of SoundEffect;
		_Music: Array [GameMusicResources] of Music;

		_Background, _Animation: Bitmap;
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
			
			DrawBitmapPart(_Animation, 0, i * 184, 712, 184, 41, 242);
			
			Sleep(67);
			
			RefreshScreen();
			ProcessEvents();			
		end;
		Sleep(1000);
 	end;
 	
	procedure ShowLoadingScreen();
	begin
		_Background := LoadBitmap(GetPathToResource('SplashBack.png', ImageResource));
		DrawBitmap(_Background, 0, 0);
		RefreshScreen();
		ProcessEvents();
		
		_Animation := LoadBitmap(GetPathToResource('SwinGameAni.png', ImageResource));
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
		FreeBitmap(_Animation);
		FreeSoundEffect(_StartSound);
	end;
 
	 procedure LoadFonts();
	 begin
		_Fonts[ArialLarge] := LoadFont(GetPathToResource('arial.ttf', FontResource), 140);
		_Fonts[WelcomeFont] := LoadFont(GetPathToResource('cour.ttf', FontResource), 30);
		_Fonts[Courier] := LoadFont(GetPathToResource('cour.ttf', FontResource), 16);
		_Fonts[CourierHuge] := LoadFont(GetPathToResource('cour.ttf', FontResource), 150);
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
		 _Images[UFO] := LoadBitmap(GetPathToResource('Ufo.png', ImageResource));		
		 _Images[Earth] := LoadBitmap(GetPathToResource('Earth.png', ImageResource));
		 _Images[Saturn] := LoadBitmap(GetPathToResource('Saturn.png', ImageResource));
		 _Images[Uranus] := LoadBitmap(GetPathToResource('Uranus.png', ImageResource));
		 _Images[Asteroid] := LoadBitmap(GetPathToResource('Asteroid.png', ImageResource));
		 _Images[SmallAsteroid] := LoadBitmap(GetPathToResource('Asteroid1.png', ImageResource));
		 _Images[Background] := LoadBitmap(GetPathToResource('Background.png', ImageResource));
		 _Images[EditorStatus] := LoadBitmap(GetPathToResource('EditorStatus.png', ImageResource));
		 _Images[MidBack_Galaxy1] := LoadBitmap(GetPathToResource('MidBack_Galaxy1.png', ImageResource));
		 _Images[MidBack_Galaxy2] := LoadBitmap(GetPathToResource('MidBack_Galaxy2.png', ImageResource));
		 _Images[MidBack_Galaxy3] := LoadBitmap(GetPathToResource('MidBack_Galaxy3.png', ImageResource));
		 _Images[MidBack_Galaxy4] := LoadBitmap(GetPathToResource('MidBack_Galaxy4.png', ImageResource));
		 _Images[WarpHoleImg] := LoadBitmap(GetPathToResource('WarpHole.png', ImageResource));
		 _Images[ExplosionImg] := LoadBitmap(GetPathToResource('Explosion.png', ImageResource));
		 _Images[StarImg] := LoadBitmap(GetPathToResource('Star.png', ImageResource));
		 _Images[StarImgEdit] := LoadBitmap(GetPathToResource('StarEdit.png', ImageResource));
		 _Images[FuelPackImg] := LoadBitmap(GetPathToResource('FuelPack.png', ImageResource));
		 _Images[FuelPackImgEdit] := LoadBitmap(GetPathToResource('FuelPackEdit.png', ImageResource));
		 _Images[WarpImg] := LoadBitmap(GetPathToResource('Warp.png', ImageResource));
		 _Images[HUDImg] := LoadBitmap(GetPathToResource('HUD.png', ImageResource));
		 _Images[ShieldStrengthImg] := LoadBitmap(GetPathToResource('Shield.png', ImageResource));
		 _Images[FuelLevelImg] := LoadBitmap(GetPathToResource('Fuel.png', ImageResource));		
		 _Images[BatteryImg] := LoadBitmap(GetPathToResource('Battery.png', ImageResource));		
		 _Images[BatteryEditImg] := LoadBitmap(GetPathToResource('BatteryEdit.png', ImageResource));		
	 end;
	 
	 procedure FreeImages();
	 var
		 i: GameImages;
	 begin
		 for i := Low(_Images) to High(_Images) do
		 begin
			 FreeBitmap(_Images[i]);
		 end;
	 end;
	 
	function ImageID(bmp: Bitmap): GameImages;
	var
		i: GameImages;
	begin
		for i := Low(_Images) to High(_Images) do
		begin
			if bmp = _Images[i] then
			begin
				result := i;
				exit;
			end;
		end;
		
		result := GameImages(-1);
	end;
 
	procedure LoadSounds();
	begin
		_Sounds[CollectStarSound] := LoadSoundEffect(GetPathToResource('CollectStar.ogg', SoundResource));
		_Sounds[CollectFuelSound] := LoadSoundEffect(GetPathToResource('CollectFuel.ogg', SoundResource));
		_Sounds[DestroySound] := LoadSoundEffect(GetPathToResource('Destroy.wav', SoundResource));
		_Sounds[WarpStartSound] := LoadSoundEffect(GetPathToResource('StartWarp.ogg', SoundResource));
		_Sounds[SirenSound] := LoadSoundEffect(GetPathToResource('Siren.ogg', SoundResource));
	end;
	 
	procedure FreeSounds();
	var
		 i: GameSounds;
	begin
		for i := Low(_Sounds) to High(_Sounds) do
		begin
			FreeSoundEffect(_Sounds[i]);
		end;
	end;
	
	procedure LoadMusic();
	begin
		_Music[IntroMusic] := SGSDK_Audio.LoadMusic(GetPathToResource('Intro.mp3', SoundResource));
		_Music[Loop1Music] := SGSDK_Audio.LoadMusic(GetPathToResource('Loop1.mp3', SoundResource));
	end;
	 
	procedure FreeMusic();
	var
		 i: GameMusicResources;
	begin
		for i := Low(_Music) to High(_Music) do
		begin
			SGSDK_Audio.FreeMusic(_Music[i]);
		end;
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
		Sleep(50);
		
		ShowMessage('Loading images...', 1);
		LoadImages();
		Sleep(50);
		
		ShowMessage('Loading sounds...', 2);
		LoadSounds();
		Sleep(50);
		
		ShowMessage('Loading music...', 3);
		LoadMusic();
		Sleep(50);
		
		//Add game level loading here...
		
		Sleep(50);
		ShowMessage('Game loaded...', 4);
		Sleep(100);
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