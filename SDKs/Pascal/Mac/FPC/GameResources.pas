unit GameResources;

interface
	uses SysUtils, SGSDK_Core, SGSDK_Font, SGSDK_Audio, SGSDK_Graphics, SGSDK_Input, SGSDK_Physics, SGSDK_MappyLoader;

	procedure LoadResources();
	procedure FreeResources();
	function GameFont(font: String): Font;
	function GameImage(image: String): Bitmap;
	function GameSound(sound: String): SoundEffect;
	function GameMusic(music: String): Music;
	function GameMap(mapName: String): Map;
 
implementation
	var
		_Images: Array of Bitmap;
		_Fonts: Array of Font;
		_Sounds: Array of SoundEffect;
		_Music: Array of Music;
		_Maps: Array of Map;

		_ImagesStr: Array of String;
		_FontsStr: Array of String;
		_SoundsStr: Array of String;
		_MusicStr: Array of String;
		_MapsStr: Array of String;

		_Background: Bitmap;
		_Animation: Sprite;
		_LoadingFont: Font;
		_StartSound: SoundEffect;

	procedure PlaySwinGameIntro();
	var
		i : Integer;
	begin
		PlaySoundEffect(_StartSound);
		for i:= 0 to 55 do
		begin
			DrawBitmap(_Background, 0, 0);
			DrawSprite(_Animation);
			UpdateSprite(_Animation);
			RefreshScreen(60);
			ProcessEvents();			
		end;
		Sleep(1500);
	end;

	procedure ShowLoadingScreen();
	begin
		_Background := LoadBitmap(GetPathToResource('SplashBack.png', ImageResource));
		DrawBitmap(_Background, 0, 0);
		RefreshScreen(60);
		ProcessEvents();

		_Animation := CreateSprite(LoadBitmap(GetPathToResource('SwinGameAni.png', ImageResource)), 4, 14, 712, 184);
		_Animation.xPos := 41;
		_Animation.yPos := 242;
		_LoadingFont := LoadFont(GetPathToResource('cour.ttf', FontResource), 18);
		_StartSound := LoadSoundEffect(GetPathToResource('SwinGameStart.ogg', SoundResource));

		PlaySwinGameIntro();
	end;

	procedure ShowMessage(message: String; number: Integer);
	begin
		DrawText(message, ColorRed, _LoadingFont, 240, 20 + (25 * number));
		RefreshScreen(60);
		ProcessEvents();
	end;

	procedure EndLoadingScreen();
	begin
		ClearScreen();
		RefreshScreen(60);
		FreeFont(_LoadingFont);
		FreeBitmap(_Background);
		FreeSprite(_Animation);
		FreeSoundEffect(_StartSound);
	end;
	
	procedure NewMap(mapName: String);
	begin
		SetLength(_Maps, Length(_Maps) + 1);
		SetLength(_MapsStr, Length(_MapsStr) + 1);
		_Maps[High(_Maps)] := LoadMap(mapName);
		_MapsStr[High(_MapsStr)] := mapName;
	end;
	
	procedure NewFont(fontName, fileName: String; size: Integer);
	begin
		SetLength(_Fonts, Length(_Fonts) + 1);
		SetLength(_FontsStr, Length(_FontsStr) + 1);
		_Fonts[High(_Fonts)] := LoadFont(GetPathToResource(fileName, FontResource), size);
		_FontsStr[High(_FontsStr)] := fontName;
	end;
	
	procedure NewImage(imageName, fileName: String);
	begin
		SetLength(_Images, Length(_Images) + 1);
		SetLength(_ImagesStr, Length(_ImagesStr) + 1);
		_Images[High(_Images)] := LoadBitmap(GetPathToResource(fileName, ImageResource));
		_ImagesStr[High(_ImagesStr)] := imageName;
	end;
	
	procedure NewSound(soundName, fileName: String);
	begin
		SetLength(_Sounds, Length(_Sounds) + 1);
		SetLength(_SoundsStr, Length(_SoundsStr) + 1);
		_Sounds[High(_Sounds)] := LoadSoundEffect(GetPathToResource(fileName, SoundResource));
		_SoundsStr[High(_SoundsStr)] := soundName;
	end;
	
	procedure NewMusic(musicName, fileName: String);
	begin
		SetLength(_Music, Length(_Music) + 1);
		SetLength(_MusicStr, Length(_MusicStr) + 1);
		_Music[High(_Music)] := LoadMusic(GetPathToResource(fileName, SoundResource));
		_MusicStr[High(_MusicStr)] := musicName;
	end;
	
	procedure LoadFonts();
	begin
		NewFont('ArialLarge', 'arial.ttf', 80);
		NewFont('Courier', 'cour.ttf', 16);
	end;

	procedure FreeFonts();
	var
		i: Integer;
	begin
		for i := Low(_Fonts) to High(_Fonts) do
		begin
			FreeFont(_Fonts[i]);
		end;
	end;

	procedure LoadImages();
	begin
		//NewImage('NoImages', 'Ufo.png');
	end;

	procedure FreeImages();
	var
		i: Integer;
	begin
		for i := Low(_Images) to High(_Images) do
		begin
			FreeBitmap(_Images[i]);
		end;
	end;

	procedure LoadSounds();
	begin
		//NewSound('NoSound', 'sound.ogg');
	end;
	
	procedure LoadMaps();
	begin
		//NewMap('test');
	end;
	
	procedure FreeSounds();
	var
		i: Integer;
	begin
		for i := Low(_Sounds) to High(_Sounds) do
		begin
			FreeSoundEffect(_Sounds[i]);
		end;
	end;

	procedure LoadMusics();
	begin
		//NewMusic('NoMusic', 'sound.mp3');
	end;

	procedure FreeMusics();
	var
		i: Integer;
	begin
		for i := Low(_Music) to High(_Music) do
		begin
			FreeMusic(_Music[i]);
		end;
	end;
	
	procedure FreeMaps();
	var
		i: Integer;
	begin
		for i := Low(_Maps) to High(_Maps) do
		begin
			FreeMap(_Maps[i]);
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
		LoadMusics();
		Sleep(50);
		
		ShowMessage('Loading maps...', 3);
		LoadMaps();
		Sleep(50);

		ShowMessage('Game loaded...', 4);
		Sleep(50);
		EndLoadingScreen();

		ChangeScreenSize(oldW, oldH);
	end;

	procedure FreeResources();
	begin
		FreeFonts();
		FreeImages();
		FreeMusics();
		FreeSounds();
		FreeMaps();
	end;

	function GameFont(font: String): Font; 
	var
		i: Integer;
	begin
		for i := Low(_FontsStr) to High(_FontsStr) do
		begin
			if _FontsStr[i] = font then begin
				result := _Fonts[i];
				exit;
			end;
		end;
		raise exception.create('Font ' + font + ' does not exist...');
	end;

	function GameImage(image: String): Bitmap;
	var
	i: Integer;
	begin
		for i := Low(_ImagesStr) to High(_ImagesStr) do
		begin
			if _ImagesStr[i] = image then begin
				result := _Images[i];
				exit;
			end;
		end;
		raise exception.create('Image ' + image + ' does not exist...');
	end;

	function GameSound(sound: String): SoundEffect; 
	var
		i: Integer;
	begin
		for i := Low(_SoundsStr) to High(_SoundsStr) do
		begin
			if _SoundsStr[i] = sound then begin
				result := _Sounds[i];
				exit;
			end;
		end;
		raise exception.create('Sound ' + sound + ' does not exist...');
	end;
	
	function GameMap(mapName: String): Map;
	var
		i: Integer;
	begin
		for i := Low(_MapsStr) to High(_MapsStr) do
		begin
			if _MapsStr[i] = mapName then begin
				result := _Maps[i];
				exit;
			end;
		end;
		raise exception.create('Map ' + mapName + ' does not exist...');
	end;
	
	function GameMusic(music: String): Music;
	var
		i: Integer;
	begin
		for i := Low(_MusicStr) to High(_MusicStr) do
		begin
			if _MusicStr[i] = music then begin
				result := _Music[i];
				exit;
			end;
		end;
		raise exception.create('Music ' + music + ' does not exist...');
	end;
end.