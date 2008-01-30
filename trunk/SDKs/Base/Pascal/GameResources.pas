///
/// GameResources.pas
///
/// --------------------------------------
///
///  This code loads and frees resourced used by the game.
///  Add your code to load images, music, fonts, and sounds
///  to the procedures below.
///
/// --------------------------------------
unit GameResources;

interface
	uses SysUtils, SGSDK_Core, SGSDK_Font,
       SGSDK_Audio, SGSDK_Graphics, SGSDK_Input,
       SGSDK_Physics, SGSDK_MappyLoader;

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
		_Animation: Bitmap;
		_LoadingFont: Font;
		_StartSound: SoundEffect;

  // Creates a new Mappy Map from mapFile file.
  //
  // PARAMETERS:
  // - mapName: The name of the map to load
  //
  // SIDE EFFECTS:
  // - Loads the map from file into _Maps
	procedure NewMap(mapName: String);
	begin
		SetLength(_Maps, Length(_Maps) + 1);
		SetLength(_MapsStr, Length(_MapsStr) + 1);
		_Maps[High(_Maps)] := LoadMap(mapName);
		_MapsStr[High(_MapsStr)] := mapName;
	end;

  // Creates a new font.
  //
  // PARAMETERS:
  // - fontName: name to call font in _Fonts. Used when you access the font.
  // - fileName: name of the font file to load. Must be in resources/fonts
  // - size: Size of font to load
  //
  // SIDE EFFECTS:
  // - Loads the font from file into _Fonts
	procedure NewFont(fontName, fileName: String; size: Integer);
	begin
		SetLength(_Fonts, Length(_Fonts) + 1);
		SetLength(_FontsStr, Length(_FontsStr) + 1);
		_Fonts[High(_Fonts)] := LoadFont(GetPathToResource(fileName, FontResource), size);
		_FontsStr[High(_FontsStr)] := fontName;
	end;

  // Creates a new image.
  //
  // PARAMETERS:
  // - imageName: name to call image in _images. Used when you access the image.
  // - fileName: name of the image file to load. Must be in resources/images
  //
  // SIDE EFFECTS:
  // - Loads the image from file into _Images
	procedure NewImage(imageName, fileName: String);
	begin
		SetLength(_Images, Length(_Images) + 1);
		SetLength(_ImagesStr, Length(_ImagesStr) + 1);
		_Images[High(_Images)] := LoadBitmap(GetPathToResource(fileName, ImageResource));
		_ImagesStr[High(_ImagesStr)] := imageName;
	end;
	
  // Creates a new image with transparent key colour.
  //
  // PARAMETERS:
  // - imageName: name to call image in _images. Used when you access the image.
  // - fileName: name of the image file to load. Must be in resources/images
  // - transColour: colour of a pixel to be transparent
  //
  // SIDE EFFECTS:
  // - Loads the image from file into _Images with transparent key colour
	procedure NewTransparentColourImage(imageName, fileName: String; transColour: Colour);
	begin
		SetLength(_Images, Length(_Images) + 1);
		SetLength(_ImagesStr, Length(_ImagesStr) + 1);
		_Images[High(_Images)] := LoadBitmap(GetPathToResource(fileName, ImageResource), true, transColour);
		_ImagesStr[High(_ImagesStr)] := imageName;
	end;
	
  // Creates a new image with transparent key color.
  //
  // PARAMETERS:
  // - imageName: name to call image in _images. Used when you access the image.
  // - fileName: name of the image file to load. Must be in resources/images
  // - transColor: color of a pixel to be transparent
  //
  // SIDE EFFECTS:
  // - Loads the image from file into _Images with transparent key color
	procedure NewTransparentColorImage(imageName, fileName: String; transColor: Color);
	begin
		NewTransparentColourImage(imageName, fileName, transColor);
	end;

  // Creates a new sound.
  //
  // PARAMETERS:
  // - soundName: name to call sound in _sounds. Used when you access the sound.
  // - fileName: name of the sound file to load. Must be in resources/sounds
  //
  // SIDE EFFECTS:
  // - Loads the sound from file into _sounds
	procedure NewSound(soundName, fileName: String);
	begin
		SetLength(_Sounds, Length(_Sounds) + 1);
		SetLength(_SoundsStr, Length(_SoundsStr) + 1);
		_Sounds[High(_Sounds)] := LoadSoundEffect(GetPathToResource(fileName, SoundResource));
		_SoundsStr[High(_SoundsStr)] := soundName;
	end;

  // Creates a new music.
  //
  // PARAMETERS:
  // - musicName: name to call music in _musics. Used when you access the music.
  // - fileName: name of the music file to load. Must be in resources/musics
  //
  // SIDE EFFECTS:
  // - Loads the music from file into _musics
	procedure NewMusic(musicName, fileName: String);
	begin
		SetLength(_Music, Length(_Music) + 1);
		SetLength(_MusicStr, Length(_MusicStr) + 1);
		_Music[High(_Music)] := LoadMusic(GetPathToResource(fileName, SoundResource));
		_MusicStr[High(_MusicStr)] := musicName;
	end;


  // Load the fonts you need for your game.
  //
  // SIDE EFFECTS:
  // - Loads the game's fonts
	procedure LoadFonts();
	begin
		NewFont('ArialLarge', 'arial.ttf', 80);
		NewFont('Courier', 'cour.ttf', 16);
    //TODO: Add code here to load the fonts you want to use
	end;

  // Load the images you need for your game.
  //
  // SIDE EFFECTS:
  // - Loads the game's images
	procedure LoadImages();
	begin
    //TODO: Add code here to load the images you want to use
		//NewImage('NoImages', 'Ufo.png');
	end;

  // Load the soundeffects you need for your game.
  //
  // SIDE EFFECTS:
  // - Loads the game's soundeffects
	procedure LoadSounds();
	begin
    //TODO: Add code here to load the sound effects you want to use
		//NewSound('NoSound', 'sound.ogg');
	end;

  // Load the music you need for your game.
  //
  // SIDE EFFECTS:
  // - Loads the game's music
 	procedure LoadMusics();
	begin
		//NewMusic('NoMusic', 'sound.mp3');
	end;

  // Load the maps you need for your game.
  //
  // SIDE EFFECTS:
  // - Loads the game's maps
	procedure LoadMaps();
	begin
    //TODO: Add code here to load the maps you want to use
		//NewMap('test');
	end;

  // Get the Font you created with name font.
  //
  // PARAMETERS:
  // - font: name of the font to get
  //
  // RETURNS:
  // - the font
  //
  // EXCEPT:
  // - if the font isn't found an error is returned
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

  // Get the image you created with name image.
  //
  // PARAMETERS:
  // - image: name of the image to get
  //
  // RETURNS:
  // - the image
  //
  // EXCEPT:
  // - if the image isn't found an error is returned
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

  // Get the soundeffect you created with name sound.
  //
  // PARAMETERS:
  // - sound: name of the soundeffect to get
  //
  // RETURNS:
  // - the soundeffect
  //
  // EXCEPT:
  // - if the soundeffect isn't found an error is returned
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

  // Get the map you created with name mapName.
  //
  // PARAMETERS:
  // - mapName: name of the map to get
  //
  // RETURNS:
  // - the map
  //
  // EXCEPT:
  // - if the map isn't found an error is returned
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

  // Get the music you created with name music.
  //
  // PARAMETERS:
  // - music: name of the music to get
  //
  // RETURNS:
  // - the music
  //
  // EXCEPT:
  // - if the music isn't found an error is returned
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
  

  // Frees the fonts that you have loaded.
  //
  // SIDE EFFECTS:
  // - Frees the game's fonts
	procedure FreeFonts();
	var
		i: Integer;
	begin
		for i := Low(_Fonts) to High(_Fonts) do
		begin
			FreeFont(_Fonts[i]);
		end;
	end;

  // Frees the images that you have loaded.
  //
  // SIDE EFFECTS:
  // - Frees the game's images
  procedure FreeImages();
	var
		i: Integer;
	begin
		for i := Low(_Images) to High(_Images) do
		begin
			FreeBitmap(_Images[i]);
		end;
	end;

  // Frees the images that you have loaded.
  //
  // SIDE EFFECTS:
  // - Frees the game's images
	procedure FreeSounds();
	var
		i: Integer;
	begin
		for i := Low(_Sounds) to High(_Sounds) do
		begin
			FreeSoundEffect(_Sounds[i]);
		end;
	end;

  // Frees the music that you have loaded.
  //
  // SIDE EFFECTS:
  // - Frees the game's music
  // - Stops playing any music
	procedure FreeMusics();
	var
		i: Integer;
	begin
    StopMusic();
    Sleep(100);

		for i := Low(_Music) to High(_Music) do
		begin
			FreeMusic(_Music[i]);
		end;
	end;

  // Frees the maps that you have loaded.
  //
  // SIDE EFFECTS:
  // - Frees the game's maps
	procedure FreeMaps();
	var
		i: Integer;
	begin
		for i := Low(_Maps) to High(_Maps) do
		begin
			FreeMap(_Maps[i]);
		end;
	end;

  // Plays the SwinGame intro. Please leave this
  // in all SwinGames.
  //
  // SIDE EFFECTS:
  // - Plays the starting sound
  // - Draws the background and animation
  // - Refreshes screen
	procedure PlaySwinGameIntro();
	var
		i : Integer;
	begin
		PlaySoundEffect(_StartSound);
		for i:= 0 to 13 do
		begin
			DrawBitmap(_Background, 0, 0);
			DrawBitmapPart(_Animation, (i div 7) * 712, (i mod 7) * 184, 712, 184, 41, 242);
			RefreshScreen(60);
			ProcessEvents();
			Sleep(67);
		end;
		Sleep(1500);
	end;

  // Loads the resourced needed to show the loading screen,
  // and plays the intro.
  //
  // SIDE EFFECTS:
  // - Loads _Background, _Animation, _LoadingFont, and _StartSound
  // - Plays Intro
	procedure ShowLoadingScreen();
	begin
		_Background := LoadBitmap(GetPathToResource('SplashBack.png', ImageResource));
		DrawBitmap(_Background, 0, 0);
		RefreshScreen(60);
		ProcessEvents();

		_Animation := LoadBitmap(GetPathToResource('SwinGameAni.png', ImageResource));
		_LoadingFont := LoadFont(GetPathToResource('cour.ttf', FontResource), 18);
		_StartSound := LoadSoundEffect(GetPathToResource('SwinGameStart.ogg', SoundResource));

		PlaySwinGameIntro();
	end;

  // This plays the "Loading ..." messages while your
  // resources are being loaded.
  //
  // SIDE EFFECTS:
  // - Draws text to the screen
  // - Refreshes screen
	procedure ShowMessage(message: String; number: Integer);
	begin
		DrawText(message, ColorRed, _LoadingFont, 240, 20 + (25 * number));
		RefreshScreen(60);
		ProcessEvents();
	end;

  // Ends the loading screen, and frees the set surfaces.
  //
  // SIDE EFFECTS:
  // - Clears the screen
  // - Frees _LoadingFont, _Background, _Animation, and _StartSound
	procedure EndLoadingScreen();
	begin
		ProcessEvents();
		Sleep(500);
		ClearScreen();
		RefreshScreen(60);
		FreeFont(_LoadingFont);
		FreeBitmap(_Background);
		FreeBitmap(_Animation);
		FreeSoundEffect(_StartSound);
	end;

  // Call this to load your resources and to
  // display the loading screen.
  //
  // SIDE EFFECTS:
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

		ShowMessage('Loading maps...', 4);
		LoadMaps();
		Sleep(50);

		ShowMessage('Game loaded...', 5);
		Sleep(50);
		EndLoadingScreen();

		ChangeScreenSize(oldW, oldH);
	end;

  // Free all of the resources that you have loaded
  // for your game. This should be called at the end
  // of the program.
  //
  // SIDE EFFECTS:
  // - Frees all loaded Fonts, Images, Music, Sound, and Maps
	procedure FreeResources();
	begin
		FreeFonts();
		FreeImages();
		FreeMusics();
		FreeSounds();
		FreeMaps();
	end;

end.
