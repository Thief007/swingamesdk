// Change History:
//
// Version 3:
// - 2009-06-23: Andrew: Created


/// @module ResourceManager
unit sgResourceManager;

interface
  uses sgTypes;
  
  //=============================================================================
    
  /// @lib
  procedure MapBitmap(name, filename: String);
  
  /// @lib
  procedure MapTransparentBitmap(name, filename: String; transparentColor: Color);
  
  /// @lib
  function HasBitmap(name: String): Boolean;
  
  /// @lib
  function GetBitmap(name: String): Bitmap;
  
  /// @lib
  procedure ReleaseBitmap(name: String);
  
  /// @lib
  procedure ReleaseAllBitmaps();
  
  //=============================================================================
  
  /// @lib
  procedure MapFont(name, filename: String; size: LongInt);
  
  /// @lib
  function HasFont(name: String): Boolean;
  
  /// @lib
  function GetFont(name: String): Font;
  
  /// @lib
  procedure ReleaseFont(name: String);
  
  /// @lib
  procedure ReleaseAllFonts();
  
  //=============================================================================
  
  /// @lib
  procedure MapSoundEffect(name, filename: String);
  
  /// @lib
  function HasSoundEffect(name: String): Boolean;
  
  /// @lib
  function GetSoundEffect(name: String): SoundEffect;
  
  /// @lib
  procedure ReleaseSoundEffect(name: String);
  
  /// @lib
  procedure ReleaseAllSoundEffects();
  
  //=============================================================================
  
  /// @lib
  procedure MapMusic(name, filename: String);
  
  /// @lib
  function HasMusic(name: String): Boolean;
  
  /// @lib
  function GetMusic(name: String): Music;
  
  /// @lib
  procedure ReleaseMusic(name: String);
  
  /// @lib
  procedure ReleaseAllMusic();
  
  //=============================================================================
  
  /// @lib
  procedure MapTileMap(name, filename: String);
  
  /// @lib
  function HasTileMap(name: String): Boolean;
  
  /// @lib
  function GetTileMap(name: String): Map;
  
  /// @lib
  procedure ReleaseTileMap(name: String);
  
  /// @lib
  procedure ReleaseAllTileMaps();
  
  //=============================================================================
  
  /// @lib
  procedure ReleaseAllResources();
  
  //=============================================================================
  
  // procedure LoadResourceBundle(name: String);
  // 
  // procedure FreeResourceBundle(name: String);
  
implementation
  uses SysUtils, sgCore, sgText, sgAudio, sgGraphics, sgInput, sgTileMap, stringhash;
  
  //=============================================================================
  
  type 
    tResourceContainer = class(tObject)
    private
      resource_val : Pointer;
    public
      constructor Create(data: Pointer);
    
      property Resource: Pointer read resource_val;
    end;
  
    constructor tResourceContainer.create(data: Pointer);
    begin
      inherited create;
    
      resource_val := data;
    end;
  
  //=============================================================================
  
  type ReleaseFunction = procedure(var res: Pointer);
  
  var
    _Images: tStringHash;
    _Fonts: tStringHash;
    _SoundEffects: tStringHash;
    _Music: tStringHash;
    _TileMaps: tStringHash;
  
  //=============================================================================
  
  procedure ReleaseAll(tbl: tStringHash; releaser: ReleaseFunction);
  var
    iter: tStrHashIterator;
    res: Pointer;
  begin
    iter := tbl.getIterator();
    
    while iter.hasNext() do
    begin
      res := tResourceContainer(iter.value).resource;
      releaser(res);
    end;
    
    tbl.clear();
  end;
  
  //=============================================================================
  
  procedure MapBitmap(name, filename: String);
  var
    obj: tResourceContainer;
    bmp: Bitmap;
  begin
    //WriteLn('Mapping bitmap... ', filename);
    //WriteLn(name, '->', GetPathToResource(filename, BitmapResource));
    bmp := LoadBitmap(GetPathToResource(filename, BitmapResource));
    //WriteLn(name, ' (', filename, ') => ', hexstr(bmp));
    //WriteLn(GetPathToResource(filename, BitmapResource));
    obj := tResourceContainer.Create(bmp);
    _Images.values[name] := obj;
  end;
  
  procedure MapTransparentBitmap(name, filename: String; transparentColor: Color);
  var
    obj: tResourceContainer;
  begin
    obj := tResourceContainer.Create(LoadBitmap(GetPathToResource(filename, BitmapResource), true, transparentColor));
    _Images.values[name] := obj;
  end;
  
  function HasBitmap(name: String): Boolean;
  begin
    result := _Images.containsKey(name);
  end;
  
  function GetBitmap(name: String): Bitmap;
  begin
    result := Bitmap(tResourceContainer(_Images.values[name]).Resource);
  end;
  
  procedure ReleaseBitmap(name: String);
  var
    bmp: Bitmap;
  begin
    bmp := GetBitmap(name);
    _Images.remove(name);
    FreeBitmap(bmp);
  end;
  
  procedure ReleaseAllBitmaps();
  begin
    ReleaseAll(_Images, @FreeBitmap);
  end;
  // var
  //   iter: tStrHashIterator;
  //   bmp: Bitmap;
  // begin
  //   iter := _Images.getIterator();
  //   
  //   while iter.hasNext() do
  //   begin
  //     bmp := GetBitmap(iter.key);
  //     FreeBitmap(bmp);
  //   end;
  //   
  //   _Images.clear();
  // end;
  
  //=============================================================================
  
  procedure MapFont(name, filename: String; size: LongInt);
  var
    obj: tResourceContainer;
    fnt: Font;
  begin
    fnt := LoadFont(GetPathToResource(filename, FontResource), size);
    obj := tResourceContainer.Create(fnt);
    _Fonts.values[name] := obj;
  end;
  
  function HasFont(name: String): Boolean;
  begin
    result := _Fonts.containsKey(name);
  end;
  
  function GetFont(name: String): Font;
  begin
    result := Font(tResourceContainer(_Fonts.values[name]).Resource);
  end;
  
  procedure ReleaseFont(name: String);
  var
    fnt: Font;
  begin
    fnt := GetFont(name);
    _Fonts.remove(name);
    FreeFont(fnt);
  end;
  
  procedure ReleaseAllFonts();
  begin
    ReleaseAll(_Fonts, @FreeFont);
  end;
  
  //=============================================================================
  
  procedure MapSoundEffect(name, filename: String);
  var
    obj: tResourceContainer;
    snd: SoundEffect;
  begin
    snd := LoadSoundEffect(GetPathToResource(filename, SoundResource));
    obj := tResourceContainer.Create(snd);
    _SoundEffects.values[name] := obj;
  end;
  
  function HasSoundEffect(name: String): Boolean;
  begin
    result := _SoundEffects.containsKey(name);
  end;
  
  function GetSoundEffect(name: String): SoundEffect;
  begin
    result := SoundEffect(tResourceContainer(_SoundEffects.values[name]).Resource);
  end;
  
  procedure ReleaseSoundEffect(name: String);
  var
    snd: SoundEffect;
  begin
    snd := GetSoundEffect(name);
    _SoundEffects.remove(name);
    FreeSoundEffect(snd);
  end;
  
  procedure ReleaseAllSoundEffects();
  begin
    ReleaseAll(_SoundEffects, @FreeSoundEffect);
  end;
  
  //=============================================================================
  
  procedure MapMusic(name, filename: String);
  var
    obj: tResourceContainer;
    mus: Music;
  begin
    mus := LoadMusic(GetPathToResource(filename, SoundResource));
    obj := tResourceContainer.Create(mus);
    _Music.values[name] := obj;
  end;
  
  function HasMusic(name: String): Boolean;
  begin
    result := _Music.containsKey(name);
  end;
  
  function GetMusic(name: String): Music;
  begin
    result := Music(tResourceContainer(_Music.values[name]).Resource);
  end;
  
  procedure ReleaseMusic(name: String);
  var
    mus: Music;
  begin
    mus := GetMusic(name);
    _Music.remove(name);
    FreeMusic(mus);
  end;
  
  procedure ReleaseAllMusic();
  begin
    ReleaseAll(_Music, @FreeMusic);
  end;
  
  //=============================================================================
  
  procedure MapTileMap(name, filename: String);
  var
    obj: tResourceContainer;
    theMap: Map;
  begin
    theMap := LoadMap(GetPathToResource(filename, MapResource));
    obj := tResourceContainer.Create(theMap);
    _TileMaps.values[name] := obj;
  end;
  
  function HasTileMap(name: String): Boolean;
  begin
    result := _TileMaps.containsKey(name);
  end;
  
  function GetTileMap(name: String): Map;
  begin
    result := Map(tResourceContainer(_TileMaps.values[name]).Resource);
  end;
  
  procedure ReleaseTileMap(name: String);
  var
    theMap: Map;
  begin
    theMap := GetTileMap(name);
    _TileMaps.remove(name);
    FreeMap(theMap);
  end;
  
  procedure ReleaseAllTileMaps();
  begin
    ReleaseAll(_TileMaps, @FreeMap);
  end;
  
  //=============================================================================
  
  procedure ReleaseAllResources();
  begin
    // FreeFonts();
    ReleaseAllBitmaps();
    // FreeMusics();
    // FreeSounds();
    // FreeMaps();
  end;
  
  
  
  // Creates a new Mappy Map from mapFile file.
  //
  // PARAMETERS:
  // - mapName: The name of the map to load
  //
  // SIDE EFFECTS:
  // - Loads the map from file into _Maps
  // procedure NewMap(mapName: String);
  // begin
  //   SetLength(_Maps, Length(_Maps) + 1);
  //   SetLength(_MapsStr, Length(_MapsStr) + 1);
  //   _Maps[High(_Maps)] := LoadMap(mapName);
  //   _MapsStr[High(_MapsStr)] := mapName;
  // end;

  // Creates a new font.
  //
  // PARAMETERS:
  // - fontName: name to call font in _Fonts. Used when you access the font.
  // - fileName: name of the font file to load. Must be in resources/fonts
  // - size: Size of font to load
  //
  // SIDE EFFECTS:
  // - Loads the font from file into _Fonts
  // procedure NewFont(fontName, fileName: String; size: Integer);
  // begin
  //   SetLength(_Fonts, Length(_Fonts) + 1);
  //   SetLength(_FontsStr, Length(_FontsStr) + 1);
  //   _Fonts[High(_Fonts)] := LoadFont(GetPathToResource(fileName, FontResource), size);
  //   _FontsStr[High(_FontsStr)] := fontName;
  // end;
  
  // Creates a new sound.
  //
  // PARAMETERS:
  // - soundName: name to call sound in _sounds. Used when you access the sound.
  // - fileName: name of the sound file to load. Must be in resources/sounds
  //
  // SIDE EFFECTS:
  // - Loads the sound from file into _sounds
  // procedure NewSound(soundName, fileName: String);
  // begin
  //   SetLength(_Sounds, Length(_Sounds) + 1);
  //   SetLength(_SoundsStr, Length(_SoundsStr) + 1);
  //   _Sounds[High(_Sounds)] := LoadSoundEffect(GetPathToResource(fileName, SoundResource));
  //   _SoundsStr[High(_SoundsStr)] := soundName;
  // end;

  // Creates a new music.
  //
  // PARAMETERS:
  // - musicName: name to call music in _musics. Used when you access the music.
  // - fileName: name of the music file to load. Must be in resources/musics
  //
  // SIDE EFFECTS:
  // - Loads the music from file into _musics
  // procedure NewMusic(musicName, fileName: String);
  // begin
  //   SetLength(_Music, Length(_Music) + 1);
  //   SetLength(_MusicStr, Length(_MusicStr) + 1);
  //   _Music[High(_Music)] := LoadMusic(GetPathToResource(fileName, SoundResource));
  //   _MusicStr[High(_MusicStr)] := musicName;
  // end;


  // Load the fonts you need for your game.
  //
  // SIDE EFFECTS:
  // - Loads the game's fonts
  // procedure LoadFonts();
  // begin
  //   NewFont('ArialLarge', 'arial.ttf', 80);
  //   NewFont('Courier', 'cour.ttf', 16);
  //   //TODO: Add code here to load the fonts you want to use
  // end;

  // Load the images you need for your game.
  //
  // SIDE EFFECTS:
  // - Loads the game's images
  // procedure LoadImages();
  // begin
  //   //TODO: Add code here to load the images you want to use
  //   //NewImage('NoImages', 'Ufo.png');
  // end;

  // Load the soundeffects you need for your game.
  //
  // SIDE EFFECTS:
  // - Loads the game's soundeffects
  // procedure LoadSounds();
  // begin
  //   //TODO: Add code here to load the sound effects you want to use
  //   //NewSound('NoSound', 'sound.ogg');
  // end;

  // Load the music you need for your game.
  //
  // SIDE EFFECTS:
  // - Loads the game's music
  // procedure LoadMusics();
  // begin
  //   //NewMusic('NoMusic', 'sound.mp3');
  // end;

  // Load the maps you need for your game.
  //
  // SIDE EFFECTS:
  // - Loads the game's maps
  // procedure LoadMaps();
  // begin
  //   //TODO: Add code here to load the maps you want to use
  //   //NewMap('test');
  // end;

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
  // function GameFont(font: String): Font;
  // var
  //   i: Integer;
  // begin
  //   for i := Low(_FontsStr) to High(_FontsStr) do
  //   begin
  //     if _FontsStr[i] = font then begin
  //       result := _Fonts[i];
  //       exit;
  //     end;
  //   end;
  // 
  //   raise exception.create('Font ' + font + ' does not exist...');
  // end;

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
  // function GameImage(image: String): Bitmap;
  // var
  // i: Integer;
  // begin
  //   for i := Low(_ImagesStr) to High(_ImagesStr) do
  //   begin
  //     if _ImagesStr[i] = image then begin
  //       result := _Images[i];
  //       exit;
  //     end;
  //   end;
  //   raise exception.create('Image ' + image + ' does not exist...');
  // end;

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
  // function GameSound(sound: String): SoundEffect;
  // var
  //   i: Integer;
  // begin
  //   for i := Low(_SoundsStr) to High(_SoundsStr) do
  //   begin
  //     if _SoundsStr[i] = sound then begin
  //       result := _Sounds[i];
  //       exit;
  //     end;
  //   end;
  //   raise exception.create('Sound ' + sound + ' does not exist...');
  // end;

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
  // function GameMap(mapName: String): Map;
  // var
  //   i: Integer;
  // begin
  //   for i := Low(_MapsStr) to High(_MapsStr) do
  //   begin
  //     if _MapsStr[i] = mapName then begin
  //       result := _Maps[i];
  //       exit;
  //     end;
  //   end;
  //   raise exception.create('Map ' + mapName + ' does not exist...');
  // end;

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
  // function GameMusic(music: String): Music;
  // var
  //   i: Integer;
  // begin
  //   for i := Low(_MusicStr) to High(_MusicStr) do
  //   begin
  //     if _MusicStr[i] = music then begin
  //       result := _Music[i];
  //       exit;
  //     end;
  //   end;
  //   raise exception.create('Music ' + music + ' does not exist...');
  // end;
  

  // Frees the fonts that you have loaded.
  //
  // SIDE EFFECTS:
  // - Frees the game's fonts
  // procedure FreeFonts();
  // var
  //   i: Integer;
  // begin
  //   for i := Low(_Fonts) to High(_Fonts) do
  //   begin
  //     FreeFont(_Fonts[i]);
  //   end;
  // end;

  // Frees the images that you have loaded.
  //
  // SIDE EFFECTS:
  // - Frees the game's images
  // procedure FreeSounds();
  // var
  //   i: Integer;
  // begin
  //   for i := Low(_Sounds) to High(_Sounds) do
  //   begin
  //     FreeSoundEffect(_Sounds[i]);
  //   end;
  // end;

  // Frees the music that you have loaded.
  //
  // SIDE EFFECTS:
  // - Frees the game's music
  // - Stops playing any music
  // procedure FreeMusics();
  // var
  //   i: Integer;
  // begin
  //   StopMusic();
  //   Sleep(100);
  // 
  //   for i := Low(_Music) to High(_Music) do
  //   begin
  //     FreeMusic(_Music[i]);
  //   end;
  // end;

  // Frees the maps that you have loaded.
  //
  // SIDE EFFECTS:
  // - Frees the game's maps
  // procedure FreeMaps();
  // var
  //   i: Integer;
  // begin
  //   for i := Low(_Maps) to High(_Maps) do
  //   begin
  //     FreeMap(_Maps[i]);
  //   end;
  // end;

  // Plays the SwinGame intro. Please leave this
  // in all SwinGames.
  //
  // SIDE EFFECTS:
  // - Plays the starting sound
  // - Draws the background and animation
  // - Refreshes screen
  // procedure PlaySwinGameIntro();
  // const
  //   ANI_X = 143;
  //   ANI_Y = 134;
  //   ANI_W = 546;
  //   ANI_H = 327;
  //   ANI_V_CELL_COUNT = 6;
  //   ANI_CELL_COUNT = 11;
  // var
  //   i : Integer;
  // begin
  //   PlaySoundEffect(_StartSound);
  //   Sleep(200);
  //   for i:= 0 to ANI_CELL_COUNT - 1 do
  //   begin
  //     DrawBitmap(_Background, 0, 0);
  //     DrawBitmapPart(_Animation, 
  //       (i div ANI_V_CELL_COUNT) * ANI_W, (i mod ANI_V_CELL_COUNT) * ANI_H,
  //       ANI_W, ANI_H, ANI_X, ANI_Y);
  //     RefreshScreen();
  //     ProcessEvents();
  //     Sleep(20);
  //   end;
  //   Sleep(1500);
  // end;

  // Loads the resourced needed to show the loading screen,
  // and plays the intro.
  //
  // SIDE EFFECTS:
  // - Loads _Background, _Animation, _LoadingFont, and _StartSound
  // - Plays Intro
  // procedure ShowLoadingScreen();
  // begin
  //   _Background := LoadBitmap(GetPathToResource('SplashBack.png', BitmapResource));
  //   DrawBitmap(_Background, 0, 0);
  //   RefreshScreen(60);
  //   ProcessEvents();
  // 
  //   _Animation := LoadBitmap(GetPathToResource('SwinGameAni.png', BitmapResource));
  //   _LoaderEmpty := LoadBitmap(GetPathToResource('loader_empty.png', BitmapResource));
  //   _LoaderFull := LoadBitmap(GetPathToResource('loader_full.png', BitmapResource));
  //   _LoadingFont := LoadFont(GetPathToResource('arial.ttf', FontResource), 12);
  //   _StartSound := LoadSoundEffect(GetPathToResource('SwinGameStart.ogg', SoundResource));
  // 
  //   PlaySwinGameIntro();
  // end;

  // This plays the "Loading ..." messages while your
  // resources are being loaded.
  //
  // SIDE EFFECTS:
  // - Draws text to the screen
  // - Refreshes screen
  // procedure ShowMessage(message: String; number: Integer);
  // const
  //   TX = 310; TY = 493; 
  //   TW = 200; TH = 25;
  //   STEPS = 5;
  //   BG_X = 279; BG_Y = 453;
  // var
  //   fullW: Integer;
  // begin
  //   fullW := 260 * number div STEPS;
  //   DrawBitmap(_LoaderEmpty, BG_X, BG_Y);
  //   DrawBitmapPart(_LoaderFull, 0, 0, fullW, 66, BG_X, BG_Y);
  //       
  //   DrawTextLines(message, ColorWhite, ColorTransparent, _LoadingFont, AlignCenter, TX, TY, TW, TH);
  //   RefreshScreen(60);
  //   ProcessEvents();
  // end;

  // Ends the loading screen, and frees the set surfaces.
  //
  // SIDE EFFECTS:
  // - Clears the screen
  // - Frees _LoadingFont, _Background, _Animation, and _StartSound
  // procedure EndLoadingScreen();
  // begin
  //   ProcessEvents();
  //   Sleep(500);
  //   ClearScreen();
  //   RefreshScreen(60);
  //   FreeFont(_LoadingFont);
  //   FreeBitmap(_Background);
  //   FreeBitmap(_Animation);
  //   FreeBitmap(_LoaderEmpty);
  //   FreeBitmap(_LoaderFull);
  //   FreeSoundEffect(_StartSound);
  // end;

  // Call this to load your resources and to
  // display the loading screen.
  //
  // SIDE EFFECTS:
  // procedure LoadResources();
  // var
  //   oldW, oldH: Integer;
  // begin
  //   oldW := ScreenWidth();
  //   oldH := ScreenHeight();
  // 
  //   ChangeScreenSize(800, 600);
  // 
  //   //Remove sleeps once "real" game resources are being loaded
  //   ShowLoadingScreen();
  //       
  //   ShowMessage('loading fonts', 0);
  //   LoadFonts();
  //   Sleep(100);
  // 
  //   ShowMessage('loading images', 1);
  //   LoadImages();
  //   Sleep(100);
  // 
  //   ShowMessage('loading sounds', 2);
  //   LoadSounds();
  //   Sleep(100);
  // 
  //   ShowMessage('loading music', 3);
  //   LoadMusics();
  //   Sleep(100);
  // 
  //   ShowMessage('loading maps', 4);
  //   LoadMaps();
  //   Sleep(100);
  // 
  //   ShowMessage('SwinGame loaded', 5);
  //   Sleep(100);
  //   EndLoadingScreen();
  // 
  //   ChangeScreenSize(oldW, oldH);
  // end;

initialization
begin
  //WriteLn('Here...');
  _Images := tStringHash.Create(False, 1024);
  _SoundEffects := tStringHash.Create(False, 1024);
  _Fonts := tStringHash.Create(False, 1024);
  _Music := tStringHash.Create(False, 1024);
  _TileMaps := tStringHash.Create(False, 1024);
end;

end.