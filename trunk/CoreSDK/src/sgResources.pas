// Change History:
//
// Version 3:
// - 2009-06-23: Andrew: Created


/// @module ResourceManager
unit sgResources;

interface
  uses sgTypes;
  
  //----------------------------------------------------------------------------
    
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
  
  //----------------------------------------------------------------------------
  
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
  
  //----------------------------------------------------------------------------
  
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
  
  //----------------------------------------------------------------------------
  
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
  
  //----------------------------------------------------------------------------
  
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
  
  
  
  //----------------------------------------------------------------------------
  // Bundle handling routines
  //----------------------------------------------------------------------------
  
  /// @lib
  procedure LoadResourceBundle(name: String; showProgress: Boolean); overload;
  
  /// @lib LoadResourceBundle(name, True)
  /// @uname LoadResourceBundleWithProgress
  procedure LoadResourceBundle(name: String); overload;
  
  /// @lib
  procedure ReleaseResourceBundle(name: String);
  
  /// @lib
  function HasResourceBundle(name: String): Boolean;
  
  //----------------------------------------------------------------------------
  // Release all resources procedure
  //----------------------------------------------------------------------------
  /// @lib
  procedure ReleaseAllResources();
  
  
  
  //----------------------------------------------------------------------------
  // Resource Path and Application Path
  //----------------------------------------------------------------------------
  
  /// @lib GetPathToResource
  function GetPathToResource(filename: String; kind: ResourceKind): String; overload;
  
  /// @lib GetPathToOtherResource
  /// @uname GetPathToOtherResource
  function GetPathToResource(filename: String): String; overload;
  
  /// @lib GetPathToResourceWithBaseAndKind
  /// @uname GetPathToResourceWithBase
  function GetPathToResourceWithBase(path, filename: String; kind: ResourceKind): String; overload;
  
  /// @lib GetPathToOtherResourceWithBase
  /// @uname GetPathToOtherResourceWithBase
  function GetPathToResourceWithBase(path, filename: String): String; overload;
  
  /// @lib
  procedure SetAppPath(path: String; withExe: Boolean);
  /// @lib
  function AppPath(): String;
  
  
  
//----------------------------------------------------------------------------
implementation
//----------------------------------------------------------------------------
  
  uses SysUtils, sgCore, sgText, sgAudio, sgGraphics, sgInput, sgTileMap, stringhash, strUtils, sgShared;
  
  //----------------------------------------------------------------------------
  // Global variables for resource management.
  //----------------------------------------------------------------------------
  
  var
    _Images: tStringHash;
    _Fonts: tStringHash;
    _SoundEffects: tStringHash;
    _Music: tStringHash;
    _TileMaps: tStringHash;
    _Bundles: tStringHash;
  
  
  //----------------------------------------------------------------------------
  // Private types
  //----------------------------------------------------------------------------
  
  type 
    // The resource contain is an object to hold the resource for the 
    // hash table
    tResourceContainer = class(tObject)
    private
      resource_val : Pointer;
    public
      constructor Create(data: Pointer);
    
      property Resource: Pointer read resource_val;
    end;
    
    //
    // Used in loading bundles
    //
    tResourceIdentifier = record
        name, path: String;
        size: Integer;
        kind: ResourceKind;
      end;
    
    //
    // Used to store bundle details
    //
    tResourceBundle = class(tObject)
    public
      identifiers: array of tResourceIdentifier;
      
      constructor Create();
      procedure add(res: tResourceIdentifier);
      procedure LoadResources(showProgress: Boolean; kind: ResourceKind); overload;
      procedure LoadResources(showProgress: Boolean); overload;
      procedure ReleaseResources();
    end;
    
    //
    // Used by release all
    //
    ReleaseFunction = procedure(name: String);
  
  //----------------------------------------------------------------------------
  // Private type functions/procedures
  //----------------------------------------------------------------------------
  
  constructor tResourceContainer.create(data: Pointer);
  begin
    inherited create;
    resource_val := data;
  end;
  
  constructor tResourceBundle.create();
  begin
    inherited create;
    SetLength(identifiers, 0);
  end;
  
  procedure tResourceBundle.add(res: tResourceIdentifier);
  begin
    SetLength(identifiers, Length(identifiers) + 1);
    identifiers[High(identifiers)] := res;
  end;
  
  procedure tResourceBundle.LoadResources(showProgress: Boolean; kind: ResourceKind); overload;
  var
    current: tResourceIdentifier;
    i: Integer;
  begin
    for i := Low(identifiers) to High(identifiers) do
    begin
      current := identifiers[i];
      
      if current.kind = kind then
      begin
        case kind of
          BitmapResource: MapBitmap(current.name, current.path);
          FontResource:   MapFont(current.name, current.path, current.size);
          SoundResource:  MapSoundEffect(current.name, current.path);
          MusicResource:  MapMusic(current.name, current.path);
          MapResource:    MapTileMap(current.name, current.path);
        end;        
      end;
    end;
  end;
  
  procedure tResourceBundle.LoadResources(showProgress: Boolean); overload;
  begin
    LoadResources(showProgress, BitmapResource);
    LoadResources(showProgress, FontResource);
    LoadResources(showProgress, MusicResource);
    LoadResources(showProgress, MapResource);
    LoadResources(showProgress, SoundResource);
  end;
  
  procedure tResourceBundle.ReleaseResources();
  var
    current: tResourceIdentifier;
    i: Integer;
  begin
    for i := Low(identifiers) to High(identifiers) do
    begin
      current := identifiers[i];
      
      case current.kind of
        BitmapResource: ReleaseBitmap(current.name);
        FontResource:   ReleaseFont(current.name);
        SoundResource:  ReleaseSoundEffect(current.name);
        MusicResource:  ReleaseMusic(current.name);
        MapResource:    ReleaseTileMap(current.name);
      end;        
    end;
    SetLength(identifiers, 0);
  end;
  
  //----------------------------------------------------------------------------
  
  procedure ReleaseAll(tbl: tStringHash; releaser: ReleaseFunction);
  var
    iter: tStrHashIterator;
    names: array of String;
    i: Integer;
  begin
    SetLength(names, tbl.count);
    iter := tbl.getIterator();
    i := 0;
    
    while iter.hasNext() do
    begin
      names[i] := iter.key;
      i += 1;
    end;
    
    for i := Low(names) to High(names) do
    begin
      releaser(names[i]);
    end;
    
    tbl.deleteAll();
  end;
  
  //----------------------------------------------------------------------------
  
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
    if not _Images.setValue(name, obj) then
      raise Exception.create('Error loaded Bitmap resource twice, ' + name);
  end;
  
  procedure MapTransparentBitmap(name, filename: String; transparentColor: Color);
  var
    obj: tResourceContainer;
  begin
    obj := tResourceContainer.Create(LoadBitmap(GetPathToResource(filename, BitmapResource), true, transparentColor));
    if not _Images.setValue(name, obj) then
      raise Exception.create('Error loaded Bitmap resource twice, ' + name);
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
    _Images.remove(name).Free();
    FreeBitmap(bmp);
  end;
  
  procedure ReleaseAllBitmaps();
  begin
    ReleaseAll(_Images, @ReleaseBitmap);
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
  
  //----------------------------------------------------------------------------
  
  procedure MapFont(name, filename: String; size: LongInt);
  var
    obj: tResourceContainer;
    fnt: Font;
  begin
    fnt := LoadFont(GetPathToResource(filename, FontResource), size);
    obj := tResourceContainer.Create(fnt);
    if not _Fonts.setValue(name, obj) then
      raise Exception.create('Error loaded Font resource twice, ' + name);
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
    _Fonts.remove(name).free();
    FreeFont(fnt);
  end;
  
  procedure ReleaseAllFonts();
  begin
    ReleaseAll(_Fonts, @ReleaseFont);
  end;
  
  //----------------------------------------------------------------------------
  
  procedure MapSoundEffect(name, filename: String);
  var
    obj: tResourceContainer;
    snd: SoundEffect;
  begin
    snd := LoadSoundEffect(GetPathToResource(filename, SoundResource));
    obj := tResourceContainer.Create(snd);
    if not _SoundEffects.setValue(name, obj) then
      raise Exception.create('Error loaded Sound Effect resource twice, ' + name);
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
    _SoundEffects.remove(name).Free();
    FreeSoundEffect(snd);
  end;
  
  procedure ReleaseAllSoundEffects();
  begin
    ReleaseAll(_SoundEffects, @ReleaseSoundEffect);
  end;
  
  //----------------------------------------------------------------------------
  
  procedure MapMusic(name, filename: String);
  var
    obj: tResourceContainer;
    mus: Music;
  begin
    mus := LoadMusic(GetPathToResource(filename, SoundResource));
    obj := tResourceContainer.Create(mus);
    if not _Music.setValue(name, obj) then
      raise Exception.create('Error loaded Music resource twice, ' + name);
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
    _Music.remove(name).Free();
    FreeMusic(mus);
  end;
  
  procedure ReleaseAllMusic();
  begin
    ReleaseAll(_Music, @ReleaseMusic);
  end;
  
  //----------------------------------------------------------------------------
  
  procedure MapTileMap(name, filename: String);
  var
    obj: tResourceContainer;
    theMap: Map;
  begin
    theMap := LoadMap(GetPathToResource(filename, MapResource));
    obj := tResourceContainer.Create(theMap);
    if not _TileMaps.setValue(name, obj) then
      raise Exception.create('Error loaded Map resource twice, ' + name);
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
    _TileMaps.remove(name).Free();
    FreeMap(theMap);
  end;
  
  procedure ReleaseAllTileMaps();
  begin
    ReleaseAll(_TileMaps, @ReleaseTileMap);
  end;
  
  //----------------------------------------------------------------------------
  
  procedure ReleaseAllResources();
  begin
    ReleaseAllFonts();
    ReleaseAllBitmaps();
    ReleaseAllMusic();
    ReleaseAllSoundEffects();
    ReleaseAllTileMaps();
    _Bundles.deleteAll();
  end;
  
  //----------------------------------------------------------------------------
  
  function StringToResourceKind(kind: String): ResourceKind;
  begin
    if kind = 'BITMAP' then result := BitmapResource
    else if kind = 'SOUND' then result := SoundResource
    else if kind = 'MUSIC' then result := MusicResource
    else if kind = 'FONT' then result := FontResource
    else if kind = 'MAP' then result := MapResource
    else if kind = 'PANEL' then result := PanelResource
    else result := OtherResource;
  end;
  
  procedure LoadResourceBundle(name: String; showProgress: Boolean); overload;
  var
    input: Text; //the bundle file
    delim: TSysCharSet;
    line: String;
    i: Integer;
    current: tResourceIdentifier;
    bndl: tResourceBundle;
  begin
    Assign(input, GetPathToResource(name));
    Reset(input);
    
    delim := [ ',' ]; //comma delimited
    i := 0;
    
    bndl := tResourceBundle.Create();
    
    while not EOF(input) do
    begin
      i := i + 1;
      ReadLn(input, line);
      line := Trim(line);
      if Length(line) = 0 then continue;
      
      current.kind := StringToResourceKind(ExtractDelimited(1, line, delim));
      current.name := ExtractDelimited(2, line, delim);
      if Length(current.name) = 0 then 
        raise Exception.Create('Error loading resource bundle, no name supplied on line ' + IntToStr(i));
      current.path := ExtractDelimited(3, line, delim);
      if Length(current.path) = 0 then 
        raise Exception.Create('Error loading resource bundle, no path supplied on line ' + IntToStr(i));
      if current.kind = FontResource then
      begin
        if not TryStrToInt(ExtractDelimited(4, line, delim), current.size) then
          raise Exception.Create('Error loading resource bundle, no size supplied on line ' + IntToStr(i));
      end
      else current.size := 0;
      
      //WriteLn('Bundle: ', current.name, ' - ', current.path, ' - ', current.size);
      
      bndl.add(current);
    end;
    
    bndl.LoadResources(showProgress);
    
    if not _Bundles.setValue(name, bndl) then //store bundle
      raise Exception.create('Error loaded Bundle twice, ' + name);
    
    Close(input);
  end;
  
  procedure LoadResourceBundle(name: String); overload;
  begin
    LoadResourceBundle(name, True);
  end;
  
  function HasResourceBundle(name: String): Boolean;
  begin
    result := _Bundles.containsKey(name);
  end;
  
  procedure ReleaseResourceBundle(name: String);
  var
    bndl: tResourceBundle;
  begin
    if HasResourceBundle(name) then
    begin
      bndl := tResourceBundle(_Bundles.remove(name));
      bndl.ReleaseResources();
      bndl.Free();
    end;
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
  //----------------------------------------------------------------------------
  // Resource Path and Application Path
  //----------------------------------------------------------------------------
  
  function GetPathToResourceWithBase(path, filename: String; kind: ResourceKind): String; overload;
  begin
    case kind of
    {$ifdef UNIX}
      FontResource: result := GetPathToResourceWithBase(path, 'fonts/' + filename);
      SoundResource: result := GetPathToResourceWithBase(path, 'sounds/' + filename);
      BitmapResource: result := GetPathToResourceWithBase(path, 'images/' + filename);
      MapResource: result := GetPathToResourceWithBase(path, 'maps/' + filename);
    {$else}
      FontResource: result := GetPathToResourceWithBase(path, 'fonts\' + filename);
      SoundResource: result := GetPathToResourceWithBase(path, 'sounds\' + filename);
      BitmapResource: result := GetPathToResourceWithBase(path, 'images\' + filename);
      MapResource: result := GetPathToResourceWithBase(path, 'maps\' + filename);
    {$endif}
      
      else result := GetPathToResourceWithBase(path, filename);
    end;
  end;
  
  function GetPathToResourceWithBase(path, filename: String): String; overload;
  begin
    {$ifdef UNIX}
      {$ifdef DARWIN}
        result := path + '/../Resources/';
      {$else}
        result := path + '/Resources/';
      {$endif}
    {$else}
    //Windows
      result := path + '\resources\';
    {$endif}
    result := result + filename;
  end;
  
  function GetPathToResource(filename: String): String; overload;
  begin
    result := GetPathToResourceWithBase(applicationPath, filename);
  end;
  
  function GetPathToResource(filename: String; kind: ResourceKind): String; overload;
  begin
    result := GetPathToResourceWithBase(applicationPath, filename, kind);
  end;
  
  procedure SetAppPath(path: String; withExe: Boolean);
  begin
    if withExe then applicationPath := ExtractFileDir(path)
    else applicationPath := path;
  end;
  
  function AppPath(): String;
  begin
    result := applicationPath;
  end;
  
  //---------------------------------------------------------------------------

initialization
begin
  _Images := tStringHash.Create(False, 1024);
  _SoundEffects := tStringHash.Create(False, 1024);
  _Fonts := tStringHash.Create(False, 1024);
  _Music := tStringHash.Create(False, 1024);
  _TileMaps := tStringHash.Create(False, 1024);
  _Bundles := tStringHash.Create(False, 1024);
end;

end.