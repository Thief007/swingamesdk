//=============================================================================
// sgResources.pas
//=============================================================================
// Change History:
//
// Version 3:
// - 2009-07-29: Andrew : Renamed Get... functions and check for opened audio
// - 2009-07-28: Andrew : Added ShowLogos splash screen
// - 2009-07-17: Andrew : Small fixes for return types.
// - 2009-07-14: Andrew : Added resource loading and freeing procedures.
//                      : Added FreeNotifier
// - 2009-07-08: Andrew : Fixed iterator use in release all
// - 2009-07-05: Clinton: Fixed delphi-support for ExtractDelimited, formatting
// - 2009-07-03: Andrew : Fixed header comments
// - 2009-06-23: Andrew : Created
//
//=============================================================================

/// @module Resources
unit sgResources;

//=============================================================================
interface
//=============================================================================

  {$I sgTrace.inc}

  uses sgTypes;

  //----------------------------------------------------------------------------

  /// @lib
  ///
  /// @class Bitmap
  /// @constructor
  /// @sn initWithName:%s forFilename:%s
  function MapBitmap(name, filename: String): Bitmap;

  /// @lib
  ///
  /// @class Bitmap
  /// @constructor
  /// @sn initWithName:%s forFilename:%s andColorKey:%s
  function MapTransparentBitmap(name, filename: String; transparentColor: Color): Bitmap;
  
  /// @lib
  function HasBitmap(name: String): Boolean;
  
  /// @lib
  function BitmapNamed(name: String): Bitmap;
  
  /// @lib
  procedure ReleaseBitmap(name: String);
    
  /// @lib
  procedure ReleaseAllBitmaps();
  
  //----------------------------------------------------------------------------
  
  /// @lib
  ///
  /// @class Map
  /// @constructor
  /// @sn initWithName:%s forFilename:%s andSize:%s
  function MapFont(name, filename: String; size: LongInt): Font;

  /// @lib
  function HasFont(name: String): Boolean;

  /// @lib
  function FontNamed(name: String): Font;

  /// @lib
  procedure ReleaseFont(name: String);

  /// @lib
  procedure ReleaseAllFonts();

  //----------------------------------------------------------------------------

  /// @lib
  ///
  /// @class SoundEffect
  /// @constructor
  /// @sn initWithName:%s forFilename:%s
  function MapSoundEffect(name, filename: String): SoundEffect;

  /// @lib
  function HasSoundEffect(name: String): Boolean;

  /// @lib
  function SoundEffectNamed(name: String): SoundEffect;

  /// @lib
  procedure ReleaseSoundEffect(name: String);

  /// @lib
  procedure ReleaseAllSoundEffects();

  //----------------------------------------------------------------------------

  /// @lib
  ///
  /// @class Music
  /// @constructor
  /// @sn initWithName:%s forFilename:%s
  function MapMusic(name, filename: String): Music;

  /// @lib
  function HasMusic(name: String): Boolean;

  /// @lib
  function MusicNamed(name: String): Music;

  /// @lib
  procedure ReleaseMusic(name: String);

  /// @lib
  procedure ReleaseAllMusic();

  //----------------------------------------------------------------------------

  /// @lib
  ///
  /// @class Map
  /// @constructor
  /// @sn initWithName:%s forFilename:%s
  function MapTileMap(name, filename: String): Map;

  /// @lib
  function HasTileMap(name: String): Boolean;

  /// @lib
  function TileMapNamed(name: String): Map;

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
  
  /// @lib PathToResource
  function PathToResource(filename: String; kind: ResourceKind): String; overload;
  
  /// @lib PathToOtherResource
  /// @uname PathToOtherResource
  function PathToResource(filename: String): String; overload;
  
  ///
  /// @uname SetAppPathWithExe
  /// @sn setAppPath:%s withExe:%s
  /// @lib 
  procedure SetAppPath(path: String; withExe: Boolean); overload;
  
  /// @lib SetAppPath(path, True)
  procedure SetAppPath(path: String); overload;
  
  /// @lib
  function AppPath(): String;
  
  //----------------------------------------------------------------------------
  // Startup related code
  //----------------------------------------------------------------------------
  
  /// Show the Swinburne and SwinGame logos for 1 second
  ///
  /// @lib
  procedure ShowLogos();
  
  //----------------------------------------------------------------------------
  // Loading of actual resources
  //----------------------------------------------------------------------------
    
  /// Loads the `SoundEffect` from the supplied path. To ensure that your game
  /// is able to work across multiple platforms correctly ensure that you use
  /// `PathToResource` to get the full path to the files in the projects
  /// resources folder.
  ///
  /// LoadSoundEffect can load wav and ogg audio files.
  ///
  /// `FreeSoundEffect` should be called to free the resources used by the 
  /// `SoundEffect` data once the resource is no longer needed.
  ///
  /// @param path the path to the sound effect file to load. 
  ///
  /// @lib
  ///
  /// @class SoundEffect
  /// @constructor
  /// @sn initFromPath:%s
  function LoadSoundEffect(path: String): SoundEffect;

  /// Loads the `Music` from the supplied path. To ensure that your game
  /// is able to work across multiple platforms correctly ensure that you use
  /// `PathToResource` to get the full path to the files in the projects 
  /// resources folder.
  ///
  /// LoadMusic can load mp3, wav and ogg audio files.
  ///
  /// `FreeMusic` should be called to free the resources used by the 
  /// `Music` data once the resource is no longer needed.
  ///
  /// @param path the path to the music file to load.
  ///
  /// @lib
  ///
  /// @class Music
  /// @constructor
  /// @sn initFromPath:%s
  function LoadMusic(path: String): Music;
  
  //----------------------------------------------------------------------------
  // Freeing of actual resources
  //----------------------------------------------------------------------------
  
  /// Frees the resources used by a `Music` resource. All loaded
  /// `Music` should be freed once it is no longer needed. 
  ///
  /// @lib
  ///
  /// @class Music
  /// @dispose
  procedure FreeMusic(var mus: Music);
  
  /// Frees the resources used by a `SoundEffect` resource. All loaded
  /// `SoundEffect`s should be freed once they are no longer needed.
  ///
  /// @lib
  ///
  /// @class SoundEffect
  /// @dispose
  procedure FreeSoundEffect(var effect: SoundEffect);
  
  /// Creates a bitmap in memory that is the specified width and height (in pixels).
  /// The new bitmap is initially transparent and can be used as the target 
  /// for various drawing operations. Once you have drawn the desired image onto
  /// the bitmap you can call OptimiseBitmap to optimise the surface.
  ///
  /// @lib
  ///
  /// @class Bitmap
  /// @constructor
  /// @sn initWithWidth:%s andHeight:%s
  function CreateBitmap(width, height: LongInt): Bitmap;
  
  /// Loads a bitmap from file using where the specified transparent color
  /// is used as a color key for the transparent color.
  ///
  /// @lib LoadBitmapWithTransparentColor
  ///
  /// @class Bitmap
  /// @constructor
  /// @sn initWithPath:%s withTransparency:%s usingColor:%s
  function LoadBitmap(pathToBitmap: String; transparent: Boolean; transparentColor: Color): Bitmap; overload;
  
  /// Loads a bitmap from file into a Bitmap variable. This can then be drawn to
  /// the screen. Bitmaps can be of bmp, jpeg, gif, png, etc. Images may also
  /// contain alpha values, which will be drawn correctly by the API. All
  /// bitmaps must be freed using the FreeBitmap once you are finished with
  /// them.
  /// 
  /// @lib
  ///
  /// @class Bitmap
  /// @constructor
  /// @sn initWithPath:%s
  function LoadBitmap(pathToBitmap : String): Bitmap; overload;
  
  /// Loads a bitmap with a transparent color key. The transparent color is then
  /// setup as the color key to ensure the image is drawn correctly. Alpha
  /// values of Images loaded in this way will be ignored. All bitmaps must be
  /// freed using the FreeBitmap once you are finished with them.
  ///
  /// @lib LoadBitmapWithTransparentColor(pathToBitmap, True, transparentColor)
  ///
  /// @class Bitmap
  /// @constructor
  /// @sn initWithPath:%s transparentColor:%s
  function LoadTransparentBitmap(pathToBitmap : String; transparentColor : Color): Bitmap; overload;
  
  /// Frees a loaded bitmap. Use this when you will no longer be drawing the
  /// bitmap (including within Sprites), and when the program exits.
  ///
  /// @lib
  ///
  /// @class Bitmap
  /// @dispose
  procedure FreeBitmap(var bitmapToFree : Bitmap);
  
  /// Created bitmaps can be optimised for faster drawing to the screen. This
  /// optimisation should be called only once after all drawing to the bitmap
  /// is complete. Optimisation should not be used if the bitmap is to be
  /// drawn onto in the future. All loaded bitmaps are optimised during loading.
  ///
  /// @lib
  ///
  /// @class Bitmap
  /// @method OptimiseBitmap
  procedure OptimiseBitmap(surface: Bitmap);
  
  /// Loads a font from file with the specified side. Fonts must be freed using
  /// the FreeFont routine once finished with. Once the font is loaded you
  /// can set its style using SetFontStyle. Fonts are then used to draw and
  /// measure text in your programs.
  /// 
  /// @lib
  ///
  /// @class Font
  /// @constructor
  /// @sn initWithFontName:%s andSize:%s
  function LoadFont(fontName: String; size: LongInt): Font;
  
  /// Frees the resources used by the loaded Font.
  /// 
  /// @lib
  ///
  /// @class Font
  /// @dispose
  procedure FreeFont(var fontToFree: Font);
    
  /// Reads the map files specified by mapName and return a new `Map` with all
  /// the details.
  ///
  /// @lib
  ///
  /// @class Map
  /// @constructor
  /// @sn initWithMapName: %s
  function LoadMap(mapName: String): Map;
  
  /// Frees the resources associated with the map.
  ///
  /// @lib
  /// @class Map
  /// @dispose
  procedure FreeMap(var m: Map);
  
  
  
  
  //----------------------------------------------------------------------------
  // Notifier of resource freeing
  //----------------------------------------------------------------------------
  
  /// Using this procedure you can register a callback that is executed
  /// each time a resource is freed. This is called by different versions of
  /// SwinGame to keep track of resources and should not be called by user code.
  ///
  /// @lib
  procedure RegisterFreeNotifier(fn: FreeNotifier);


//=============================================================================
implementation
//=============================================================================

  uses SysUtils, StrUtils, Classes, // system
       stringhash,         // libsrc
       SDL, SDL_Mixer, SDL_ttf, SDL_Image,
       sgCore, sgText, sgAudio, sgGraphics, sgInput, sgTileMap, sgShared, sgSprites, sgTrace; // Swingame

  //----------------------------------------------------------------------------
  // Global variables for resource management.
  //----------------------------------------------------------------------------

  var
    _Images: TStringHash;
    _Fonts: TStringHash;
    _SoundEffects: TStringHash;
    _Music: TStringHash;
    _TileMaps: TStringHash;
    _Bundles: TStringHash;
    _FreeNotifier: FreeNotifier = nil;
    // The full path location of the current executable (or script). This is
    // particuarly useful when determining the path to resources (images, maps,
    // sounds, music etc).
    applicationPath: String;
    
  
  procedure CallFreeNotifier(p: Pointer);
  begin
    if Assigned(_FreeNotifier) then
    begin
      try
        // Write('calling...');
        _FreeNotifier(p);
        // WriteLn('done!');
      except
        ErrorMessage := 'Error calling free notifier';
        HasException := True;
      end;
    end;
  end;
  
  procedure RegisterFreeNotifier(fn: FreeNotifier);
  begin
    _FreeNotifier := fn;
  end;
  
  //----------------------------------------------------------------------------
  // Private types
  //----------------------------------------------------------------------------
  
  type 
    // The resource contain is an object to hold the resource for the 
    // hash table
    TResourceContainer = class(tObject)
    private
      resource_val : Pointer;
    public
      constructor Create(data: Pointer);
      
      property Resource: Pointer read resource_val;
    end;
    
    //
    // Used in loading bundles
    //
    TResourceIdentifier = record
      name, path: String;
      size: Integer;
      kind: ResourceKind;
    end;
    
    //
    // Used to store bundle details
    //
    TResourceBundle = class(tObject)
    public
      identifiers: array of TResourceIdentifier;
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

  constructor TResourceContainer.create(data: Pointer);
  begin
    inherited create;
    resource_val := data;
  end;

  constructor TResourceBundle.create();
  begin
    inherited create;
    SetLength(identifiers, 0);
  end;

  procedure TResourceBundle.add(res: tResourceIdentifier);
  begin
    SetLength(identifiers, Length(identifiers) + 1);
    identifiers[High(identifiers)] := res;
  end;

  procedure TResourceBundle.LoadResources(showProgress: Boolean; kind: ResourceKind); //overload;
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

  procedure TResourceBundle.LoadResources(showProgress: Boolean); //overload;
  begin
    LoadResources(showProgress, BitmapResource);
    LoadResources(showProgress, FontResource);
    LoadResources(showProgress, MusicResource);
    LoadResources(showProgress, MapResource);
    LoadResources(showProgress, SoundResource);
  end;

  procedure TResourceBundle.ReleaseResources();
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

  procedure ReleaseAll(tbl: TStringHash; releaser: ReleaseFunction);
  var
    iter: TStrHashIterator;
    names: array of String;
    i: Integer;
  begin
    if tbl.count = 0 then exit;
    
    SetLength(names, tbl.count);
    iter := tbl.getIterator();
    i := 0;
    
    while i < Length(names) do
    begin
      names[i] := iter.key;
      i := i + 1;
      iter.next;
    end;

    for i := Low(names) to High(names) do
    begin
      releaser(names[i]);
    end;

    tbl.deleteAll();
  end;

  //----------------------------------------------------------------------------

  function MapBitmap(name, filename: String): Bitmap;
  var
    obj: tResourceContainer;
    bmp: Bitmap;
  begin
    //WriteLn('Mapping bitmap... ', filename);
    //WriteLn(name, '->', PathToResource(filename, BitmapResource));
    bmp := LoadBitmap(PathToResource(filename, BitmapResource));
    //WriteLn(name, ' (', filename, ') => ', hexstr(bmp));
    //WriteLn(PathToResource(filename, BitmapResource));
    obj := tResourceContainer.Create(bmp);
    if not _Images.setValue(name, obj) then
      raise Exception.create('Error loaded Bitmap resource twice, ' + name);
    
    result := bmp;
  end;

  function MapTransparentBitmap(name, filename: String; transparentColor: Color): Bitmap;
  var
    obj: tResourceContainer;
    bmp: Bitmap;
  begin
    bmp := LoadBitmap(PathToResource(filename, BitmapResource), true, transparentColor);
    obj := tResourceContainer.Create(bmp);
    if not _Images.setValue(name, obj) then
      raise Exception.create('Error loaded Bitmap resource twice, ' + name);
    result := bmp;
  end;

  function HasBitmap(name: String): Boolean;
  begin
    result := _Images.containsKey(name);
  end;

  function BitmapNamed(name: String): Bitmap;
  var
    tmp : TObject;
  begin
    tmp := _Images.values[name];
    if assigned(tmp) then
      result := Bitmap(tResourceContainer(tmp).Resource)
    else 
      result := nil;
  end;

  procedure ReleaseBitmap(name: String);
  var
    bmp: Bitmap;
  begin
    bmp := BitmapNamed(name);
    if (assigned(bmp)) then
    begin
      _Images.remove(name).Free();
      FreeBitmap(bmp);
    end;
  end;

  procedure ReleaseAllBitmaps();
  begin
    ReleaseAll(_Images, @ReleaseBitmap);
  end;
  
  //----------------------------------------------------------------------------
  
  function MapFont(name, filename: String; size: LongInt): Font;
  var
    obj: tResourceContainer;
    fnt: Font;
  begin
    fnt := LoadFont(PathToResource(filename, FontResource), size);
    obj := tResourceContainer.Create(fnt);
    if not _Fonts.setValue(name, obj) then
      raise Exception.create('Error loaded Font resource twice, ' + name);
    result := fnt;
  end;
  
  function HasFont(name: String): Boolean;
  begin
    result := _Fonts.containsKey(name);
  end;

  function FontNamed(name: String): Font;
  var
    tmp : TObject;
  begin
    tmp := _Fonts.values[name];
    if assigned(tmp) then result := Font(tResourceContainer(tmp).Resource)
    else result := nil;
  end;
  
  procedure ReleaseFont(name: String);
  var
    fnt: Font;
  begin
    fnt := FontNamed(name);
    if (assigned(fnt)) then
    begin
      _Fonts.remove(name).free();
      FreeFont(fnt);
    end;
  end;
  
  procedure ReleaseAllFonts();
  begin
    ReleaseAll(_Fonts, @ReleaseFont);
  end;
  
  //----------------------------------------------------------------------------
  
  function MapSoundEffect(name, filename: String): SoundEffect;
  var
    obj: tResourceContainer;
    snd: SoundEffect;
  begin
    if not AudioOpen then exit;
    
    snd := LoadSoundEffect(PathToResource(filename, SoundResource));
    obj := tResourceContainer.Create(snd);
    if not _SoundEffects.setValue(name, obj) then
      raise Exception.create('Error loaded Sound Effect resource twice, ' + name);
    result := snd;
  end;
  
  function HasSoundEffect(name: String): Boolean;
  begin
    result := _SoundEffects.containsKey(name);
  end;

  function SoundEffectNamed(name: String): SoundEffect;
  var
    tmp : TObject;
  begin
    tmp := _SoundEffects.values[name];
    if assigned(tmp) then result := SoundEffect(tResourceContainer(tmp).Resource)
    else result := nil;
  end;
  
  procedure ReleaseSoundEffect(name: String);
  var
    snd: SoundEffect;
  begin
    snd := SoundEffectNamed(name);
    if (assigned(snd)) then
    begin
      _SoundEffects.remove(name).Free();
      FreeSoundEffect(snd);
    end;
  end;
  
  procedure ReleaseAllSoundEffects();
  begin
    ReleaseAll(_SoundEffects, @ReleaseSoundEffect);
  end;
  
  //----------------------------------------------------------------------------
  
  function MapMusic(name, filename: String): Music;
  var
    obj: tResourceContainer;
    mus: Music;
  begin
    mus := LoadMusic(PathToResource(filename, SoundResource));
    obj := tResourceContainer.Create(mus);
    if not _Music.setValue(name, obj) then
      raise Exception.create('Error loaded Music resource twice, ' + name);
    result := mus;
  end;
  
  function HasMusic(name: String): Boolean;
  begin
    result := _Music.containsKey(name);
  end;
  
  function MusicNamed(name: String): Music;
  var
    tmp : TObject;
  begin
    tmp := _Music.values[name];
    if assigned(tmp) then result := Music(tResourceContainer(tmp).Resource)
    else result := nil;
  end;
  
  procedure ReleaseMusic(name: String);
  var
    mus: Music;
  begin
    mus := MusicNamed(name);
    if (assigned(mus)) then
    begin
      _Music.remove(name).Free();
      FreeMusic(mus);
    end;
  end;
  
  procedure ReleaseAllMusic();
  begin
    ReleaseAll(_Music, @ReleaseMusic);
  end;

  //----------------------------------------------------------------------------
  
  function MapTileMap(name, filename: String): Map;
  var
    obj: tResourceContainer;
    theMap: Map;
  begin
    theMap := LoadMap(PathToResource(filename, MapResource));
    obj := tResourceContainer.Create(theMap);
    if not _TileMaps.setValue(name, obj) then
      raise Exception.create('Error loaded Map resource twice, ' + name);
    result := theMap;
  end;
  
  function HasTileMap(name: String): Boolean;
  begin
    result := _TileMaps.containsKey(name);
  end;
  
  function TileMapNamed(name: String): Map;
  var
    tmp : TObject;
  begin
    tmp := _TileMaps.values[name];
    if assigned(tmp) then result := Map(tResourceContainer(tmp).Resource)
    else result := nil;
  end;
  
  procedure ReleaseTileMap(name: String);
  var
    theMap: Map;
  begin
    theMap := TileMapNamed(name);
    if assigned(theMap) then
    begin
      _TileMaps.remove(name).Free();
      FreeMap(theMap);
    end;
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

  {$ifndef FPC} // Delphi land
  function ExtractDelimited(index: integer; value: string; delim: TSysCharSet): string;
  var
    strs: TStrings;
  begin
    // Assumes that delim is [','] and uses simple commatext mode - better check
    if delim <> [','] then
      raise Exception.create('Internal SG bug using ExtractDelimited');
    // okay - let a stringlist do the work
    strs := TStringList.Create();
    strs.CommaText := value;
    if (index >= 0) and (index < strs.Count) then
      result := strs.Strings[index]
    else
      result := '';
    // cleanup
    strs.Free();
  end;
  {$else}
  // proper ExtractDelimited provided by StrUtils
  {$endif}
  
  //----------------------------------------------------------------------------
  
  procedure LoadResourceBundle(name: String; showProgress: Boolean); overload;
  var
    input: Text; //the bundle file
    delim: TSysCharSet;
    line, path: String;
    i: Integer;
    current: tResourceIdentifier;
    bndl: tResourceBundle;
  begin
    path := PathToResource(name);
    
    if not FileExists(path) then
    begin
      RaiseException('Unable to locate resource bundle ' + path);
      exit; 
    end;
    
    Assign(input, path);
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
      begin
        RaiseException('Error loading resource bundle, no name supplied on line ' + IntToStr(i));
        exit;
      end;

      current.path := ExtractDelimited(3, line, delim);
      if Length(current.path) = 0 then 
      begin
        RaiseException('Error loading resource bundle, no path supplied on line ' + IntToStr(i));
        exit;
      end;

      if current.kind = FontResource then
      begin
        if not TryStrToInt(ExtractDelimited(4, line, delim), current.size) then
        begin
          RaiseException('Error loading resource bundle, no size supplied on line ' + IntToStr(i));
          exit;
        end;
      end
      else current.size := 0;
      
      //WriteLn('Bundle: ', current.name, ' - ', current.path, ' - ', current.size);
      
      bndl.add(current);
    end;
    
    bndl.LoadResources(showProgress);
    
    if not _Bundles.setValue(name, bndl) then //store bundle
    begin
      RaiseException('Error loaded Bundle twice, ' + name);
      exit;
    end;
    
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
  
  //----------------------------------------------------------------------------
  
  
  function PathToResourceWithBase(path, filename: String; kind: ResourceKind): String; overload; forward;
  function PathToResourceWithBase(path, filename: String): String; overload; forward;
  
  
  function PathToResourceWithBase(path, filename: String; kind: ResourceKind): String; overload;
  begin
    case kind of
    {$ifdef UNIX}
      FontResource: result := PathToResourceWithBase(path, 'fonts/' + filename);
      SoundResource: result := PathToResourceWithBase(path, 'sounds/' + filename);
      BitmapResource: result := PathToResourceWithBase(path, 'images/' + filename);
      MapResource: result := PathToResourceWithBase(path, 'maps/' + filename);
    {$else}
      FontResource: result := PathToResourceWithBase(path, 'fonts\' + filename);
      SoundResource: result := PathToResourceWithBase(path, 'sounds\' + filename);
      BitmapResource: result := PathToResourceWithBase(path, 'images\' + filename);
      MapResource: result := PathToResourceWithBase(path, 'maps\' + filename);
    {$endif}
      
      else result := PathToResourceWithBase(path, filename);
    end;
  end;

  function PathToResourceWithBase(path, filename: String): String; overload;
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
  
  function PathToResource(filename: String): String; overload;
  begin
    result := PathToResourceWithBase(applicationPath, filename);
  end;
  
  function PathToResource(filename: String; kind: ResourceKind): String; overload;
  begin
    result := PathToResourceWithBase(applicationPath, filename, kind);
  end;
  
  procedure SetAppPath(path: String); overload;
  begin
    SetAppPath(path, True);
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
  
  //----------------------------------------------------------------------------
  
  function LoadSoundEffect(path: String): SoundEffect;
  begin
    if not FileExists(path) then
    begin
      RaiseException('Unable to locate music ' + path);
      exit;
    end;

    result := Mix_LoadWAV(pchar(path));
    if result = nil then
    begin
      RaiseException('Error loading sound effect: ' + SDL_GetError());
      exit;
    end;
  end;
  
  function LoadMusic(path: String): Music;
  begin
    if not FileExists(path) then
    begin
      RaiseException('Unable to locate music ' + path);
      exit;
    end;
    
    result := Mix_LoadMUS(pchar(path));
    if result = nil then
    begin
      RaiseException('Error loading sound effect: ' + SDL_GetError());
      exit;
    end;
  end;
  
  procedure FreeSoundEffect(var effect: SoundEffect);
  begin
    if assigned(effect) then
    begin
      CallFreeNotifier(effect);
      Mix_FreeChunk(effect);
    end;
    effect := nil;
  end;
  
  procedure FreeMusic(var mus: Music);
  begin
    if assigned(mus) then
    begin
      CallFreeNotifier(mus);
      Mix_FreeMusic(mus);
    end;
    mus := nil;
  end;
  
  //----------------------------------------------------------------------------
  
  function CreateBitmap(width, height: LongInt): Bitmap;
  begin
    if (width < 1) or (height < 1) then
    begin
      RaiseException('Bitmap width and height must be greater then 0');
      exit;
    end;
    if (baseSurface = nil) or (baseSurface^.format = nil) then
    begin
      RaiseException('Unable to CreateBitmap as the window is not open');
      exit;
    end;
    
    New(result);
    
    with baseSurface^.format^ do
    begin
      result^.surface := SDL_CreateRGBSurface(SDL_SRCALPHA, width, height, 32,
                       RMask, GMask, BMask, AMask);
    end;
    
    if result^.surface = nil then
    begin
      Dispose(result);
      RaiseException('Failed to create a bitmap: ' + SDL_GetError());
      exit;
    end;
    
    result^.width := width;
    result^.height := height;
    SDL_SetAlpha(result^.surface, SDL_SRCALPHA, 0);
    SDL_FillRect(result^.surface, nil, ColorTransparent);
  end;
  
  // Sets the non-transparent pixels in a Bitmap. This is then used for
  // collision detection, allowing the original surface to be optimised.
  //
  // @param bmp  A pointer to the Bitmap being set
  // @param surface The surface with pixel data for this Bitmap
  procedure SetNonTransparentPixels(bmp: Bitmap; surface: PSDL_Surface; transparentColor: Color);
  var
    r, c: LongInt;
  begin
    SetLength(bmp^.nonTransparentPixels, bmp^.width, bmp^.height);

    for c := 0 to bmp^.width - 1 do
    begin
      for r := 0 to bmp^.height - 1 do
      begin
        bmp^.nonTransparentPixels[c, r] :=
          (GetPixel32(surface, c, r) <> transparentColor);
      end;
    end;
  end;
  
  procedure OptimiseBitmap(surface: Bitmap);
  var
    oldSurface: PSDL_Surface;
  begin
    if surface = nil then
    begin
      RaiseException('No bitmap supplied');
      exit;
    end;

    
    oldSurface := surface^.surface;
    SetNonAlphaPixels(surface, oldSurface);
    surface^.surface := SDL_DisplayFormatAlpha(oldSurface);
    SDL_FreeSurface(oldSurface);
  end;
  
  function LoadBitmap(pathToBitmap: String; transparent: Boolean; transparentColor: Color): Bitmap; overload;
  var
    loadedImage: PSDL_Surface;
    correctedTransColor: Color;
  begin
    if not FileExists(pathToBitmap) then
    begin
      RaiseException('Unable to locate bitmap ' + pathToBitmap);
      exit;
    end;

    
    loadedImage := IMG_Load(pchar(pathToBitmap));
    
    if loadedImage <> nil then
    begin
      new(result);
      if not transparent then result^.surface := SDL_DisplayFormatAlpha(loadedImage)
      else result^.surface := SDL_DisplayFormat(loadedImage);
      //result^.surface := loadedImage;
      
      //WriteLn('Loaded ', pathToBitmap);
      //WriteLn('  at ', HexStr(result^.surface));

      result^.width := result^.surface^.w;
      result^.height := result^.surface^.h;

      if transparent then
      begin
        correctedTransColor := ColorFrom(result, transparentColor);
        SDL_SetColorKey(result^.surface, SDL_RLEACCEL or SDL_SRCCOLORKEY, correctedTransColor);
        SetNonTransparentPixels(result, loadedImage, correctedTransColor);
      end
      else
      begin
        SetNonAlphaPixels(result, loadedImage);
      end;

      if loadedImage <> result^.surface then SDL_FreeSurface(loadedImage);
    end
    else
    begin
      RaiseException('Error loading image: ' + pathToBitmap + ': ' + SDL_GetError());
      exit;
    end;
  end;

  function LoadBitmap(pathToBitmap : String): Bitmap; overload;
  begin
    result := LoadBitmap(pathToBitmap, false, ColorBlack);
  end;

  function LoadTransparentBitmap(pathToBitmap : String; transparentColor : Color): Bitmap; overload;
  begin
    result := LoadBitmap(pathToBitmap, true, transparentColor);
  end;

  procedure FreeBitmap(var bitmapToFree : Bitmap);
  begin
    if Assigned(bitmapToFree) then
    begin
      // WriteLn('Free Bitmap - ', HexStr(bitmapToFree));
      CallFreeNotifier(bitmapToFree);
      // WriteLn('Done Free Bitmap - ', HexStr(bitmapToFree));
      
      if Assigned(bitmapToFree^.surface) then
      begin
        //WriteLn('Free Bitmap - ', HexStr(bitmapToFree^.surface));
        SDL_FreeSurface(bitmapToFree^.surface);
      end;
      bitmapToFree^.surface := nil;
      
      Dispose(bitmapToFree);
      bitmapToFree := nil;
    end;
  end;
  
  //----------------------------------------------------------------------------
  
  function LoadFont(fontName: String; size: LongInt): Font;
  begin
    if not FileExists(fontName) then
    begin
      RaiseException('Unable to locate font ' + fontName);
      exit;
    end;

    result := TTF_OpenFont(PChar(fontName), size);
    
    if result = nil then
    begin
      RaiseException('LoadFont failed: ' + TTF_GetError());
      exit;
    end;
  end;
  
  procedure FreeFont(var fontToFree: Font);
  begin
    if Assigned(fontToFree) then
    begin
      CallFreeNotifier(fontToFree);
      try
        TTF_CloseFont(fontToFree);
        fontToFree := nil;
      except
        RaiseException('Unable to free the specified font');
        exit;
      end;
    end;
  end;
  
  //----------------------------------------------------------------------------
  
  function ReadInt(var stream: text): Word;
  var
    c: char;
    c2: char;
    i: LongInt;
    i2: LongInt;
  begin
    Read(stream ,c);
    Read(stream ,c2);

    i := LongInt(c);
    i2 := LongInt(c2) * 256;

    result := i + i2;
  end;

  procedure LoadMapInformation(m: Map; var stream: text);
  var
    header: LongInt;
  begin
    header := ReadInt(stream);

    if header = 0 then
    begin
      m^.MapInfo.Version := ReadInt(stream);
      m^.MapInfo.MapWidth := ReadInt(stream);
    end
    else
    begin
      m^.MapInfo.Version := 1;
      m^.MapInfo.MapWidth := header;
    end;

    //m^.MapInfo.MapWidth := ReadInt(stream);
    m^.MapInfo.MapHeight := ReadInt(stream);
    m^.MapInfo.BlockWidth := ReadInt(stream);
    m^.MapInfo.BlockHeight := ReadInt(stream);
    m^.MapInfo.NumberOfBlocks := ReadInt(stream);
    m^.MapInfo.NumberOfAnimations := ReadInt(stream);
    m^.MapInfo.NumberOfLayers := ReadInt(stream);
    m^.MapInfo.CollisionLayer := ReadInt(stream);
    m^.MapInfo.TagLayer := ReadInt(stream);
    m^.MapInfo.GapX := 0;
    m^.MapInfo.GapY := 0;
    m^.MapInfo.StaggerX := 0;
    m^.MapInfo.StaggerY := 0;
    m^.MapInfo.Isometric := false;

      {
      //Debug
      WriteLn('MapInformation');
      WriteLn('');
      WriteLn(m^.MapInfo.MapWidth);
      WriteLn(m^.MapInfo.MapHeight);
      WriteLn(m^.MapInfo.BlockWidth);
      WriteLn(m^.MapInfo.BlockHeight);
      WriteLn(m^.MapInfo.NumberOfBlocks);
      WriteLn(m^.MapInfo.NumberOfAnimations);
      WriteLn(m^.MapInfo.NumberOfLayers);
      WriteLn(m^.MapInfo.CollisionLayer);
      WriteLn(m^.MapInfo.TagLayer);
      WriteLn('');
      ReadLn();
      }
  end;

  procedure LoadIsometricInformation(m: Map; var stream: text);
  begin
    m^.MapInfo.GapX := ReadInt(stream);
    m^.MapInfo.GapY := ReadInt(stream);
    m^.MapInfo.StaggerX := ReadInt(stream);
    m^.MapInfo.StaggerY := ReadInt(stream);

    if ((m^.MapInfo.StaggerX = 0) and (m^.MapInfo.StaggerY = 0)) then
    begin
      m^.MapInfo.Isometric := false;
      m^.MapInfo.GapX := 0;
      m^.MapInfo.GapY := 0;
    end
    else
      m^.MapInfo.Isometric := true;

  end;


  procedure LoadAnimationInformation(m: Map; var stream: text);
  var
    i, j: LongInt;
  begin

    if m^.MapInfo.NumberOfAnimations > 0 then
    begin

      SetLength(m^.AnimationInfo, m^.MapInfo.NumberOfAnimations);

      for i := 0 to m^.MapInfo.NumberOfAnimations - 1 do
      begin

        m^.AnimationInfo[i].AnimationNumber := i + 1;
        m^.AnimationInfo[i].Delay := ReadInt(stream);
        m^.AnimationInfo[i].NumberOfFrames := ReadInt(stream);

        SetLength(m^.AnimationInfo[i].Frame, m^.AnimationInfo[i].NumberOfFrames);

        for j := 0 to m^.AnimationInfo[i].NumberOfFrames - 1 do
        begin
          m^.AnimationInfo[i].Frame[j] := ReadInt(stream);
        end;

        m^.AnimationInfo[i].CurrentFrame := 0;

      end;

      {
      //Debug
      WriteLn('Animation Information');
      WriteLn('');
      for i := 0 to m^.MapInfo.NumberOfAnimations - 1 do
      begin
        WriteLn(m^.AnimationInfo[i].AnimationNumber);
        WriteLn(m^.AnimationInfo[i].Delay);
        WriteLn(m^.AnimationInfo[i].NumberOfFrames);

        for j := 0 to m^.AnimationInfo[i].NumberOfFrames - 1 do
        begin
          WriteLn(m^.AnimationInfo[i].Frame[j]);
        end;
      end;
      WriteLn('');
      ReadLn();
      }
    end;
  end;

  procedure LoadLayerData(m: Map; var stream: text);
  var
    l, y, x: LongInt;
  begin

    SetLength(m^.LayerInfo, m^.MapInfo.NumberOfLayers - m^.MapInfo.Collisionlayer - m^.MapInfo.TagLayer);

    for y := 0 to Length(m^.LayerInfo) - 1 do
    begin

      SetLength(m^.LayerInfo[y].Animation, m^.MapInfo.MapHeight);
      SetLength(m^.LayerInfo[y].Value, m^.MapInfo.MapHeight);

      for x := 0 to m^.MapInfo.MapHeight - 1 do
      begin

        SetLength(m^.LayerInfo[y].Animation[x], m^.MapInfo.MapWidth);
        SetLength(m^.LayerInfo[y].Value[x], m^.MapInfo.MapWidth);
      end;
    end;

    for l := 0 to m^.MapInfo.NumberOfLayers - m^.MapInfo.Collisionlayer - m^.MapInfo.Taglayer - 1 do
    begin
      for y := 0 to m^.MapInfo.MapHeight - 1 do
      begin
        for x := 0 to m^.MapInfo.MapWidth - 1 do
        begin

          m^.LayerInfo[l].Animation[y][x] := ReadInt(stream);
          m^.LayerInfo[l].Value[y][x] := ReadInt(stream);
        end;
      end;
    end;

    {
    //Debug
    WriteLn('Layer Information');
    WriteLn(Length(m^.Layerinfo));
    WriteLn('');

    for l := 0 to Length(m^.LayerInfo) - 1 do
    begin
      for y := 0 to m^.MapInfo.MapHeight - 1 do
      begin
        for x := 0 to m^.MapInfo.MapWidth - 1 do
        begin
          Write(m^.LayerInfo[l].Animation[y][x]);
          Write(',');
          Write(m^.LayerInfo[l].Value[y][x]);
          Write(' ');
        end;
      end;
      WriteLn('');
      ReadLn();
    end;
    }


  end;

  procedure LoadCollisionData(m: Map; var stream: text);
  var
    y, x: LongInt;
  begin
    if m^.MapInfo.CollisionLayer = 1 then
    begin
      SetLength(m^.CollisionInfo.Collidable, m^.MapInfo.MapHeight);

      for y := 0 to m^.MapInfo.MapHeight - 1 do
      begin
        SetLength(m^.CollisionInfo.Collidable[y], m^.MapInfo.MapWidth);
      end;

      for y := 0 to m^.MapInfo.MapHeight - 1 do
      begin
        for x := 0 to m^.MapInfo.MapWidth - 1 do
        begin
          // True/False
          m^.CollisionInfo.Collidable[y][x] := (ReadInt(stream) <> 0);
//          if ReadInt(stream) <> 0 then
//            m^.CollisionInfo.Collidable[y][x] := true
//          else
//            m^.CollisionInfo.Collidable[y][x] := false
        end;
      end;


      //Debug
      {
      for y := 0 to m^.MapInfo.MapHeight - 1 do
      begin
        for x := 0 to m^.MapInfo.MapWidth - 1 do
        begin
          if m^.CollisionInfo.Collidable[y][x] = true then
            Write('1')
          else
            Write('0')
        end;
        WriteLn('');
      end;
      ReadLn();
      }
    end;
  end;

  procedure LoadTagData(m: Map; var stream: text);
  var
    py, px, smallestTagIdx, temp: LongInt;
    evt: MapTag;
  begin
    //SetLength(m^.TagInfo, High(Tags));
    //SetLength(m^.TagInfo.Tag, m^.MapInfo.MapHeight);
    {for y := 0 to m^.MapInfo.MapHeight - 1 do
    begin
      SetLength(m^.TagInfo.Tag[y], m^.MapInfo.MapWidth);
    end;}

    //The smallest "non-graphics" tile, i.e. the tags
    smallestTagIdx := m^.MapInfo.NumberOfBlocks - 23;

    for py := 0 to m^.MapInfo.MapHeight - 1 do
    begin
      for px := 0 to m^.MapInfo.MapWidth - 1 do
      begin
        temp := ReadInt(stream);
        evt := MapTag(temp - smallestTagIdx);
        //TODO: Optimize - avoid repeated LongIng(evt) conversions
        if (evt >= MapTag1) and (evt <= MapTag24) then
        begin
          SetLength(m^.TagInfo[LongInt(evt)], Length(m^.TagInfo[LongInt(evt)]) + 1);

          with m^.TagInfo[LongInt(evt)][High(m^.TagInfo[LongInt(evt)])] do
          begin
            x := px;
            y := py;
          end;
        end
      end;
    end;


    //Debug
    {
    for y := 0 to m^.MapInfo.MapHeight - 1 do
    begin
      for x := 0 to m^.MapInfo.MapWidth - 1 do
      begin
        Write(' ');
        Write(LongInt(m^.TagInfo.Tag[y][x]));
      end;
      WriteLn('');
    end;
    ReadLn();
    }
  end;

  procedure LoadBlockSprites(m: Map; fileName: String);
  var
    fpc: LongIntArray; //Array of LongInt;
  begin
    SetLength(fpc, m^.MapInfo.NumberOfBlocks);
    m^.Tiles := CreateSprite(LoadBitmap(fileName), true, fpc,
                             m^.MapInfo.BlockWidth,
                             m^.MapInfo.BlockHeight);
    m^.Tiles^.currentCell := 0;
  end;
  
  //mapFile and imgFile are full-path+filenames
  function LoadMapFiles(mapFile, imgFile: String): Map;
  var
    f: text;
    m: Map;
  begin
    if not FileExists(mapFile) then begin RaiseException('Unable to locate map: ' + mapFile); exit; end;
    if not FileExists(imgFile) then begin RaiseException('Unable to locate map images: ' + imgFile); exit; end;
    
    //Get File
    assign(f, mapFile);
    reset(f);
    
    //Create Map
    New(m);
    
    //Load Map Content
    LoadMapInformation(m, f);
    if (m^.MapInfo.Version > 1) then LoadIsometricInformation(m, f);
    LoadAnimationInformation(m, f);
    LoadLayerData(m, f);
    LoadCollisionData(m, f);
    LoadTagData(m, f);
    //Close File
    close(f);

    LoadBlockSprites(m, imgFile);
    m^.Frame := 0;
    result := m;

    //WriteLn(m^.MapInfo.Version);
  end;

  function LoadMap(mapName: String): Map;
  var
    mapFile, imgFile: String;
  begin
    mapFile := PathToResource(mapName + '.sga', MapResource);
    imgFile := PathToResource(mapName + '.png', MapResource);
    result := LoadMapFiles(mapFile, imgFile);
  end;

  procedure FreeMap(var m: Map);
  begin
    if assigned(m) then
    begin
      CallFreeNotifier(m);
      
      // Free the one bitmap associated with the map
      FreeBitmap(m^.Tiles^.bitmaps[0]);
      FreeSprite(m^.Tiles);
      Dispose(m);
      m := nil;
    end;
  end;
  
  //----------------------------------------------------------------------------
  
  procedure ShowLogos();
  const
    ANI_X = 143;
    ANI_Y = 134;
    ANI_W = 546;
    ANI_H = 327;
    ANI_V_CELL_COUNT = 6;
    ANI_CELL_COUNT = 11;
  var
    i: Integer;
    f: Font;
    txt: String;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgResources', 'ShowLogos');
    {$ENDIF}
    
    ClearScreen();
    try
      try
        LoadResourceBundle('splash.txt', False);
      
        DrawBitmap(BitmapNamed('Swinburne'), 286, 171);
        f := FontNamed('ArialLarge');
        txt := 'SwinGame API by Swinburne University of Technology';
        DrawText(txt, ColorWhite, f, (ScreenWidth() - TextWidth(f, txt)) div 2, 500);
    
        for i := 1 to 60 do
        begin
          ProcessEvents();
          RefreshScreen(60);
          if WindowCloseRequested() or KeyDown(vk_Escape) then break;
        end;
    
        if AudioOpen then PlaySoundEffect(SoundEffectNamed('SwinGameStart'));
        for i:= 0 to ANI_CELL_COUNT - 1 do
        begin
          DrawBitmap(BitmapNamed('SplashBack'), 0, 0);
          DrawBitmapPart(BitmapNamed('SwinGameAni'), 
            (i div ANI_V_CELL_COUNT) * ANI_W, (i mod ANI_V_CELL_COUNT) * ANI_H,
            ANI_W, ANI_H - 1, ANI_X, ANI_Y);
          RefreshScreen();
          ProcessEvents();
          if WindowCloseRequested() or KeyDown(vk_Escape) then continue;
          Sleep(15);
        end;
        Sleep(1000);
      except on e:Exception do
        {$IFDEF TRACE}
          Trace('sgResources', 'Error', 'ShowLogos', 'Error loading and drawing splash.');
          Trace('sgResources', 'Error', 'ShowLogos', e.Message);
        {$ENDIF}
      end;
    finally
      try
        ReleaseResourceBundle('splash.txt');
      except on e1: Exception do
        {$IFDEF TRACE}
          Trace('sgResources', 'Error', 'ShowLogos', 'Error freeing splash.');
          Trace('sgResources', 'Error', 'ShowLogos', e.Message);
        {$ENDIF}
      end;
    end;
    
    {$IFDEF TRACE}
      TraceExit('sgResources', 'ShowLogos');
    {$ENDIF}
  end;
  


//=============================================================================

  initialization
  begin
    InitialiseSwinGame();
    
    _Images := TStringHash.Create(False, 1024);
    _SoundEffects := TStringHash.Create(False, 1024);
    _Fonts := TStringHash.Create(False, 1024);
    _Music := TStringHash.Create(False, 1024);
    _TileMaps := TStringHash.Create(False, 1024);
    _Bundles := TStringHash.Create(False, 1024);
    
    if ParamCount() >= 0 then
      SetAppPath(ParamStr(0), True)
  end;

end.