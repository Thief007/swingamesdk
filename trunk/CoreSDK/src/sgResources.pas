//=============================================================================
// sgResources.pas
//=============================================================================
// Change History:
//
// Version 3:
// - 2009-12-07: Andrew : Moved out loading of image resources... more to follow
// - 2009-11-10: Andrew : Changed sn to csn tags
// - 2009-11-06: Andrew : Moved out loading of audio resources... others to follow
// - 2009-10-16: Andrew : Moved free notifier, and ensured free notifier called after dispose
// - 2009-09-11: Andrew : Fixed to load resources without needing path
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

{$I sgTrace.inc}

/// @module Resources
unit sgResources;

//=============================================================================
interface
  uses sgTypes;
//=============================================================================

  /// @lib
  ///
  /// @class Map
  /// @constructor
  /// @csn initWithName:%s forFilename:%s
  function MapTileMap(name, filename: String): Map;

  /// @lib
  function HasTileMap(name: String): Boolean;

  /// @lib
  function FetchTileMap(name: String): Map;

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
  
  
  
  /// Reads the map files specified by mapName and return a new `Map` with all
  /// the details.
  ///
  /// @lib
  ///
  /// @class Map
  /// @constructor
  /// @csn initWithMapName: %s
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
       sgCore, sgText, sgAudio, sgGraphics, sgInput, sgTileMap, sgShared, sgSprites, sgTrace, sgImages; // Swingame

  //----------------------------------------------------------------------------
  // Global variables for resource management.
  //----------------------------------------------------------------------------

  var
    _TileMaps: TStringHash;
    _Bundles: TStringHash;
    // The full path location of the current executable (or script). This is
    // particuarly useful when determining the path to resources (images, maps,
    // sounds, music etc).
    applicationPath: String;
    
  
  procedure RegisterFreeNotifier(fn: FreeNotifier);
  begin
    {$IFDEF TRACE}
      TraceEnter('sgResources', 'sgResources.RegisterFreeNotifier');
      Trace('sgResources', 'Info', 'sgResources.RegisterFreeNotifier', 'fn: ' + HexStr(fn));
    {$ENDIF}
    _FreeNotifier := fn;
    {$IFDEF TRACE}
      TraceExit('sgResources', 'sgResources.RegisterFreeNotifier');
    {$ENDIF}

  end;
  
  //----------------------------------------------------------------------------
  // Private types
  //----------------------------------------------------------------------------
  
  type 
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

  //----------------------------------------------------------------------------
  // Private type functions/procedures
  //----------------------------------------------------------------------------

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
    {$IFDEF TRACE}
      TraceEnter('sgResources', 'TResourceBundle.LoadResources');
    {$ENDIF}
    {$IFDEF TRACE}
      Trace('sgResources', 'Info', 'TResourceBundle.LoadResources', 'Calling BitmapResource');
    {$ENDIF}
    LoadResources(showProgress, BitmapResource);
    {$IFDEF TRACE}
      Trace('sgResources', 'Info', 'TResourceBundle.LoadResources', 'Calling FontResource');
    {$ENDIF}
    LoadResources(showProgress, FontResource);
    {$IFDEF TRACE}
      Trace('sgResources', 'Info', 'TResourceBundle.LoadResources', 'Calling MusicResource');
    {$ENDIF}
    LoadResources(showProgress, MusicResource);
    {$IFDEF TRACE}
      Trace('sgResources', 'Info', 'TResourceBundle.LoadResources', 'Calling MapResource');
    {$ENDIF}
    LoadResources(showProgress, MapResource);
    {$IFDEF TRACE}
      Trace('sgResources', 'Info', 'TResourceBundle.LoadResources', 'Calling SoundResource');
    {$ENDIF}
    LoadResources(showProgress, SoundResource);
    {$IFDEF TRACE}
      TraceExit('sgResources', 'TResourceBundle.LoadResources');
    {$ENDIF}
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
  
  function MapTileMap(name, filename: String): Map;
  var
    obj: tResourceContainer;
    theMap: Map;
  begin
    theMap := LoadMap(filename);
    obj := tResourceContainer.Create(theMap);
    if not _TileMaps.setValue(name, obj) then
      raise Exception.create('Error loaded Map resource twice, ' + name);
    result := theMap;
  end;
  
  function HasTileMap(name: String): Boolean;
  begin
    result := _TileMaps.containsKey(name);
  end;
  
  function FetchTileMap(name: String): Map;
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
    theMap := FetchTileMap(name);
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
    {$IFDEF TRACE}
      TraceEnter('sgResources', 'LoadResourceBundle');
    {$ENDIF}
    
    path := name;
    if not FileExists(path) then
    begin
      path := PathToResource(name);
      
      if not FileExists(path) then
      begin
        RaiseException('Unable to locate resource bundle ' + path);
        exit;
      end;
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
    {$IFDEF TRACE}
      TraceExit('sgResources', 'LoadResourceBundle');
    {$ENDIF}
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
      // Free the one bitmap associated with the map
      FreeBitmap(m^.Tiles^.bitmaps[0]);
      FreeSprite(m^.Tiles);
      Dispose(m);
      CallFreeNotifier(m);
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
    oldW, oldH: Integer;
    //isStep: Boolean;
    isPaused: Boolean;
    isSkip: Boolean;
    
    procedure InnerProcessEvents();
    begin
      ProcessEvents();
      if (KeyDown(vk_LSUPER) or KeyDown(vk_LCTRL)) and KeyTyped(vk_p) then
      begin
        isPaused := not isPaused;
      end;
      if WindowCloseRequested() or KeyDown(vk_Escape) then isSkip := true;
    end;
  begin
    
    isPaused := false;
    isSkip := false;
    
    {$IFDEF TRACE}
      TraceEnter('sgResources', 'ShowLogos');
    {$ENDIF}
    try
      try
        oldW := ScreenWidth();
        oldH := ScreenHeight();
        if (oldW <> 800) or (oldH <> 600) then ChangeScreenSize(800, 600);
        ToggleWindowBorder();
        
        LoadResourceBundle('splash.txt', False);
        
        ClearScreen();
        DrawBitmap(FetchBitmap('Swinburne'), 286, 171);
        f := FetchFont('ArialLarge');
        txt := 'SwinGame API by Swinburne University of Technology';
        DrawText(txt, ColorWhite, f, (ScreenWidth() - TextWidth(f, txt)) div 2, 500);
        f := FetchFont('LoadingFont');
        DrawText(DLL_VERSION, ColorWhite, f, 5, 580);
        
        i := 1;
        while isPaused or (i < 60) do
        begin
          i += 1;
          InnerProcessEvents();
          RefreshScreen(60);
          if isSkip then break;
        end;
    
        if AudioOpen then PlaySoundEffect(FetchSoundEffect('SwinGameStart'));
        for i:= 0 to ANI_CELL_COUNT - 1 do
        begin
          DrawBitmap(FetchBitmap('SplashBack'), 0, 0);
          DrawBitmapPart(FetchBitmap('SwinGameAni'), 
            (i div ANI_V_CELL_COUNT) * ANI_W, (i mod ANI_V_CELL_COUNT) * ANI_H,
            ANI_W, ANI_H - 1, ANI_X, ANI_Y);
          RefreshScreen();
          InnerProcessEvents();
          if isSkip then break;
          Delay(15);
        end;
        
        while SoundEffectPlaying(FetchSoundEffect('SwinGameStart')) or isPaused do
        begin
          InnerProcessEvents();
          if isSkip then break;
        end;
        
      except on e:Exception do
        {$IFDEF TRACE}
        begin
          Trace('sgResources', 'Error', 'ShowLogos', 'Error loading and drawing splash.');
          Trace('sgResources', 'Error', 'ShowLogos', e.Message);
        end;
        {$ENDIF}
      end;
    finally
      try
        ReleaseResourceBundle('splash.txt');
      except on e1: Exception do
        {$IFDEF TRACE}
        begin
          Trace('sgResources', 'Error', 'ShowLogos', 'Error freeing splash.');
          Trace('sgResources', 'Error', 'ShowLogos', e1.Message);
         end;
        {$ENDIF}
      end;
      ToggleWindowBorder();
      
      if (oldW <> 800) or (oldH <> 600) then ChangeScreenSize(oldW, oldH);
    end;
    
    {$IFDEF TRACE}
      TraceExit('sgResources', 'ShowLogos');
    {$ENDIF}
  end;
  


//=============================================================================

  initialization
  begin
    {$IFDEF TRACE}
      TraceEnter('sgResources', 'initialization');
    {$ENDIF}
    
    InitialiseSwinGame();
    
    _TileMaps := TStringHash.Create(False, 1024);
    _Bundles := TStringHash.Create(False, 1024);
    
    try
        if ParamCount() >= 0 then SetAppPath(ParamStr(0), True)
    except
    end;
    
    {$IFDEF TRACE}
      TraceExit('sgResources', 'initialization');
    {$ENDIF}
  end;

end.