program MapEditorTest;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}
uses
  sgTypes, sgCore, sgAudio, sgText, sgGraphics, sgTrace, sgResources,
  sgCamera, sgGeometry, sgImages, sgInput, sgPhysics, 
  sgSprites, sgTimers, SysUtils, StrUtils, Classes, 
    stringhash, MyStrUtils, sgNamedIndexCollection, sgShared;
type
  Direction = (N, NE, E, SE, S, SW, W, NW);
  
  Tile = ^TileData;
  TileData = Record
    //TileID:     LongInt;    // The tile's unique id
    //Kind:       LongInt;    // The "kind" of the tile - links to KindNames etc.
    Position:   Point2D;    //Position of the top right corner of the Tile
    //Center:     Point2D;    //position of the center of the Tile
    TileBitmap: Bitmap;     // Bitmap of the Tile.
    //Values:     Array of Single;    // Array of single values.
    //SurroundingTiles: Array[Direction] of Tile;   // The adjcent tiles, can be nil if none.
  end;

  Map = ^MapData;
  MapData = Record
    Tiles:        Array of Array of TileData;         // The actual tiles -> in col,row order
    //KindOfTiles:  Array of Array of Tile;         // The tiles indexed by kind -> kind, # order
    MapWidth:     LongInt;                        // The Map width (# of grid in x axis)
    MapHeight:    LongInt;                        //The Map height (# of grid in Y axis)
    TileWidth:    LongInt;                        // The Tile width
    TileHeight:   LongInt;                        // The Tile height
    
  end;

// load map func *********
function LoadMap(filename:string): Map;
var
  path:         String;           // path of the map file
  textFile:     Text;             // text file that is loaded
  id:           String;           // id for the line processor to identify how to handle the data
  data,line:    String;           // line is read then seperated into id and data.
  lineno:       Integer;          // line number for exceptions
  bitmapArray:  Array of Bitmap;  // had to make a list of bitmap since i can't load 2 bitmaps with the same image.
  
  //bgLayerData:  Array of Array of Integer;  // ti: data read from the file
  
  //str to int func **********
  function MyStrToInt(str: String; allowEmpty: Boolean) : LongInt;
  begin
    if allowEmpty and (Length(str) = 0) then
    begin
      result := -1;
    end
    else if not TryStrToInt(str, result) then
    begin
      result := 0;
      RaiseException('Error at line ' + IntToStr(lineNo) + ' in map ' + filename + '. Value is not an integer : ' + str);
    end
    else if result < 0 then
    begin
      result := 0;
      RaiseException('Error at line ' + IntToStr(lineNo) + ' in map ' + filename + '. Values should be positive : ' + str);
    end;
  end;
  
  // ADD tile procedure*************
  // 
  // Reads the ti: data
  // Reading the regions from the file
  procedure AddTile();
  var
    bitmapIdxs: Array of LongInt;
    RowNum: Integer;
    i:integer;
  begin
    RowNum:= 0;
    
    if Length(result^.Tiles) = 0 then 
      SetLength(result^.Tiles, result^.MapHeight, result^.MapWidth); //set lengths of tiles array
    
    while RowNum < result^.MapHeight do
    begin      
      ReadLn(textFile, data);
      lineNo := lineNo + 1;
      bitmapIdxs := ProcessRange(data);
      if Length(bitmapIdxs)<>result^.MapWidth then
      RaiseException('Error at line ' + IntToStr(lineNo) + ' in map ' + filename + '. Length of bitmapIdxs is ' + IntToStr(Length(bitmapIdxs)) + ' but should be ' + IntToStr(result^.MapWidth));
      
      for i:=low(bitmapIdxs) to high(bitmapIdxs) do
      begin
        if bitmapIdxs[i] <> -1 then // used -1 as no bitmap.
        begin
          result^.Tiles[RowNum, i].TileBitmap := bitmapArray[bitmapIdxs[i]];
        end;
      end;
      
      RowNum :=RowNum +1;
    end;
  end;
  
  procedure AddBitmap();
  begin
    SetLength(bitmapArray, length(bitmapArray)+1);
    bitmapArray[high(bitmapArray)] := LoadBitmap(data);
  end;
  
  procedure ProcessTile();
  var
  i,j: integer;
  begin
  for i:=low(result^.Tiles) to high(result^.Tiles) do
    for j:=low(result^.Tiles[i]) to high(result^.Tiles[i]) do
      begin
        result^.Tiles[i,j].Position.X:=j*result^.TileWidth;
        result^.Tiles[i,j].Position.Y:=i*result^.TileHeight;
      end;
  end;
  
  // Reads id and data for 
  // mw = map width
  // mh = map height
  procedure ProcessMapLineData();
  begin
    case LowerCase(id)[2] of
      'w': result^.MapWidth := MyStrToInt(data, false); //map width
      'h': result^.MapHeight := MyStrToInt(data, false); //  map height
      else
      begin
        RaiseException('Error at line' + IntToStr(lineNo) + 'in map' + filename + '. error with id: ' + id + '. Id not recognized.');
        exit;
      end;
    end;
  end;
  
  procedure ProcessTileLineData();
  begin
    case LowerCase(id)[2] of
      'w': result^.TileWidth := MyStrToInt(data, false);        //  tile width
      'h': result^.TileHeight := MyStrToInt(data, false);      // tile height
      'i': AddTile(); 
      'b': AddBitmap();
    end;
  end;
  //Process line procedure*************
  procedure ProcessLine();
  begin
    // Split line into id and data
    id := ExtractDelimited(1, line, [':']);
    data := ExtractDelimited(2, line, [':']);
    
    // Verify that id is two chars
    if Length(id) <> 2 then
    begin
      RaiseException('Error at line ' + IntToStr(lineNo) + ' in map ' + filename + '. Error with id: ' + id + '. This id should contain 2 characters.');
      exit;
    end;
      
    // Process based on id
    case LowerCase(id)[1] of // in all cases the data variable is read
      'm': ProcessMapLineData();
      't': ProcessTileLineData();
      else
      begin
        RaiseException('Error at line ' + IntToStr(lineNo) + ' in map ' + filename + '. Error with id: ' + id + '. id not recognized.');
        exit;
      end;
    end;
  end;  
begin
  //load map starts here
  path:= FilenameToResource(filename, MapResource);
  Assign(textFile, Path);
  lineNo:=0;

  New(result);
  SetLength(result^.Tiles, 0);

  //moves cursor to 1,1
  Reset(textFile);
  ReadLn(textFile,line);
  lineNo := lineNo + 1;
  if line <> 'Map Loader #v0.1' then 
  begin
    RaiseException('Error: Map File Corrupted ' + filename);
    exit;
  end;
  
  while not EOF(textFile) do
  begin
    ReadLn(textFile, line);
    lineNo := lineNo + 1;
    line := Trim(line);
    if Length(line) = 0 then continue;  //skip empty lines
    if MidStr(line,1,2) = '//' then continue; //skip lines starting with //
    ProcessLine();
  end;
  
  ProcessTile();  
end;

//tiles
  



Procedure DrawMap(mp:Map);
var
  j, i, startRow, startCol, endRow, endCol : integer; 
begin
  startRow:= Trunc(CameraY()/mp^.TileHeight);
  startCol:= Trunc(CameraX()/Mp^.TileWidth);
  endRow:= Trunc(startRow + (ScreenHeight/mp^.TileHeight))+1;
  endCol:= Trunc(startCol + (ScreenWidth/mp^.TileWidth))+1;
  

  
  if endRow >= (mp^.MapHeight) then endRow := mp^.Mapheight-1;
  if endCol >= (mp^.MapWidth) then endCol := mp^.MapWidth-1;
  if startRow < 0 then startRow :=0;
  if startCol < 0 then startCol :=0;
  W
    writeln(startRow);
  writeln(startcol);
  Writeln(endRow);
  Writeln(endCol);
  
  for i:=startRow to endRow-1 do
    for j:=startCol to endCol do
      if mp^.Tiles[i,j].TileBitmap <> nil then
      begin
          Writeln(i, j);
          DrawBitmap(mp^.Tiles[i,j].TileBitmap, mp^.Tiles[i,j].Position);
      end
end;

procedure UpdateCamera();
begin
  if KeyDown(VK_LEFT) then
    MoveCameraBy(-1,0)
  else if KeyDown(VK_RIGHT) then
    MoveCameraBy(1,0)
  else if KeyDown(VK_UP) then
    MoveCameraBy(0,-1)
  else if KeyDown(VK_DOWN) then
    MoveCameraBy(0,1)   
end;
procedure Main();
var
  myMap: Map;
begin
  OpenAudio();
  OpenGraphicsWindow('Maps Tests', 640, 480);
  myMap := LoadMap('test1.txt');
  repeat // The game loop...
    ProcessEvents();
    UpdateCamera();
    //DrawBitmap(BitmapNamed('SplashBack'), 0, 0);
    ClearScreen(ColorBlack);
      
    DrawMap(myMap);
    DrawFramerate(0,0);
    
    RefreshScreen();
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  CloseAudio();

  
  
end;

begin
  Main();
end.
