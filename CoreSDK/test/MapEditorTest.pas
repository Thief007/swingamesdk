program MapEditorTest;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}
uses
  sgTypes, sgCore, sgAudio, sgText, sgGraphics, sgTrace, sgResources,
  sgCamera, sgGeometry, sgImages, sgInput, sgPhysics,
  sgSprites, sgTimers, SysUtils, StrUtils, Classes,
    stringhash, MyStrUtils, sgNamedIndexCollection, sgShared;
type
  Direction = (N, NE, E, SE, S, SW, W, NW);
  BitmapCell = Record
    Cell:         LongInt;
    Bmp:          Bitmap;
    end;
  Tile = ^TileData;
  TileData = Record
    TileID:           LongInt;                      // The tile's unique id
    //Kind:           LongInt;                      // The "kind" of the tile - links to KindNames etc.
    Position:         Point2D;                      // Position of the top right corner of the Tile
    //Center:         Point2D;                      // position of the center of the Tile
    TileBitmapCells:  array of BitmapCell;          // Bitmap of the Tile.
    TileShape:        Shape;                        //shape of tile
    //Values:     Array of Single;                  // Array of single values.
    //SurroundingTiles: Array[Direction] of Tile;   // The adjcent tiles, can be nil if none.
  end;
  Map = ^MapData;
  MapData = Record
    Tiles:        Array of Array of TileData;     // The actual tiles -> in col,row order
    SelectedTiles:Array of LongInt;               // id of selected tiles
    Isometric:    Boolean;
    MapPrototype: ShapePrototype;
    mapHighlightcolor: Color;
    //KindOfTiles:  Array of Array of Tile;       // The tiles indexed by kind -> kind, # order
    MapWidth:     LongInt;                        // The Map width (# of grid in x axis)
    MapHeight:    LongInt;                        // The Map height (# of grid in Y axis)
    MapLayer:     LongInt;                        // The Number of layers within the map
    TileWidth:    LongInt;                        // The Tile width
    TileHeight:   LongInt;                        // The Tile height
    TileStaggerX:  LongInt;                        // Offset of the tile's X Position
    TileStaggerY:  LongInt;                        // Offset of the tile's Y Position
  end;
// load map func *********
function LoadMap(filename:string): Map;
var
  path:         String;                         // path of the map file
  textFile:     Text;                           // text file that is loaded
  id:           String;                         // id for the line processor to identify how to handle the data
  data,line:    String;                         // line is read then seperated into id and data.
  lineno:       LongInt;                        // line number for exceptions
  bitmapCellArray:  Array of BitmapCell;        // had to make a list of bitmap since i can't load 2 bitmaps with the same image.
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
      RaiseException('Error at line ' + IntToStr(lineNo) + ' in map ' + filename + '. Value is not an LongInt : ' + str);
    end
    else if result < 0 then
    begin
      result := 0;
      RaiseException('Error at line ' + IntToStr(lineNo) + ' in map ' + filename + '. Values should be positive : ' + str);
    end;
  end;
// use data processed to set tile properties.
  Procedure SetTiles();
  var
    j,k,id : LongInt;
  begin
    id:=0;
    SetLength(result^.Tiles, result^.MapHeight, result^.MapWidth); //set lengths of tiles
    for j:=low(result^.Tiles) to high (result^.Tiles) do
      for k:=low(result^.Tiles[j]) to high(result^.Tiles[j]) do
      begin
        SetLength(result^.Tiles[j,k].TileBitmapCells, result^.MapLayer); // set length of bitmapcells per tile.
        result^.Tiles[j,k].TileID:=id;
        id+=1;
      end;
  end;
  // ADD tile procedure*************
  //
  // Reads the ti: data
  // Reading the regions from the file
  procedure AddTile();
  var
    bitmapIdxs: Array of LongInt;
    layer, i,rowNum:LongInt;
  begin
    RowNum:= 0;
    if Length(result^.Tiles) = 0 then SetTiles();
    layer:=MyStrToInt(data, false);
    while RowNum < result^.MapHeight do
    begin
      ReadLn(textFile, data);
      lineNo += 1;
      bitmapIdxs := ProcessRange(data);
      if Length(bitmapIdxs)<>result^.MapWidth then
      RaiseException('Error at line ' + IntToStr(lineNo) + ' in map ' + filename + '. Length of bitmapIdxs is ' + IntToStr(Length(bitmapIdxs)) + ' but should be ' + IntToStr(result^.MapWidth));
      for i:=low(bitmapIdxs) to high(bitmapIdxs) do
      begin
        if bitmapIdxs[i] <> -1 then // used -1 as no bitmap.
        begin
          result^.Tiles[rowNum, i].TileBitmapCells[layer].Bmp  := bitmapCellArray[bitmapIdxs[i]].Bmp;
          result^.Tiles[rowNum, i].TileBitmapCells[layer].Cell := bitmapCellArray[bitmapIdxs[i]].Cell;
        end;
      end;
      RowNum += 1;
    end;
  end;
  //procedure to add bitmap to bitmapcell array gives cell value of 0
  procedure AddBitmap();
  begin
    SetLength(bitmapCellArray, length(bitmapCellArray)+1);
    bitmapCellArray[high(bitmapCellArray)].Bmp := LoadBitmap(data);
    bitmapCellArray[high(bitmapCellArray)].Cell:= 0; 
  end;

  //sets up positions of each tile
  procedure ProcessTile();
  var
  i,j: LongInt;
  begin
    for i:=low(result^.Tiles) to high(result^.Tiles) do
      for j:=low(result^.Tiles[i]) to high(result^.Tiles[i]) do
        begin
          if ((i mod 2) =1) then
          begin
            result^.Tiles[i,j].Position.X:=j*result^.TileWidth+result^.TileStaggerX;
            result^.Tiles[i,j].Position.Y:=i*result^.TileHeight+(i*result^.TileStaggerY);
            result^.Tiles[i,j].TileShape:=ShapeAtPoint(result^.MapPrototype, result^.Tiles[i,j].Position);
            ShapeSetColor(result^.Tiles[i,j].TileShape, result^.mapHighlightcolor);
          end
          else
          begin
            result^.Tiles[i,j].Position.X:=j*result^.TileWidth;
            result^.Tiles[i,j].Position.Y:=i*result^.TileHeight+(i*result^.TileStaggerY);
            result^.Tiles[i,j].TileShape:=ShapeAtPoint(result^.MapPrototype, result^.Tiles[i,j].Position);
            ShapeSetColor(result^.Tiles[i,j].TileShape, result^.mapHighlightcolor);
          end
        end;
  end;
  // Reads id and data for
  // mw = map width
  // mh = map height
  // ml = map layers
  procedure ProcessMapLineData();
  begin
    case LowerCase(id)[2] of
      'w': result^.MapWidth  := MyStrToInt(data, false); //map width
      'h': if length(id)=2 then
        begin
          result^.MapHeight := MyStrToInt(data, false); //  map height
        end
        else
        begin
          case LowerCase(id)[3] of
            'c': result^.mapHighlightcolor:= RGBAColor(
              StrToInt(ExtractDelimited(1, data, [','])),
              StrToInt(ExtractDelimited(2, data, [','])),
              StrToInt(ExtractDelimited(3, data, [','])),
              StrToInt(ExtractDelimited(4, data, [','])));
          end;
        end;
      'l': result^.MapLayer  := MyStrToInt(data, false); // number of layers
      'i': if( MyStrToInt(data, false) = 0) then
            begin result^.Isometric := false; end
          else if (MyStrToInt(data, false) = 1) then result^.Isometric := true;
      else
      begin
        RaiseException('Error at line' + IntToStr(lineNo) + 'in map' + filename + '. error with id: ' + id + '. Id not recognized.');
        exit;
      end;
    end;
  end;
  // processes lines regarding tiles
  // tw = tile width
  // th = tile height
  // ti = tile info
  // tb = tile bitmap
  // to = tile offset
  procedure ProcessTileLineData();
  begin
    case LowerCase(id)[2] of
      'w': result^.TileWidth  := MyStrToInt(data, false);        //  tile width
      'h': result^.TileHeight := MyStrToInt(data, false);      // tile height
      'i': AddTile();
      'b': AddBitmap();
      'o':
        begin
          result^.TileStaggerX := StrToInt(ExtractDelimited(1, data, [',']));
          result^.TileStaggerY := StrToInt(ExtractDelimited(2, data, [',']));
        end;
    end;
  end;
  //processes lines regarding grid bitmap.
  // gb = grid bitmap
  procedure ProcessGridLineData();
  var
  bitmapCellIds : array of LongInt;
  cellRegions   : array of LongInt;
  gridBitmap    : Bitmap;
  i,
  cellWidth,
  cellHeight,
  cellRows,
  cellCols,
  cellCount     :LongInt;
  begin
    bitmapCellIds   := ProcessRange(ExtractDelimitedWithRanges(1,data));
    cellRegions     := ProcessRange(ExtractDelimitedWithRanges(2,data));
    gridBitmap      := LoadBitmap(ExtractDelimitedWithRanges(3,data));
    cellWidth       := MyStrToInt(ExtractDelimitedWithRanges(4,data),false);
    cellHeight      := MyStrToInt(ExtractDelimitedWithRanges(5,data),false);
    cellRows        := MyStrToInt(ExtractDelimitedWithRanges(6,data),false);
    cellCols        := MyStrToInt(ExtractDelimitedWithRanges(7,data),false);
    cellCount       := MyStrToInt(ExtractDelimitedWithRanges(8,data),false);
    SetBitmapCellDetails(gridBitmap, cellWidth, cellHeight, cellRows, cellCols, cellCount);
    for i:=low(bitmapCellIds) to high(bitmapCellIds) do
    begin
      SetLength(bitmapCellArray, length(bitmapCellArray)+1);
      bitmapCellArray[high(bitmapCellArray)].Cell := cellRegions[i];
      bitmapCellArray[high(bitmapCellArray)].Bmp  := gridBitmap;
    end;
  end;
  //Process line procedure*************
  procedure ProcessLine();
  begin
    // Split line into id and data
    id   := ExtractDelimited(1, line, [':']);
    data := ExtractDelimited(2, line, [':']);
    // Verify that id is two chars
    if ((Length(id) <> 2) AND (Length(id) <> 3)) then
    begin
      RaiseException('Error at line ' + IntToStr(lineNo) + ' in map ' + filename + '. Error with id: ' + id + '. This id should contain 2 or 3 characters.');
      exit;
    end;
    // Process based on id
    case LowerCase(id)[1] of // in all cases the data variable is read
      'm': ProcessMapLineData();
      't': ProcessTileLineData();
      'g': ProcessGridLineData();
      else
      begin
        RaiseException('Error at line ' + IntToStr(lineNo) + ' in map ' + filename + '. Error with id: ' + id + '. id not recognized.');
        exit;
      end;
    end;
  end;
  procedure AddMapPrototype();
  var
  pts:Point2DArray;
  begin
    SetLength(Pts, 5);
    pts[0].X:=(0+result^.TileStaggerX);
    pts[0].Y:=(0+result^.TileStaggerY);
    pts[1].X:=(0+result^.TileStaggerX+result^.TileWidth);
    pts[1].Y:=(0+result^.TileStaggerY);
    pts[2].X:=(0+result^.TileStaggerX+result^.TileWidth);
    pts[2].Y:=(0+result^.TileStaggerY+result^.TileHeight);
    pts[3].Y:=(0+result^.TileStaggerY+result^.TileHeight);
    pts[3].X:=(0+result^.TileStaggerX);
    pts[4].X:=(0+result^.TileStaggerX);
    pts[4].Y:=(0+result^.TileStaggerY);
    result^.MapPrototype:=PrototypeFrom(pts, pkTriangleStrip);
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
    line   := Trim(line);
    if Length(line) = 0 then continue;  //skip empty lines
    if MidStr(line,1,2) = '//' then continue; //skip lines starting with //
    ProcessLine();
  end;
  
  AddMapPrototype();
writeln('addedprototype');
  ProcessTile();
writeln('processedTile')  ;
end;


Procedure RangeOfRect(r:Rectangle; mp:Map; out startX, startY, endX, endY:LongInt  );
begin
  // Calculate the tiles that are on the screen - only draw these
  startY:= Trunc(r.Y/mp^.TileHeight);
  startX:= Trunc(r.X/mp^.TileWidth);
  endY:= Trunc(startY + (r.Height/mp^.TileHeight))+1;
  endX:= Trunc(startX + (r.Width/mp^.TileWidth))+1;

  // Adjust the end and start to be in range of the array
  if endY >= (mp^.MapHeight) then endY := mp^.Mapheight-1;
  if endX >= (mp^.MapWidth) then endX := mp^.MapWidth-1;
  if startY < 0 then startY :=0;
  if startX < 0 then startX :=0;
end;


//draw map procedure.
procedure DrawMap(mp: Map);
var
  h,j, i, startRow, startCol, endRow, endCol : LongInt;
  screenRect : Rectangle;
begin
  screenRect.X:=CameraX();
  screenRect.Y:=CameraY();
  screenRect.Width:=ScreenWidth();
  screenRect.Height:=ScreenHeight();
  //DrawRectangle(colorWhite,screenRect);
  RangeOfRect(screenRect,mp, startCol, startRow, endCol, endRow);
  for h:= 0 to mp^.MapLayer - 1 do
  begin
    for i:=startRow to endRow do
    begin
      for j:=startCol to endCol do
      begin
        if mp^.Tiles[i,j].TileBitmapCells[h].Bmp <> nil then
        begin
            DrawCell(mp^.Tiles[i,j].TileBitmapCells[h].Bmp, mp^.Tiles[i,j].TileBitmapCells[h].Cell, mp^.Tiles[i,j].Position);
        end;
      end; // end of col in row
    end; // end of row
  end; // end of layers
end;

//checks if tile is within selectedtile array.
function TileSelected(map:Map; tile:Tile):Boolean;
var
k:LongInt;
begin
  for k:=low(map^.SelectedTiles) to high(map^.SelectedTiles) do
    begin
      if map^.SelectedTiles[k] = tile^.TileID then
      begin
        result:=True;
        exit;
      end
    end;
  result :=False;
end;

//updates camera position based on user input
procedure UpdateCamera();
begin
  if KeyDown(VK_LEFT) then
    MoveCameraBy(-2,0)
  else if KeyDown(VK_RIGHT) then
    MoveCameraBy(2,0)
  else if KeyDown(VK_UP) then
    MoveCameraBy(0,-2)
  else if KeyDown(VK_DOWN) then
    MoveCameraBy(0,2)
end;

//procedure to deselect a tile. (includes removing from array and reducing array length)
procedure Deselect(map:Map; tile:Tile);
var
l,m : LongInt;
begin
  for l:=low(map^.SelectedTiles) to high(map^.SelectedTiles) do
    if (map^.SelectedTiles[l] = tile^.TileID) then
      begin
        for m:=l to high(map^.SelectedTiles) do
          begin
            map^.Selectedtiles[m]:= map^.SelectedTiles[m+1];
          end;
        SetLength(map^.SelectedTiles, length(map^.SelectedTiles)-1);
      end;
end;
//procedure to highlight one tile.
procedure HighlightTile(highlightedTile: Tile; map:Map);
begin
  DrawShape(highlightedTile^.TileShape);
end;
//updates whether a tile is highlighted.
procedure UpdateHighlight(map:Map);
var
i,j:LongInt;
begin
  for i:=low(map^.Tiles) to high(map^.Tiles) do
    for j:=low(map^.Tiles[i]) to high(map^.Tiles[i]) do
      begin
        if TileSelected(map, @map^.Tiles[i,j]) then
          HighlightTile(@map^.Tiles[i,j],map);
      end;
end;
//updates whether a tile should be selected or deselected.
procedure UpdateSelect(map:Map);
var
i,j:LongInt;
begin
  for i:=low(map^.Tiles) to high(map^.Tiles) do
    for j:=low(map^.Tiles[i]) to high(map^.Tiles[i]) do
      if ((map^.TileStaggerX + map^.TileStaggerY) = 0) then
        begin
          if PointInShape(ToWorld(MousePosition()), map^.Tiles[i,j].TileShape) then
            begin
              if NOT TileSelected(map, @map^.Tiles[i,j]) then
              begin
                SetLength(map^.SelectedTiles, Length(map^.SelectedTiles)+1);
                map^.SelectedTiles[high(map^.SelectedTiles)]:=map^.Tiles[i,j].TileID;
              end
              else if TileSelected(map, @map^.Tiles[i,j]) then Deselect(map, @map^.Tiles[i,j]);
            end
        end
end;

//updates all event driven actions.
procedure UpdateActions(map:Map);
begin
  UpdateCamera();
  
  if MouseClicked(LeftButton) then
    UpdateSelect(map);
end;

//Main procedure 
procedure Main();
var
  myMap: Map;
begin
  OpenAudio();
  OpenGraphicsWindow('Maps Tests', 640, 480);
  writeln('Map Loading');
  myMap := LoadMap('test1.txt');
  writeln('map loaded');
  repeat // The game loop...
    ProcessEvents();
    UpdateActions(myMap);
    ClearScreen(ColorBlack);
    DrawMap(myMap);
    UpdateHighlight(myMap);
    DrawFramerate(0,0);
    RefreshScreen();
  until WindowCloseRequested();
  ReleaseAllResources();
  CloseAudio();
end;
begin
  Main();
end.
