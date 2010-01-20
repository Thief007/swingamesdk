unit sgMaps;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}
interface
  uses
    sgTypes;
  type
    Direction = (mdNorthWest, mdNorth, mdNorthEast, mdWest, mdEast, mdSouthWest, mdSouth, mdSouthEast);

    
    BitmapCell = Record
      Cell:         LongInt;
      Bmap:         Bitmap;
      KindIdx:      LongInt;
    end;

    
    Marker = record
      position:     Point2D;
      Id:           LongInt;
    end;
    
    Tile = ^TileData;
    TileData = Record
      TileID:           LongInt;                      // The tile's unique id
      Kind:             LongInt;                      // The "kind" of the tile - links to KindNames etc.
      Position:         Point2D;                      // Position of the top right corner of the Tile
      Center:           Point2D;                      // position of the center of the Tile
      TileBitmapCells:  array of BitmapCell;          // Bitmap of the Tile.
      TileShape:        Shape;                        // shape of tile
      Values:           Array of Single;              // Array of single values.
      SurroundingTiles: Array[Direction] of Tile;     // The adjcent tiles, can be nil if none.
    end;
    
    Map = ^MapData;
    MapData = Record
      Tiles:            Array of Array of TileData;     // The actual tiles -> in col,row order
      SelectedTiles:    Array of LongInt;               // id of selected tiles
      Isometric:        Boolean;
      valueIds:         NamedIndexCollection;           // has names of values
      kindids:          NamedIndexCollection;           // has names of kinds
      MarkerIds:        NamedIndexCollection;           // has names of Markers
      MapMarkers:       Array of Marker;
      MapPrototype:     ShapePrototype;
      MapHighlightcolor:Color;
      MapWidth:         LongInt;                        // The Map width (# of grid in x axis)
      MapHeight:        LongInt;                        // The Map height (# of grid in Y axis)
      MapLayer:         LongInt;                        // The Number of layers within the map
      TileWidth:        LongInt;                        // The Tile width
      TileHeight:       LongInt;                        // The Tile height
      TileStaggerX:     LongInt;                        // Offset of the tile's X Position
      TileStaggerY:     LongInt;                        // Offset of the tile's Y Position
      MapDefaultValues: Array of Array of Single;   //default values of tiles depending on bitmaps.
    end;

  function LoadMap(filename:string): Map;
  procedure DrawMap(map: Map); overload;
  //procedure DrawMap(map: Map; withHighlight: Boolean); overload;
  function TileSelected(map:Map; tile:Tile):Boolean;
  procedure UpdateSelect(map:Map);
  procedure Deselect(map:Map; tile:Tile);
  procedure HighlightTile(highlightedTile: Tile; map:Map);
  procedure AllocateValue(map:map; tile:Tile; name:String; val:Single);

  function TileAt(map: Map; row, col: LongInt): Tile;

  implementation
  uses
    sgCore, sgAudio, sgText, sgGraphics, sgTrace, sgResources,
    sgCamera, sgGeometry, sgImages, sgInput, sgPhysics,
    sgSprites, sgTimers, SysUtils, StrUtils, Classes,
      stringhash, MyStrUtils, sgNamedIndexCollection, sgShared;

  // load map func *********
  function LoadMap(filename:string): Map;
  type
    SpecificValuesLoader = record
      row:      LongInt;
      col:      LongInt;
      name:     string;
      value:     Single;
    end;
  var
    tempValues:   Array of SpecificValuesLoader;
    path:         String;                         // path of the map file
    textFile:     Text;                           // text file that is loaded
    id:           String;                         // id for the line processor to identify how to handle the data
    data,line:    String;                         // line is read then seperated into id and data.
    lineno:       LongInt;                        // line number for exceptions
    bitmapCellArray:  Array of BitmapCell;        // had to make a list of bitmap since i can't load 2 bitmaps with the same image.
  // use data processed to set tile properties.
    Procedure SetTiles();
    var
      row,col,id : LongInt;
    begin
      id:=0;
      SetLength(result^.Tiles, result^.MapHeight, result^.MapWidth); //set lengths of tiles
      for row:=low(result^.Tiles) to high (result^.Tiles) do
        for col:=low(result^.Tiles[row]) to high(result^.Tiles[row]) do
        begin
          SetLength(result^.Tiles[row,col].TileBitmapCells, result^.MapLayer); // set length of bitmapcells per tile.
          result^.Tiles[row,col].TileID:=id;
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
      rowNum:= 0;
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
            result^.Tiles[rowNum, i].TileBitmapCells[layer].Bmap := bitmapCellArray[bitmapIdxs[i]].Bmap;
            result^.Tiles[rowNum, i].TileBitmapCells[layer].Cell := bitmapCellArray[bitmapIdxs[i]].Cell;
            
            if layer = 0 then
            begin
              result^.Tiles[rowNum, i].kind := bitmapCellArray[bitmapIdxs[i]].KindIdx;
            end;
            //result^.Tiles[row,col].Values:= result^.MapDefaultValues
          end
          else if layer = 0 then
           result^.Tiles[rowNum, i].kind := -1;
        end;
        RowNum += 1;
      end;
    end;
    //procedure to add bitmap to bitmapcell array gives cell value of 0
    procedure AddBitmap(data:String);
    var
    index:LongInt;
    begin
      index:=MyStrToInt(ExtractDelimited(1,data,[',']),false);
      if (index+1)> length(bitmapCellArray) then
      begin
      SetLength(bitmapCellArray, index+1);
      bitmapCellArray[index].Bmap := LoadBitmap(ExtractDelimited(2,data,[',']));
      bitmapCellArray[index].Cell:= 0;
      end;
    end;

    //Procedure that adds the surrounding tiles to each tile.
    procedure CreateSurroundingTiles();
    var
      row,col: LongInt;
    begin
      for row:=low(result^.Tiles) to high(result^.Tiles) do
        for col:=low(result^.Tiles[row]) to high(result^.Tiles[row]) do
        begin
          result^.Tiles[row,col].SurroundingTiles[mdNorthWest]  := TileAt(result, row-1,  col-1);
          result^.Tiles[row,col].SurroundingTiles[mdNorth]      := TileAt(result, row-1,  col);
          result^.Tiles[row,col].SurroundingTiles[mdNorthEast]  := TileAt(result, row-1,  col+1);
          result^.Tiles[row,col].SurroundingTiles[mdWest]       := TileAt(result, row,    col-1);
          result^.Tiles[row,col].SurroundingTiles[mdEast]       := TileAt(result, row,    col+1);
          result^.Tiles[row,col].SurroundingTiles[mdSouthWest]  := TileAt(result, row+1,  col-1);
          result^.Tiles[row,col].SurroundingTiles[mdSouth]      := TileAt(result, row+1,  col);
          result^.Tiles[row,col].SurroundingTiles[mdSouthEast]  := TileAt(result, row+1,  col+1);
        end;
    end;    

    procedure AllocateDefaultValues(map:Map; kindIdx:LongInt; var tile: TileData);
    var
      i: Integer;
    begin
      SetLength(tile.values, NameCount(map^.valueIds));
      for i := 0 to High(tile.values) do tile.values[i] := 0;
      
      if (kindIdx < 0) or (kindIdx > High(map^.MapDefaultValues)) then exit;

      //Allocate space for the values
      //SetLength(tile.values, Length(map^.MapDefaultValues[kindIdx]));
      
      for i := 0 to High(tile.values) do
      begin
        if i > High(map^.MapDefaultValues[kindIdx]) then exit;
        tile.values[i] := map^.MapDefaultValues[kindIdx, i];
      end;
    end;

    procedure AllocateSpecificValues();
    var
    i: LongInt;
    begin
    for i := low(tempValues) to high(tempValues) do
    begin
      AllocateValue(result, @result^.tiles[tempValues[i].row, tempValues[i].col], tempValues[i].name, tempValues[i].value);
    end;
      
    end;
    
    //sets up positions of each tile
    procedure ProcessTiles();
    var
      row,col: LongInt;
    begin
      for row:=low(result^.Tiles) to high(result^.Tiles) do
        for col:=low(result^.Tiles[row]) to high(result^.Tiles[row]) do
          begin
            //Allocate position and center given stagger
            result^.Tiles[row,col].Position.X := (col * result^.TileWidth) - ((row mod 2) * result^.TileStaggerX);      // stagger alternate lines
            result^.Tiles[row,col].Position.Y := (row * result^.TileHeight) - ((row + 1) * result^.TileStaggerY);       // position y value
            result^.Tiles[row,col].Center.X := result^.Tiles[row,col].Position.X + (result^.TileWidth/2);
            result^.Tiles[row,col].Center.Y := result^.Tiles[row,col].Position.Y + (result^.TileHeight/2);

            //Tile shape and color
            result^.Tiles[row,col].TileShape  := ShapeAtPoint(result^.MapPrototype, result^.Tiles[row,col].Position);   // Shape of the tile
            ShapeSetColor(result^.Tiles[row,col].TileShape, result^.mapHighlightcolor);

            //kind
            AllocateDefaultValues(result, result^.Tiles[row,col].kind, result^.Tiles[row,col]);
            AllocateSpecificValues();
          end;
      CreateSurroundingTiles();
    end;


    //Add one or many name(s) to an index collection.

    Procedure AddNamesToCollection(var col: NamedIndexCollection; names: String);
    var
      i, namesLength:LongInt;    
    begin
      //number of names = ,'s + 1 (commas + 1)
      namesLength := CountDelimiter(names, ',') + 1;
      //WriteLn('Reading ', namesLength, ' names from ', names);
      
      for i := 1 to namesLength do
      begin
        AddName(col, ExtractDelimited(i,names,[',']));
      end;
    end;

  
    // Adds default values depending on kind
    procedure AddDefaultValue(data:String);
    var
    defaultValues:SingleArray;
    kind,i: LongInt;
    begin
      kind:=MyStrToInt(ExtractDelimited(1,data,[',']),false);
      
      defaultValues:=ProcessFloatRange(ExtractDelimitedWithRanges(2,data));
      //add exception if length of default value <> length of valueIds.names.
      if ((length(result^.MapDefaultValues) = 0) AND (kind+1 > length(result^.MapDefaultValues))) then
       //sets length of default value to kind +1 and number of names of value
          SetLength(result^.MapDefaultValues, kind+1, NameCount(result^.valueIds));
      for i:=low(result^.MapDefaultValues[kind]) to high(result^.MapDefaultValues[kind]) do
      begin
        result^.MapDefaultValues[kind, i] := defaultValues[i];
      end; 
    end;

    //Maps kinds to the bitmap
    procedure MapBitmapToKind(data:string);
    var
    i: LongInt;
    kindIds:LongIntArray;
    begin
      kindIds:=ProcessRange(ExtractDelimitedWithRanges(1,data));
      if length(kindIds) = length(bitmapCellArray) then
        begin
          for i:=low(bitmapCellArray) to high(bitmapCellArray) do
          begin
          bitmapCellArray[i].KindIdx := kindIds[i];
          end;
        end
        else
        RaiseException('The number of kinds passed in does not match the number of bitmaps');
    end;

    procedure LoadSpecificValue(data:String);
    var
    row,col:LongInt;
    val:single;
    name:String;
    begin
      setlength(tempValues, length(tempValues)+1);
      row:=MyStrToInt(ExtractDelimited(1, data, [',']), false);
      col:=MyStrToInt(ExtractDelimited(2, data, [',']), false);
      name:=ExtractDelimited(3, data, [',']);
      writeln(name,row,col);
      val:=StrToFloat(ExtractDelimited(4, data, [',']));
      tempValues[high(tempValues)].row := row;
      tempValues[high(tempValues)].col := col;
      tempValues[high(tempValues)].name := name;
      tempValues[high(tempValues)].value := val;

    end;
    //process lines that deals with value
    procedure ProcessValueLineData();
    begin
      case LowerCase(id)[3] of
        
        // Value Name
        'n':  AddNamesToCollection(result^.valueIds, data);
              
        // Value Default
        'd':  AddDefaultValue(data);
      end;
    end;

    //process lines that deals with kind
    procedure ProcessKindLineData();
    begin
      case LowerCase(id)[3] of
      'n': AddNamesToCollection(result^.kindids, data);
      'i': MapBitmapToKind(data);
      end;
    end;
    
      // Reads id and data for
    // mw = map width
    // mh = map height
    // ml = map layers
    // mhc = map highlight color
    // mvn = map value name
    procedure ProcessMapLineData();
    begin
      case LowerCase(id)[2] of
        //map width
        'w':  result^.MapWidth  := MyStrToInt(data, false);
        
        //  map height
        'h':  if length(id)=2 then
                result^.MapHeight := MyStrToInt(data, false) 
              else if LowerCase(id)[3] = 'c' then result^.mapHighlightcolor:= RGBAColor(
                StrToInt(ExtractDelimited(1, data, [','])),
                StrToInt(ExtractDelimited(2, data, [','])),
                StrToInt(ExtractDelimited(3, data, [','])),
                StrToInt(ExtractDelimited(4, data, [','])));

        // number of layers        
        'l':  result^.MapLayer  := MyStrToInt(data, false);

        //Isometric 
        'i':  if( MyStrToInt(data, false) = 0) then
                result^.Isometric := false
              else if (MyStrToInt(data, false) = 1) then
                result^.Isometric := true;  
        //kind  
        'k':  ProcessKindLineData();      
        //Value
        'v':  ProcessValueLineData();
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
    // tl = tile layer
    // tb = tile bitmap
    // to = tile offset
    procedure ProcessTileLineData();
    begin
      case LowerCase(id)[2] of
        'w': result^.TileWidth  := MyStrToInt(data, false);        //  tile width
        'h': result^.TileHeight := MyStrToInt(data, false);      // tile height
        'l': AddTile();
        'b': AddBitmap(data);
        'o':
          begin
            result^.TileStaggerX := StrToInt(ExtractDelimited(1, data, [',']));
            result^.TileStaggerY := StrToInt(ExtractDelimited(2, data, [',']));
          end;
        'v':LoadSpecificValue(data);
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
        bitmapCellArray[high(bitmapCellArray)].Bmap  := gridBitmap;
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
      pts[0].X:=0;
      pts[0].Y:=(result^.TileStaggerY);
      pts[1].X:=((-result^.TileStaggerX)+result^.TileWidth);
      pts[1].Y:=0;
      pts[2].X:=result^.TileWidth;
      pts[2].Y:=(result^.TileHeight - result^.TileStaggerY);
      pts[3].X:=(result^.TileStaggerX);
      pts[3].Y:=(result^.TileHeight);
      pts[4].X:=0;
      pts[4].Y:=(result^.TileStaggerY);
      result^.MapPrototype:=PrototypeFrom(pts, pkTriangleStrip);
    end;
  begin
    //load map starts here
    path:= FilenameToResource(filename, MapResource);
    Assign(textFile, Path);
    lineNo:=0;
    New(result);
    InitNamedIndexCollection(result^.valueIds);
    InitNamedIndexCollection(result^.kindids);
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
      if Length(line) = 0 then continue;  //skip emapty lines
      if MidStr(line,1,2) = '//' then continue; //skip lines starting with //
      ProcessLine();
    end;
    AddMapPrototype();
    writeln('PrototypeAdded');
    ProcessTiles();
    writeln('TileProcessed')  ;
  end;


  Procedure RangeOfRect(r:Rectangle; map:Map; out startX, startY, endX, endY:LongInt  );
  begin
    // Calculate the tiles that are on the screen - only draw these
    startY := Trunc(r.Y / map^.TileHeight) - 1;
    startX := Trunc(r.X / map^.TileWidth) - 1;
    
    endY   := Trunc(startY + (r.Height/(map^.TileHeight - map^.TileStaggerY))) + 2;
    endX   := Trunc(startX + ((r.Width + map^.TileStaggerX)/map^.TileWidth)) + 3;

    // Adjust the end and start to be in range of the array
    if endY >= (map^.MapHeight) then endY := map^.MapHeight-1;
    if endX >= (map^.MapWidth) then endX := map^.MapWidth-1;
    if startY < 0 then startY := 0;
    if startX < 0 then startX := 0;
  end;

  procedure PushMapClip(map:Map);
  var
  width, height : LongInt;
  begin
    width := ((map^.MapWidth) * map^.TileWidth) - (2 * (map^.TileStaggerX));
    height := ((map^.MapHeight) * map^.TileHeight) - (map^.TileStaggerY * (map^.MapHeight - 1)) -(2*map^.TileStaggerY);

    {$IFDEF DEBUG}
      DrawRectangle(ColorBlue, false, RectangleFrom(0, 0, width ,height));
    {$ENDIF}
    PushClip(RectangleFrom(-CameraX(), -CameraY(), width ,height));
    
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
    DrawShapeAsLineStrip(screen, highlightedTile^.TileShape, false, CameraPos*-1);
  end;
  //updates whether a tile is highlighted.
  procedure UpdateHighlight(map:Map);
  var
  row,col:LongInt;
  begin
    for row:=low(map^.Tiles) to high(map^.Tiles) do
      for col:=low(map^.Tiles[row]) to high(map^.Tiles[row]) do
        begin
          if TileSelected(map, @map^.Tiles[row,col]) then
            HighlightTile(@map^.Tiles[row,col],map);
        end;
  end;
  //updates whether a tile should be selected or deselected.
  procedure UpdateSelect(map:Map);
  var
  row,col,startCol, startRow, endCol, endRow:LongInt;
  begin
    RangeOfRect(CameraScreenRect(), map, startCol, startRow, endCol, endRow);
      for row:=startRow to endRow do
      begin
        for col:=startCol to endCol do
        begin
          if PointInShape(ToWorld(MousePosition()), map^.Tiles[row,col].TileShape) then
            begin
              if NOT TileSelected(map, @map^.Tiles[row,col]) then
              begin
                SetLength(map^.SelectedTiles, Length(map^.SelectedTiles)+1);
                map^.SelectedTiles[high(map^.SelectedTiles)]:=map^.Tiles[row,col].TileID;
              end
              else if TileSelected(map, @map^.Tiles[row,col]) then Deselect(map, @map^.Tiles[row,col]);
            end
        end;
      end;
        
  end;

  //draw map procedure.
  procedure DrawMap(map: Map);
  var
    layer,col, row, startRow, startCol, endRow, endCol : LongInt;
  begin
    PushMapClip(map);
    
    RangeOfRect(CameraScreenRect(), map, startCol, startRow, endCol, endRow);

    // Loop through all the layers + and additional one for debug info
    for layer:= 0 to map^.MapLayer do
    begin
      for row:=startRow to endRow do
      begin
        for col:=startCol to endCol do
        begin
          //Show debug information over last layer
          if (layer = map^.MapLayer) then // and showdebug
          begin
            if TileSelected(map, @map^.Tiles[row,col]) then
            begin
              //Draw debug information
              DrawText(IntToStr(col) + ':' + IntToStr(row), map^.mapHighlightcolor, map^.Tiles[row,col].Position);
              DrawText(IntToStr(map^.Tiles[row,col].SurroundingTiles[mdWest]^.kind), map^.mapHighlightcolor, map^.Tiles[row,col].Position.x, map^.Tiles[row,col].Position.y + 10);
              DrawText(FloatToStr(map^.Tiles[row,col].values[0]), map^.mapHighlightcolor, map^.Tiles[row,col].Position.x, map^.Tiles[row,col].Position.y + 20);
              
              DrawLine(map^.mapHighlightcolor, map^.Tiles[row,col].center,map^.Tiles[row,col].SurroundingTiles[mdWest]^.center);
              DrawLine(map^.mapHighlightcolor, map^.Tiles[row,col].center,map^.Tiles[row,col].SurroundingTiles[mdNorthWest]^.center);
              DrawLine(map^.mapHighlightcolor, map^.Tiles[row,col].center,map^.Tiles[row,col].SurroundingTiles[mdNorth]^.center);
              DrawLine(map^.mapHighlightcolor, map^.Tiles[row,col].center,map^.Tiles[row,col].SurroundingTiles[mdNorthEast]^.center);
              DrawLine(map^.mapHighlightcolor, map^.Tiles[row,col].center,map^.Tiles[row,col].SurroundingTiles[mdEast]^.center);
              DrawLine(map^.mapHighlightcolor, map^.Tiles[row,col].center,map^.Tiles[row,col].SurroundingTiles[mdSouth]^.center);
              DrawLine(map^.mapHighlightcolor, map^.Tiles[row,col].center,map^.Tiles[row,col].SurroundingTiles[mdSouthWest]^.center);
              DrawLine(map^.mapHighlightcolor, map^.Tiles[row,col].center,map^.Tiles[row,col].SurroundingTiles[mdSouthEast]^.center);
            end;
          end
          else if map^.Tiles[row,col].TileBitmapCells[layer].Bmap <> nil then
          begin
              DrawCell(map^.Tiles[row,col].TileBitmapCells[layer].Bmap, map^.Tiles[row,col].TileBitmapCells[layer].Cell, map^.Tiles[row,col].Position);
          end;
        end; // end of col in row
      end; // end of row
    end; // end of layers
    UpdateHighlight(map);
    PopClip();
  end;

  function TileAt(map: Map; row, col: LongInt): Tile;
  begin    
    if (row < Low(map^.Tiles)) or (row > High(map^.Tiles)) or (col < Low(map^.Tiles[row])) or (col > High(map^.Tiles[row])) then
      result := nil
    else
      result := @map^.Tiles[row,col];
  end;
  
  procedure AllocateValue(map:map; tile:Tile; name:String; val:Single);
  var
    idx: LongInt;
  begin  
    if not assigned(map) then exit;
    if not assigned(tile) then exit;
    
    idx := IndexOf(map^.valueIds, name);
    if (idx < 0) OR (idx > High(tile^.values)) then exit;
    
    //WriteLn('AllocateValue: ', name, ' := ', val, ' @idx ', idx, ' ', High(tile^.values));
    tile^.Values[idx] := val;
  end;
  
  initialization
  begin
    InitialiseSwinGame();
  end;

end.
