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
    end;

    {Marker = record
      position:     Point2D;
      Id:           LongInt;
    end;}

    
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
      SelectedTiles:    LongIntArray;                   // id of selected tiles
      Isometric:        Boolean;                        // Map Isometric
      valueIds:         NamedIndexCollection;           // has names of values
      kindids:          NamedIndexCollection;           // has names of kinds
      MarkerIds:        NamedIndexCollection;           // has names of Markers
      //MapMarkers:       Array of Marker;
      MapPrototype:     ShapePrototype;                 // prototype of the tiles
      MapHighlightcolor:Color;                          // highlight color
      MapWidth:         LongInt;                        // The Map width (# of grid in x axis)
      MapHeight:        LongInt;                        // The Map height (# of grid in Y axis)
      MapLayer:         LongInt;                        // The Number of layers within the map
      TileWidth:        LongInt;                        // The Tile width
      TileHeight:       LongInt;                        // The Tile height
      TileStagger:      Vector;                         // Offset of the tile's X Position
      MapDefaultValues: Array of Array of Single;       //default values of tiles depending on bitmaps.
    end;





  function LoadMap(filename:string): Map;
  procedure DrawMap(map: Map); overload;
  function TileSelected(map:Map; tile:Tile):Boolean;
  procedure UpdateSelect(map:Map);
  procedure Deselect(map:Map; tile:Tile);
  procedure HighlightTile(highlightedTile: Tile; map:Map);
  procedure AllocateValue(m:Map; tile:Tile; name:String; val:Single);

//==================================//
//collision functions and procedures//
//==================================//
  
  function  GetPotentialCollisions(map: Map; s: Sprite): Rectangle;
  function SpriteHasCollidedWithTile(map: Map; k: LongInt; s: Sprite; out collidedX, collidedY: LongInt): Boolean; overload;
  procedure MoveOut(m:map; s: Sprite; x, y: LongInt);

  
//===============================//
//return map properties functions//
//===============================//

  function TileStagger(m: Map) : Vector;
  function LayerCount(m: Map) : LongInt;  
  function TileHeight(m: Map) : LongInt;  
  function TileWidth(m: Map) : LongInt;
  function MapHighlightcolor(m: Map) : color;
  function CountSelectedTiles(m: Map) : LongInt;
  function MapPrototype(m: Map) : ShapePrototype;
  function MapHeight(m: Map) : Longint;
  function MapWidth(m: Map) : Longint;
  function ValueName(m : map; idx:LongInt): String;
  function KindName(m : map; idx:LongInt): String;

  
//================================//
//return tile properties functions//
//================================//

  function TileId(t: Tile): LongInt;
  function TileNeighbour(t: tile; d:Direction): Tile;
  function TileShape(t: tile): Shape;
  function TileCenter(t: tile): Point2D;
  function TilePosition(t: tile): Point2D;
  function TileValueName(m: Map; id: LongInt): string;
  function TileValueId(m: Map; n: String): LongInt;
  function TileValue(t: Tile; vId: LongInt) : Single;
  function TileKindName(m: Map; t: Tile) : String;
  function TileKind(t: Tile) : LongInt;
  function TileAt(m: Map; id:LongInt) : Tile; Overload;
  function TileAt(m: Map; const pos:Point2D) : Tile; Overload;
  function TileAt(m: Map; row, col: LongInt) : Tile; Overload;

  //======================//
  // Set Tile procedures  //
  //======================//
  procedure SetTileKind(t : Tile; VId : LongInt; value : Single);  
  procedure SetTileKind(t: Tile; kindId:LongInt);
  procedure SetTileBitmap(t: Tile; layer:LongInt; bmp:Bitmap; cell:LongInt);




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
      value:    Single;
    end;


    BitmapCellKind = record
      Cell:         LongInt;
      Bmap:         Bitmap;
      KindIdx:      LongInt;    
    end;


  var
    tempValues:       Array of SpecificValuesLoader;  // contains the row,col, name and value
    path:             String;                         // path of the map file
    textFile:         Text;                           // text file that is loaded
    id:               String;                         // id for the line processor to identify how to handle the data
    data,line:        String;                         // line is read then seperated into id and data.
    lineno:           LongInt;                        // line number for exceptions
    bitmapCellArray:  Array of BitmapCellKind;        // had to make a list of bitmap since i can't load 2 bitmaps with the same image.


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
        if NOT (result^.Isometric) then
        begin
          result^.Tiles[row,col].SurroundingTiles[mdNorthWest]  := TileAt(result, row-1,  col-1);
          result^.Tiles[row,col].SurroundingTiles[mdNorth]      := TileAt(result, row-1,  col);
          result^.Tiles[row,col].SurroundingTiles[mdNorthEast]  := TileAt(result, row-1,  col+1);
          result^.Tiles[row,col].SurroundingTiles[mdWest]       := TileAt(result, row,    col-1);
          result^.Tiles[row,col].SurroundingTiles[mdEast]       := TileAt(result, row,    col+1);
          result^.Tiles[row,col].SurroundingTiles[mdSouthWest]  := TileAt(result, row+1,  col-1);
          result^.Tiles[row,col].SurroundingTiles[mdSouth]      := TileAt(result, row+1,  col);
          result^.Tiles[row,col].SurroundingTiles[mdSouthEast]  := TileAt(result, row+1,  col+1);
        end
        else
        begin
          result^.Tiles[row,col].SurroundingTiles[mdNorthWest]  := TileAt(result, row-1,  col);
          result^.Tiles[row,col].SurroundingTiles[mdNorth]      := TileAt(result, row-2,  col);
          result^.Tiles[row,col].SurroundingTiles[mdNorthEast]  := TileAt(result, row-1,  col+1 -((row mod 2)*2));
          result^.Tiles[row,col].SurroundingTiles[mdWest]       := TileAt(result, row,    col-1);
          result^.Tiles[row,col].SurroundingTiles[mdEast]       := TileAt(result, row,    col+1);
          result^.Tiles[row,col].SurroundingTiles[mdSouthWest]  := TileAt(result, row+1,  col);
          result^.Tiles[row,col].SurroundingTiles[mdSouth]      := TileAt(result, row+2,  col);
          result^.Tiles[row,col].SurroundingTiles[mdSouthEast]  := TileAt(result, row+1,  col+1 -((row mod 2)*2));
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
            result^.Tiles[row,col].Position.X := (col * result^.TileWidth) - ((row mod 2) * result^.TileStagger.X);      // stagger alternate lines
            result^.Tiles[row,col].Position.Y := (row * result^.TileHeight) - ((row + 1) * result^.TileStagger.Y);       // position y value
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
        'i':  if( data = 'true') then
              begin
                result^.Isometric := true;
                result^.TileStagger.X := (result^.TileWidth/2);
                result^.TileStagger.Y := (result^.TileHeight/2);
              end
              else
              begin
                result^.Isometric := false;
                result^.TileStagger.X := 0;
                result^.TileStagger.Y := 0;
              end;
              
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
      pts[0].Y:=(result^.TileStagger.Y);
      pts[1].X:=((-result^.TileStagger.X)+result^.TileWidth);
      pts[1].Y:=0;
      pts[2].X:=result^.TileWidth;
      pts[2].Y:=(result^.TileHeight - result^.TileStagger.Y);
      pts[3].X:=(result^.TileStagger.X);
      pts[3].Y:=(result^.TileHeight);
      pts[4].X:=0;
      pts[4].Y:=(result^.TileStagger.Y);
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
  
  procedure MapRangeFromRect(r:Rectangle; map:Map; out startX, startY, endX, endY:LongInt  );
  begin
    //WriteLn('Getting range for ', RectangleToString(r));
    // Calculate the tiles that are on the screen - only draw these
    
    // Trunc so that 210 / 50 becomes 4 - i.e. the index of the 5th tile... (saves - 1)
    startY := Trunc(RectangleTop(r) / (map^.TileHeight - map^.TileStagger.Y));
    startX := Trunc(RectangleLeft(r) / map^.TileWidth);
    
    // equation is (bottom - stagger) / (height - stagger) = redone with + 1
    endY   := Trunc(RectangleBottom(r) / (map^.TileHeight - map^.TileStagger.Y)) + 1;
    endX   := Trunc((RectangleRight(r) + map^.TileStagger.X) / map^.TileWidth);

    // Adjust the end and start to be in range of the array
    if endY >= (map^.MapHeight) then endY := map^.MapHeight-1;
    if endX >= (map^.MapWidth) then endX := map^.MapWidth-1;
    if startY < 0 then startY := 0;
    if startX < 0 then startX := 0;   
    
    //WriteLn('result ', startX, ':', startY, ' ', endX, ':', endY);
  end;

  procedure PushMapClip(map:Map);
  var
  width, height : LongInt;
  begin
    width := Round(((map^.MapWidth) * map^.TileWidth) - (2 * (map^.TileStagger.X)));
    height := Round(((map^.MapHeight) * map^.TileHeight) - (map^.TileStagger.Y * (map^.MapHeight - 1)) -(2*map^.TileStagger.Y));

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
    if not assigned(highlightedTile) then exit;
    
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
    MapRangeFromRect(CameraScreenRect(), map, startCol, startRow, endCol, endRow);
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
    dir:Direction;
  begin
    PushMapClip(map);
    
    MapRangeFromRect(CameraScreenRect(), map, startCol, startRow, endCol, endRow);

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
              DrawText(IntToStr(map^.Tiles[row,col].kind), map^.mapHighlightcolor, map^.Tiles[row,col].Position.x, map^.Tiles[row,col].Position.y + 10);
              DrawText(FloatToStr(map^.Tiles[row,col].values[0]), map^.mapHighlightcolor, map^.Tiles[row,col].Position.x, map^.Tiles[row,col].Position.y + 20);

              for dir:=low(map^.Tiles[row,col].SurroundingTiles) to high(map^.Tiles[row,col].SurroundingTiles) do
              begin
                if not assigned(map^.Tiles[row,col].SurroundingTiles[dir]) then exit
                else
                DrawLine(map^.mapHighlightcolor, map^.Tiles[row,col].center,map^.Tiles[row,col].SurroundingTiles[dir]^.center);
              end;
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

  procedure AllocateValue(m:Map; tile:Tile; name:String; val:Single);
  var
    idx: LongInt;
  begin  
    if not assigned(m) then exit;
    if not assigned(tile) then exit;
    
    idx := IndexOf(m^.valueIds, name);
    if (idx < 0) OR (idx > High(tile^.values)) then exit;
    
    //WriteLn('AllocateValue: ', name, ' := ', val, ' @idx ', idx, ' ', High(tile^.values));
    tile^.Values[idx] := val;
  end;


    //gets potential collision and makes a rect out of it.
  function GetPotentialCollisions(map: Map; s: Sprite): Rectangle;
  var
    startPoint, endPoint: Rectangle;
    startX, startY, endX, endY: LongInt;
  begin
    with map^ do begin
      startPoint := RectangleFrom(
        round( ((s^.position.x - s^.velocity.x) / TileWidth) - 1) * TileWidth,
        round( ((s^.position.y - s^.velocity.y) / tileheight) - 1) * tileheight,
        (round( SpriteWidth(s) / TileWidth) + 2) * TileWidth,
        (round( SpriteHeight(s) / tileheight) + 2) * tileheight
      );
      endPoint := RectangleFrom(
        round(((s^.position.x + SpriteWidth(s)) / TileWidth) - 1) * TileWidth,
        round(((s^.position.y + SpriteHeight(s)) / tileheight) - 1) * tileheight,
        (round(SpriteWidth(s) / TileWidth) + 2) * TileWidth,
        (round(SpriteHeight(s) / tileheight) + 2) * tileheight
      );
    end; // with

    //Encompassing Rectangle
    if startPoint.x < endPoint.x then
    begin
      startX := round(startPoint.x);
      endX := round(endPoint.x + endPoint.width);
    end
    else
    begin
      startX := round(endPoint.x);
      endX := round(startPoint.x + startPoint.width);
    end;

    if startPoint.y < endPoint.y then
    begin
      startY := round(startPoint.y);
      endY := round(endPoint.y + endPoint.height);
    end
    else
    begin
      startY := round(endPoint.y);
      endY := round(startPoint.y + startPoint.height);
    end;
    
    // -1 of width and height so as not to project into next tiles (200/50 = 4... the 5th tile, we want 199/50 = 3... the 4th tile)
    result := RectangleFrom(startX, startY, endX - startX - 1, endY - startY - 1);
    //drawRectangle(colorpink, RectangleFrom(startX, startY, endX - startX, endY - startY));

    //Debug Info
    //DrawRectangle(ColorYellow, startPoint);
    //DrawRectangle(ColorWhite, endPoint);
    //DrawRectangle(ColorGreen, result);
  end;

// outs the tile X and Y that the sprite has collided with  map and result is a boolean if it did collide.
  function SpriteHasCollidedWithTile(map: Map; k: LongInt; s: Sprite; out collidedX, collidedY: LongInt): Boolean; overload;
  var
    y, x, dy, dx, i, j, initY, initX: LongInt;
    yRange, xRange: LongInt;
    xStart, yStart, xEnd, yEnd: LongInt;
    rectSearch: Rectangle;
    side: CollisionSide;
  begin
    result := false;
    if map = nil then begin RaiseException('No Map supplied (nil)'); exit; end;
    if s = nil then begin RaiseException('No Sprite suppled (nil)'); exit; end;
    
    //makes a rect of bounding area to search so that it doesn't need to search the whole map.
    
    rectSearch := GetPotentialCollisions(map, s);
    
    //makes range out of rect.
    //WriteLn('Creating search rectangle for sprite at ', RectangleToString(SpriteCollisionRectangle(s)));
    //WriteLn('                            search rect ', RectangleToString(rectSearch));
    //WriteLn('                            heading     ', PointToString(SpriteVelocity(s)));
    MapRangeFromRect(rectSearch, map, xStart, yStart, xEnd, yEnd);

    
    //checks which side to use to check for collision
    side := SideForCollisionTest(s^.velocity);
    
    // Use the side to determine search order, allowing the tiles to be 
    // checked in a sensible order based on movement of sprite
    case side of
      TopLeft:      begin dy := 1;  dx := 1;  initY := yStart;  initX := xStart;  end;
      TopRight:     begin dy := 1;  dx := -1; initY := yStart;  initX := xEnd;    end;
      BottomLeft:   begin dy := -1; dx := 1;  initY := yEnd;    initX := xStart;  end;
      BottomRight:  begin dy := -1; dx := -1; initY := yEnd;    initX := xEnd;    end;
      Top:          begin dy := 1;  dx := 1;  initY := yStart;  initX := xStart;  end;
      Bottom:       begin dy := -1; dx := 1;  initY := yEnd;    initX := xStart;  end;
      Left:         begin dy := 1;  dx := 1;  initY := yStart;  initX := xStart;  end;
      Right:        begin dy := 1;  dx := -1; initY := yStart;  initX := xEnd;    end;
      else          begin dy := 1;  dx := 1;  initY := yStart;  initX := xStart;  end;
    end;
    
    // Determine the number of tiles to check in the x and y directions
    yRange := yEnd - yStart;
    xRange := xEnd - xStart;
    
    //writeln ('StartY: ', yStart,  ', endY:', yEnd);
    //writeln ('StartX: ', xStart,  ', endX:', xEnd);
    
    with map^ do 
    begin
      // For i = the tiles within the y range (inclusive of start/end)
      for i := 0 to yRange do
      begin
        y := initY + i * dy;
        
        // for j = the tiles within the x range (inclusive of start/end)
        for j := 0 to xRange do
        begin
          x := initX + j * dx;
          
          // Writeln ('  Checking tile: ', x,  ', ', y);
          // DrawShape(Tiles[y, x].TileShape);
          
          if Tiles[y, x].Kind = k then
          begin
            if SpriteShapeCollision(s, Tiles[y,x].TileShape) then
            begin
              result := true;
              collidedX := x;
              collidedY := y;
              exit;
            end;
          end;
        end;
      end;
    end; // with
    
    collidedX := -1;
    collidedY := -1;
  end;

  procedure MoveOut(m: map; s: Sprite; x, y: LongInt);
  var
    kickVector: Vector;
    sprRect: Rectangle;
    tileshape: shape;
    velocity: Vector;
  begin
    tileshape:= m^.Tiles[y,x].TileShape;
    sprRect := SpriteCollisionRectangle(s);
    velocity:= s^.Velocity;

    //Draw the sprite rectangle
    //DrawRectangle(ColorYellow, sprRect);
    //DrawShape(tileShape);
    //WriteLn(x, ',', y);
    kickVector := VectorOutOfShapeFromRect(tileshape, sprRect, velocity);

    sprRect.x += kickVector.x;
    sprRect.y += kickVector.y;
    
    //DrawRectangle(ColorWhite, sprRect);
    //writeln('v.x: ',kickVector.x, 'v.y: ',kickVector.y);
    MoveSprite(s, kickVector);;
  end;




  //==================================================================
  //FUNCTIONS THAT RETURNS MAP PROPERTIES
  //==================================================================

  function MapWidth(m: Map) : Longint;
  begin
    result := -1;
    if NOT Assigned(m) then exit
    else
    result := m^.MapWidth;
  end;

  
  function MapHeight(m: Map) : Longint;
  begin
    result := -1;
    if NOT Assigned(m) then exit
    else
    result := m^.MapHeight;
  end;

  function MapPrototype(m: Map) : ShapePrototype;
  begin
    result := m^.MapPrototype;
  end;

  function CountSelectedTiles(m: Map) : LongInt;
  begin
    result := -1;
    if NOT assigned(m) then exit
    else
    result:= length(m^.SelectedTiles);
  end;

  function SelectedTiles(m: Map) : LongIntArray;
  begin
    result := nil;
    if NOT Assigned(m) then exit
    else
    result := m^.Selectedtiles;
  end;

  function MapHighlightcolor(m: Map) : color;
  begin
    result := m^.MapHighlightcolor;
  end;

  
  function TileWidth(m: Map) : LongInt;
  begin
    result := -1;
    if NOT Assigned(m) then exit
    else
    result := m^.TileWidth;
  end;
  
  function TileHeight(m: Map) : LongInt;
  begin
    result := -1;
    if NOT Assigned(m) then exit
    else
    result := m^.TileHeight;
  end;

  function LayerCount(m: Map) : LongInt;
  begin
    result := -1;
    if NOT Assigned(m) then exit
    else
    result := m^.MapLayer;
  end;

  function TileStagger(m: Map) : Vector;
  begin
    result := VectorTo(0,0);
    if NOT Assigned(m) then exit
    else
    result := m^.TileStagger;
  end;


  //=========================//
  //TILE RETURN FUNCTIONS    //
  //=========================//

  function TileAt(m: Map; const pos:Point2D) : Tile; Overload;
  begin
    //result:=********************
    result := nil;
  end;

  function TileAt(m: Map; id:LongInt) : Tile; Overload;
  var
    row,col : LongInt;
  begin
    result := nil;
    row := id div m^.MapWidth;
    col := id mod m^.MapWidth;
    if (row >= m^.MapHeight) AND (col >= m^.MapWidth) then exit
    else
    result := TileAt(m, row,col);
  end;

  function TileAt(m: Map; row, col: LongInt): Tile; overload;
  begin    
    if (not assigned(m)) or (row < Low(m^.Tiles)) or (row > High(m^.Tiles)) or (col < Low(m^.Tiles[row])) or (col > High(m^.Tiles[row])) then
      result := nil
    else
      result := @m^.Tiles[row,col];
  end;

  function TileKind(t: Tile) : LongInt;
  begin
    result := -1;
    if NOT Assigned(t) then exit
    else
    result := t^.Kind;
  end;

  function TileKindName(m: Map; t: Tile) : String;
  begin
    result := '';
    if NOT Assigned(t) then exit
    else
      result := NameAt(m^.KindIds, Tilekind(t));
  end;

  function TileValue(t: Tile; vId: LongInt) : Single;
  begin
    result := -1;
    if NOT Assigned(t) then exit
    else
    result := t^.Values[vId];
  end;

  function TileValueId(m: Map; n: String): LongInt;
  begin
    result := -1;
    if NOT Assigned(m) then exit
    else
    result := IndexOf(m^.ValueIds, n);
  end;

  function TileValueName(m: Map; id: LongInt): string;
  begin
    result := '';
    if NOT Assigned(m) then exit
    else
    result := NameAt(m^.ValueIds, id);
  end;

  function TilePosition(t: tile): Point2D;
  begin
    result.X := -1;
    result.Y := -1;
    if not Assigned(t) then exit
    else
    result := t^.Position;
  end;

  function TileCenter(t: tile): Point2D;
  begin
    result.X := -1;
    result.Y := -1;
    if not Assigned(t) then exit
    else
    result := t^.Center;
  end;


  function TileShape(t: tile): Shape;
  begin
    result := nil;
    if NOT assigned(t) then exit
    else
    result := t^.TileShape;
  end;

  function TileNeighbour(t: tile; d:Direction): Tile;
  begin
    result := nil;
    if NOT Assigned(t) then exit
    else
    result := t^.SurroundingTiles[d];
  end;

  function TileId(t: Tile): LongInt;
  begin
    result := -1;
    if NOT Assigned(t) then exit
    else
    result := t^.TileId;
  end;

  function TileBitmap(t: Tile; layer:LongInt): Bitmap;
  begin
    result := nil;
    if not Assigned(t) then exit
    else
    result := t^.TileBitmapCells[layer].BMap;
  end;

  function KindName(m : map; idx:LongInt): String;
  begin
    result := '';
    if not Assigned(m) then exit
    else
    result:= NameAt(m^.KindIds, idx);
  end;
  
  function ValueName(m : map; idx:LongInt): String;
  begin
    result := '';
    if not Assigned(m) then exit
    else
    result:= NameAt(m^.ValueIds, idx);
  end;
  //======================//
  // Set Tile procedures  //
  //======================//
  
  procedure SetTileBitmap(t: Tile; layer:LongInt; bmp:Bitmap; cell:LongInt);
  begin
    t^.TileBitmapCells[layer].BMap := bmp;
    t^.TileBitmapCells[layer].Cell := cell;
  end;
  
  procedure SetTileKind(t: Tile; kindId:LongInt);
  begin
    t^.Kind := KindId;
  end;
  
  procedure SetTileKind(t : Tile; VId : LongInt; value : Single);
  begin
    t^.Values[VId] := value;
  end;

  

  initialization
  begin
    InitialiseSwinGame();
  end;
end.
