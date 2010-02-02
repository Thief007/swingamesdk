unit sgMaps;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}
interface
  uses
    sgTypes;
  type
    Direction = (mdNorthWest, mdNorth, mdNorthEast, mdWest, mdEast, mdSouthWest, mdSouth, mdSouthEast);

    
    {BitmapCell = Record
      Cell:         LongInt;
      Bmap:         Bitmap;
    end;}

      type
      BitmapCellKind = record
      Cell:         LongInt;
      Bmap:         Bitmap;
      KindIdx:      LongInt;    
    end;

    {Marker = record
      position:     Point2D;
      Id:           LongInt;
    end;}

    BitmapCellKindPtr = ^BitmapCellKind;
    Tile = ^TileData;
    TileData = Record
      TileID:             LongInt;                      // The tile's unique id
      Kind:               LongInt;                      // The "kind" of the tile - links to KindNames etc.
      Position:           Point2D;                      // Position of the top right corner of the Tile
      Center:             Point2D;                      // position of the center of the Tile
      TileBitmapCellKind: Array of BitmapCellKindPtr;      // ptr to bitmapCellKindData
      TileShape:          Shape;                        // shape of tile
      Values:             Array of Single;              // Array of single values.
      SurroundingTiles:   Array[Direction] of Tile;     // The adjcent tiles, can be nil if none.
    end;

    
    Map = ^MapData;
    MapData = Record
      Tiles:              Array of Array of TileData;     // The actual tiles -> in col,row order
      SelectedTiles:      LongIntArray;                   // id of selected tiles
      Isometric:          Boolean;                        // Map Isometric
      valueIds:           NamedIndexCollection;           // has names of values
      kindids:            NamedIndexCollection;           // has names of kinds
      MarkerIds:          NamedIndexCollection;           // has names of Markers
      //MapMarkers:        Array of Marker;
      MapPrototype:       ShapePrototype;                 // prototype of the tiles
      MapHighlightcolor:  Color;                          // highlight color
      MapWidth:           LongInt;                        // The Map width (# of grid in x axis)
      MapHeight:          LongInt;                        // The Map height (# of grid in Y axis)
      MapLayer:           LongInt;                        // The Number of layers within the map
      TileWidth:          LongInt;                        // The Tile width
      TileHeight:         LongInt;                        // The Tile height
      TileStagger:        Vector;                         // Offset of the tile's X Position
      MapDefaultValues:   Array of Array of Single;       //default values of tiles depending on bitmaps.
      BitmapCellKind:     Array of BitmapCellKind;    // Bitmap/cell/kinds.
    end;





  function LoadMap(filename:string): Map;
  procedure DrawMap(map: Map); overload;
  procedure DrawMapDebug(map: map);
  function TileSelected(map:Map; tile:Tile):Boolean;
  procedure UpdateSelect(map:Map);
  procedure Deselect(map:Map; tile:Tile);
  procedure HighlightTile(highlightedTile: Tile);
  procedure AllocateValue(m:Map; tile:Tile; name:String; val:Single);
  procedure SaveMap(m:Map; filename:String);
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


  //==================================================================
  //Load Map Functions
  //==================================================================
  function LoadMap(filename:string): Map;

  type
    SpecificValuesLoader = record
      row:      LongInt;
      col:      LongInt;
      name:     string;
      value:    Single;
    end;



  var
    tempValues:       Array of SpecificValuesLoader;  // contains the row,col, name and value
    path:             String;                         // path of the map file
    textFile:         Text;                           // text file that is loaded
    id:               String;                         // id for the line processor to identify how to handle the data
    data,line:        String;                         // line is read then seperated into id and data.
    lineno:           LongInt;                        // line number for exceptions



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
          SetLength(result^.Tiles[row,col].TileBitmapCellKind, result^.MapLayer); // set length of bitmapcells per tile.
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
      current : LongInt;
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
        result^.Tiles[rowNum, i].TileBitmapCellKind[layer] := nil;
          current := bitmapIdxs[i];
          if current <> -1 then // used -1 as no bitmap.
          begin
            //result^.Tiles[rowNum, i].TileBitmapCellKind[layer].Bmap    := bitmapCellArray[current].Bmap;
            //result^.Tiles[rowNum, i].TileBitmapCells[layer].Cell    := bitmapCellArray[current].Cell;
            //result^.Tiles[rowNum, i].TileBitmapCells[layer].KindIdx := bitmapCellArray[current].KindIdx;
            result^.Tiles[rowNum, i].TileBitmapCellKind[layer] := @result^.BitmapCellKind[current];
            
            if layer = 0 then
            begin
              writeln('adding kind');
              result^.Tiles[rowNum, i].kind := result^.BitmapCellKind[current].KindIdx;
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
      if (index+1)> length(result^.BitmapCellKind) then
      begin
      SetLength(result^.BitmapCellKind, index+1);
      result^.BitmapCellKind[index].Bmap := LoadBitmap(ExtractDelimited(2,data,[',']));
      result^.BitmapCellKind[index].Cell:= 0;
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
      if length(kindIds) = length(result^.BitmapCellKind) then
        begin
          for i:=low(result^.BitmapCellKind) to high(result^.BitmapCellKind) do
          begin
          result^.BitmapCellKind[i].KindIdx := kindIds[i];
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

    procedure AddSpecificKind(data: string);
    var
    row,col,kidx:longInt;
    begin
      row := MyStrToInt(ExtractDelimited(1, data, [',']));
      col := MyStrToInt(ExtractDelimited(2, data, [',']));
      kidx := MyStrToInt(ExtractDelimited(3, data, [',']));
      SetTileKind(TileAt(result,row,col), kidx);
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
        'k':AddSpecificKind(data);
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
      
      SetBitmapCellDetails(gridBitmap, cellWidth, cellHeight, cellCols, cellRows, cellCount);
      for i:=low(bitmapCellIds) to high(bitmapCellIds) do
      begin
        SetLength(result^.BitmapCellKind, length(result^.BitmapCellKind)+1);
        result^.BitmapCellKind[high(result^.BitmapCellKind)].Cell := cellRegions[i];
        result^.BitmapCellKind[high(result^.BitmapCellKind)].Bmap  := gridBitmap;
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
    if startX >= (map^.MapWidth) then startX := map^.MapWidth-1;
    if startY >= (map^.MapHeight) then startY := map^.MapHeight-1;
    
    if endY <0 then endY := 0;
    if endX <0 then endX := 0;
    
    if startY < 0 then startY := 0;
    if startX < 0 then startX := 0;   
    
    //WriteLn('result ', startX, ':', startY, ' ', endX, ':', endY);
  end;

  procedure PushMapClip(map:Map; startCol, startRow, endCol, endRow : LongInt);
  var
    rowCount, colCount: LongInt;
    width, height : LongInt;
    pt: Point2D;
  begin
    //WriteLn('cols: ', startCol, ' to ', endCol);
   // WriteLn('rows: ', startRow, ' to ', endRow);
    rowCount := (endRow - startRow)+1;
    colCount := (endCol - startCol)+1;
    //writeln('rowcount: ',rowCount, ' ','colcount: ',ColCount);
    // # of columns * tilewidth - stagger X for even rows.
    width := Round ( (colCount * map^.TileWidth) - ( ( (startRow+1) mod 2)* (map^.TileStagger.X)));
    // # of row ^ tileheight - number of rows * staggerY  -- in conjunction with if rowcount = 20...
    height := Round(((rowCount) * map^.TileHeight) - ((rowCount)*(map^.TileStagger.Y)));
    
   // writeln('width: ',width, ' ','height: ',height);

    pt := TileAt(map, startRow, startCol)^.position;
    pt := PointAdd(pt, InvertVector(CameraPos()));
    //the last row doesnt clip at 20 because the camera has not reached the next row.
    if rowcount = 20 then height-=round(map^.TileStagger.Y);
    //apply stagger x only on the first column and even values.
    if (startCol = 0) and  not(startRow Mod 2=0) then pt.X += map^.TileStagger.X;
    //apply stagger to top of the map only.
    if (startrow = 0) then pt.Y += map^.TileStagger.Y;
    //WriteLn('Clip At: ', PointToString(pt));
    PushClip(RectangleFrom(pt.x, pt.y, width ,height));
  end;

  //checks if tile is Selected.
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
  procedure HighlightTile(highlightedTile: Tile);
  begin
    if not assigned(highlightedTile) then exit;
    DrawShapeAsLineStrip(screen, highlightedTile^.TileShape, false, CameraPos*-1);
  end;

  
  //updates whether a tile is highlighted.
  procedure UpdateHighlight(map:Map);
  var
  i:LongInt;
  begin
    for i:=low(map^.SelectedTiles) to high(map^.SelectedTiles) do
            HighlightTile(TileAt(map, map^.SelectedTiles[i]));
  end;
  
  //updates whether a tile should be selected or deselected.
  procedure UpdateSelect(map:Map);
  var
    //startCol, startRow, endCol, endRow:LongInt;
    t: Tile;
  begin
    t := TileAt(map, ToWorld(MousePosition()));
    if t = nil then exit;
    if not TileSelected(map, t) then
    begin
      SetLength(map^.SelectedTiles, Length(map^.SelectedTiles)+1);
      map^.SelectedTiles[high(map^.SelectedTiles)]:=t^.TileId;
      writeln('selected: ',t^.TileId);
    end
    else
    begin
    writeln('deselecting: ',t^.TileId);
      Deselect(map, TileAt(map, t^.TileId));
    end;
  end;

  //draw map procedure.
  procedure DrawMap(map: Map);
  var
    layer,col, row, startRow, startCol, endRow, endCol : LongInt;
   // dir:Direction;
  begin
    MapRangeFromRect(CameraScreenRect(), map, startCol, startRow, endCol, endRow);

    PushMapClip(map, startCol, startRow, endCol, endRow);
    
    // Loop through all the layers + and additional one for debug info
    for layer:= 0 to map^.MapLayer-1 do
    begin
      for row:=startRow to endRow do
      begin
        for col:=startCol to endCol do
        begin
          if map^.Tiles[row,col].TileBitmapCellKind[layer] <> nil then
          if map^.Tiles[row,col].TileBitmapCellKind[layer]^.Bmap <> nil then
          begin
           // writeln(HexStr(map^.Tiles[row,col].TileBitmapCellKind[layer]^.Bmap));
              DrawCell(map^.Tiles[row,col].TileBitmapCellKind[layer]^.Bmap, map^.Tiles[row,col].TileBitmapCellKind[layer]^.Cell, map^.Tiles[row,col].Position);
          end;
        end; // end of col in row
      end; // end of row
    end; // end of layers
    PopClip();
  end;

  procedure DrawMapDebug(map: map);
  var
  dir: Direction;
  row,col,startRow, startCol, endRow, endCol : LongInt;
  begin
  MapRangeFromRect(CameraScreenRect(), map, startCol, startRow, endCol, endRow);
    for row:=startRow to endRow do
    begin
      for col:=startCol to endCol do
      begin
  //Show debug information over last layer
        if TileSelected(map, @map^.Tiles[row,col]) then
        begin
          //Draw debug information
          DrawText(IntToStr(col) + ':' + IntToStr(row), map^.mapHighlightcolor, map^.Tiles[row,col].Position);
          DrawText(IntToStr(map^.Tiles[row,col].kind), map^.mapHighlightcolor, map^.Tiles[row,col].Position.x, map^.Tiles[row,col].Position.y + 10);
          DrawText(FloatToStr(map^.Tiles[row,col].values[0]), map^.mapHighlightcolor, map^.Tiles[row,col].Position.x, map^.Tiles[row,col].Position.y + 20);

          for dir:=low(map^.Tiles[row,col].SurroundingTiles) to high(map^.Tiles[row,col].SurroundingTiles) do
          begin
            if not assigned(map^.Tiles[row,col].SurroundingTiles[dir]) then continue
            else
            DrawLine(map^.mapHighlightcolor, map^.Tiles[row,col].center,map^.Tiles[row,col].SurroundingTiles[dir]^.center);
          end;
        end;
      end;
    end;
        UpdateHighlight(map);
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
//---------------------------------------------------------------------------
// SAVE MAP
//---------------------------------------------------------------------------
  Procedure SaveMap(m:Map; filename:String);
  type
  cellkind = record
    cell      : LongInt;
    kindIdx      : LongInt;
    end;

  cellKindArray = array of cellkind;
  
  BitmapCells = record
    bmp       : Bitmap;
    Cellkinds : cellKindArray;
    startIdx  : LongInt;
    kindIdx   : LongInt;
  end;
  var
  path : string;
  output: text;
  bitmapCellsArray:  Array of BitmapCells;
    procedure _CheckAddBitmap(bmp: Bitmap; cell,kindIdx: Integer);
    var
      i,j:LongInt;
    begin
     // WriteLn('Checking ', HexStr(bmp), ' ', cell, ' kind ', kindIdx);
      
      if not assigned(bmp) then exit;
      // For all of the cells we have so far
      for i:=low(bitmapCellsArray) to high(bitmapCellsArray) do
      begin
        //if this is the bitmap we are after...
        if (bitmapCellsArray[i].bmp = bmp) then
        begin
          //check if we have the cell...
          for j:=low(bitmapCellsArray[i].CellKinds) to high(bitmapCellsArray[i].CellKinds) do
          begin
            //if found... exit out of proc
            if bitmapCellsArray[i].CellKinds[j].cell = cell then exit;
          end;
          // Need to add cellkind to bitmap
          SetLength(bitmapCellsArray[i].CellKinds, length(bitmapCellsArray[i].CellKinds) + 1);
          bitmapCellsArray[i].CellKinds[high(bitmapCellsArray[i].CellKinds)].cell := Cell;
          bitmapCellsArray[i].CellKinds[high(bitmapCellsArray[i].CellKinds)].kindIdx := kindIdx;
          
          exit;
        end;
      end;
      
      // Got here because there was no matching bitmap... so add it
      SetLength(bitmapCellsArray, length(bitmapCellsArray) + 1);
     // writeln(Length(bitmapCellsArray));
      SetLength(bitmapCellsArray[high(bitmapCellsArray)].CellKinds, 1);
      bitmapCellsArray[high(bitmapCellsArray)].CellKinds[0].Cell := cell;
      bitmapCellsArray[high(bitmapCellsArray)].Bmp := bmp;
      bitmapCellsArray[high(bitmapCellsArray)].CellKinds[0].KindIdx := kindIdx;
      
     // writeln('cur '+bitmapName(bitmapCellsArray[i].bmp),' passed '+bitmapName(bmp));
    end;

    procedure _SetStartIdx();
    var
    i:LongInt;
    begin
      bitmapCellsArray[0].startIdx:=0;
      for i:= (low(bitmapCellsArray)+1) to high(bitmapCellsArray) do
      begin
        bitmapCellsArray[i].startIdx := (bitmapCellsArray[i-1].startIdx + length(bitmapCellsArray[i-1].CellKinds));
      end;
    end;


    function _IndexOfMapCellKind(ptr:BitmapCellKindPtr): LongInt;
    var
    i:LongInt;
    begin
    result := -1;
    if ptr = nil then exit;
    for i := low(m^.BitmapCellKind) to high(m^.BitmapCellKind) do
    begin
      if ptr = @m^.BitmapCellKind[i] then
      result:= i;
    end;
    
    end;
    
    procedure _WriteBitmapInfo();
    var
    i,j:Longint;
    endIdx : longint;
    cells : LongIntArray;
    currentBitmap : BitmapCells;
    cellrange:string;
    begin
      for i:=low(bitmapCellsArray) to high(bitmapCellsArray) do
      begin
        currentBitmap := bitmapCellsArray[i];
        if length(currentBitmap.CellKinds) = 1 then
        begin
          writeln(output, 'tb:',currentBitmap.startIdx,','+BitmapName(currentBitmap.bmp));
        end
        else
        begin

          //make cell into LongIntArray of cells
          SetLength(cells,length(currentBitmap.CellKinds));
          for j := low(currentBitmap.CellKinds) to high(currentBitmap.CellKinds) do
          begin
            cells[j] := currentBitmap.CellKinds[j].cell;
          end;
         cellrange := LongIntArrayToRange(cells);
          endIdx:=bitmapCellsArray[i].startIdx+length(bitmapCellsArray[i].CellKinds)-1;
        writeln(output,'gb:[',bitmapCellsArray[i].startIdx,'-',endIdx,'],',cellrange,
              ',',BitmapName(bitmapCellsArray[i].bmp),
              ',',IntToStr(BitmapCellWidth(bitmapCellsArray[i].bmp)),
              ',',IntToStr(BitmapCellHeight(bitmapCellsArray[i].bmp)),
              ',',IntToStr(BitmapCellRows(bitmapCellsArray[i].bmp)),
              ',',IntToStr(BitmapCellColumns(bitmapCellsArray[i].bmp)),
              ',',IntToStr(BitmapCellCount(bitmapCellsArray[i].bmp)));
        end;
      end;
    end;

    
    
    procedure _SaveMapBitmaps();
    var
      row,col,bmpIdx:LongInt;
      current :Array of BitmapCellKind;
    begin
      for row:=low(m^.Tiles) to high(m^.Tiles) do
      begin
        for col:=low(m^.Tiles[row]) to high(m^.Tiles[row]) do
        begin
          current := m^.BitmapCellKind;

          
          for bmpIdx := low(current) to high(current) do
          begin
            _CheckAddBitmap(current[bmpIdx].Bmap, current[bmpIdx].cell, current[bmpIdx].KindIdx);
          end;
        end;
      end;
      _SetStartIdx();
      _WriteBitmapInfo();
    end;

  procedure _WriteKindBitmap();
  var
    i,j : LongInt;
    mki : string;
  begin
    mki := 'mki:';
    mki += '['+IntToStr(bitmapCellsArray[0].CellKinds[0].kindIdx);
    
    for i := Low(bitmapCellsArray)+1 to High(bitmapCellsArray) do
      for j := Low(bitmapCellsArray[i].CellKinds) to High(bitmapCellsArray[i].CellKinds) do
      begin
        mki+=','+IntToStr(bitmapCellsArray[i].CellKinds[j].kindIdx);
      end;
    mki+=']';
    writeln(output,mki);
  end;

  procedure _WriteKindName();
  var
  mkn : string;
  i : LongInt;
  begin
    mkn:='mkn:'+NameAt(m^.kindIds,0);
    for i:=1 to NameCount(m^.kindIds)-1 do
    begin
      mkn+=','+NameAt(m^.kindIds, i);
    end;
    writeln(output, mkn);
  end;

  procedure _WriteTileLayers();
  var
    ids : LongIntArray;
    row,col, layer : LongInt;
    currentTile: TileData;
  begin
    SetLength(ids, m^.MapWidth);
    for layer := 0 to m^.MapLayer-1 do
    begin
      writeln(output, 'tl: ', IntToStr(layer));
      for row := Low(m^.Tiles) to High(m^.Tiles) do
      begin
        for col := Low(m^.Tiles[row]) to High(m^.Tiles[row]) do
        begin
          currentTile := m^.Tiles[row,col];
          //add indexes to ids.
          ids[col]:=_IndexOfMapCellKind(currentTile.TileBitmapCellKind[layer]);
        end; // end col
        writeln(output, LongIntArrayToRange(ids));
      end; // end row
    end; // end layer
  end;

  begin
    path:= PathToResource('maps/' +filename);
    SetLength(bitmapCellsArray, 0);
    
    Assign(output, Path);
    rewrite(output);
    writeln(output, 'Map Loader #v0.1');
    writeln(output, '// mw : width of map');
    writeln(output, 'mw:'+IntToStr(m^.MapWidth));
    writeln(output, '// mh : height of map');
    writeln(output, 'mh:'+IntToStr(m^.MapHeight));
    writeln(output, '// ml : Number of map Layers');
    writeln(output, 'ml:'+IntToStr(m^.MapLayer));
    writeln(output, '//mhc : r,g,b,a (byte)');
    writeln(output, 'mhc:'+ColorToString(m^.MapHighlightcolor));
    writeln(output, '// tw : Width of tile');
    writeln(output, 'tw:'+IntToStr(m^.TileWidth));
    writeln(output, '// th : Height of tile');
    writeln(output, 'th:'+IntToStr(m^.TileHeight));
    writeln(output, '// mi : isometric:true or false');
    writeln(output, 'mi:',m^.Isometric);
    writeln(output, '// Tile Bitmaps');
    writeln(output, '//');
    writeln(output, '// Format:');
    writeln(output, '// tb: [tile index], [bitmap filename]');
    writeln(output, '// gb: [tile index range], [cell range], [bitmap filename], [cell width], [cell height] [cells Row] [cells columns] [cell count]');
    _SaveMapBitmaps();
    writeln(output, '//');
    writeln(output, '// Tile -> Kind Mapping');
    writeln(output, '// Format:');
    writeln(output, '// mkn: [name1], [name2]...');
    writeln(output, '// mki: [kind index ids - position of values match bitmap index]');
    _WriteKindBitmap();
    _WriteKindName();
    _WriteTileLayers();
    close(output);
  end;
  



//====================================== //
//           collision codes            //
//=====================================//

    //gets potential collision and makes a rect out of it.
  function GetPotentialCollisions(map: Map; s: Sprite): Rectangle;
  var
    startPoint, endPoint: Rectangle;
    startX, startY, endX, endY: LongInt;
  begin
    with map^ do
    begin
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

  function TileAt(m: Map; const pos:Point2D): Tile; overload;
  var
  row,col : LongInt;
  begin
    if (pos.y <0) or (pos.x <0) then
    begin
    result:=nil;
    exit;
    end;
    row := Trunc(pos.Y / (m^.TileHeight - m^.TileStagger.Y));
    col := Trunc(pos.X / m^.TileWidth);
    result := TileAt(m, row, col);
   //writeln(pos.x,' ',pos.y);
    //writeln(row,' ',col);
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
    result := t^.TileBitmapCellKind[layer]^.BMap;
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
    t^.TileBitmapCellKind[layer]^.BMap := bmp;
    t^.TileBitmapCellKind[layer]^.Cell := cell;
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
