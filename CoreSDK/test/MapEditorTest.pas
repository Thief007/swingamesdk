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
function LoadMap(filename:string):Map;
var
  
  path: String; //path of the map file
  textFile:Text; //text file that is loaded
  id:string; // id for the line processor to identify how to handle the data
  data,line:String; // line is read then seperated into id and data.
  lineno:integer; // line number for exceptions
  bitmapArray : Array of Bitmap; // had to make a list of bitmap since i can't load 2 bitmaps with the same image.
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
 //ADD tile procedure*************
  procedure AddTile();
  var
    x : LongInt; //x of array
    y : LongInt; // y of array
  begin
    y:= MyStrToInt(data, false);
    ReadLn(textFile, data);
    lineNo := lineNo + 1;
    x := MyStrToInt(data, false);
    result^.Tiles[x,y].Position.Y := y*result^.TileHeight; //y position = y on grid * tile height
    result^.Tiles[x,y].Position.X := x*result^.TileWidth; // x position = x on grid * tile width
     
    ReadLn(textFile, Data);
    lineNo := lineNo + 1;
    if data <> '--' then // used -- as no bitmap.
    begin      
      result^.Tiles[x, y].TileBitmap := bitmapArray[MyStrToInt(data, false)];
    end;
      //result^.Tiles[x, y].TileID := 1;

  end;
  
  procedure AddBitmap();
  begin
    SetLength(bitmapArray, length(bitmapArray)+1);
    bitmapArray[high(bitmapArray)] := LoadBitmap(data);
  end;
 //Process line procedure*************
  procedure ProcessLine();
  begin
    // Split line into id and data
    id := ExtractDelimited(1, line, [':']);
    data := ExtractDelimited(2, line, [':']);
    
    // Verify that id is a single char
    if Length(id) <> 2 then
    begin
      RaiseException('Error at line ' + IntToStr(lineNo) + ' in map ' + filename + '. Error with id: ' + id + '. This id should contain 2 characters.');
      exit;
    end;
    
    
    // Process based on id
    case LowerCase(id)[1] of // in all cases the data variable is read
      'm':
        begin
          case LowerCase(id)[2] of
          'w':result^.MapWidth := MyStrToInt(data, false); //map width
          'h':
            begin  
              result^.MapHeight := MyStrToInt(data, false); // b = map height
              SetLength(result^.Tiles, result^.MapWidth, result^.MapHeight);//set lengths of tiles array
            end;
          end;
        end;
      't':
        begin
          case LowerCase(id)[2] of
          'w':result^.TileWidth := MyStrToInt(data, false);        // c = tile width
          'h':result^.TileHeight := MyStrToInt(data, false);      // d = tile height
          'i':AddTile(); 
          'b':AddBitmap();
          end;
        end;
      else
      begin
        RaiseException('Error at line ' + IntToStr(lineNo) + ' in map ' + filename + '. Error with id: ' + id + '. id not recognized.');
        exit;
      end;
    end;
  end;
  
  
  begin
  path:= FilenameToResource(filename, MapResource);
  Assign(textFile, Path);
  lineNo:=0;
  result:= new(Map);

//moves cursor to 1,1
  Reset(textFile);
  ReadLn(textFile,line);
  if line <> 'Map Loader #v0.1' then 
    begin
      WriteLn('Map File Corrupted');
    end;
  while not EOF(textFile) do
    begin
       lineNo := lineNo + 1;
       ReadLn(textFile, line);
       line := Trim(line);
       if Length(line) = 0 then continue;  //skip empty lines
       if MidStr(line,1,2) = '//' then continue; //skip lines starting with //
       ProcessLine();
       end;
      
    end;

//tiles
  



Procedure DrawMap(mapName:Map);
var
  j : integer; 
  i : integer;
begin
  for i:=low(mapName^.Tiles) to high(mapName^.Tiles) do
    for j:=low(mapName^.Tiles[i]) to high(mapName^.Tiles[i])do
      if mapName^.Tiles[i,j].TileBitmap <> nil then
      begin
          DrawBitmap(mapName^.Tiles[i,j].TileBitmap, mapName^.Tiles[i,j].Position);
      end
end;

    
procedure Main();
var
  myMap: Map;
begin
  OpenAudio();
  OpenGraphicsWindow('Animation Tests', 640, 480);
  myMap := LoadMap('test1.txt');
  repeat // The game loop...
    ProcessEvents();
    
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
