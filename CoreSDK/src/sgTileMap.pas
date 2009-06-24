//=============================================================================
//          sgTileMap.pas
//=============================================================================
//
// Responsible for loading and processing a "Mappy" data file exported using
// the Lua script specifically written for SwinGame to create map files.
//
// Change History:
//
// Version 3.0:
// - 2009-06-22: Clinton: Comment format, cleanup and new additions.
// - 2009-06-17: Andrew: added meta tags, renamed from "mappy" to tilemap
//
// Version 2:
// - 2008-12-17: Andrew: Moved all integers to LongInt
//
// Version 1.1.5:
// - 2008-04-18: Andrew:  Fix extensions to work with Delphi.
//
// Version 1.1:
// - 2008-04-02: Stephen: Added MapWidth(), MapHeight(), BlockWidth(), 
//                        BlockHeight(), GapX(), GapY(), StaggerX(), StaggerY(),
//                        LoadIsometricInformation(), LoadMapv2(), 
//                      : various bug fixes
// - 2008-04-02: Andrew: Removed gap loading as mappy support has not been 
//                       updated on the web, and this version is unable to 
//                       read the old files.
// - 2008-03-29: Stephen: MapData record now contains GapX, GapY, StaggerX, 
//                        StaggerY, Isometric
//                      : LoadMapInformation, now loads the new isometric related data
//                      : DrawMap now draws isometric tiles with their correct offsets
// - 2008-01-30: Andrew: Added const to vector param, increased search for collision tests
// - 2008-01-25: Andrew: Fixed compiler hints
// - 2008-01-22: Andrew: Re-added CollidedWithMap to allow compatibility with 1.0
// - 2008-01-21: Stephen: CollidedWithMap replaced with 3 Routines, 
//                        - HasSpriteCollidedWithMapTile, 
//                        - MoveSpriteOutOfTile, 
//                        - WillCollideOnSide
// - 2008-01-17: Aki + Andrew: Refactor
//
// Version 1.0:
// - Various
//=============================================================================

///@module TileMap
///@static
unit sgTileMap;

//=============================================================================
interface
//=============================================================================

  uses  sgTypes;

  /// @lib
  /// @class Map
  /// @constructor
  function LoadMap(mapName : String): Map;

  /// @lib
  /// @class Map
  /// @constructor
  function LoadMapFiles(mapFile, imgFile: String): Map;

  /// @lib
  /// @class Map
  /// @method Draw
  procedure DrawMap(m : Map);

  /// @lib
  /// @class Map
  /// @method HasSpriteCollidedWithTile
  function SpriteHasCollidedWithMapTile(m: Map; spr: Sprite): Boolean; overload;

  /// @lib SpriteHasCollidedWithMapTileOutXY
  /// @class Map
  /// @overload  HasSpriteCollidedWithTile HasSpriteCollidedWithTileOutXY
  function SpriteHasCollidedWithMapTile(m: Map; spr: Sprite; out collidedX, collidedY: LongInt): Boolean; overload;

  /// @lib
  /// @class Map
  /// @method WillCollideOnSide
  function WillCollideOnSide(m: Map; spr: Sprite): CollisionSide;
  
  /// @lib
  /// @class Map
  /// @method MoveSpriteOutOfTile
  procedure MoveSpriteOutOfTile(m: Map; spr: Sprite; x, y: LongInt);
  
  /// @lib
  /// @class Map
  /// @method EventCount
  function EventCount(m : Map; eventType : Event): LongInt;
  
  /// @lib
  /// @class Map
  /// @method EventPositionX
  function EventPositionX(m : Map; eventType : Event; eventnumber : LongInt): LongInt;
  
  /// @lib
  /// @class Map
  /// @method EventPositionY
  function EventPositionY(m : Map; eventType : Event; eventnumber : LongInt): LongInt;
  
  /// @lib
  /// @class Sprite
  /// @self 2
  /// @method CollisionWithMap
  function CollisionWithMap(m : Map; spr : Sprite; const vec: Vector): CollisionSide;
  
  /// @lib
  /// @class Map
  /// @getter Width
  function MapWidth(m : Map): LongInt;
  
  /// @lib
  /// @class Map
  /// @getter Height
  function MapHeight(m : Map): LongInt;
  
  /// @lib
  /// @class Map
  /// @getter BlockWidth
  function BlockWidth(m : Map): LongInt;
  
  /// @lib
  /// @class Map
  /// @getter BlockHeight
  function BlockHeight(m : Map): LongInt;
  
  /// @lib
  /// @class Map
  /// @getter GapX
  function GapX(m : Map): LongInt;
  
  /// @lib
  /// @class Map
  /// @getter GapY
  function GapY(m : Map): LongInt;
  
  /// @lib
  /// @class Map
  /// @getter StaggerX
  function StaggerX(m : Map): LongInt;
  
  /// @lib
  /// @class Map
  /// @getter StaggerY
  function StaggerY(m : Map): LongInt;
  
  /// @lib
  /// @class Map
  /// @self 2
  /// @method GetTileFromPoint
  function GetTileFromPoint(point: Point2D; m: Map): Tile;
  
  /// @lib
  /// @class Map
  /// @method GetEventAtTile
  function GetEventAtTile(m : Map; xIndex, yIndex: LongInt): Event;
  
  /// @lib
  /// @class Map
  /// @dispose
  procedure FreeMap(var m: Map);


//=============================================================================
implementation
//=============================================================================

  uses SysUtils, Classes, sgGraphics, sgCamera, sgCore, sgPhysics, sgMath, sgShapes;

  function ReadInt(var stream : text): Word;
  var
    c : char;
    c2 : char;
    i : LongInt;
    i2 : LongInt;
  begin
    Read(stream ,c);
    Read(stream ,c2);
  
    i := LongInt(c);
    i2 := LongInt(c2) * 256;
  
    result := i + i2; 
  end;

  procedure LoadMapInformation(m : Map; var stream : text);
  var
    header: LongInt;
  begin
    header := ReadInt(stream);
  
    if header = 0 then
    begin
      m.MapInfo.Version := ReadInt(stream);
      m.MapInfo.MapWidth := ReadInt(stream);
    end
    else
    begin
      m.MapInfo.Version := 1;
      m.MapInfo.MapWidth := header;
    end;

    //m.MapInfo.MapWidth := ReadInt(stream);
        m.MapInfo.MapHeight := ReadInt(stream);
        m.MapInfo.BlockWidth := ReadInt(stream);
        m.MapInfo.BlockHeight := ReadInt(stream);
        m.MapInfo.NumberOfBlocks := ReadInt(stream);
        m.MapInfo.NumberOfAnimations := ReadInt(stream);
        m.MapInfo.NumberOfLayers := ReadInt(stream);
        m.MapInfo.CollisionLayer := ReadInt(stream);
      m.MapInfo.EventLayer := ReadInt(stream);
      m.MapInfo.GapX := 0;
      m.MapInfo.GapY := 0;
      m.MapInfo.StaggerX := 0;
      m.MapInfo.StaggerY := 0;
      m.MapInfo.Isometric := false;
    
      {
      //Debug
      WriteLn('MapInformation');
      WriteLn('');
      WriteLn(m.MapInfo.MapWidth);
      WriteLn(m.MapInfo.MapHeight);
      WriteLn(m.MapInfo.BlockWidth);
      WriteLn(m.MapInfo.BlockHeight);
      WriteLn(m.MapInfo.NumberOfBlocks);
      WriteLn(m.MapInfo.NumberOfAnimations);
      WriteLn(m.MapInfo.NumberOfLayers);
      WriteLn(m.MapInfo.CollisionLayer);
      WriteLn(m.MapInfo.EventLayer);
      WriteLn('');
      ReadLn();
      }
  end;

  procedure LoadIsometricInformation(m : Map; var stream : text);
  begin
    m.MapInfo.GapX := ReadInt(stream);
      m.MapInfo.GapY := ReadInt(stream);
      m.MapInfo.StaggerX := ReadInt(stream);
      m.MapInfo.StaggerY := ReadInt(stream);

    if ((m.MapInfo.StaggerX = 0) and (m.MapInfo.StaggerY = 0)) then
        m.MapInfo.Isometric := false
      else
        m.MapInfo.Isometric := true;
      
      if (m.MapInfo.Isometric = false) then
      begin
        m.MapInfo.GapX := 0;
        m.MapInfo.GapY := 0;
        m.MapInfo.StaggerX := 0;
        m.MapInfo.StaggerY := 0;
      end;
  end;


  procedure LoadAnimationInformation(m : Map; var stream : text);
  var
    i, j : LongInt;
  begin

    if m.MapInfo.NumberOfAnimations > 0 then
    begin
  
      SetLength(m.AnimationInfo, m.MapInfo.NumberOfAnimations);
    
      for i := 0 to m.MapInfo.NumberOfAnimations - 1 do
      begin
      
        m.AnimationInfo[i].AnimationNumber := i + 1;
        m.AnimationInfo[i].Delay := ReadInt(stream);
        m.AnimationInfo[i].NumberOfFrames := ReadInt(stream);
      
        SetLength(m.AnimationInfo[i].Frame, m.AnimationInfo[i].NumberOfFrames);
      
        for j := 0 to m.AnimationInfo[i].NumberOfFrames - 1 do
        begin
          m.AnimationInfo[i].Frame[j] := ReadInt(stream);
        end;
      
        m.AnimationInfo[i].CurrentFrame := 0;
      
      end;
    
      {
      //Debug
      WriteLn('Animation Information');
      WriteLn('');
      for i := 0 to m.MapInfo.NumberOfAnimations - 1 do
      begin
        WriteLn(m.AnimationInfo[i].AnimationNumber);
        WriteLn(m.AnimationInfo[i].Delay);
        WriteLn(m.AnimationInfo[i].NumberOfFrames);
      
        for j := 0 to m.AnimationInfo[i].NumberOfFrames - 1 do
        begin
          WriteLn(m.AnimationInfo[i].Frame[j]);
        end;
      end;
      WriteLn('');
      ReadLn();   
      }
    end;
  end;

  procedure LoadLayerData(m : Map; var stream : text);
  var
    l, y, x : LongInt;
  begin

    SetLength(m.LayerInfo, m.MapInfo.NumberOfLayers - m.MapInfo.Collisionlayer - m.MapInfo.EventLayer);

    for y := 0 to Length(m.LayerInfo) - 1 do
    begin

      SetLength(m.LayerInfo[y].Animation, m.MapInfo.MapHeight);
      SetLength(m.LayerInfo[y].Value, m.MapInfo.MapHeight);
    
      for x := 0 to m.MapInfo.MapHeight - 1 do
      begin

        SetLength(m.LayerInfo[y].Animation[x], m.MapInfo.MapWidth);
        SetLength(m.LayerInfo[y].Value[x], m.MapInfo.MapWidth);
      end;
    end;

    for l := 0 to m.MapInfo.NumberOfLayers - m.MapInfo.Collisionlayer - m.MapInfo.Eventlayer - 1 do
    begin
      for y := 0 to m.MapInfo.MapHeight - 1 do
      begin
        for x := 0 to m.MapInfo.MapWidth - 1 do
        begin

          m.LayerInfo[l].Animation[y][x] := ReadInt(stream);
          m.LayerInfo[l].Value[y][x] := ReadInt(stream);
        end;
      end;
    end;

    {
    //Debug
    WriteLn('Layer Information');
    WriteLn(Length(m.Layerinfo));
    WriteLn('');
  
    for l := 0 to Length(m.LayerInfo) - 1 do
    begin
      for y := 0 to m.MapInfo.MapHeight - 1 do
      begin
        for x := 0 to m.MapInfo.MapWidth - 1 do
        begin
          Write(m.LayerInfo[l].Animation[y][x]);
          Write(',');
          Write(m.LayerInfo[l].Value[y][x]);
          Write(' ');
        end;
      end;
      WriteLn('');
      ReadLn();
    end;
    }
  
  
  end;

  procedure LoadCollisionData(m : Map; var stream : text);
  var
    y, x: LongInt;
  begin
    if m.MapInfo.CollisionLayer = 1 then
    begin
      SetLength(m.CollisionInfo.Collidable, m.MapInfo.MapHeight);
    
      for y := 0 to m.MapInfo.MapHeight - 1 do
      begin
        SetLength(m.CollisionInfo.Collidable[y], m.MapInfo.MapWidth);
      end;
    
      for y := 0 to m.MapInfo.MapHeight - 1 do
      begin
        for x := 0 to m.MapInfo.MapWidth - 1 do
        begin
          if ReadInt(stream) <> 0 then
            m.CollisionInfo.Collidable[y][x] := true
          else
            m.CollisionInfo.Collidable[y][x] := false
        end;
      end;
    
    
      //Debug
      {
      for y := 0 to m.MapInfo.MapHeight - 1 do
      begin
        for x := 0 to m.MapInfo.MapWidth - 1 do
        begin
          if m.CollisionInfo.Collidable[y][x] = true then
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

  procedure LoadEventData(m : Map; var stream : text);
  var
    py, px, smallestEventIdx, temp: LongInt;
    evt: Event;
  begin
    //SetLength(m.EventInfo, High(Events));
  
    //SetLength(m.EventInfo.Event, m.MapInfo.MapHeight);
  
    {for y := 0 to m.MapInfo.MapHeight - 1 do
    begin
      SetLength(m.EventInfo.Event[y], m.MapInfo.MapWidth);
    end;}
  
    //The smallest "non-graphics" tile, i.e. the events
    smallestEventIdx := m.MapInfo.NumberOfBlocks - 23;
  
    for py := 0 to m.MapInfo.MapHeight - 1 do
    begin
      for px := 0 to m.MapInfo.MapWidth - 1 do
      begin
        temp := ReadInt(stream);
        evt := Event(temp - smallestEventIdx);
      
        if (evt >= Event1) and (evt <= Event24) then 
        begin
          SetLength(m.EventInfo[LongInt(evt)], Length(m.EventInfo[LongInt(evt)]) + 1);
        
          with m.EventInfo[LongInt(evt)][High(m.EventInfo[LongInt(evt)])] do
          begin
            x := px;
            y := py;
          end;
        end
      end;
    end;
  
  
    //Debug
    {
    for y := 0 to m.MapInfo.MapHeight - 1 do
    begin
      for x := 0 to m.MapInfo.MapWidth - 1 do
      begin
        Write(' ');
        Write(LongInt(m.EventInfo.Event[y][x]));
      end;
      WriteLn('');
    end;
    ReadLn();
    }
  end;

  procedure LoadBlockSprites(m : Map; fileName : String);
  var
    fpc : LongIntArray; //Array of LongInt;
  begin
    SetLength(fpc, m.MapInfo.NumberOfBlocks);
    m.Tiles := CreateSprite(LoadBitmap(fileName), true, fpc,
                            m.MapInfo.BlockWidth, m.MapInfo.BlockHeight);
    m.Tiles.currentFrame := 0;
  end;

  procedure DrawMap(m : Map);
  var
    l, y ,x : LongInt;
    XStart, YStart, XEnd, YEnd : LongInt;
    f : LongInt;
  begin
    if m = nil then raise Exception.Create('No Map supplied (nil)');
  
    //WriteLn('GX, GY: ', ToWorldX(0), ',' , ToWorldY(0));
    //WriteLn('bw, bh: ', m.MapInfo.BlockWidth, ', ', m.MapInfo.BlockHeight);
  
    //Screen Drawing Starting Point
    XStart := round((ToWorldX(0) / m.MapInfo.BlockWidth) - (m.MapInfo.BlockWidth * 1));
    YStart := round((ToWorldY(0) / m.MapInfo.BlockHeight) - (m.MapInfo.BlockHeight * 1));
  
    //Screen Drawing Ending point
    XEnd := round(XStart + (sgCore.ScreenWidth() / m.MapInfo.BlockWidth) + (m.MapInfo.BlockWidth * 1));
    YEnd := round(YStart + (sgCore.ScreenHeight() / m.MapInfo.BlockHeight) + (m.MapInfo.BlockHeight * 1));

  
    //WriteLn('DrawMap ', XStart, ',', YStart, ' - ',  XEnd, ',', YEnd);
  
    if YStart < 0 then YStart := 0;
    if YStart >= m.MapInfo.MapHeight then exit;
    if YEnd < 0 then exit;
    if YEnd >= m.MapInfo.MapHeight then YEnd := m.MapInfo.MapHeight - 1;
      
    if XStart < 0 then XStart := 0;
    if XStart >= m.MapInfo.MapWidth then exit;
    if XEnd < 0 then exit;
    if XEnd >= m.MapInfo.MapWidth then XEnd := m.MapInfo.MapWidth - 1;
  
  
  
    for y := YStart  to YEnd do
    begin
      //GapX and GapY = The distance between each tile (rectangular), can be different to the normal width and height of the block
      //StaggerX and StaggerY = The isometric Offset
  
  
      //Isometric Offset for Y
      if (m.MapInfo.Isometric = true) then
        m.Tiles.y := y * m.MapInfo.StaggerY
      else
        m.Tiles.y := y * m.MapInfo.BlockHeight; 
  
      for x := XStart  to XEnd do
      begin
      
        //Isometric Offset for X
        if (m.MapInfo.Isometric = true) then
        begin
          m.Tiles.x := x * m.MapInfo.GapX;
          if ((y MOD 2) = 1) then
            m.Tiles.x := m.Tiles.x + m.MapInfo.StaggerX;
        end
        else
          m.Tiles.x := x * m.MapInfo.BlockWidth;
      
        for l := 0 to m.MapInfo.NumberOfLayers - m.MapInfo.CollisionLayer - m.MapInfo.EventLayer - 1 do
        begin
          if (m.LayerInfo[l].Animation[y][x] = 0) and (m.LayerInfo[l].Value[y][x] > 0) then
          begin
            m.Tiles.currentFrame := m.LayerInfo[l].Value[y][x] - 1;
            //DrawSprite(m.Tiles, CameraX, CameraY, sgCore.ScreenWidth(), sgCore.ScreenHeight());
            DrawSprite(m.Tiles);
          end
          else if (m.LayerInfo[l].Animation[y][x] = 1) then
          begin
                        f := round(m.Frame/10) mod (m.AnimationInfo[m.LayerInfo[l].Value[y][x]].NumberOfFrames);
                        m.Tiles.currentFrame := m.AnimationInfo[m.LayerInfo[l].Value[y][x]].Frame[f] - 1;   
            DrawSprite(m.Tiles);
          end;
        end;
      end;
    end;
  
    m.Frame := (m.Frame + 1) mod 1000;
  end;

  function LoadMap(mapName: String): Map;
  var
    mapFile, imgFile: String;
  begin
    mapFile := GetPathToResource(mapName + '.sga', MapResource);
    imgFile := GetPathToResource(mapName + '.png', MapResource);
      
    result := LoadMapFiles(mapFile, imgFile);
  end;

  function LoadMapFiles(mapFile, imgFile: String): Map;
  var
    filestream : text;
    m : Map;
  begin
    if not FileExists(mapFile) then raise Exception.Create('Unable to locate map: ' + mapFile);
    if not FileExists(imgFile) then raise Exception.Create('Unable to locate images: ' + imgFile);
        
    //Get File
    assign(filestream, mapFile);
    reset(filestream);
  
    //Create Map
    New(m);

    //Load Map Content
    LoadMapInformation(m, filestream);
    if (m.MapInfo.Version > 1) then
      LoadIsometricInformation(m, filestream);
    LoadAnimationInformation(m, filestream);
    LoadLayerData(m, filestream);
    LoadCollisionData(m, filestream);
    LoadEventData(m, filestream); 
    //Closes File
    close(filestream);  
  
    LoadBlockSprites(m, imgFile);
    m.Frame := 0;
    result := m;
  
    //WriteLn(m.MapInfo.Version);
  end;

  //Gets the number of Event of the specified type
  function EventCount(m : Map; eventType : Event): LongInt;
  begin
    if m = nil then raise Exception.Create('No Map supplied (nil)');
    if (eventType < Event1) or (eventType > Event24) then raise Exception.Create('EventType is out of range');
  
    result := Length(m.EventInfo[LongInt(eventType)]);
  
    {count := 0;
  
    for y := 0 to m.MapInfo.MapWidth - 1 do
    begin
      for x := 0 to m.MapInfo.MapHeight - 1 do
      begin
        if event = m.EventInfo.Event[y][x] then
          count := count + 1;
      end;
    end;
    result := count;}
  end;

  // Gets the Top Left X Coordinate of the Event
  function EventPositionX(m : Map; eventType : Event; eventnumber : LongInt): LongInt;
  begin
    if (eventnumber < 0) or (eventnumber > EventCount(m, eventType) - 1) then raise Exception.Create('Event number is out of range');

    if (m.MapInfo.Isometric = true) then
    begin
      result := m.EventInfo[LongInt(eventType)][eventnumber].x * m.MapInfo.GapX;
      if ((m.EventInfo[LongInt(eventType)][eventnumber].y MOD 2) = 1) then
        result := result + m.MapInfo.StaggerX;
      end
    
    else
      result := m.EventInfo[LongInt(eventType)][eventnumber].x * m.MapInfo.BlockWidth;
  
  end;

  // Gets the Top Left Y Coordinate of the Event
  function EventPositionY(m : Map; eventType : Event; eventnumber : LongInt): LongInt;
  begin
    if (eventnumber < 0) or (eventnumber > EventCount(m, eventType) - 1) then raise Exception.Create('Event number is out of range');
  
    if (m.MapInfo.Isometric = true) then
    begin
      result := m.EventInfo[LongInt(eventType)][eventnumber].y * m.MapInfo.StaggerY;
    end
    else      
    begin
      result := m.EventInfo[LongInt(eventType)][eventnumber].y * m.MapInfo.BlockHeight;
    end;
  end;

  function BruteForceDetection(m: Map; spr: Sprite): Boolean;
  const
    SEARCH_RANGE = 0;
  var
    XStart, XEnd, YStart, YEnd : LongInt;
    y, x, yCache: LongInt;
  begin
    result := false;
  
    XStart := round((spr.x / m.MapInfo.BlockWidth) - ((spr.width / m.MapInfo.BlockWidth) - SEARCH_RANGE));
    XEnd := round((spr.x / m.MapInfo.BlockWidth) + ((spr.width / m.MapInfo.BlockWidth) + SEARCH_RANGE));
    YStart := round((spr.y / m.MapInfo.BlockHeight) - ((spr.height / m.MapInfo.BlockHeight) - SEARCH_RANGE));
    YEnd := round((spr.y / m.MapInfo.BlockHeight) + ((spr.height / m.MapInfo.BlockHeight) + SEARCH_RANGE));

    if YStart < 0 then YStart := 0;
    if YStart >= m.MapInfo.MapHeight then exit;
    if YEnd < 0 then exit;
    if YEnd >= m.MapInfo.MapHeight then YEnd := m.MapInfo.MapHeight - 1;
      
    if XStart < 0 then XStart := 0;
    if XStart >= m.MapInfo.MapWidth then exit;
    if XEnd < 0 then exit;
    if XEnd >= m.MapInfo.MapWidth then XEnd := m.MapInfo.MapWidth - 1;

    for y := YStart to YEnd do
    begin
      yCache := y * m.MapInfo.BlockHeight;
    
      for x := XStart to XEnd do
      begin
        if m.CollisionInfo.Collidable[y][x] = true then
        begin
          if SpriteRectCollision(spr, 
               x * m.MapInfo.BlockWidth, 
               yCache, 
               m.MapInfo.BlockWidth, 
               m.MapInfo.BlockHeight) then
          begin
            result := true;
            exit;
          end;
        end;
      end;
    end;
  end;

  function BruteForceDetectionComponent(m : Map; var spr: Sprite; xOffset, yOffset: LongInt): Boolean;
  begin
    spr.x := spr.x + xOffset;
    spr.y := spr.y + yOffset;

    if BruteForceDetection(m, spr) then
    begin
      result := true;
    end
    else
      result := false;

    spr.x := spr.x - xOffset;
    spr.y := spr.y - yOffset;
  end;

  procedure MoveOut(sprt: Sprite; movement: Vector; x, y, width, height: LongInt);
  var
    kickVector: Vector;
    sprRect, tgtRect : Rectangle;
  begin
    sprRect := RectangleFrom(sprt);
    tgtRect := RectangleFrom(x, y, width, height);
  
    kickVector := VectorOutOfRectFromRect(sprRect, tgtRect, movement);
  
    MoveSprite(sprt, kickVector);
  end;

  function GetPotentialCollisions(m: Map; spr: Sprite): Rectangle;
    function GetBoundingRectangle() : Rectangle;
    var
      startPoint, endPoint: Rectangle;
      startX, startY, endX, endY : LongInt;
    begin
      startPoint := RectangleFrom(
                      round( ((spr.x - spr.movement.x) / m.MapInfo.BlockWidth) - 1) * m.MapInfo.BlockWidth,
                      round( ((spr.y - spr.movement.y) / m.MapInfo.BlockHeight) -1) * m.MapInfo.BlockHeight,
                      (round( spr.width / m.MapInfo.BlockWidth) + 2) * m.MapInfo.BlockWidth,
                      (round( spr.height / m.MapInfo.BlockHeight) + 2) * m.MapInfo.BlockHeight);
                    
      endPoint := RectangleFrom(  round(((spr.x + spr.width) / m.MapInfo.BlockWidth) - 1) * m.MapInfo.BlockWidth,
                      round(((spr.y + spr.height) / m.MapInfo.BlockHeight) - 1) * m.MapInfo.BlockHeight,
                      (round(spr.width / m.MapInfo.BlockWidth) + 2) * m.MapInfo.BlockWidth,
                      (round(spr.height / m.MapInfo.BlockHeight) + 2) * m.MapInfo.BlockHeight);
    
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
    
      result := RectangleFrom( startX, startY, endX - startX, endY - startY);
      
      //Debug Info              
      //DrawRectangle(ColorYellow, startPoint.x, startPoint.y, startPoint.width, startPoint.height);
      //DrawRectangle(ColorWhite, endPoint.x, endPoint.y, endPoint.width, endPoint.height);
      //DrawRectangle(ColorGreen, result.x, result.y, result.width, result.height);
    end;
  var
    //Respresents the Rectangle that encompases both the Current and Previous positions of the Sprite.
    searchRect : Rectangle;
  begin
  
    //Gets the Bounding Collision Rectangle
    searchRect := GetBoundingRectangle();
    result := searchRect;
    
  end;

  function WillCollideOnSide(m: Map; spr: Sprite): CollisionSide;
  type
    Collisions = record
      Top, Bottom, Left, Right: Boolean;
    end;
  var
    col : Collisions;
  begin
    col.Right   := (spr.movement.x > 0) and BruteForceDetectionComponent(m, spr, spr.width, 0);
    col.Left    := (spr.movement.x < 0) and BruteForceDetectionComponent(m, spr, -spr.width, 0);
    col.Top   := (spr.movement.y < 0) and BruteForceDetectionComponent(m, spr, 0, -spr.height);
    col.Bottom  := (spr.movement.y > 0) and BruteForceDetectionComponent(m, spr, 0, spr.height);

    if col.Right and col.Bottom then result := BottomRight
    else if col.Left and col.Bottom then result := BottomLeft
    else if col.Right and col.Top then result := TopRight
    else if col.Left and col.Top then result := TopLeft
    else if col.Left then result := Left
    else if col.Right then result := Right
    else if col.Top then result := Top
    else if col.Bottom then result := Bottom
    else result := None;
  end;

  procedure MoveSpriteOutOfTile(m: Map; spr: Sprite; x, y: LongInt);
  begin
    if m = nil then raise Exception.Create('No Map supplied (nil)');
    if spr = nil then raise Exception.Create('No Sprite suppled (nil)');
    if (x < 0 ) or (x >= m.mapInfo.mapWidth) then raise Exception.Create('x is outside the bounds of the map');
    if (y < 0 ) or (y >= m.mapInfo.mapWidth) then raise Exception.Create('y is outside the bounds of the map');
        
    MoveOut(spr, spr.movement, x * m.MapInfo.BlockWidth, y * m.MapInfo.BlockHeight, m.MapInfo.BlockWidth, m.MapInfo.BlockHeight);
  end;


  function SpriteHasCollidedWithMapTile(m: Map; spr: Sprite): Boolean; overload;
  var
    x, y : LongInt;
  begin
    result := SpriteHasCollidedWithMapTile(m,spr, x, y);
  end;

  function SpriteHasCollidedWithMapTile(m: Map; spr: Sprite; out collidedX, collidedY: LongInt): Boolean; overload;
  var
    y, x, yCache, dy, dx, i, j, initY, initX : LongInt;
    xStart, yStart, xEnd, yEnd : LongInt;
    rectSearch : Rectangle;
    side : CollisionSide;
  begin
    result := false;
    if m = nil then raise Exception.Create('No Map supplied (nil)');
    if spr = nil then raise Exception.Create('No Sprite suppled (nil)');
  
    rectSearch := GetPotentialCollisions(m, spr);
    side := GetSideForCollisionTest(spr.movement);
  
    yStart := round(rectSearch.y / m.MapInfo.BlockHeight);
    yEnd := round((rectSearch.y + rectSearch.height) / m.MapInfo.BlockHeight);
    xStart := round(rectSearch.x / m.MapInfo.BlockWidth);
    xEnd := round((rectSearch.x + rectSearch.width) / m.MapInfo.BlockWidth);
  
    if yStart < 0 then yStart := 0;
    if yStart >= m.MapInfo.MapHeight then exit;
    if yEnd < 0 then exit;
    if yEnd >= m.MapInfo.MapHeight then yEnd := m.MapInfo.MapHeight - 1;
      
    if xStart < 0 then xStart := 0;
    if xStart >= m.MapInfo.MapWidth then exit;
    if xEnd < 0 then exit;
    if xEnd >= m.MapInfo.MapWidth then xEnd := m.MapInfo.MapWidth - 1;
  
    result := false;
  
    case side of
      TopLeft: begin dy := 1; dx := 1; initY := yStart; initX := xStart; end;
      TopRight: begin dy := 1; dx := -1; initY := yStart; initX := xEnd; end;
      BottomLeft: begin dy := -1; dx := 1; initY := yEnd; initX := xStart; end;
      BottomRight: begin dy := -1; dx := -1; initY := yEnd; initX := xEnd; end;
      Top: begin dy := 1; dx := 1; initY := yStart; initX := xStart; end;
      Bottom: begin dy := -1; dx := 1; initY := yEnd; initX := xStart; end;
      Left: begin dy := 1; dx := 1; initY := yStart; initX := xStart; end;
      Right: begin dy := 1; dx := -1; initY := yStart; initX := xEnd; end;
      else
      begin dy := 1; dx := 1; initY := yStart; initX := xStart; end;
    end;
  
    for i := yStart to yEnd do
    begin
      y := initY + (i - yStart) * dy;
      yCache := y * m.MapInfo.BlockHeight;
      for j := xStart to xEnd do
      begin
        x := initX + (j - xStart) * dx;
        if m.CollisionInfo.Collidable[y][x] = true then
        begin     
          if SpriteRectCollision(spr, 
               x * m.MapInfo.BlockWidth, 
               yCache, 
               m.MapInfo.BlockWidth, 
               m.MapInfo.BlockHeight) then
          begin
            result := true;
            collidedX := x;
            collidedY := y;
            exit;
          end;
        end;
      end;
    end;
  
    collidedX := -1;
    collidedY := -1;
    
  end;

  procedure FreeMap(var m: Map);
  begin
    FreeBitmap(m.Tiles.bitmaps[0]);
    FreeSprite(m.Tiles);
    Dispose(m);
    m := nil;
  end;

  function CollisionWithMap(m : Map; spr : Sprite; const vec: Vector): CollisionSide;
  var
    x, y: LongInt;
    temp: Vector;
  begin
    result := None;
    temp := spr.movement;
    spr.movement := vec;
    if sgTileMap.SpriteHasCollidedWithMapTile(m, spr, x, y) then
    begin
      MoveSpriteOutOfTile(m, spr, x, y);
      result := WillCollideOnSide(m, spr);
    end;
    spr.movement := temp;
  end;

  function MapWidth(m : Map): LongInt;
  begin
    result := m.MapInfo.MapWidth;
  end;

  function MapHeight(m : Map): LongInt;
  begin
    result := m.MapInfo.MapHeight;
  end;

  function BlockWidth(m : Map): LongInt;
  begin
    result := m.MapInfo.BlockWidth;
  end;

  function BlockHeight(m : Map): LongInt;
  begin
    result := m.MapInfo.BlockHeight;
  end;

  function GapX(m : Map): LongInt;
  begin
    result := m.MapInfo.GapX;
  end;

  function GapY(m : Map): LongInt;
  begin
    result := m.MapInfo.GapY;
  end;

  function StaggerX(m : Map): LongInt;
  begin
    result := m.MapInfo.StaggerX;
  end;

  function StaggerY(m : Map): LongInt;
  begin
    result := m.MapInfo.StaggerY;
  end;

  //Determines whether the specified point is within the tile provided
  function IsPointInTile(point: Point2D; x, y: LongInt; m : Map): Boolean;
  var
    tri1, tri2 : Triangle;
  begin
    result := false;

    if m.MapInfo.Isometric then
    begin
      //Create Triangles
      tri1 := TriangleFrom(x, y + m.MapInfo.BlockHeight / 2, x + m.MapInfo.BlockWidth / 2, y, x + m.MapInfo.BlockWidth / 2, y + m.MapInfo.BlockHeight);
        tri2 := TriangleFrom(x + m.MapInfo.BlockWidth, y + m.MapInfo.BlockHeight / 2, x + m.MapInfo.BlockWidth / 2, y, x + m.MapInfo.BlockWidth / 2, y + m.MapInfo.BlockHeight);
        if PointInTriangle(point, tri1) or PointInTriangle(point, tri2) then result := true;
    end
    else
    begin
      result := PointInRect(point, x, y, m.MapInfo.BlockWidth, m.MapInfo.BlockHeight);
    end;
  end;


  //This function will get the tile that is under the given point2D, isometric maps are taken into consideration.
  //The function will return a Tile, which gives the x,y Index that the tile occurs in the map structure, also
  //the top-right corner of the tile, and the 4 points that construct the tile.
  //Note that for Isometric tiles, the 4 points will form a diamond.
  function GetTileFromPoint(point: Point2D; m: Map): Tile;
  var
    x, y, tx, ty : LongInt;
  
  begin
    //Returns -1,-1 if no tile has this point
    result.xIndex := -1;
    result.yIndex := -1;
    result.topCorner := PointAt(0,0);
    result.PointA := PointAt(0,0);
    result.PointB := PointAt(0,0);
    result.PointC := PointAt(0,0);
    result.PointD := PointAt(0,0);

    for y := 0  to m.MapInfo.MapHeight - 1 do
    begin
      //Isometric Offset for Y
      if (m.MapInfo.Isometric = true) then 
        ty := y * m.MapInfo.StaggerY
      else 
        ty := y * m.MapInfo.BlockHeight;  
  
      for x := 0  to m.MapInfo.MapWidth - 1  do
      begin
      
        //Isometric Offset for X
        if (m.MapInfo.Isometric = true) then
        begin
          tx := x * m.MapInfo.GapX;
          if ((y MOD 2) = 1) then
            tx := tx + m.MapInfo.StaggerX;
        end
        else
          tx := x * m.MapInfo.BlockWidth;
        
        if IsPointInTile(point, tx, ty, m) then
        begin
          if (m.MapInfo.Isometric = true) then
          begin
            result.xIndex := x;
            result.yIndex := y;
            result.topCorner := PointAt(tx, ty);
            result.PointA := PointAt(tx, ty + m.MapInfo.BlockHeight / 2);
            result.PointB := PointAt(tx + m.MapInfo.BlockWidth / 2, ty);
            result.PointC := PointAt(tx + m.MapInfo.BlockWidth / 2, ty + m.MapInfo.BlockHeight);
            result.PointD := PointAt(tx + m.MapInfo.BlockWidth, ty + m.MapInfo.BlockHeight / 2);
          exit;
          end
          else
          begin
            result.xIndex := x;
            result.yIndex := y;
            result.topCorner := PointAt(tx,ty);
            result.PointA := PointAt(tx, ty);
            result.PointB := PointAt(tx + m.MapInfo.BlockWidth, ty);
            result.PointC := PointAt(tx, ty + m.MapInfo.BlockHeight);
            result.PointD := PointAt(tx + m.MapInfo.BlockWidth, ty + m.MapInfo.BlockHeight);
          end;
        end;
      end;
    end;

  end;

  //Returns the Event of the tile at the given index's.
  //Note, that if the tile does not have an event, this function will return Event(-1)
  function GetEventAtTile(m : Map; xIndex, yIndex: LongInt): Event;
  var
    i, j: LongInt;
  begin
    result := Event(-1);

    for i := 0  to 23 do
    begin
      if (Length(m.EventInfo[i]) > 0) then
      begin
        for j := 0 to (Length(m.EventInfo[i]) - 1) do
        begin
          if (m.EventInfo[i][j].x = xIndex) and (m.EventInfo[i][j].y = yIndex) then begin result := Event(i); exit; end;
        end;
      end;  
    end;
  end;
  
end.