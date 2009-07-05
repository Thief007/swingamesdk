//=============================================================================
// sgPhysics.pas
//=============================================================================
//
// Responsible for performing collisions and vector maths.
//
// Change History:
//
// Version 3.0:
// - 2009-06-29: Andrew : Removed all need for Collision Side
//                      : Changed to use Circle Type
// - 2009-06-26: Andrew : Added CircleRectCollision
//                      : Added CircleLinesCollision
//                      : Added CircleCircleCollision
//                      : Added CollideCircleCircle
// - 2009-06-25: Andrew : Moved VectorInRect, VectorFrom... functions to Geometry
// - 2009-06-24: Andrew : Added BitmapPointCollision 
// - 2009-06-23: Clinton: Renamed VectorFrom to VectorFrom
//                      : Renamed HaveSpritesCollided to SpritesCollided
//                      : Move Vector/Angle/Matrix code to sgMath.pas unit
// - 2009-06-17: Clinton: Comment cleanup (moved to interface) and new comments
//                      : General parameter name cleanup/normalisation
//                      : Renamed GetUnitVector to UnitVector
//                      : Optimised LimitMagnitude (see renamed)
//                      : Optimised VectorNormal
//                      : Renamed GetVectorFromAngle to VectorFromAngle
//                      : Renamed MultiplyVector to VectorMultiply
//                      : Renamed Multiply to MatrixMultiply
//                      : Renamed CalculateVectorFromTo to VectorFromTo
//                      : Renamed PointToVector to VectorFromPoint
//                      : Renamed CalculateAngleBetween to CalculateAngle
//                      : Renamed LimitMagnitude to LimitVector
//                      : Renamed VectorIsWithinRect to VectorInRect
//                      : Renamed RectangleHasCollidedWithLine to RectLineCollision
//                      : Renamed IsZeroVector to VectorIsZero
//                      : Renamed HasSpriteCollidedWithRect to SpriteRectCollision
//                      : Renamed HasSpriteCollidedWithBitmap to SpriteBitmapCollision
//                      : Renamed bounded (params) to bbox (or BBox in method)
//                      : Renamed HasBitmapCollidedWithRect with BitmapRectCollision
//                      : Renamed HasBitmapPartCollidedWithRect to BitmapPartRectCollision
//                      : Renamed VectorFromPointToRectangle to VectorFromPointToRect
//                      : Renamed CircleHasCollidedWithLine to CircleLineCollision
//                      : Removed VectorCollision (was renamed to CircleCollision)
//                      : Renamed CircleCollisionWithLine to CollideCircleLine
//                      : Renamed CircularCollision to CollideCircles
//                      : Renamed Magnitude to VectorMagnitude
//                      : Optimised VectorOutOfCircleFromPoint (slightly)
// 
// - 2009-06-15: Andrew: Added meta tags
//
// Version 2.0:
// - 2008-12-10: Andrew: Moved types to Core
//
// Version 1.1:
// - 2008-01-30: Andrew: Fixed rectangle collision with bitmap
//                     : Fixed vector out for 0, 90, 180, 270 deg
//                     : Fixed GetSideForCollisionTest for same deg of movement
// - 2008-01-25: Andrew: Fixed compiler hints
// - 2008-01-22: Andrew: Correct Circular Collision to handle situations where 
//                       the balls have overlaped.
// - 2008-01-21: Andrew: General refactoring, adding new collision routines
//               using Rectangle and Point2D.
// - 2008-01-18: Aki, Andrew, Stephen: Refactor
//
// Version 1.0:
// - Various
//=============================================================================

{$I sgTrace.inc}

/// @module Physics
/// @static
unit sgPhysics;

//=============================================================================
interface
//=============================================================================

  uses sgTypes;

  //---------------------------------------------------------------------------
  // Sprite <-> Sprite Collision Detection
  //---------------------------------------------------------------------------

  /// Returns ``true`` if the specifed `Sprites` (``s1`` and ``s2``) have
  /// collided. Will use simple bounding box tests first, and low-level pixel
  /// tests if needed.
  /// @lib
  function SpritesCollided(s1, s2: Sprite): Boolean;

  //---------------------------------------------------------------------------
  // Sprite <-> Rectangle Collision Detection
  //---------------------------------------------------------------------------

  /// Determined if a sprite has collided with a given rectangle. The rectangles
  /// coordinates are expressed in "world" coordinates.
  ///
  /// @param s The sprite to check
  /// @param x The x location of the rectangle
  /// @param y The y location of the rectangle
  /// @param width The width of the rectangle
  /// @param height The height of the rectangle
  /// @returns True if the sprite collides with the rectangle
  ///
  /// @lib
  /// @class Sprite
  /// @method RectCollision
  function SpriteRectCollision(s: Sprite; x, y: Single; width, height: LongInt): Boolean; overload;

  /// @lib SpriteRectangleCollision
  /// @class Sprite
  /// @overload RectCollision RectangleCollision
  function SpriteRectCollision(s: Sprite; const rect: Rectangle): Boolean; overload;


  //---------------------------------------------------------------------------
  // Sprite <-> Bitmap Collision Detection
  //---------------------------------------------------------------------------

  /// Determines if the `Sprite` ``s`` has collided with the bitmap ``bmp``.
  /// The ``x`` and ``y`` values specify the world location of the bitmap.
  /// If ``bbox`` is true only simple bounding box testing is used, otherwise
  /// pixel level testing is used.
  ///
  /// @lib SpriteBitmapBBoxCollision
  /// @class Sprite
  /// @overload BitmapCollision BitmapBBoxCollision
  function SpriteBitmapCollision(s: Sprite; bmp: Bitmap; x, y: Single; bbox: Boolean): Boolean; overload;

  /// Determines if the `Sprite` ``s`` has collided with the bitmap ``bmp``.
  /// The ``pt`` (`Point2D`) value specifies the world location of the bitmap.
  /// If ``bbox`` is true only simple bounding box testing is used, otherwise
  /// pixel level testing is used.
  ///
  /// @lib SpriteBitmapAtPointBBoxCollision
  /// @class Sprite
  /// @overload BitmapCollision BitmapAtPointBBoxCollision
  function SpriteBitmapCollision(s: Sprite; bmp: Bitmap; const pt: Point2D; bbox: Boolean): Boolean; overload;

  /// Determines if the `Sprite` ``s`` has collided with the bitmap ``bmp`` using
  /// pixel level testing if required.
  /// The ``x`` and ``y`` values specify the world location of the bitmap.
  ///
  /// @lib SpriteBitmapBBoxCollision(s, bmp, x, y, False)
  /// @uname SpriteBitmapCollision
  /// @class Sprite
  /// @method BitmapCollision
  function SpriteBitmapCollision(s: Sprite; bmp: Bitmap; x, y: Single): Boolean; overload;

  /// Determines if the `Sprite` ``s`` has collided with the bitmap ``bmp`` using
  /// pixel level testing if required.
  /// The ``pt`` (`Point2D`) value specifies the world location of the bitmap.
  ///
  /// @lib SpriteBitmapAtPointBBoxCollision(s, bmp, pt, False)
  /// @uname SpriteBitmapAtPointCollision
  /// @class Sprite
  /// @overload BitmapCollision BitmapAtPointCollision
  function SpriteBitmapCollision(s: Sprite; bmp: Bitmap; const pt: Point2D): Boolean; overload;

  /// Determines if the `Sprite` ``s`` has collided with a part (``rect``) of
  /// the bitmap ``bmp`` using pixel level testing if required.
  /// The ``pt`` (`Point2D`) value specifies the world location of the bitmap.
  /// If ``bbox`` is true only simple bounding box testing is used, otherwise
  /// pixel level testing is used.
  ///
  /// @lib SpriteBitmapPartCollision
  /// @class Sprite
  /// @overload BitmapCollision BitmapPartCollision
  function SpriteBitmapCollision(s: Sprite; bmp: Bitmap; const pt: Point2D; const part: Rectangle; bbox: Boolean): Boolean; overload;


  //---------------------------------------------------------------------------
  // Bitmap <-> Rectangle Collision Tests
  //---------------------------------------------------------------------------

  /// Returns True if the bitmap ``bmp`` has collided with the rectangle specified.
  /// The ``x`` and ``y`` values specify the world location of the bitmap.
  /// The rectangles world position (``rectX`` and ``rectY``) and size
  /// (``rectWidth`` and ``rectHeight``) need to be provided.
  /// If ``bbox`` is true only simple bounding box testing is used, otherwise
  /// pixel level testing is used.
  ///
  /// @lib BitmapRectBBoxCollision
  /// @class Bitmap
  /// @overload RectCollision RectBBoxCollision
  function BitmapRectCollision(bmp: Bitmap; x, y: LongInt; bbox: Boolean; rectX, rectY, rectWidth, rectHeight: LongInt): Boolean; overload;

  /// Returns True if the bitmap ``bmp`` has collided with the rectangle specified.
  /// The ``x`` and ``y`` values specify the world location of the bitmap.
  /// The rectangle ``rect`` needs to be provided (in world coordinates).
  /// If ``bbox`` is true only simple bounding box testing is used, otherwise
  /// pixel level testing is used.
  ///
  /// @lib BitmapRectangleBBoxCollision
  /// @class Bitmap
  /// @overload RectCollision RectangleBBoxCollision
  function BitmapRectCollision(bmp: Bitmap; x, y: LongInt; bbox: Boolean; const rect: Rectangle): Boolean; overload;

  /// Returns True if the bitmap ``bmp`` has collided with the rectangle
  /// specified using pixel level testing if required.
  /// The ``x`` and ``y`` values specify the world location of the bitmap.
  /// The rectangles world position (``rectX`` and ``rectY``) and size
  /// (``rectWidth`` and ``rectHeight``) need to be provided.
  ///
  /// @lib BitmapRectBBoxCollision(bmp, x, y, False, rectX, rectY, rectWidth, rectHeight)
  /// @uname BitmapRectCollision
  /// @class Bitmap
  /// @method RectCollision
  function BitmapRectCollision(bmp: Bitmap; x, y, rectX, rectY, rectWidth, rectHeight: LongInt): Boolean; overload;

  /// Returns True if the bitmap ``bmp`` has collided with the rectangle
  /// specified using pixel level testing if required.
  /// The ``x`` and ``y`` values specify the world location of the bitmap.
  /// The rectangle ``rect`` needs to be provided in world coordinates.
  ///
  /// @lib BitmapRectangleBBoxCollision(bmp, x, y, False, rect)
  /// @uname BitmapRectangleCollision
  /// @class Bitmap
  /// @overload RectCollision RectangleCollision
  function BitmapRectCollision(bmp: Bitmap; x, y: LongInt; const rect: Rectangle): Boolean; overload;

  /// Returns True if a ``part`` (rectangle) of the bitmap ``bmp`` has collided
  /// with the rectangle (``rect``) specified.
  /// The ``x`` and ``y`` values specify the world location of the bitmap.
  /// The rectangle ``rect`` needs to be provided in world coordinates.
  /// If ``bbox`` is true only simple bounding box testing is used, otherwise
  /// pixel level testing is used.
  ///
  /// @lib
  /// @class Bitmap
  /// @method PartRectCollision
  function BitmapPartRectCollision(bmp: Bitmap; x, y: LongInt; const part: Rectangle; bbox: Boolean; const rect: Rectangle): Boolean;


  //---------------------------------------------------------------------------
  // Bitmap <-> Point
  //---------------------------------------------------------------------------

  /// Returns True if a point (``ptX``,``ptY``) is located within the bitmap
  /// ``bmp`` when it is drawn at ``x``,``y``.
  /// The ``x`` and ``y`` values specify the world location of the bitmap.
  /// The ``ptX`` and ``ptY`` needs to be provided in world coordinates.
  /// If ``bbox`` is true only simple bounding box testing is used, otherwise
  /// pixel level testing is used.
  ///
  /// @lib BitmapPointBBoxCollision
  /// @class Bitmap
  /// @overload PointCollision PointBBoxCollision
  function BitmapPointCollision(bmp: Bitmap; x, y: LongInt; bbox: Boolean; ptX, ptY: Single): Boolean; overload;
  
  /// Returns True if a point (``ptX``,``ptY``) is located within the bitmap
  /// ``bmp`` when it is drawn at ``x``,``y``, using pixel level collisions.
  /// The ``x`` and ``y`` values specify the world location of the bitmap.
  /// The ``ptX`` and ``ptY`` needs to be provided in world coordinates.
  ///
  /// @lib BitmapPointBBoxCollision(bmp, x, y, False, ptX, ptY)
  /// @uname BitmapPointCollision
  /// @class Bitmap
  /// @method PointCollision
  function BitmapPointCollision(bmp: Bitmap; x, y: LongInt; ptX, ptY: Single): Boolean; overload;
  
  /// Returns True if a point (``pt``) is located within the bitmap
  /// ``bmp`` when it is drawn at ``x``,``y``.
  /// The ``x`` and ``y`` values specify the world location of the bitmap.
  /// The point ``pt`` needs to be provided in world coordinates.
  /// If ``bbox`` is true only simple bounding box testing is used, otherwise 
  /// pixel level testing is used.
  ///  
  /// @lib BitmapPointPtCollisionBBox
  /// @class Bitmap
  /// @overload PointCollision PointPtBBoxCollision
  function BitmapPointCollision(bmp: Bitmap; x, y: LongInt; bbox: Boolean; const pt: Point2D): Boolean; overload;
  
  /// Returns True if a point (``pt``) is located within the bitmap
  /// ``bmp`` when it is drawn at ``x``,``y``, using pixel level collisions.
  /// The ``x`` and ``y`` values specify the world location of the bitmap.
  /// The point ``pt`` needs to be provided in world coordinates.
  ///
  /// @lib BitmapPointPtCollisionBBox(bmp, x, y, False, pt)
  /// @uname BitmapPointPtCollision
  /// @class Bitmap
  /// @overload PointCollision PointPtCollision
  function BitmapPointCollision(bmp: Bitmap; x, y: LongInt; const pt: Point2D): Boolean; overload;
  
  /// Returns True if a point (``ptX``,``ptY``) is located within the ``part`` (rectangle) of the bitmap
  /// ``bmp`` when it is drawn at ``x``,``y``, using pixel level collisions. For bounding box collisions
  /// use the rectangle collision functions.
  /// The ``x`` and ``y`` values specify the world location of the bitmap.
  /// The ``ptX`` and ``ptY`` needs to be provided in world coordinates.
  ///
  /// @lib
  function BitmapPointCollisionPart(bmp: Bitmap; x, y: LongInt; const part: Rectangle; ptX, ptY: Single): Boolean; overload;
  
  /// Returns True if a point (``pt``) is located within the ``part`` (rectangle) of the bitmap
  /// ``bmp`` when it is drawn at ``x``,``y``, using pixel level collisions. For bounding box collisions
  /// use the rectangle collision functions.
  /// The ``x`` and ``y`` values specify the world location of the bitmap.
  /// The point ``pt`` needs to be provided in world coordinates.
  ///  
  /// @lib  BitmapPointXYCollisionPart
  function BitmapPointCollisionPart(bmp: Bitmap; x, y: LongInt; const part: Rectangle; const pt: Point2D): Boolean; overload;


  //---------------------------------------------------------------------------
  // Bitmap <-> Bitmap Collision Tests
  //---------------------------------------------------------------------------

  /// Returns True if two bitmaps have collided using per pixel testing if required.
  /// The ``x`` and ``y`` parameters specify the world location of the bitmaps (``bmp1`` and ``bmp2``).
  ///
  /// @lib BitmapsBBoxCollided(bmp1, x1, y1, False, bmp2, x2, y2, False)
  /// @uname BitmapsCollided
  /// @class Bitmap
  /// @method BitmapCollision
  function BitmapsCollided(bmp1: Bitmap; x1, y1: LongInt; bmp2: Bitmap; x2, y2: LongInt): Boolean; overload;
  
  /// Returns True if two bitmaps have collided using per pixel testing if required. 
  /// The ``pt`` (`Point2D`) parameters specify the world location of the bitmaps (``bmp1`` and ``bmp2``).
  ///
  /// @lib BitmapsBBoxAtPointsCollided(bmp1, pt1, False, bmp2, pt2, False)
  /// @uname BitmapsAtPointsCollided
  /// @class Bitmap
  /// @overload BitmapCollision BitmapAtPointCollision
  function BitmapsCollided(bmp1: Bitmap; const pt1: Point2D; bmp2: Bitmap; const pt2: Point2D): Boolean; overload;
  
  /// Returns True if two bitmaps have collided. 
  /// The ``x`` and ``y`` parameters specify the world location of the bitmaps (``bmp1`` and ``bmp2``). 
  /// If a ``bbox`` parameter is true then only a simple (quick) bounding box test is 
  /// used, otherwise a longer per pixel check is used (if required) in the collision region.
  ///
  /// @lib BitmapsBBoxCollided
  /// @class Bitmap
  /// @overload BitmapCollision BitmapBBoxCollision
  function BitmapsCollided(bmp1: Bitmap; x1, y1: LongInt; bbox1: Boolean; bmp2: Bitmap; x2, y2: LongInt; bbox2: Boolean): Boolean; overload;
  
  /// Returns True if two bitmaps have collided using per pixel testing if required. 
  /// The ``pt`` (`Point2D`) parameters specify the world location of the bitmaps (``bmp1`` and ``bmp2``). 
  /// If a ``bbox`` parameter is true then only a simple (quick) bounding box test is 
  /// used, otherwise a longer per pixel check is used (if required) in the collision region. 
  ///
  /// @lib BitmapsBBoxAtPointsCollided
  /// @class Bitmap
  /// @overload BitmapCollision BitmapAtPointsBBoxCollision
  function BitmapsCollided(bmp1: Bitmap; const pt1: Point2D; bbox1: Boolean; bmp2: Bitmap; const pt2: Point2D; bbox2: Boolean): Boolean; overload;
  
  /// Returns True if the specified parts (``part1`` and ``part2`` rectangles) of the two 
  /// bitmaps (``bmp1`` and ``bmpt2``) have collided, using pixel level collision if required. 
  /// The ``pt`` (`Point2D`) parameters specify the world location of the bitmaps (``bmp1`` and ``bmp2``). 
  ///
  /// @lib BitmapsPartsBBoxCollided(bmp1, pt1, part1, False, bmp2, pt2, part2, False)
  /// @uname BitmapPartsCollided
  /// @class Bitmap
  /// @overload BitmapCollision BitmapPartCollision
  function BitmapsCollided(bmp1: Bitmap; const pt1: Point2D; const part1: Rectangle; bmp2: Bitmap; const pt2: Point2D; const part2: Rectangle): Boolean; overload;
  
  /// Returns True if the specified parts (``part1`` and ``part2`` rectangles) 
  /// of the two bitmaps (``bmp1`` and ``bmpt2``) have collided. 
  /// The ``pt`` (`Point2D`) parameters specify the world location of the bitmaps. 
  /// If a ``bbox`` parameter is true then only a simple (quick) bounding box test is
  /// used, otherwise a slower per pixel test is used (if required) in the collision region. 
  ///
  /// @lib BitmapsPartsBBoxCollided
  /// @class Bitmap
  /// @overload BitmapCollision BitmapPartsBBoxCollision
  function BitmapsCollided(bmp1: Bitmap; const pt1: Point2D; const part1: Rectangle; bbox1: Boolean; bmp2: Bitmap; const pt2: Point2D; const part2: Rectangle; bbox2: Boolean): Boolean; overload;


  //---------------------------------------------------------------------------
  // Geometry Collision Tests
  //---------------------------------------------------------------------------

  /// Returns True if the Circle collised with rectangle `rect`.
  ///
  /// @lib
  function CircleRectCollision(const c: Circle; const rect: Rectangle): Boolean;

  /// Returns True if the circle has collided with any of the lines from the `rect` rectangle.
  ///
  /// @lib
  function CircleLinesCollision(const c: Circle; const lines: LinesArray): Boolean;

  /// Returns True if the circles have collided.
  ///
  /// @lib
  function CircleCircleCollision(const c1, c2: Circle): Boolean;
  
  /// Returns True if the Circle has collided with the Triangle `tri`.
  ///
  /// @lib
  function CircleTriangleCollision(const c: Circle; const tri: Triangle): Boolean;
  
  //---------------------------------------------------------------------------
  // Sprite / Geometry Collision Tests
  //---------------------------------------------------------------------------
  
  /// Returns True if the `Sprite` ``s``, represented by a bounding circle, has 
  /// collided with a ``line``. The diameter for the bounding circle is 
  /// based on the sprites width or height value -- whatever is largest.
  ///
  /// @lib SpriteCircleLineCollision
  function CircleLineCollision(s: Sprite; const line: LineSegment): Boolean;
  
  /// Returns True if the bounding rectangle of the `Sprite` ``s`` has collided 
  /// with the ``line`` specified.
  ///
  /// @lib SpriteRectLineCollision
  function RectLineCollision(s: Sprite; const line: LineSegment): Boolean; overload;

  /// Returns True if the rectangle ``rect`` provided has collided with the
  /// ``line``.
  ///
  /// @lib RectLineCollision
  function RectLineCollision(const rect: Rectangle; const line: LineSegment): Boolean; overload;


  //---------------------------------------------------------------------------
  // Side to check based on movement direction
  //---------------------------------------------------------------------------

  /// @lib
  function GetSideForCollisionTest(const movement: Vector): CollisionSide;

  
  //---------------------------------------------------------------------------
  // Collision Effect Application ( angle + energy/mass transfer)
  //---------------------------------------------------------------------------
  
  /// @lib
  procedure CollideCircleLine(s: Sprite; const line: LineSegment);
  
  /// Collide sprite ``s`` with the stationary circle ``c``.
  ///
  /// @lib
  procedure CollideCircleCircle(s: Sprite; const c: Circle);

  /// @lib
  procedure CollideCircleRectangle(s: Sprite; const rect: Rectangle); overload;
  
  /// @lib
  procedure CollideCircles(s1, s2: Sprite);
  
  ///@lib
  procedure CollideCircleLines(s: Sprite; const lines: LinesArray);
  
  // /// @lib
  // procedure CollideCircleRectangleBounds(s: Sprite; const rect: Rectangle);



//=============================================================================
implementation
//=============================================================================

  uses
    SysUtils, {Math, Classes,} sgTrace,
    sgCore, sgGraphics, sgCamera, sgGeometry, sgSprites;


  //---------------------------------------------------------------------------

  function BitmapPartRectCollision(bmp: Bitmap; x, y: LongInt; const part: Rectangle; bbox: Boolean; const rect: Rectangle): Boolean;
  var
    i, j: LongInt;
    left1, right1, left2, right2, overRight, overLeft: LongInt;
    top1, bottom1, top2, bottom2, overTop, overBottom: LongInt;
    yPixel1, xPixel1: LongInt;
  begin
    result := RectanglesIntersect(RectangleFrom(x, y, part.width, part.height), rect);
    if  bbox or (not result) then exit;

    //reset result
    result := false;

    left1 := x;
    right1 := x + part.width - 1;
    top1 := y;
    bottom1 := y + part.height - 1;

    left2 := Round(rect.x);
    right2 := Round(rect.x) + rect.width - 1;
    top2 := Round(rect.y);
    bottom2 := Round(rect.y) + rect.height - 1;

    if bottom1 > bottom2 then overBottom := bottom2
    else overBottom := bottom1;

    if top1 < top2 then overTop := top2
    else overTop := top1;

    if right1 > right2 then overRight := right2
    else overRight := right1;

    if left1 < left2 then overLeft := left2
    else overLeft := left1;

    for i := overTop to overBottom do
    begin
      yPixel1 := i - top1 + Round(part.y);

      for j := overLeft to overRight do
      begin
        xPixel1 := j - left1 + Round(part.x);

        if IsPixelDrawnAtPoint(bmp, xPixel1, yPixel1) then
        begin
          result := true;
          exit;
        end;
      end;
    end;    
  end;

  function BitmapRectCollision(bmp: Bitmap; x, y: LongInt; bbox: Boolean; const rect: Rectangle): Boolean; overload; {New for 1.2}
  begin
    result := BitmapPartRectCollision(bmp, x, y, RectangleFrom(0, 0, bmp), bbox, rect);
  end;
  
  function BitmapRectCollision(bmp: Bitmap; x, y: LongInt; bbox: Boolean; rectX, rectY, rectWidth, rectHeight: LongInt): Boolean; overload; {New for 1.2}
  begin
    result := BitmapRectCollision(bmp, x, y, bbox, RectangleFrom(rectX, rectY, rectWidth, rectHeight));
  end;

  function BitmapRectCollision(bmp: Bitmap; x, y, rectX, rectY, rectWidth, rectHeight: LongInt): Boolean; overload;
  begin
    result := BitmapRectCollision(bmp, x, y, false, RectangleFrom(rectX, rectY, rectWidth, rectHeight));
  end;

  function BitmapRectCollision(bmp: Bitmap; x, y: LongInt; const rect: Rectangle): Boolean; overload;
  begin
    result := BitmapRectCollision(bmp, x, y, false, rect);
  end;
  
  function SpriteRectCollision(s: Sprite; x, y: Single; width, height: LongInt): Boolean; overload;
  var
    bmp: Bitmap;
    offX1, offY1: LongInt;
  begin
    if s = nil then raise Exception.Create('The specified sprite is nil');

    if width < 0 then
    begin
      x := x + width;
      width := -width;
    end;
    
    if height < 0 then
    begin
      y := y + height;
      height := -height;
    end;
    
    if (width < 1) or (height < 1) then 
      raise Exception.Create('Rectangle width and height must be greater then 0');
    
    if s^.y + SpriteHeight(s) <= y then result := false
    else if s^.y >= y + height then result := false
    else if s^.x + SpriteWidth(s) <= x then result := false
    else if s^.x >= x + width then result := false
    else
    begin
      if not s^.usePixelCollision then result := true
      else
      begin
        if s^.spriteKind = AnimMultiSprite then
        begin
          offX1 := (s^.currentFrame mod s^.cols) * s^.width;
          offY1 := (s^.currentFrame - (s^.currentFrame mod s^.cols)) div s^.cols * s^.height;
          bmp := s^.bitmaps[0];
        end
        else
        begin
          bmp := s^.bitmaps[s^.currentFrame];
          offX1 := 0;
          offY1 := 0;
        end;
        result := BitmapPartRectCollision(
                    bmp, Round(s^.x), Round(s^.y), 
                    RectangleFrom(offX1, offY1, s^.width, s^.height),
                    s^.usePixelCollision = false, RectangleFrom(x, y, width, height));
      end;
    end;
  end;

  function SpriteRectCollision(s: Sprite; const rect: Rectangle): Boolean; overload;
  begin
    result := SpriteRectCollision(s, rect.x, rect.y, rect.width, rect.height);
  end;
  
  /// Performs a collision detection within two bitmaps at the given x, y
  /// locations. The bbox values indicate if each bitmap should use per
  /// pixel collision detection or a bbox collision detection. This version
  /// uses pixel based checking at all times.
  ///
  /// When both bitmaps are using bbox collision the routine checks to see
  /// if the bitmap rectangles intersect. If one is bbox and the other is
  /// pixel based the routine checks to see if a non-transparent pixel in the
  /// pixel based image intersects with the bounds of the bbox image. If
  /// both are pixel based, the routine checks to see if two non-transparent
  /// pixels collide.
  ///
  /// Note: Bitmaps do not need to actually be drawn on the screen.
  ///
  /// @param bmp1, bmp2: The bitmap images to check for collision
  /// @param x1, y1:        The x,y location of bmp 1
  /// @param bbox1:      Indicates if bmp1 should use bbox collision
  /// @param x2, y2:        The x,y location of bmp 2
  /// @param bbox2:      Indicates if bmp2 should use bbox collision
  ///
  /// @returns          True if the bitmaps collide.
  /// 
  function CollisionWithinBitmapImages(
             bmp1: Bitmap; x1, y1, w1, h1, offsetX1, offsetY1: LongInt; bbox1: Boolean;
             bmp2: Bitmap; x2, y2, w2, h2, offsetX2, offsetY2: LongInt; bbox2: Boolean
           ): Boolean; overload;
  var
    left1, left2, overLeft: LongInt;
    right1, right2, overRight: LongInt;
    top1, top2, overTop: LongInt;
    bottom1, bottom2, overBottom: LongInt;
    i, j, xPixel1, yPixel1, xPixel2, yPixel2: LongInt;
  begin
    if (bmp1 = nil) or (bmp2 = nil) then
      raise Exception.Create('One or both of the specified bitmaps are nil');
    
    if (w1 < 1) or (h1 < 1) or (w2 < 1) or (h2 < 1) then
      raise Exception.Create('Bitmap width and height must be greater then 0');
    
    result := false;
 
    left1 := x1;
    right1 := x1 + w1 - 1;
    top1 := y1;
    bottom1 := y1 + h1 - 1;

    left2 := x2;
    right2 := x2 + w2 - 1;
    top2 := y2;
    bottom2 := y2 + h2 - 1;

    if bottom1 > bottom2 then overBottom := bottom2
    else overBottom := bottom1;

    if top1 < top2 then overTop := top2
    else overTop := top1;

    if right1 > right2 then overRight := right2
    else overRight := right1;

    if left1 < left2 then overLeft := left2
    else overLeft := left1;

    for i := overTop to overBottom do
    begin
      yPixel1 := i - top1 + offsetY1;
      yPixel2 := i - top2 + offsetY2;

      for j := overLeft to overRight do
      begin
        xPixel1 := j - left1 + offsetX1;
        xPixel2 := j - left2 + offsetX2;

        if (bbox1 or IsPixelDrawnAtPoint(bmp1, xPixel1, yPixel1))
           and (bbox2 or IsPixelDrawnAtPoint(bmp2, xPixel2, yPixel2)) then
        begin
          result := true;
          exit;
        end;
      end;
    end;
  end;

  function CollisionWithinBitmapImages(bmp1: Bitmap; x1, y1: LongInt; bbox1: Boolean; bmp2: Bitmap; x2, y2: LongInt; bbox2: Boolean): Boolean; overload;
  begin
    result := CollisionWithinBitmapImages(
                bmp1, x1, y1, bmp1^.width, bmp1^.height, 0, 0, bbox1, 
                bmp2, x2, y2, bmp2^.width, bmp2^.height, 0, 0, bbox2);
  end;

  /// Performs a collision detection within two bitmaps at the given x, y
  /// locations using per pixel collision detection. This checks to see if
  /// two non-transparent pixels collide.
  ///
  /// @param bmp1, bmp2:  The bitmap images to check for collision
  /// @param x1, y1:      The x,y location of bmp 1
  /// @param x2, y2:      The x,y location of bmp 2
  ///
  /// @returns        True if the bitmaps collide.
  function CollisionWithinBitmapImages(bmp1: Bitmap; x1, y1: LongInt; bmp2: Bitmap; x2, y2: LongInt): Boolean; overload;
  begin
    result := CollisionWithinBitmapImages(bmp1, x1, y1, false, bmp2, x2, y2, false);
  end;

  function CollisionWithinSpriteImages(s1, s2: Sprite): Boolean;
  var
    bmp1, bmp2: Bitmap;
    offX1, offY1, offX2, offY2: LongInt;
  begin
    if (s1 = nil) or (s2 = nil) then
      raise Exception.Create('One of the sprites specified is nil');
    
    if s1^.spriteKind = AnimMultiSprite then
    begin
      offX1 := (s1^.currentFrame mod s1^.cols) * s1^.width;
      offY1 := (s1^.currentFrame - (s1^.currentFrame mod s1^.cols)) div s1^.cols * s1^.height;
      bmp1 := s1^.bitmaps[0];
    end
    else
    begin
      bmp1 := s1^.bitmaps[s1^.currentFrame];
      offX1 := 0;
      offY1 := 0;
    end;
    
    if s2^.spriteKind = AnimMultiSprite then
    begin
      offX2 := (s2^.currentFrame mod s2^.cols) * s2^.width;
      offY2 := (s2^.currentFrame - (s2^.currentFrame mod s2^.cols)) div s2^.cols * s2^.height;
      bmp2 := s2^.bitmaps[0];
    end
    else
    begin
      bmp2 := s2^.bitmaps[s2^.currentFrame];
      offX2 := 0;
      offY2 := 0;
    end;
    
    result := CollisionWithinBitmapImages(
                bmp1, Round(s1^.x), Round(s1^.y), SpriteWidth(s1), SpriteHeight(s1), 
                offX1, offY1, not s1^.usePixelCollision, 
                bmp2, Round(s2^.x), Round(s2^.y), SpriteWidth(s2), SpriteHeight(s2),
                offX2, offY2, not s2^.usePixelCollision);
  end;

  /// Checks to see if two bitmaps have collided, performs a bbox check
  /// then, if required, it performs a per pixel check on the colliding region.
  ///
  /// @param bmp1, bmp2: The bitmap images to check for collision
  /// @param x1, y1:        The x,y location of bmp 1
  /// @param bbox1:      Indicates if bmp1 should use bbox collision
  /// @param x2, y2:        The x,y location of bmp 2
  /// @param bbox2:      Indicates if bmp2 should use bbox collision
  ///
  /// @returns          True if the bitmaps collide.
  function BitmapsCollided(bmp1: Bitmap; x1, y1: LongInt; bmp2: Bitmap; x2, y2: LongInt): Boolean; overload;
  begin
    result := BitmapsCollided(bmp1, x1, y1, false, bmp2, x2, y2, false);
  end;

  function BitmapsCollided(bmp1: Bitmap; const pt1: Point2D; bmp2: Bitmap; const pt2: Point2D): Boolean; overload;
  begin
    result := BitmapsCollided(bmp1, Round(pt1.x), Round(pt1.y), false, 
                                  bmp2, Round(pt2.x), Round(pt2.y), false);
  end;


  function BitmapsCollided(bmp1: Bitmap; x1, y1: LongInt; bbox1: Boolean; bmp2: Bitmap; x2, y2: LongInt; bbox2: Boolean): Boolean; overload;
  begin
    if not BitmapRectCollision(bmp1, x1, y1, true, x2, y2, bmp2^.width, bmp2^.height) then
      result := false
    else
      result := CollisionWithinBitmapImages(bmp1, x1, y1, bbox1, bmp2, x2, y2, bbox2);
  end;
  
  function BitmapsCollided(bmp1: Bitmap; const pt1: Point2D; const part1: Rectangle; bmp2: Bitmap; const pt2: Point2D; const part2: Rectangle): Boolean; overload;
  begin
    result := BitmapsCollided(bmp1, pt1, part1, true, bmp2, pt2, part2, true);  
  end;
  
  function BitmapsCollided(bmp1: Bitmap; const pt1: Point2D; bbox1: Boolean; bmp2: Bitmap; const pt2: Point2D; bbox2: Boolean): Boolean; overload;
  begin
    result := BitmapsCollided(bmp1, Round(pt1.x), Round(pt1.y), bbox1, bmp2, Round(pt2.x), Round(pt2.y), bbox2);
  end;
  
  function BitmapsCollided(bmp1: Bitmap; const pt1: Point2D; const part1: Rectangle; bbox1: Boolean; bmp2: Bitmap; const pt2: Point2D; const part2: Rectangle; bbox2: Boolean): Boolean; overload;
  begin
    if not BitmapRectCollision(bmp1, Round(pt1.x), Round(pt1.y), true, Round(pt2.x), Round(pt2.y), part2.width, part2.height) then
      result := false
    else if bbox1 and bbox2 then
      result := true
    else
      result := CollisionWithinBitmapImages(
                  bmp1, Round(pt1.x), Round(pt1.y), part1.width, part1.height, Round(part1.x), Round(part1.y), bbox1, 
                  bmp2, Round(pt2.x), Round(pt2.y), part2.width, part2.height, Round(part2.x), Round(part2.y), bbox2);
  end;

  function SpritesCollided(s1, s2: Sprite): Boolean;
  begin
    if not SpriteRectCollision(s1, s2^.x, s2^.y, s2^.width, s2^.height) then
    begin
      result := false;
      exit;
    end;

    if s1^.usePixelCollision or s2^.usePixelCollision then
    begin
      result := CollisionWithinSpriteImages(s1, s2);
    end
    else
    begin
      result := true;
    end;
  end;


  function SpriteBitmapCollision(s: Sprite; bmp: Bitmap; x, y: Single): Boolean; overload;
  begin
    result := SpriteBitmapCollision(s, bmp, x, y, false);
  end;

  function SpriteBitmapCollision(s: Sprite; bmp: Bitmap; const pt: Point2D): Boolean; overload;
  begin
    result := SpriteBitmapCollision(s, bmp, pt.x, pt.y, false);
  end;
  
  function SpriteBitmapCollision(s: Sprite; bmp: Bitmap; const pt: Point2D; const part: Rectangle; bbox: Boolean): Boolean; overload;
  var
    tmp: Bitmap;
    offX, offY: LongInt;
  begin
    if not SpriteRectCollision(s, pt.x, pt.y, part.width, part.height) then
    begin
      result := false;
      exit;
    end
    else if bbox then 
    begin
      result := true;
      exit;
    end;

    if s^.spriteKind = AnimMultiSprite then
    begin
      offX := (s^.currentFrame mod s^.cols) * s^.width;
      offY := (s^.currentFrame - (s^.currentFrame mod s^.cols)) div s^.cols * s^.height;
      tmp := s^.bitmaps[0];
    end
    else
    begin
      tmp := s^.bitmaps[s^.currentFrame];
      offX := 0;
      offY := 0;
    end;
    
    result := CollisionWithinBitmapImages(
                tmp, Round(s^.x), Round(s^.y),  s^.width, s^.height, offX, offY, not s^.usePixelCollision, 
                bmp, Round(pt.x), Round(pt.y), part.width, part.height, Round(part.x), Round(part.y), bbox);
  end;
  
  /// Determines if a sprite has collided with a bitmap using pixel level
  /// collision detection with the bitmap.
  ///
  /// @param s:     The sprite to check for collision
  /// @param bmp:     The bitmap image to check for collision
  /// @param x, y:           The x,y location of the bitmap
  /// @param bbox        Indicates if bmp should use bbox collision
  ///
  /// @returns               True if the bitmap has collided with the sprite.
  ///
  function SpriteBitmapCollision(s: Sprite; bmp: Bitmap; x, y: Single; bbox: Boolean): Boolean; overload;
  begin
    result := SpriteBitmapCollision(s, bmp, PointAt(x, y), RectangleFrom(bmp), bbox);
  end;

  function SpriteBitmapCollision(s: Sprite; bmp: Bitmap; const pt: Point2D; bbox: Boolean): Boolean; overload;
  begin
    result := SpriteBitmapCollision(s, bmp, pt, RectangleFrom(bmp), bbox);
  end;
  
  function CircleLineCollision(s: Sprite; const line: LineSegment): Boolean;
  var
    r: Single;
    dist: Single;
  begin
    if SpriteWidth(s) > SpriteHeight(s) then
      r := SpriteWidth(s) div 2
    else
      r := SpriteHeight(s) div 2;
      
    dist := PointLineDistance(s^.x + r, s^.y + r, line);
    result := dist < r;
  end;
  
  function RectLineCollision(const rect: Rectangle; const line: LineSegment): Boolean; overload;
  begin
    result := LineIntersectsLines(line, LinesFrom(rect));
  end;
    
  function RectLineCollision(s: Sprite; const line: LineSegment): Boolean; overload;
  begin
    result := RectLineCollision(RectangleFrom(s), line);
  end;
  
  //You need to test for collisions on the ...
  function GetSideForCollisionTest (const movement: Vector): CollisionSide;
  const SMALL = 0.1; //The delta for the check
  begin
    if movement.x < -SMALL then //Going Left...
    begin
      if movement.y < -SMALL then result := BottomRight
      else if movement.y > SMALL then result := TopRight
      else result := Right;
    end
    else if movement.x > SMALL then //Going Right
    begin
      if movement.y < -SMALL then result := BottomLeft
      else if movement.y > SMALL then result := TopLeft
      else result := Left;      
    end
    else // Going Up or Down
    begin
      if movement.y < -SMALL then result := Bottom
      else if movement.y > SMALL then result := Top
      else result := None;
    end;
  end;
  
  //----------------------------------------------------------------------------
  // Bitmap <--> Point collision detection
  //----------------------------------------------------------------------------
  
  function BitmapPointCollision(bmp: Bitmap; x, y: LongInt; bbox: Boolean; ptX, ptY: Single): Boolean; overload;
  begin
    if bbox then
      result := PointInRect(ptX, ptY, RectangleFrom(x, y, bmp))
    else
      result := BitmapPointCollision(bmp, x, y, ptX, ptY);
  end;
  
  function BitmapPointCollision(bmp: Bitmap; x, y: LongInt; ptX, ptY: Single): Boolean; overload;
  begin
    result := IsPixelDrawnAtPoint(bmp, Round(ptX - x), Round(ptY - y));
  end;
  
  function BitmapPointCollision(bmp: Bitmap; x, y: LongInt; bbox: Boolean; const pt: Point2D): Boolean; overload;
  begin
    result := BitmapPointCollision(bmp, x, y, bbox, pt.x, pt.y);
  end;
  
  function BitmapPointCollision(bmp: Bitmap; x, y: LongInt; const pt: Point2D): Boolean; overload;
  begin
    result := BitmapPointCollision(bmp, x, y, False, pt.x, pt.y);
  end;
  
  function BitmapPointCollisionPart(bmp: Bitmap; x, y: LongInt; const part: Rectangle; ptX, ptY: Single): Boolean; overload;
  begin
    result := BitmapPointCollision(bmp, x, y, False, ptX + part.x, ptY + part.y);
  end;
  
  function BitmapPointCollisionPart(bmp: Bitmap; x, y: LongInt; const part: Rectangle; const pt: Point2D): Boolean; overload;
  begin
    result := BitmapPointCollision(bmp, x, y, False, pt.x + part.x, pt.y + part.y);
  end;
  
  
  //----------------------------------------------------------------------------
  // Collision Effect Application (angle + mass/energy transfer)
  //----------------------------------------------------------------------------

  procedure CollideCircleLine(s: Sprite; const line: LineSegment);
  var
    npx, npy, dotPod: Single;
    toLine: Vector;
    intersect: Point2D;
  begin
    //TODO: fix collision pt.... cast back along movement...
    intersect := ClosestPointOnLine(CenterPoint(s), line);

    //DrawSprite(s);
    //DrawCircle(ColorRed, intersect, 2);
    
    toLine := UnitVector(VectorFromCenterSpriteToPoint(s, intersect));
    // Project movement across to-line
    dotPod := - DotProduct(toLine, s^.movement);
    
    npx := dotPod * toLine.x;
    npy := dotPod * toLine.y;
    
    s^.movement.x := s^.movement.x + 2 * npx;
    s^.movement.y := s^.movement.y + 2 * npy;
    
    //DrawLine(ColorYellow, CenterPoint(s).x, CenterPoint(s).y, CenterPoint(s).x + (s^.Movement.x * 10), CenterPoint(s).y + (s^.Movement.y * 10));
    //RefreshScreen(1) ;
  end;
  
  procedure CollideCircleLines(s: Sprite; const lines: LinesArray);
  var 
    outVec, mvmt: Vector;
    maxIdx: Integer;
    mvmtMag, prop: Single;
  begin
    mvmt := s^.movement;
    maxIdx := -1;
    outVec := VectorOverLinesFromCircle(CircleFrom(s), lines, mvmt, maxIdx);
    if maxIdx < 0 then exit;
     
    MoveSprite(s, outVec);
    CollideCircleLine(s, lines[maxIdx]);
    
    // do part movement
    mvmtMag := VectorMagnitude(mvmt);
    prop := VectorMagnitude(outVec) / mvmtMag; //proportion of move "undone" by back out
    if prop > 0 then MoveSprite(s, prop); //TODO: Allow proportion of move to be passed in (overload)... then do movement based on prop * pct
  end;
  
  
  procedure CollideCircles(s1, s2: Sprite);
  var
    c1, c2: Circle;
    colNormalAngle, a1, a2, optP: Single;
    n: Vector;
  begin
    if (s1^.mass <= 0) or (s2^.mass <= 0) then
    begin
      raise Exception.Create('Collision with 0 or negative mass... ensure that mass is greater than 0');
    end;
    
    c1 := CircleFrom(s1);
    c2 := CircleFrom(s2);
    
    //TODO: What if height > width!!
    //TODO: Change backout from using direction... VectorFromPoints(s1c, s2c)??
    
    //if s1^.mass < s2^.mass then
    if VectorMagnitude(s1^.movement) > VectorMagnitude(s2^.movement) then
    begin
      //move s1 out
      n := VectorOutOfCircleFromCircle(c1, c2, s1^.movement);
      s1^.x := s1^.x + n.x;
      s1^.y := s1^.y + n.y;
    end
    else
    begin
      //move s2 out
      n := VectorOutOfCircleFromCircle(c2, c1, s2^.movement);
      s2^.x := s2^.x + n.x;
      s2^.y := s2^.y + n.y;
    end;
      
    colNormalAngle := CalculateAngle(s1, s2);
    // COLLISION RESPONSE
    // n = vector connecting the centers of the balls.
    // we are finding the components of the normalised vector n
    n := VectorFrom(Cos(colNormalAngle), Sin(colNormalAngle));
    // now find the length of the components of each movement vectors
    // along n, by using dot product.
    a1 := DotProduct(s1^.Movement, n);
    // Local a1# = c.dx*nX  +  c.dy*nY
    a2 := DotProduct(s2^.Movement, n);
    // Local a2# = c2.dx*nX +  c2.dy*nY
    // optimisedP = 2(a1 - a2)
    // ----------
    // m1 + m2
    optP := (2.0 * (a1-a2)) / (s1^.mass + s2^.mass);
    // now find out the resultant vectors
    // Local r1% = c1.v - optimisedP * mass2 * n
    s1^.movement.x := s1^.movement.x - (optP * s2^.mass * n.x);
    s1^.movement.y := s1^.movement.y - (optP * s2^.mass * n.y);
    // Local r2% = c2.v - optimisedP * mass1 * n
    s2^.movement.x := s2^.movement.x + (optP * s1^.mass * n.x);
    s2^.movement.y := s2^.movement.y + (optP * s1^.mass * n.y);
  end;
  
  procedure CollideCircleCircle(s: Sprite; const c: Circle);
  var
    hitLine: LineSegment;
    outVec, mvmt, normal, colVec: Vector;
    mvmtMag, prop: Single;
    spriteCenter, hitPt: Point2D;
  begin
    //TODO: what if height > width!!
    spriteCenter := CenterPoint(s);
    mvmt := s^.movement;
    
    outVec := VectorOutOfCircleFromCircle(CircleFrom(s), c, mvmt);
    // Back out of circle
    MoveSprite(s, outVec);
    
    // Normal of the collision...
    colVec := UnitVector(VectorFromPoints(c.center, spriteCenter));
    normal := VectorNormal(colVec);
    
    hitPt := AddVectors(c.center, VectorMultiply(colVec, c.radius + 1.42));
    hitLine := LineFromVector(AddVectors(hitPt, VectorMultiply(normal, 100)), VectorMultiply(normal, -200));
    
    // DrawSprite(s);
    // DrawLine(ColorWhite, hitLine);
    // RefreshScreen(1);
    
    CollideCircleLine(s, hitLine);

    // do part movement
    mvmtMag := VectorMagnitude(mvmt);
    prop := VectorMagnitude(outVec) / mvmtMag; //proportion of move "undone" by back out
    if prop > 0 then MoveSprite(s, prop); //TODO: Allow proportion of move to be passed in (overload)... then do movement based on prop * pct
  end;
  
  
  //TODO: bounds based checking, need VectorIntoShape...
  procedure CollideCircleRectangle(s: Sprite; const rect: Rectangle; bounds: Boolean); overload;
  var
    hitIdx: LongInt;
    lines: LinesArray;
    outVec, mvmt: Vector;
    mvmtMag, prop: Single;
  begin
    mvmt := s^.Movement;
    
    // Get the line hit...
    lines := LinesFrom(rect);
    // if bounds then
    //   outVec := VectorInLinesFromCircle(CircleFrom(s), lines, mvmt, hitIdx)
    // else 
      outVec := VectorOverLinesFromCircle(CircleFrom(s), lines, mvmt, hitIdx);
    
    if hitIdx = -1 then exit;
    
    // back out of rectangle
    MoveSprite(s, outVec);
    // if bounds then
    // begin
    //   ClearScreen();
    //   DrawSprite(s);
    //   DrawRectangle(ColorRed, rect);
    //   DrawCircle(ColorYellow, AddVectors(CenterPoint(s), InvertVector(outVec)), 2);
    //   DrawLine(ColorRed, CenterPoint(s), AddVectors(CenterPoint(s), VectorMultiply(s^.movement, 10)));
    //   RefreshScreen(1);
    // end;
    
    // bounce...
    CollideCircleLine(s, lines[hitIdx]);

    // if bounds then
    // begin
    //   DrawSprite(s);
    //   DrawLine(ColorRed, CenterPoint(s), AddVectors(CenterPoint(s), VectorMultiply(s^.movement, 10)));
    //   RefreshScreen(1);
    // end;
    
    // do part movement
    mvmtMag := VectorMagnitude(mvmt);
    prop := VectorMagnitude(outVec) / mvmtMag; //proportion of move "undone" by back out
    if prop > 0 then MoveSprite(s, prop); //TODO: Allow proportion of move to be passed in (overload)... then do movement based on prop * pct
  end;
  
  procedure CollideCircleRectangle(s: Sprite; const rect: Rectangle); overload;
  begin
    CollideCircleRectangle(s, rect, False);
  end;

  procedure CollideCircleRectangleBounds(s: Sprite; const rect: Rectangle);
  begin
    CollideCircleRectangle(s, rect, True);
  end;

  
  //----------------------------------------------------------------------------




  //---------------------------------------------------------------------------
  // Geometry Collision Tests
  //---------------------------------------------------------------------------
  
  function CircleRectCollision(const c: Circle; const rect: Rectangle): Boolean;
  begin
    if CircleLinesCollision(c, LinesFrom(rect)) then result := True
    else result := PointInRect(c.center, rect.x, rect.y, rect.width, rect.height);
  end;
  
  function CircleLinesCollision(const c: Circle; const lines: LinesArray): Boolean;
  var
    pt: Point2D;
    i: Integer;
  begin
    result := False;
    
    for i := 0 to High(lines) do
    begin
      pt := ClosestPointOnLineFromCircle(c, lines[i]);

      if PointPointDistance(c.center, pt) <= c.radius then
      begin
        //DrawCircle(ColorGreen, pt, 2);
        result := True;
        exit;
      end;
    end;
  end;

  function CircleCircleCollision(const c1, c2: Circle): Boolean;
  begin
    result := PointPointDistance(c1.center, c2.center) < c1.radius + c2.radius;
  end;

  function CircleTriangleCollision(const c: Circle; const tri: Triangle): Boolean;
  var
    i: Integer;
  begin
    result := False;

    for i := 0 to 2 do
    begin
      if PointInTriangle(ClosestPointOnCircle(tri[i], c), tri) then
      begin
        result := True;
        exit;
      end;
    end;
  end;

end.

