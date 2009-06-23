//=============================================================================
//          sgPhysics, sgMath.pas
//=============================================================================
//
// The Physics unit contains the code that is responsible for performing 
// collisions and vector maths.
//
// Change History:
//
// Version 3.0:
// - 2009-06-23: Clinton: Renamed VectorFrom to VectorFrom
//                      : Renamed HaveSpritesCollided to SpritesCollided
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

  uses sgTypes, sgMath;
  
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
  // Sprite Screen Position Tests 
  //---------------------------------------------------------------------------
  
  /// Returns True if a pixel of the `Sprite` ``s`` is at the screen location
  /// specified (``x`` and ``y``) which is converted to a world location.
  ///
  /// @lib
  /// @class Sprite
  /// @method IsOnScreenAt
  function IsSpriteOnScreenAt(s: Sprite; x, y: LongInt): Boolean; overload;
  
  /// Returns True if a pixel of the `Sprite` ``s`` is at the screen location
  /// specified (``pt``), which is converted to a world location.
  ///
  /// @lib IsSpriteOnScreenAtPoint
  /// @class Sprite
  /// @overload IsOnScreenAt IsOnScreenAtPoint
  function IsSpriteOnScreenAt(s: Sprite; const pt: Point2D): Boolean; overload;
  
  //---------------------------------------------------------------------------

  /// Returns True if the `Sprite` ``s``, represented by a bounding circle, has 
  /// collided with a ``line``. The diameter for the bounding circle is 
  /// based on the sprites width or height value -- whatever is largest.
  ///
  /// @lib SpriteCircleLineCollision
  function CircleLineCollision(p1: Sprite; const line: LineSegment): Boolean;
  
  /// Returns True if the bounding rectangle of the `Sprite` ``s`` has collided 
  /// with the ``line`` specified.
  ///
  /// @lib SpriteRectLineCollision
  function RectLineCollision(p1: Sprite; const line: LineSegment): Boolean; overload;
  
  /// Returns True if the rectangle ``rect`` provided has collided with the 
  /// ``line``.
  ///
  /// @lib RectLineCollision
  function RectLineCollision(const rect: Rectangle; const line: LineSegment): Boolean; overload;




  //---------------------------------------------------------------------------
  
  //TODO: are these really "point" in rect test?
  
  /// Return true if the vector (used as a point) is within the rectangle
  /// @lib
  function VectorInRect(const v: Vector; x, y, w, h: Single): Boolean; overload;

  /// @lib VectorInRectangle
  function VectorInRect(const v: Vector; const rect: Rectangle): Boolean; overload;
  
  /// @lib
  function GetSideForCollisionTest(const movement: Vector): CollisionSide;


  //---------------------------------------------------------------------------
  
  /// @lib VectorFromPointToRect
  function VectorFromPointToRect(x, y, rectX, rectY: Single; rectWidth, rectHeight: LongInt): Vector; overload;
  
  /// @lib VectorFromPointToRectangle
  function VectorFromPointToRect(x, y: Single; const rect: Rectangle): Vector; overload;
  
  /// @lib VectorFromPointPtToRectangle
  function VectorFromPointToRect(const pt: Point2D; const rect: Rectangle): Vector; overload;
  
  //---------------------------------------------------------------------------

  /// Determines the vector needed to move from x, y out of rectangle assuming movement specified
  /// @lib
  function VectorOutOfRectFromPoint(const pt: Point2D; const rect: Rectangle; const movement: Vector): Vector; 
  
  /// @lib
  function VectorOutOfRectFromRect(const srcRect, targetRect: Rectangle; const movement: Vector): Vector;  
  
  /// @lib
  function VectorOutOfCircleFromPoint(const pt, center: Point2D; radius: Single; const movement: Vector): Vector;
  
  /// @lib
  function VectorOutOfCircleFromCircle(const pt: Point2D; radius: Single; center: Point2D; radius2: Single; const movement: Vector): Vector;
  
  //---------------------------------------------------------------------------



  //---------------------------------------------------------------------------
  // Collision Effect Application ( angle + energy/mass transfer)
  //---------------------------------------------------------------------------
  
  /// @lib
  procedure CollideCircleLine(p1: Sprite; const line: LineSegment);
  /// @lib
  procedure CollideCircles(p1, p2: Sprite);

  
  
  
//=============================================================================
implementation
//=============================================================================

  uses
    SysUtils, {Math, Classes,} sgTrace,
    sgCore, sgGraphics, sgCamera, sgShapes;


  function IsPixelDrawnAtPoint(bmp: Bitmap; x, y: LongInt): Boolean;
  begin
    result := (Length(bmp.nonTransparentPixels) = bmp.width)
              and ((x >= 0) and (x < bmp.width))
              and ((y >= 0) and (y < bmp.height))
              and bmp.nonTransparentPixels[x, y];
  end;


  
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
    
    if (width < 1) or (height < 1) then 
      raise Exception.Create('Rectangle width and height must be greater then 0');
    
    if s.y + CurrentHeight(s) <= y then result := false
    else if s.y >= y + height then result := false
    else if s.x + CurrentWidth(s) <= x then result := false
    else if s.x >= x + width then result := false
    else
    begin
      if not s.usePixelCollision then result := true
      else
      begin
        if s.spriteKind = AnimMultiSprite then
        begin
          offX1 := (s.currentFrame mod s.cols) * s.width;
          offY1 := (s.currentFrame - (s.currentFrame mod s.cols)) div s.cols * s.height;
          bmp := s.bitmaps[0];
        end
        else
        begin
          bmp := s.bitmaps[s.currentFrame];
          offX1 := 0;
          offY1 := 0;
        end;
        result := BitmapPartRectCollision(
                    bmp, Round(s.x), Round(s.y), 
                    RectangleFrom(offX1, offY1, s.width, s.height),
                    s.usePixelCollision = false, RectangleFrom(x, y, width, height));
      end;
    end;
  end;

  function SpriteRectCollision(s: Sprite; const rect: Rectangle): Boolean; overload;
  begin
    result := SpriteRectCollision(s, rect.x, rect.y, rect.width, rect.height);
  end;
  
  function IsSpriteOnScreenAt(s: Sprite; x, y: LongInt): Boolean; overload;
  begin
    result := SpriteRectCollision(s, ToWorldX(x), ToWorldY(y), 1, 1);
  end;
  
  function IsSpriteOnScreenAt(s: Sprite; const pt: Point2D): Boolean; overload;
  begin
    result := SpriteRectCollision(s, ToWorldX(Round(pt.x)), ToWorldY(Round(pt.y)), 1, 1);
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
           AND (bbox2 or IsPixelDrawnAtPoint(bmp2, xPixel2, yPixel2)) then
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
                bmp1, x1, y1, bmp1.width, bmp1.height, 0, 0, bbox1, 
                bmp2, x2, y2, bmp2.width, bmp2.height, 0, 0, bbox2);
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
  ///
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
    
    if s1.spriteKind = AnimMultiSprite then
    begin
      offX1 := (s1.currentFrame mod s1.cols) * s1.width;
      offY1 := (s1.currentFrame - (s1.currentFrame mod s1.cols)) div s1.cols * s1.height;
      bmp1 := s1.bitmaps[0];
    end
    else
    begin
      bmp1 := s1.bitmaps[s1.currentFrame];
      offX1 := 0;
      offY1 := 0;
    end;
    
    if s2.spriteKind = AnimMultiSprite then
    begin
      offX2 := (s2.currentFrame mod s2.cols) * s2.width;
      offY2 := (s2.currentFrame - (s2.currentFrame mod s2.cols)) div s2.cols * s2.height;
      bmp2 := s2.bitmaps[0];
    end
    else
    begin
      bmp2 := s2.bitmaps[s2.currentFrame];
      offX2 := 0;
      offY2 := 0;
    end;
    
    result := CollisionWithinBitmapImages(
                bmp1, Round(s1.x), Round(s1.y), CurrentWidth(s1), CurrentHeight(s1), 
                offX1, offY1, not s1.usePixelCollision, 
                bmp2, Round(s2.x), Round(s2.y), CurrentWidth(s2), CurrentHeight(s2), 
                offX2, offY2, not s2.usePixelCollision);
  end;

  /// Checks to see if two bitmaps have collided, this performs a bbox check
  /// then, if required, it performs a per pixel check on the colliding region.
  ///
  /// @param bmp1, bmp2: The bitmap images to check for collision
  /// @param x1, y1:        The x,y location of bmp 1
  /// @param bbox1:      Indicates if bmp1 should use bbox collision
  /// @param x2, y2:        The x,y location of bmp 2
  /// @param bbox2:      Indicates if bmp2 should use bbox collision
  ///
  /// @returns          True if the bitmaps collide.
  ///
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
    if not BitmapRectCollision(bmp1, x1, y1, true, x2, y2, bmp2.width, bmp2.height) then
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
    if not SpriteRectCollision(s1, s2.x, s2.y, s2.width, s2.height) then
    begin
      result := false;
      exit;
    end;

    if s1.usePixelCollision or s2.usePixelCollision then
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

    if s.spriteKind = AnimMultiSprite then
    begin
      offX := (s.currentFrame mod s.cols) * s.width;
      offY := (s.currentFrame - (s.currentFrame mod s.cols)) div s.cols * s.height;
      tmp := s.bitmaps[0];
    end
    else
    begin
      tmp := s.bitmaps[s.currentFrame];
      offX := 0;
      offY := 0;
    end;
    
    result := CollisionWithinBitmapImages(
                tmp, Round(s.x), Round(s.y),  s.width, s.height, offX, offY, not s.usePixelCollision, 
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
    result := SpriteBitmapCollision(s, bmp, PointFrom(x, y), RectangleFrom(bmp), bbox);
  end;

  function SpriteBitmapCollision(s: Sprite; bmp: Bitmap; const pt: Point2D; bbox: Boolean): Boolean; overload;
  begin
    result := SpriteBitmapCollision(s, bmp, pt, RectangleFrom(bmp), bbox);
  end;
  

  function CircleLineCollision(p1: Sprite; const line: LineSegment): Boolean;
  var
    r: Single;
    dist: Single;
  begin
    if CurrentWidth(p1) > CurrentHeight(p1) then
      r := CurrentWidth(p1) div 2
    else
      r := CurrentHeight(p1) div 2;
      
    dist := PointLineDistance(p1.x + r, p1.y + r, line);
    result := dist < r;
  end;
  
  function RectLineCollision(const rect: Rectangle; const line: LineSegment): Boolean; overload;
  begin
    result := LineIntersectsLines(line, LinesFromRect(rect));
  end;
    
  function RectLineCollision(p1: Sprite; const line: LineSegment): Boolean; overload;
  begin
    result := RectLineCollision(RectangleFrom(p1), line);
  end;
  
  function VectorInRect(const v: Vector; x, y, w, h: Single): Boolean; overload;
  begin
    if v.x < x then result := false
    else if v.x > x + w then result := false
    else if v.y < y then result := false
    else if v.y > y + h then result := false
    else result := true;
  end;
  
  function VectorInRect(const v: Vector; const rect: Rectangle): Boolean; overload;
  begin
    result := VectorInRect(v, rect.x, rect.y, rect.width, rect.height);
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
  
  function VectorFromPointToRect(x, y, rectX, rectY: Single; rectWidth, rectHeight: LongInt): Vector; overload;
  var
    px, py: Single;
  begin
    if x < rectX then px := rectX
    else if x > (rectX + rectWidth) then px := rectX + rectWidth
    else px := x;
      
    if y < rectY then py := rectY
    else if y > (rectY + rectHeight) then py := rectY + rectHeight
    else py := y;
      
    result := VectorFrom(px - x, py - y);
  end;

  function VectorFromPointToRect(x, y: Single; const rect: Rectangle): Vector; overload;
  begin
    result := VectorFromPointToRect(x, y, rect.x, rect.y, rect.width, rect.height);
  end;
  
  function VectorFromPointToRect(const pt: Point2D; const rect: Rectangle): Vector; overload;
  begin
    result := VectorFromPointToRect(pt.x, pt.y, rect.x, rect.y, rect.width, rect.height);
  end;
  
  function VectorOut(const pt: Point2D; const rect: Rectangle; const movement: Vector; rectCollisionSide: CollisionSide): Vector; 
    function GetVectorDiagonal(edgeX, edgeY: Single): Vector;
    var
      toEdge: Vector;
      angle: Single;
      mvOut: Vector;
      xMag, yMag, outMag: Single;
    begin
      mvOut := UnitVector(InvertVector(movement));
      
      //Do X
      toEdge := VectorFrom(edgeX - pt.x, 0);
      angle := CalculateAngle(mvOut, toEdge);      
      xMag := VectorMagnitude(toEdge) * 1 / sgCore.Cos(angle);

      //Do Y
      toEdge := VectorFrom(0, edgeY - pt.y);
      angle := CalculateAngle(mvOut, toEdge);      
      yMag := VectorMagnitude(toEdge) * 1 / sgCore.Cos(angle);
          
      if (yMag < 0) or (xMag < yMag) then outMag := xMag
      else outMag := yMag;
      
      if outMag < 0 then outMag := 0;
      
      result := VectorMultiply(mvOut, outMag + 1);
    end;    
  begin   
    case rectCollisionSide of
      TopLeft: begin
          if (pt.x < rect.x) or (pt.y < rect.y) then result := VectorFrom(0,0)
          else result := GetVectorDiagonal(rect.x, rect.y);
        end;
      TopRight: begin
          if (pt.x > rect.x + rect.width) or (pt.y < rect.y) then result := VectorFrom(0,0)
          else result := GetVectorDiagonal(rect.x + rect.width, rect.y);
        end;
      BottomLeft: begin
          if (pt.x < rect.x) or (pt.y > rect.y + rect.height) then result := VectorFrom(0,0)
          else result := GetVectorDiagonal(rect.x, rect.y + rect.height);
        end;
      BottomRight: begin
          if (pt.x > rect.x + rect.width) or (pt.y > rect.y + rect.height) then result := VectorFrom(0,0)
          else result := GetVectorDiagonal(rect.x + rect.width, rect.y + rect.height);
        end;
      Left: begin
          if (pt.x < rect.x) or (pt.y < rect.y) or (pt.y > rect.y + rect.height) then result := VectorFrom(0, 0)
          else result := VectorFrom(rect.x - pt.x - 1, 0);
          exit;
        end;
      Right: begin
          if (pt.x > rect.x + rect.width) or (pt.y < rect.y) or (pt.y > rect.y + rect.height) then result := VectorFrom(0, 0)
          else result := VectorFrom(rect.x + rect.width - pt.x + 1, 0);
          exit;
        end;
      Top: begin
          if (pt.y < rect.y) or (pt.x < rect.x) or (pt.x > rect.x + rect.width) then result := VectorFrom(0, 0)
          else result := VectorFrom(0, rect.y - pt.y - 1);
          exit;
        end;
      Bottom: begin
          if (pt.y > rect.y + rect.height) or (pt.x < rect.x) or (pt.x > rect.x + rect.width) then result := VectorFrom(0, 0)
          else result := VectorFrom(0, rect.y + rect.height + 1 - pt.y);
          exit;
        end;
      else //Not moving... i.e. None
        begin
          result := VectorFrom(0, 0);
        end;
    end;
    
    //WriteLn('VectorOut: ', result.x:4:2, ',', result.y:4:2); //, '  angle: ', angle:4:2, ' mag: ', outMag:4:2, ' xmag: ', xMag:4:2, ' ymag: ', yMag:4:2);   
  end;

  function VectorOutOfRectFromPoint(const pt: Point2D; const rect: Rectangle; const movement: Vector): Vector; 
  begin
    result := VectorOut(pt, rect, movement, GetSideForCollisionTest(movement));
    
    if (result.x = 0) and (result.y = 0) then exit; 

    if not LineIntersectsRect(LineFromVector(pt, movement), rect) then
      result := VectorFrom(0, 0);
  end;

  function VectorOutOfCircleFromPoint(const pt, center: Point2D; radius: Single; const movement: Vector): Vector;
  var
    dx, dy, cx, cy: Single;
    mag, a, b, c, det, t, mvOut: single;
    ipt2: Point2D;
  begin
    // If the point is not in the radius of the circle, return a zero vector 
    if PointPointDistance(pt, center) > radius then
    begin
      result := VectorFrom(0, 0);
      exit;
    end;
    
    // If the magnitude of movement is very small, return a zero vector
    mag := VectorMagnitude(movement);
    if mag < 0.1 then
    begin
      result := VectorFrom(0, 0);
      exit;
    end;
    
    // Calculate the determinate (and components) from the center circle and 
    // the point+movement details
    cx := center.x;       
    cy := center.y;
    dx := movement.x; 
    dy := movement.y; 
    
    a := dx * dx + dy * dy;
    b := 2 * (dx * (pt.x - cx) + dy * (pt.y - cy));
    c := (pt.x - cx) * (pt.x - cx) + (pt.y - cy) * (pt.y - cy) - radius * radius;

    det := b * b - 4 * a * c;
    
    // If the determinate is very small, return a zero vector
    if det <= 0 then
      result := VectorFrom(0, 0)
    else
    begin
      // Calculate the vector required to "push" the vector out of the circle
      t := (-b - Sqrt(det)) / (2 * a);
      ipt2.x := pt.x + t * dx;
      ipt2.y := pt.y + t * dy;
  
      mvOut := PointPointDistance(pt, ipt2) + 1;
      result := VectorMultiply(UnitVector(InvertVector(movement)), mvOut);
    end;
  end;

  function VectorOutOfCircleFromCircle(const pt: Point2D; radius: Single; center: Point2D; radius2: Single; const movement: Vector): Vector;
  begin
    result := VectorOutOfCircleFromPoint(pt, center, radius + radius2, movement);
  end;

  function VectorOutOfRectFromRect(const srcRect, targetRect: Rectangle; const movement: Vector): Vector;  
  var
    rectCollisionSide: CollisionSide;
    p: Point2D; // p is the most distant point from the collision on srcRect
    destRect: Rectangle;
  begin
    //Which side of the rectangle did we collide with.
    rectCollisionSide := GetSideForCollisionTest(movement);
    
    //Get the top left out - default then adjust for other points out
    p.x := srcRect.x; p.y := srcRect.y;
    
    case rectCollisionSide of
      //Hit top or left of wall... bottom right in
      TopLeft:    begin p.x := p.x + srcRect.width; p.y := p.y + srcRect.height;  end;
      //Hit top or right of wall... bottom left in
      TopRight:   p.y := p.y + srcRect.height;
      //Hit bottom or left of wall... top right in
      BottomLeft:   p.x := p.x + srcRect.width;
      //Hit bottom or right of wall... top left is in
      BottomRight:  ;
      //Hit left of wall... right in
      Left:
        begin
          p.x := p.x + srcRect.width;
          if srcRect.y < targetRect.y then  p.y := p.y + srcRect.height;
        end;
      Right:
        begin
          if srcRect.y < targetRect.y then  p.y := p.y + srcRect.height;
        end;
      //Hit top of wall... bottom in
      Top:
        begin
          p.y := p.y + srcRect.height;
          if srcRect.x < targetRect.x then p.x := p.x + srcRect.width;
        end;
      Bottom: //hit bottom of wall get the top out
        begin
          if srcRect.x < targetRect.x then p.x := p.x + srcRect.width;
        end;
      
      None: begin 
          result := VectorFrom(0, 0);
          exit; 
        end;
    end; //end case
    
    //WriteLn('p = ', p.x, ',', p.y);
    
    result := VectorOut(p, targetRect, movement, rectCollisionSide);
    
    if (result.x = 0) and (result.y = 0) then exit; 

    destRect := RectangleAfterMove(srcRect, result);
    
    //Check diagonal miss...
    case rectCollisionSide of
      //Hit top left, check bottom and right;
      TopLeft: if (RectangleTop(destRect) > RectangleBottom(targetRect))  or 
              (RectangleLeft(destRect) > RectangleRight(targetRect))  then result := VectorFrom(0,0);
      //Hit top right, check bottom and left
      TopRight: if  (RectangleTop(destRect) > RectangleBottom(targetRect))  or 
                (RectangleRight(destRect) < RectangleLeft(targetRect))  then result := VectorFrom(0,0);
      //Hit bottom left, check top and right
      BottomLeft: if (RectangleBottom(destRect) < RectangleTop(targetRect))   or 
                (RectangleLeft(destRect) > RectangleRight(targetRect))  then result := VectorFrom(0,0);
      //Hit bottom right, check top and left
      BottomRight: if   (RectangleBottom(destRect) < RectangleTop(targetRect))  or 
                  (RectangleRight(destRect) < RectangleLeft(targetRect))  then result := VectorFrom(0,0);
    end   
  end;
  




  //----------------------------------------------------------------------------
  // Collision Effect Application (angle + mass/energy transfer)
  //----------------------------------------------------------------------------

  procedure CollideCircleLine(p1: Sprite; const line: LineSegment);
  var
    npx, npy, dotPod: Single;
    toLine: Vector;
    intersect: Point2D;
  begin
    intersect := ClosestPointOnLine(CenterPoint(p1), line);

    toLine := UnitVector(VectorFromCenterSpriteToPoint(p1, intersect));

    dotPod := - DotProduct(toLine, p1.movement);

    npx := dotPod * toLine.x;
    npy := dotPod * toLine.y;

    p1.movement.x := p1.movement.x + 2 * npx;
    p1.movement.y := p1.movement.y + 2 * npy;
  end;


  procedure CollideCircles(p1, p2: Sprite);
  var
    p1c, p2c: Point2D;
    colNormalAngle, a1, a2, optP: Single;
    n: Vector;
  begin

    if (p1.mass <= 0) or (p2.mass <= 0) then
    begin
      raise Exception.Create('Collision with 0 or negative mass... ensure that mass is greater than 0');
    end;
    
    p1c := CenterPoint(p1);
    p2c := CenterPoint(p2);
    
    if p1.mass < p2.mass then
    begin
      //move p1 out
      n := VectorOutOfCircleFromCircle(p1c, p1.width / 2, p2c, p2.width / 2, VectorFromPoints(p1c, p2c));
      p1.x := p1.x + n.x;
      p1.y := p1.y + n.y;
    end
    else
    begin
      //move p2 out
      n := VectorOutOfCircleFromCircle(p2c, p2.width / 2, p1c, p1.width / 2, VectorFromPoints(p2c, p1c));
      p2.x := p2.x + n.x;
      p2.y := p2.y + n.y;
    end;
      
    colNormalAngle := CalculateAngle(p1, p2);
    // COLLISION RESPONSE
    // n = vector connecting the centers of the balls.
    // we are finding the components of the normalised vector n
    n := VectorFrom(Cos(colNormalAngle), Sin(colNormalAngle));
    // now find the length of the components of each movement vectors
    // along n, by using dot product.
    a1 := DotProduct(p1.Movement, n);
    // Local a1# = c.dx*nX  +  c.dy*nY
    a2 := DotProduct(p2.Movement, n);
    // Local a2# = c2.dx*nX +  c2.dy*nY
    // optimisedP = 2(a1 - a2)
    // ----------
    // m1 + m2
    optP := (2.0 * (a1-a2)) / (p1.mass + p2.mass);
    // now find out the resultant vectors
    // Local r1% = c1.v - optimisedP * mass2 * n
    p1.movement.x := p1.movement.x - (optP * p2.mass * n.x);
    p1.movement.y := p1.movement.y - (optP * p2.mass * n.y);
    // Local r2% = c2.v - optimisedP * mass1 * n
    p2.movement.x := p2.movement.x + (optP * p1.mass * n.x);
    p2.movement.y := p2.movement.y + (optP * p1.mass * n.y);
  end;
  
  //----------------------------------------------------------------------------
  
end.

