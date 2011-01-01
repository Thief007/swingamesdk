//=============================================================================
// sgGeometry.pas
//=============================================================================
//
// Contains code to create points, vectors and matrices and operations for use
// between them (addition, multiplication). This unit is particularly useful
// for the sgPhysics unit with uses such operations for collision testing.
//
// Most of the code in this unit was in sgPhysics prior to version 3. See
// there for possible relevant change notes.
//
// Change History:
//
// Version 3.0:
// - 2010-01-13: Aaron  : Added PointAdd which adds 2 pointers together.
//                        Added PointInShape which checks if a Point is in a shape.
// - 2009-12-21: Andrew : Moved Sprite and Bitmap rectangle function to sgSprites and sgImages
// - 2009-12-18: Andrew : Added code to fix rectangles (+ve width and height)
// - 2009-11-10: Andrew : Added tracing and sn and csn tags to code
// - 2009-10-16: Andrew : Added shapes and prototypes. Ensure they are freed
// - 2009-07-29: Andrew : Renamed cos, sin, tan to avoid conflict in c
// - 2009-07-10: Andrew : Fixed missing const modifier on struct types
// - 2009-07-05: Clinton:
// - 2009-07-03: Andrew : Started adding class indicators
//                      : Started adding operator overloads
//                      : Added IdentityMatrix and MatrixToString
//                      : Added overloaded operators to Vector and Matrix2D
// - 2009-07-02: Andrew : Added comments for returning fixed size var length arrays
//                      : Increased precision of deg to rad
// - 2009-06-29: Andrew : Removed all need for Collision Side
//                      : Changed to use Circle Type
//                      : Added new circle working code
// - 2009-06-26: Andrew : Completed ClosestPointOnRectFromCircle
//                      : Completed
//                      : Fixed overloads of PointAt
//                      : Added RectangleCenter
//                      : RayCircleIntersectDistance heading can now be non-unit vector
//                      : Add DistantPointOnCircle
//                      : Added DistantPointOnCircleHeading
//                      : Added VectorOutOfRectFromCircle
//                      : Added PointInCircle
//                      : Added RectangleFrom Triangle
// - 2009-06-24: Andrew : Renamed to sgGeometry and combined with Shapes
//                      : Moved CenterPoint to sgSprite.
//                      : VectorFromCenterSpriteToPoint
//                      : VectorFromTo
// - 2009-06-23: Clinton: Created unit. Moved math routines from physics and
//                      : shapes units.
//=============================================================================

{$I sgTrace.inc}

/// SwinGame's geometry code includes the features to create, draw, and manipulate
/// shapes.
///
/// @module Geometry
/// @static
unit sgGeometry;

//=============================================================================
interface
  uses sgTypes;
//=============================================================================
  
  
//---------------------------------------------------------------------------
// Circle creation code
//---------------------------------------------------------------------------
  
  /// Creates a Circle at the point pt with the given radius.
  ///
  /// @lib
  /// @sn circleAt:%s radius:%s
  function CircleAt(const pt: Point2D; radius: LongInt): Circle; overload;
  
  /// Creates a circle at the given x,y location with the indicated radius.
  ///
  /// @lib CircleFromXY
  /// @sn circleAtX:%s y:%s radius:%s
  function CircleAt(x, y: Single; radius: LongInt): Circle; overload;
  
  
  
//---------------------------------------------------------------------------
// Circle code
//---------------------------------------------------------------------------
  
  /// Return the center point of a circle.
  ///
  /// @lib CircleCenterPoint
  function CenterPoint(const c: Circle): Point2D; overload;
  
  /// Returns the X value of the center point of a circle.
  ///
  /// @lib
  function CircleX(const c: Circle): Single;
  
  /// Returns the Y value of the center point of a circle.
  ///
  /// @lib
  function CircleY(const c: Circle): Single;
  
  /// Returns the radius of the passed in circle.
  /// 
  /// @lib
  function CircleRadius(const c: Circle): LongInt;
  
  /// Returns the point that lies on the circle's radius that is closest to the fromPt.
  ///
  /// @lib
  /// @sn closestPointFrom:%s onCircle:%s
  ///
  /// @class Circle
  /// @method ClosestPointTo
  /// @self 2
  function ClosestPointOnCircle(const fromPt: Point2D; const c: Circle): Point2D;
  
  /// Returns the point at the opposite side of a circle from a given point ``pt``.
  ///
  /// @lib
  /// @sn distantPointFrom:%s onCircle:%s
  ///
  /// @class Circle
  /// @method DistantPointFrom
  /// @self 2
  /// @csn distantPointFrom:%s
  function DistantPointOnCircle(const pt: Point2D; const c: Circle): Point2D;
  
  /// Finds the opposite side of a circle from a given point ``pt`` when travelling along the
  /// vector ``heading``. Returns False if the ray projected from point ``pt`` misses the circle.
  ///
  /// @lib
  /// @sn distantPointFrom:%s onCircle:%s heading:%s result:%s
  ///
  /// @class Circle
  /// @method DistantPointFrom
  /// @self 2
  /// @csn distantPointFrom:%s heading:%s result:%s
  function DistantPointOnCircleHeading(const pt: Point2D; const c: Circle; const heading: Vector; out oppositePt: Point2D): Boolean;
  
  /// Returns the two widest points on the circle that lie along the indicated vector.
  ///
  /// @lib
  /// @sn widestPointsOn:%s along:%s firstPt:%s secondPt:%s
  ///
  /// @class Circle
  /// @method WidestPoints
  /// @csn widestPointsAlong:%s firstPt:%s secondPt:%s
  procedure WidestPoints(const c: Circle; const along: Vector; out pt1, pt2: Point2D);
  
  /// Returns the two tangent points on the circle given the indicated vector.
  ///
  /// @lib
  /// @sn tangentPointsFrom:%s on:%s firstPt:%s secondPt:%s
  ///
  /// @class Circle
  /// @method TangentPoints
  /// @self 2
  /// @csn tangentPointsFrom:%s firstPt:%s secondPt:%s
  function TangentPoints(const fromPt: Point2D; const c: Circle; out p1, p2: Point2D): Boolean;
  
  /// Returns the distance from the ray origin to the edge of the circle where the ray heads in the
  /// direction indicated in the ray_heading parameter. This returns -1 where the ray does not hit
  /// the circle.
  ///
  /// @lib
  /// @sn distanceRayAt:%s heading:%s hitCircle:%s
  ///
  /// @class Point2D
  /// @method RayCircleIntersectDistance
  /// @self 1
  /// @csn distanceRayHeading:%s toCircle:%s
  function RayCircleIntersectDistance(const ray_origin: Point2D; const ray_heading:Vector; const c: Circle): Single;
  
  /// Returns true if the circle is completely within the rectangle.
  /// 
  /// @lib
  /// @sn circle:%s isWithin:%s
  function CircleWithinRect(const c: Circle; const rect: Rectangle): Boolean;
  
  
  
//---------------------------------------------------------------------------
// Line code
//---------------------------------------------------------------------------
  
  /// Returns true if the line segment intersects the circle.
  /// 
  /// @lib
  /// @sn lineSegment:%s intersectsCircle:%s
  function LineIntersectsCircle(const l: LineSegment; const c: Circle): Boolean;
  
  
  
//---------------------------------------------------------------------------
// Sprite <-> Rectangle Collision Detection
//---------------------------------------------------------------------------
  
  /// Returns true if the two line segments intersect.
  ///
  /// @lib
  /// @sn lineSegment:%s intersectsLinesSegment:%s
  function LineSegmentsIntersect(const line1, line2: LineSegment): boolean;
  
  /// Returns true if the triangle intersects with the rectangle.
  /// 
  /// @lib
  /// @sn triangle:%s intersectsRectangle:%s
  function TriangleRectangleIntersect(const tri: Triangle; const rect: Rectangle): Boolean;
  
  /// Returns true if any of the triangles intersects with the rectangle.
  /// 
  /// @lib
  /// @sn triangles:%s intersectsRectangle:%s
  function TrianglesRectangleIntersect(const tri: TriangleArray; const rect: Rectangle): Boolean;
  
  /// Which of the lines from the array of line segments did the circle collide with given the
  /// indicated velocity.
  /// 
  /// @lib
  /// @sn circle:%s travelling:%s hitLines:%s result:%s
  function LineCircleHit(const c: Circle; const velocity: Vector; const lines: LinesArray; out found: LineSegment): Boolean;
  
  ///  Returns distance from the line, or if the intersecting point on the line nearest
  ///    the point tested is outside the endpoints of the line, the distance to the
  ///    nearest endpoint.
  ///
  ///  Returns -1 on zero-valued denominator conditions to return an illegal distance. (
  ///    modification of Brandon Crosby's VBA code)
  ///
  /// @lib PointLineDistance
  /// @sn point:%s distanceTo:%s
  function PointLineDistance(const pt: Point2D; const line: LineSegment): Single; overload;
  
  /// Returns the distance from the x,y point to the line segment.
  /// 
  /// @lib PointXYLineDistance
  /// @sn pointX:%s y:%s distanceTo:%s
  function PointLineDistance(x, y: Single; const line: LineSegment): Single; overload;
  
  /// Returns the closest point on the line from the x,y point.
  ///
  /// @lib ClosestPointOnLineXY
  /// @sn pointFromX:%s y:%s closestToLine:%s
  function ClosestPointOnLine(x, y: Single; const line: LineSegment): Point2D; overload;
  
  /// Returns the point on the line that is closest to the indicated point.
  ///
  /// @lib ClosestPointOnLine
  /// @sn pointFrom:%s closestToLine:%s
  function ClosestPointOnLine(const fromPt: Point2D; const line: LineSegment): Point2D; overload;
  
  /// Returns the point on the rectangle that is closest to the circle.
  ///
  /// @lib
  /// @sn circle:%s closestPointOnRect:%s
  function ClosestPointOnRectFromCircle(const c: Circle; const rect: Rectangle): Point2D;
  
  /// Returns the point on the lines that is closest in the indicated array from the circle.
  ///
  /// @lib
  /// @sn circle:%s closestPointOnLines:%s
  function ClosestPointOnLinesFromCircle(const c: Circle; const lines: LinesArray): Point2D;
  
  /// Returns the point on the line that is closest to the circle.
  ///
  /// @lib
  /// @sn circle:%s closestPointOnLine:%s
  function ClosestPointOnLineFromCircle(const c: Circle; const line: LineSegment): Point2D;
  
  /// Create a Point2D that points at the X,Y location passed in.
  ///
  /// @lib PointAt
  /// @sn pointAtX:%s y:%s
  function PointAt(x, y: Single): Point2D; overload;
  
  /// Create a Point2D that points at the point from the startPoint at the end of the offset vector.
  ///
  /// @lib PointAtStartWithOffset
  /// @sn point:%s offset:%s
  function PointAt(const startPoint: Point2D; const offset: Vector): Point2D; overload;

  /// Get a text description of the point2D.
  ///
  /// @lib
  ///
  /// @class Point2D
  /// @method ToString
  /// @csn description
  function PointToString(const pt: Point2D): String;

  /// Get a text description of the line segment.
  ///
  /// @lib
  ///
  /// @class LineSegment
  /// @method ToString
  /// @csn description
  function LineToString(const ln: LineSegment): String;
  
  /// Get a text description of the rectangle.
  ///
  /// @lib
  ///
  /// @class Rectangle
  /// @method ToString
  /// @csn description
  function RectangleToString(const rect:Rectangle): String;
  
  /// Get a text description of the triangle.
  ///
  /// @lib
  ///
  /// @class Triangle
  /// @method ToString
  /// @csn description
  function TriangleToString(const tri: Triangle): String;
  
  /// Returns the center point of the rectangle.
  ///
  /// @lib
  function RectangleCenter(const rect: Rectangle): Point2D;
  
  /// Returns an array containing the four lines of the rectangle.
  ///
  /// @lib LinesFromRect
  /// @fixed_result_size 4
  function LinesFrom(const rect: Rectangle): LinesArray; overload;
  
  /// Returns an array containing the three lines from the triangle.
  ///
  /// @lib LinesFromTriangle
  /// @fixed_result_size 3
  function LinesFrom(const tri: Triangle): LinesArray; overload;
  
  /// Returns a line segment from x1,y1 to x2,y2.
  /// 
  /// @lib
  /// @sn lineFromX1:%s y1:%s x2:%s y2:%s
  function LineFrom(x1, y1, x2, y2: Single): LineSegment; overload;
  
  /// Returns a line from pt1 to pt2.
  ///
  /// @lib LineFromPointToPoint
  /// @sn lineFrom:%s to:%s
  function LineFrom(const pt1, pt2: Point2D): LineSegment; overload;
  
  /// Returns the number of lines in a given shape
  ///
  /// @lib
  ///
  /// @class Shape
  /// @getter LineCount
  function ShapeLineCount(s: Shape): LongInt;
  
  /// Returns an array of lines from a given shape. These are the lines that represent
  /// the shape.
  /// 
  /// @lib LinesFromShape
  ///
  /// @class Shape
  /// @getter Lines
  /// @length ShapeLineCount
  function LinesFrom(s: shape): LinesArray; overload;
  
  /// Returns the vector needed to move shape ``s`` out of rectangle``bounds`` given the it was moving with the velocity specified.
  /// 
  /// @lib
  /// @sn shape:%s vectorOutOfRect:%s givenHeading:%s
  /// 
  /// @class Shape
  /// @method VectorOutOfRect
  /// @csn vectorOutOfRect:%s givenHeading:%s
  function ShapeVectorOutOfRect(s: shape; const bounds: Rectangle; const velocity: Vector  ): vector;
  
  /// Returns a line from a starting point to the point at the end of the
  /// mv vector.
  ///
  /// @lib LineFromVectorWithStartPoint
  /// @sn lineFrom:%s toOffset:%s
  function LineFromVector(const pt: Point2D; const mv: Vector): LineSegment; overload;
  
  /// Returns a line from the x,y starting point to the point at the end of the
  /// mv vector.
  ///
  /// @lib LineFromVectorWithStartXY
  /// @sn lineFromX:%s y:%s toOffset:%s
  function LineFromVector(x, y: Single; const mv: Vector): LineSegment; overload;
  
  /// Returns a line from the origin to the end of the mv vector.
  ///
  /// @lib
  function LineFromVector(const mv: Vector): LineSegment; overload;
  
  /// Returns the mid point of the line segment.
  ///
  /// @lib
  function LineMidPoint(const line: LineSegment): Point2D;
  
  /// Returns a rectangle from a given x,y location with a given width
  /// and height.
  ///
  /// @lib
  /// @sn rectangleFromX:%s y:%s width:%s height:%s
  function RectangleFrom(x, y: Single; w, h: LongInt): Rectangle; overload;
  
  /// Returns a rectangle with pt1 and pt2 defining the two distant edge points.
  ///
  /// @lib RectangleForPoints
  /// @sn rectangleFrom:%s to:%s
  function RectangleFrom(const pt1, pt2: Point2D): Rectangle; overload;
  
  /// Returns a rectangle at a given point with a specified width and height.
  ///
  /// @lib RectangleAtPoint
  /// @sn rectangleFrom:%s width:%s height:%s
  function RectangleFrom(const pt: Point2D; width, height: LongInt): Rectangle; overload;
  
  /// Returns a rectangle that encloses the two points on the line segment.
  ///
  /// @lib RectangleFromLine
  function RectangleFrom(const line: LineSegment): Rectangle; overload;
  
  /// Returns a rectangle that encloses a circle.
  ///
  /// @lib RectangleFromCircle
  function RectangleFrom(const c: Circle): Rectangle; overload;
  
  /// Returns a rectangle that encloses th epoints in a triangle.
  ///
  /// @lib RectangleFromTriangle
  function RectangleFrom(const tri: Triangle): Rectangle; overload;
  
  /// Returns a rectangle that encloses the lines in the lines array.
  ///
  /// @lib RectangleFromLines
  function RectangleFrom(const lines: LinesArray): Rectangle; overload;
  
  /// Returns a rectangle that is inset from rect the amount specified.
  ///
  /// @lib
  function InsetRectangle(const rect: Rectangle; insetAmount: LongInt): Rectangle;
  
  /// Ensures that the passed in rectangle has a positive width and height.
  ///
  /// @lib FixRectangle
  procedure FixRectangle(var rect: Rectangle);
  
  /// Ensures that the passed in rectangle has a positive width and height.
  ///
  /// @lib FixRect
  /// @sn fixRectangleX:%s y:%s width:%s height:%s
  procedure FixRectangle(var x, y: Single; width,height: LongInt);
  
  /// Returns a triangle from the points passed in.
  /// 
  /// @lib
  /// @sn triangleFromAx:%s ay:%s bx:%s by:%s cx:%s cy:%s
  function TriangleFrom(ax, ay, bx, by, cx, cy: Single): Triangle; overload;
  
  /// Returns a triangle made up of the three points passed in.
  ///
  /// @lib TriangleFromPoints
  /// @sn trangleFromPtA:%s ptB:%s ptC:%s
  function TriangleFrom(const a, b, c: Point2D): Triangle; overload;
  
  
  
//----------------------------------------------------------------------------
// ShapePrototype creating functions
//----------------------------------------------------------------------------
  
  /// Create a shape prototype for a given point.
  ///
  /// @lib
  ///
  /// @class ShapePrototype
  /// @constructor
  /// @csn initPointPrototypeAt:%s
  function PointPrototypeFrom(const pt: Point2D): ShapePrototype;
  
  /// Create a shape prototype of a circle with a given radius ``r``.
  ///
  /// @lib
  /// @sn circlePrototypeFromPt:%s radius:%s
  ///
  /// @class ShapePrototype
  /// @constructor
  /// @csn initCirclePrototypeAt:%s withRadius:%s
  function CirclePrototypeFrom(const pt: Point2D; r: Single): ShapePrototype;
  
  // /// Create a shape prototype of an ellipse with a given width and height.
  // ///
  // /// @lib
  // /// @class ShapePrototype
  // /// @constructor
  // /// @csn initEllipsePrototypeAt:%s width:%s height:%s
  // function EllipsePrototypeFrom(const pt: Point2D; w, h: Single): ShapePrototype;
  
  /// Create a shape prototype for a line from ``startPt`` to ``endPt``.
  ///
  /// @lib
  /// @sn linePrototypeFromPt:%s toPt:%s
  ///
  /// @class ShapePrototype
  /// @constructor
  /// @csn initLinePrototypeFrom:%s to:%s
  function LinePrototypeFrom(const startPt, endPt: Point2D): ShapePrototype;
  
  /// Create a shape prototype for a triangle made of points ``pt1``, ``pt2``, and ``pt3``.
  ///
  /// @lib
  /// @sn trianglePrototypeFromPt1:%s pt2:%s pt3:%s
  ///
  /// @class ShapePrototype
  /// @constructor
  /// @csn initTrianglePrototype:%s point2:%s point3:%s
  function TrianglePrototypeFrom(const pt1, pt2, pt3: Point2D): ShapePrototype;
  
  /// Create a LineList prototype from the passed in points. There must be an
  /// even number of points, where each pair represents a line.
  ///
  /// @lib
  /// @sn lineListPrototypeFrom:%s
  ///
  /// @class ShapePrototype
  /// @constructor
  /// @csn initLineListPrototype:%s
  function LineListPrototypeFrom(const points: Point2DArray): ShapePrototype;
  
  /// Creates a LineStrip prototype where the points in the array represent the 
  /// points on the line. The last point of the line does not join back to the starting
  /// point.
  ///
  /// @lib
  /// @sn lineStripPrototypeFrom:%s
  ///
  /// @class ShapePrototype
  /// @constructor
  /// @csn initLineStripPrototype:%s
  function LineStripPrototypeFrom(const points: Point2DArray): ShapePrototype;
  
  // /// Creates a Polygon from the passed in points, the polygon is made up of
  // /// the points in the array and the last point does join back to the start
  // /// point.
  // ///
  // /// @lib
  // function PolygonPrototypeFrom(const points: Point2DArray): ShapePrototype;
  
  /// Creates a triangle strip from the passed in points. The passed in array
  /// must contain at least three points.
  ///
  /// @lib
  /// @sn triangleStripPrototypeFrom:%s
  ///
  /// @class ShapePrototype
  /// @constructor
  /// @csn initTriangleStripPrototype:%s
  function TriangleStripPrototypeFrom(const points: Point2DArray): ShapePrototype;
  
  /// Creates a triangle fan from the passed in points. The fan is constructed from
  /// the first point combined with all other point pairs. The array must contain
  /// at least three points.
  ///
  /// @lib
  /// @sn triangleFanPrototypeFrom:%s
  ///
  /// @class ShapePrototype
  /// @constructor
  /// @csn initTriangleFanPrototype:%s
  function TriangleFanPrototypeFrom(const points: Point2DArray): ShapePrototype;
  
  /// Creates a triangle list from the passed in points. The list is a set of 
  /// triangles. The number of points must be divisible by three.
  ///
  /// @lib
  /// @sn triangleListPrototypeFrom:%s
  ///
  /// @class ShapePrototype
  /// @constructor
  /// @csn initTriangleListPrototype:%s
  function TriangleListPrototypeFrom(const points: Point2DArray): ShapePrototype;
  
  /// Creates a shape prototype from the data in the points array. The kind
  /// indicates the type of shape prototype to create.
  ///
  /// @lib
  /// @sn prototypeFrom:%s kind:%s
  ///
  /// @class ShapePrototype
  /// @constructor
  /// @csn initShapePropertyFrom:%s withKind:%s
  function PrototypeFrom(const points: Point2DArray; kind:ShapeKind): ShapePrototype;
  
  /// Free the resources used by a ShapePrototype.
  ///
  /// @lib
  ///
  /// @class ShapePrototype
  /// @dispose
  procedure FreePrototype(var p: ShapePrototype);
  
  
  
//----------------------------------------------------------------------------
// ShapePrototype functions and procedures
//----------------------------------------------------------------------------
  
  /// Returns the minimum number of points required for the given shape kind.
  /// 
  /// @lib
  ///
  /// @class ShapePrototype
  /// @method MinimumPointsForKind
  /// @static
  function MinimumPointsForKind(k: ShapeKind): LongInt;
  
  /// Returns the number of points allocated to this shape prototype.
  ///
  /// @lib
  ///
  /// @class ShapePrototype
  /// @getter PointCount
  function PrototypePointCount(p: ShapePrototype): LongInt;
  
  /// Change the prototype's points to the passed in points. The number of points must
  /// be sufficient for the kind of shape being changed.
  ///
  /// @lib
  /// @sn prototype:%s setPointsTo:%s
  ///
  /// @class ShapePrototype
  /// @setter Points
  /// @length PrototypePointCount
  procedure PrototypeSetPoints(p: ShapePrototype; const points: Point2DArray);
  
  /// The prototype point locations.
  ///
  /// @lib
  ///
  /// @class ShapePrototype
  /// @getter Points
  /// @length PrototypePointCount
  function PrototypePoints(p: ShapePrototype): Point2DArray;
  
  /// Changes the prototype kind. This effects how shapes of this prototype
  /// are drawn to the screen, and the number of points required.
  /// 
  /// @lib
  /// @sn prototype:%s setKindTo:%s 
  ///
  /// @class ShapePrototype
  /// @setter Kind
  procedure PrototypeSetKind(p: ShapePrototype; kind: ShapeKind);
  
  /// The prototype kind.   This effects how shapes of this prototype
  /// are drawn to the screen, and the number of points required.
  ///
  /// @lib
  ///
  /// @class ShapePrototype
  /// @getter Kind
  function PrototypeKind(p: ShapePrototype): ShapeKind;
  
  
  
//----------------------------------------------------------------------------
// Shape Code
//----------------------------------------------------------------------------
  
  /// Create a shape from a given prototype at a set location in the game.
  ///
  /// @lib
  /// @sn prototype:%s createShapeAtPt:%s
  ///
  /// @class Shape
  /// @constructor
  /// @csn initShapeWithPrototype:%s atPoint:%s
  function ShapeAtPoint(p: ShapePrototype; const pt: Point2D): Shape;
  
  /// Free the resources used by a Shape.
  ///
  /// @lib
  /// @class Shape
  /// @dispose
  procedure FreeShape(var s: Shape);
  
  /// Recalculate the points of the shape. This will be required when a Shape's
  /// prototype is changed.
  ///
  /// @lib
  /// @class Shape
  /// @method UpdatePoints
  procedure UpdateShapePoints(s: Shape);
  
  /// Returns the number of points on this shape.
  ///
  /// @lib
  /// @class Shape
  /// @getter PointCount
  function ShapePointCount(s: Shape): LongInt;
  
  /// Returns all of the points for a shape, based on its kind, angle 
  /// and scale.
  ///
  /// @lib
  /// @class Shape
  /// @getter Points
  /// @length ShapePointCount
  function ShapePoints(s: Shape): Point2DArray;
  
  /// Change the angle of a Shape.
  ///
  /// @lib
  /// @sn shape:%s setAngleTo:%s
  ///
  /// @class Shape
  /// @setter Angle
  procedure ShapeSetAngle(s: Shape; angle: Single);
  
  /// Return the angle of a Shape.
  ///
  /// @lib
  ///
  /// @class Shape
  /// @getter Angle
  function ShapeAngle(s: Shape): Single;
  
  /// Change the scale of a Shape.
  ///
  /// @lib
  /// @sn shape:%s setScaleTo:%s
  ///
  /// @class Shape
  /// @setter Scale
  procedure ShapeSetScale(s: Shape; const scale: Point2D);
  
  /// Return the scale of a Shape.
  ///
  /// @lib
  ///
  /// @class Shape
  /// @getter Scale
  function ShapeScale(s: Shape): Point2D;
  
  /// Add a shape as a sub shape to a given parent shape.
  ///
  /// @lib
  /// @sn shape:%s addSubShape:%s
  ///
  /// @class Shape
  /// @method AddSubShape
  /// @csn addSubShape:%s
  procedure ShapeAddSubShape(parent, child: Shape);
  
  /// Gets the color of the shape s.
  ///
  /// @lib
  ///
  /// @class Shape
  /// @getter Color
  function ShapeColor(s: Shape): Color;
  
  /// Sets the color of the shape s.
  ///
  /// @lib
  /// @sn shape:%s setColorTo:%s
  ///
  /// @class Shape
  /// @setter Color
  procedure ShapeSetColor(s: Shape; c: Color);
  
  /// Sets the shape's prototype.
  /// 
  /// @lib
  /// @sn shape:%s setPrototypeTo:%s
  ///
  /// @class Shape
  /// @setter ShapePrototype
  procedure ShapeSetPrototype(s: Shape; p: ShapePrototype);
  
  /// Returns the shape's ShapePrototype.
  /// 
  /// @lib
  /// @sn shapeShapePrototype:%s
  /// 
  /// @class Shape
  /// @getter ShapePrototype
  function ShapeShapePrototype(s: Shape): ShapePrototype;
  
  /// Returns true if the shape and rectangle intersect
  /// 
  /// @lib
  /// @sn shape:%s intersectsRectangle:%s
  ///
  /// @class Shape
  /// @method IntersectsRectangle
  function ShapeRectangleIntersect(s: Shape; const rect: Rectangle): Boolean;
  
  /// Returns an axis aligned bounded box for the shape.
  /// 
  /// @lib
  ///
  /// @class Shape
  /// @getter AABB
  function ShapeAABB(s: Shape): Rectangle;
  
  /// Returns the kind of the shape.
  ///
  /// @lib
  ///
  /// @class Shape
  /// @getter ShapeKind
  function ShapeShapeKind(s: Shape): ShapeKind;
  
  /// Returns the number of triangles in the Shape.
  ///
  /// @lib
  /// 
  /// @class Shape
  /// @getter TriangleCount
  function ShapeTriangleCount(s: Shape): LongInt;
  
  /// Returns the number of triangles in the Shape.
  ///
  /// @lib
  function ShapeKindTriangleCount(s: Shape; kind: ShapeKind): LongInt;
  
  /// Returns the triangles for a triangle strip, list, or fan.
  ///
  /// @lib
  /// @sn shapeTriangles:%s
  /// @length ShapeTriangleCount
  ///
  /// @class Shape
  /// @method AsTriangles
  /// @csn triangles
  function ShapeTriangles(s: Shape): TriangleArray; overload;
  
  /// Returns the triangles for a triangle strip, list, or fan.
  ///
  /// @lib shapeTrianglesForKind
  /// @sn shapeTriangles:%s forKind:%s
  /// @length ShapeKindTriangleCount
  ///
  /// @class Shape
  /// @overload AsTriangles AsTrianglesForKind
  /// @csn trianglesForKind:%s
  function ShapeTriangles(s: Shape; kind: ShapeKind): TriangleArray;
  
  /// Returns the triangles for a triangle strip, list, or fan with an offset.
  ///
  /// @lib ShapeTrianglesOffset
  /// @sn shapeTriangles:%s forKind:%s offset:%s
  /// @length ShapeKindTriangleCount
  ///
  /// @class Shape
  /// @overload AsTriangles AsTrianglesOffset
  /// @csn trianglesForKind:%s offset:%s
  function ShapeTriangles(s: Shape; kind: ShapeKind; const offset: Point2D): TriangleArray;
  
  /// Returns the number of lines in a given shape and kind.
  ///
  /// @lib
  function ShapeKindLineCount(s: Shape; kind: ShapeKind): LongInt;
  
  /// Returns the lines for a triangle list or strip.
  ///
  /// @lib
  /// @sn shapeLines:%s
  /// @length ShapeLineCount
  ///
  /// @class Shape
  /// @method AsLines
  function ShapeLines(s: Shape): LinesArray; overload;
  
  /// Returns the lines for a triangle list or strip.
  ///
  /// @lib ShapeKindLines
  /// @sn shapeLines:%s forKind:%s
  /// @length ShapeKindLineCount
  ///
  /// @class Shape
  /// @overload AsLines AsLinesForKind
  /// @csn linesForKind:%s
  function ShapeLines(s: Shape; kind: ShapeKind): LinesArray; overload;
  
  /// Returns the lines for a triangle list or strip with offset.
  ///
  /// @lib ShapeLinesWithOffset
  /// @sn shapeLines:%s forKind:%s withOffset:%s
  /// @length ShapeLineCount
  ///
  /// @class Shape
  /// @overload AsLines AsLinesOffset
  /// @csn linesForKind:%s offset:%s
  function ShapeLines(s: Shape; kind: ShapeKind; const offset:Point2D): LinesArray; overload;
  
  /// Returns the triangle from the shapes details.
  /// 
  /// @lib
  /// 
  /// @class Shape
  /// @method AsTriangle
  function ShapeTriangle(s: Shape): Triangle;
  
  /// Returns the triangle from the shapes details with offset.
  /// 
  /// @lib ShapeTriangleWithOffset
  /// 
  /// @class Shape
  /// @overload AsTriangle AsTriangleOffset
  function ShapeTriangle(s: Shape; const offset:Point2D): Triangle;
  
  /// Returns the line from the shapes details.
  /// 
  /// @lib
  /// 
  /// @class Shape
  /// @method AsLine
  function ShapeLine(s: Shape): LineSegment;
  
  /// Returns the line from the shapes details with offset.
  /// 
  /// @lib ShapeLineOffset
  /// 
  /// @class Shape
  /// @overload AsLine AsLineOffset
  function ShapeLine(s: Shape; const offset:Point2D): LineSegment;
  
  /// Returns the circle from the shapes details.
  /// 
  /// @lib
  /// 
  /// @class Shape
  /// @method AsCircle
  function ShapeCircle(s: Shape): Circle;
  
  /// Returns the circle from the shapes details with offset.
  /// 
  /// @lib ShapeCircleOffset
  /// 
  /// @class Shape
  /// @overload AsCircle AsCircleOffset
  function ShapeCircle(s: Shape; const offset:Point2D): Circle;
  
  
  
//----------------------------------------------------------------------------
// 
//----------------------------------------------------------------------------
  
  /// Returns the barycenter point of the triangle.
  ///
  /// @lib
  ///
  /// @class Triangle
  /// @method Barycenter
  function TriangleBarycenter(const tri: Triangle): Point2D;
  
  /// Returns the rectangle details after it moved the amount specified within
  /// the vector.
  ///
  /// @lib
  /// @sn rectangle:%s rectangleAfterMove:%s
  ///
  /// @class Rectangle
  /// @method RectangleAfterMove
  function RectangleAfterMove(const rect: Rectangle; const mv: Vector): Rectangle;
  
  /// Returns the top (y) value of a rectangle.
  ///
  /// @lib
  ///
  /// @class Rectangle
  /// @getter Top
  function RectangleTop (const rect: Rectangle): Single;
  
  /// Returns the bottom (y) value of a rectangle.
  ///
  /// @lib
  ///
  /// @class Rectangle
  /// @getter Bottom
  function RectangleBottom(const rect: Rectangle): Single;
  
  /// Returns the left (x) value of a rectangle.
  ///
  /// @lib
  ///
  /// @class Rectangle
  /// @getter Left
  function RectangleLeft  (const rect: Rectangle): Single;
  
  /// Returns the right (x) value of a rectangle.
  ///
  /// @lib
  ///
  /// @class Rectangle
  /// @getter Right
  function RectangleRight (const rect: Rectangle): Single;
  
  /// Returns the top left corner of the rectangle.
  ///
  /// @lib
  ///
  /// @class Rectangle
  /// @getter TopLeft
  function RectangleTopLeft(const rect: Rectangle): Point2D;
  
  /// Returns the top right corner of the rectangle.
  ///
  /// @lib
  ///
  /// @class Rectangle
  /// @getter TopRight
  function RectangleTopRight(const rect: Rectangle): Point2D;
  
  /// Returns the bottom left corner of the rectangle.
  ///
  /// @lib
  ///
  /// @class Rectangle
  /// @getter BottomLeft
  function RectangleBottomLeft(const rect: Rectangle): Point2D;
  
  /// Returns the bottom right corner of the rectangle.
  ///
  /// @lib
  ///
  /// @class Rectangle
  /// @getter BottomRight
  function RectangleBottomRight(const rect: Rectangle): Point2D;
  
  /// Returns the center of the top line of the rectangle.
  /// 
  /// @lib
  /// 
  /// @class Rectangle
  /// @getter CenterTop
  function RectangleCenterTop(const rect: Rectangle): Point2D;
  
  /// Returns the center of the bottom line of the rectangle.
  /// 
  /// @lib
  /// 
  /// @class Rectangle
  /// @getter CenterBottom
  function RectangleCenterBottom(const rect: Rectangle): Point2D;
  
  /// Returns the center of the left line of the rectangle.
  /// 
  /// @lib
  /// 
  /// @class Rectangle
  /// @getter CenterLeft
  function RectangleCenterLeft(const rect: Rectangle): Point2D;
  
  /// Returns the center of the right line of the rectangle.
  /// 
  /// @lib
  /// 
  /// @class Rectangle
  /// @getter CenterRight
  function RectangleCenterRight(const rect: Rectangle): Point2D;
  
  
  /// Returns a rectangle that is offset by the vector.
  ///
  /// @lib
  function RectangleOffset(const rect: Rectangle; const vec: Vector): Rectangle;
  
  /// Returns true if the two rectangles intersect.
  ///
  /// @lib
  /// @sn rectangle:%s intersectsRectangle:%s
  ///
  /// @class Rectangle
  /// @method Intersects
  function RectanglesIntersect(const rect1, rect2: Rectangle): Boolean;
  
  /// Returns the intersection of two rectangles.
  ///
  /// @lib
  /// @sn rectangle:%s intersectionWithRectangle:%s
  /// 
  /// @class Rectangle
  /// @method Intersection
  function Intersection(const rect1, rect2: Rectangle): Rectangle;
  
  /// Returns the intersection point of two lines.
  ///
  /// @lib
  /// @sn line:%s intersectionWith:%s result:%s
  ///
  /// @class LineSegment
  /// @method IntersectionPoint
  /// @csn intersectionWith:%s result:%s
  function LineIntersectionPoint(const line1, line2: LineSegment; out pt: Point2D) : boolean;
  
  /// Returns the intersection point of a ray with a line, returning true if the ray intesects with the line.
  ///
  /// @lib
  /// @sn rayFrom:%s heading:%s intersectsLine:%s result:%s
  ///
  /// @class LineSegment
  /// @self 3
  /// @method RayIntersectionPoint
  /// @csn rayFrom:%s heading:%s intersectsAt:%s
  function RayIntersectionPoint(const fromPt: Point2D; const heading: Vector; const line: LineSegment; out pt: Point2D) : boolean;
  
  /// Returns true if the line intersect any of the lines in the array.
  ///
  /// @lib
  /// @sn lineSegment:%s intersectsLines:%s
  ///
  /// @class LineSegment
  /// @method IntersectsLines
  function LineIntersectsLines(const line: LineSegment; const lines: LinesArray): boolean;
  
  /// Returns true if the line intersects the rectangle.
  ///
  /// @lib
  /// @sn lineSegment:%s intersectsRectangle:%s
  ///
  /// @class LineSegment
  /// @method IntersectsRect
  function LineIntersectsRect(const line: LineSegment; const rect: Rectangle): boolean;
  
  
  
//---------------------------------------------------------------------------
// Point test operations
//---------------------------------------------------------------------------
  
  /// Returns true if the point ``pt`` is in the Triangle ``tri``.
  ///
  /// @lib
  /// @sn point:%s inTriangle:%s
  ///
  /// @class Point2D
  /// @method InTriangle
  function PointInTriangle(const pt: Point2D; const tri: Triangle): Boolean;
  
  /// Returns true if the point is within the rectangle.
  ///
  /// @lib PointInRectXY
  /// @sn point:%s inRectX:%s y:%s width:%s height:%s
  ///
  /// @class Point2D
  /// @overload InRect InRectPtXYWH 
  /// @csn inRectX:%s y:%s width:%s height:%s
  function PointInRect(const pt: Point2D; x, y, w, h: Single): Boolean; overload;
  
  /// Returns True if point ``pt`` is in the Rectangle ``rect``.
  ///
  /// @lib
  /// @sn point:%s inRect:%s
  ///
  /// @class Point2D
  /// @method InRect
  function PointInRect(const pt: Point2D; const rect: Rectangle): Boolean; overload;
  
  /// Returns true if the point (ptX, ptY) is within the rectangle.
  ///
  /// @lib PointXYInRectXY
  /// @sn pointX:%s ptY:%s inRectX:%s y:%s width:%s height:%s
  function PointInRect(ptX, ptY, x, y, w, h: Single): Boolean; overload;
  
  /// Returns true if the x,y point is within the rectangle.
  ///
  /// @lib PointXYInRect
  /// @sn pointX:%s ptY:%s inRect:%s
  function PointInRect(x, y: Single; const rect: Rectangle): Boolean; overload;
  
  /// Returns True if the point ``pt`` is in the circle.
  ///
  /// @lib
  /// @sn point:%s inCircle:%s
  ///
  /// @class Point2D
  /// @method InCircle
  function PointInCircle(const pt: Point2D; const c: Circle): Boolean;
  
  /// Returns True if point ``pt`` is on the line segment ``line``.
  ///
  /// @lib PointOnLine
  /// @sn point:%s onLine:%s
  ///
  /// @class Point2D
  /// @method OnLine
  function PointOnLine(const pt: Point2D; const line: LineSegment): Boolean;

  /// Returns True if point 'pt' is in the shape.
  ///
  /// @lib
  /// @sn point:%s inShape:%s
  ///
  /// @class Point2D
  /// @method InShape
  function PointInShape(const pt: Point2d; s:Shape):Boolean;
  
  
  
//---------------------------------------------------------------------------
// Vector Creation and Operations
//---------------------------------------------------------------------------
  
  /// Returns a new `Vector` using the ``x`` and ``y`` values provided. 
  ///
  /// @lib VectorTo(x, y, False)
  /// @uname VectorTo
  ///
  /// @sn vectorToX:%s y:%s
  function VectorTo(x, y: Single): Vector; overload;
  
  /// Creates a new `Vector` with the ``x`` and ``y`` values provided, and will 
  /// invert the ``y`` value if the ``invertY`` parameter is True. The inversion 
  /// of the ``y`` value provides a convienient option for handling screen 
  /// related vectors.
  ///
  /// @lib
  /// @uname VectorToWithInvertY
  /// 
  /// @sn vectorToX:%s y:%s invertY:%s
  function VectorTo(x, y: Single; invertY: boolean): Vector; overload;

  /// Adds the two parameter vectors (``v1`` and ``v2``) together and returns 
  /// the result as a new `Vector`.
  /// 
  /// @lib
  /// 
  /// @sn vector:%s addVector:%s
  ///
  /// @class Vector
  /// @method AddVector
  /// @csn vectorByAdding:%s
  function AddVectors(const v1, v2: Vector): Vector;
  
  {$ifdef FPC}
  /// Adds the two vectors together resulting in a new vector.
  ///
  /// @class Vector
  /// @calls AddVectors
  operator + (const v1, v2: Vector) r : Vector;
  {$endif}
  
  /// Subtracts the second vector parameter (``v2``) from the first vector
  /// (``v1``) and returns the result as new `Vector`.
  ///
  /// @lib
  /// @sn vector:%s subtractVector:%s
  ///
  /// @class Vector
  /// @method SubtractVector
  function SubtractVectors(const v1, v2: Vector): Vector;
  
  {$ifdef FPC}
  /// @class Vector
  /// @calls SubtractVectors
  operator - (const v1, v2: Vector) r : Vector;
  {$endif}

  /// Multiplies each component (``x`` and ``y`` values) of the ``v1`` vector
  /// by the ``s`` scalar value and returns the result as a new `Vector`.
  ///
  /// @lib
  /// @sn vector:%s multipliedBy:%s
  ///
  /// @class Vector
  /// @method Multiply
  function VectorMultiply(const v: Vector; s: Single): Vector;

  {$ifdef FPC}
  /// @class Vector
  /// @calls VectorMultiply
  operator * (const v: Vector; s: Single) r : Vector;
  {$endif}
  
  /// Determines if two vectors are equal.
  /// 
  /// @lib
  /// @sn vector:%s equals:%s
  /// 
  /// @class Vector
  /// @method EqualsVector
  function VectorsEqual(const v1, v2: Vector): Boolean;
  
  {$ifdef FPC}
  /// @class Vector
  /// @calls VectorsEqual
  operator = (const v1, v2: Vector) r : Boolean;
  {$endif}
  
  /// Determines if two vectors are not equal.
  /// 
  /// @lib
  /// @sn vector:%s doesNotEqual:%s
  /// 
  /// @class Vector
  /// @method DoesNotEqualVector
  function VectorsNotEqual(const v1, v2: Vector): Boolean;
  
  {$ifdef BLAH_BLAH_123}
  // Never compile this... FPC doesn't allow <> operator
  // but this is required by C#, so we need to pretend it exists...
  
  /// @class Vector
  /// @calls VectorsNotEqual
  operator <> (const v1, v2: Vector) r : Boolean;
  {$endif}
  
  /// Calculates the dot product (scalar product) between the two vector
  /// parameters  rovided (``v1`` and ``v2``). It returns the result as a
  /// scalar value.
  ///
  /// If the result is 0.0 it means that the vectors are orthogonal (at right
  /// angles to each other). If ``v1`` and ``v2`` are unit vectors (length of
  /// 1.0) and the dot product is 1.0, it means that ``v1`` and ``v2`` vectors
  /// are parallel.
  ///
  /// @lib
  /// @sn vector:%s dotProduct:%s
  ///
  /// @class Vector
  /// @method DotProduct
  function DotProduct(const v1, v2: Vector): Single;

  /// Returns a new `Vector` that is perpendicular ("normal") to the parameter
  /// vector ``v`` provided. The concept of a "normal" vector is usually
  /// extracted from (or associated with) a line. See `LineNormal`.
  ///
  /// @lib
  /// @sn vectorNormal:%s
  /// 
  /// @class Vector
  /// @getter Normal
  function VectorNormal(const v: Vector): Vector;

  /// Returns a unit vector (length is 1.0) that is "normal" (prependicular) to
  /// the ``line`` parameter. A normal vector is useful for calculating the
  /// result of a collision such as sprites bouncing off walls (lines).
  /// 
  /// @lib
  /// @sn lineNormal:%s
  ///
  /// @class LineSegment
  /// @getter Normal
  function LineNormal(const line: LineSegment): Vector;

  /// Returns a new Vector that is an inverted version of the parameter
  /// vector (v). In other words, the -/+ sign of the x and y values are changed.
  ///
  /// @lib
  /// @sn vectorInvert:%s
  ///
  /// @class Vector
  /// @getter Inverted
  function InvertVector(const v: Vector): Vector;

  /// Returns a new `Vector` that is a based on the parameter ``v`` however
  /// its magnitude (length) will be limited (truncated) if it exceeds the
  /// specified limit value.
  ///
  /// @lib
  /// @sn vector:%s limitMagnitude:%s
  ///
  /// @class Vector
  /// @method LimitToMagnitude
  function LimitVector(const v: Vector; limit: Single): Vector;

  /// Returns the unit vector of the parameter vector (v). The unit vector has a
  /// magnitude of 1, resulting in a vector that indicates the direction of
  /// the original vector.
  /// 
  /// @lib
  /// @sn vectorUnitVector:%s
  /// 
  /// @class Vector
  /// @getter UnitVector
  function UnitVector(const v: Vector): Vector;

  /// Test to see if the ``x`` and ``y`` components of the provided vector
  /// parameter ``v`` are zero.
  ///
  /// @lib
  ///
  /// @class Vector
  /// @getter IsZero
  function VectorIsZero(const v: Vector): Boolean;

  /// Returns the magnitude (or "length") of the parameter vector (v) as a 
  /// scalar value.
  ///
  /// @lib
  ///
  /// @class Vector
  /// @getter Magnitude
  function VectorMagnitude(const v: Vector): Single;
  
  /// Returns the squared magnitude (or "length") of the parameter vector (v) as a 
  /// scalar value.
  ///
  /// @lib
  ///
  /// @class Vector
  /// @getter MagnitudeSq
  function VectorMagnitudeSq(const v: Vector): Single;
  
  /// Returns a new `Vector` created using the angle and magnitude (length). 
  /// The angle and magnitude are scalar values and the angle is in degrees.
  ///
  /// @lib
  /// @sn vectorFromAngle:%s magnitude:%s
  function VectorFromAngle(angle, magnitude: Single): Vector;
  
  /// Returns a new `Vector` using the x and y value of a Point2D parameter.
  ///
  /// @lib
  function VectorToPoint(const p1: Point2D): Vector;
  
  /// Returns a `Vector` created from the difference from the ``p1`` to 
  /// the second ``p2`` points (`Point2D`).
  ///
  /// @lib
  /// @sn vectorFrom:%s to:%s
  ///
  /// @class Point2D
  /// @method ToPoint
  function VectorFromPoints(const p1, p2: Point2D): Vector;
  
  /// Returns a new `Vector` created from the start and end points of a 
  /// `LineSegment`. Useful for calculating angle vectors or extracting a 
  /// normal vector (see `LineNormal`) for the line.
  ///
  /// @lib
  ///
  /// @class LineSegment
  /// @method AsVector
  function LineAsVector(const line: LineSegment): Vector;
  
  /// Return true if the vector (used as a point) is within the rectangle
  /// 
  /// @lib VectorInRectXY
  /// @sn vector:%s inRectX:%s y:%s width:%s height:%s
  /// 
  /// @class Vector
  /// @method InRect
  /// @csn inRectX:%s y:%s width:%s height:%s
  function VectorInRect(const v: Vector; x, y, w, h: Single): Boolean; overload;
  
  /// Returns true if the vector ends within the rectangle when started at the origin.
  /// 
  /// @lib VectorInRect
  /// @sn vector:%s inRectangle:%s
  /// 
  /// @class Vector
  /// @overload InRect InRectangle
  /// @csn inRect:%s
  function VectorInRect(const v: Vector; const rect: Rectangle): Boolean; overload;
  
  /// Returns a vector from the specified point to the specified rectangle.
  /// 
  /// @lib VectorFromPointToRect
  /// @sn vectorFromX:%s y:%s toRectX:%s y:%s width:%s height:%s
  function VectorFromPointToRect(x, y, rectX, rectY: Single; rectWidth, rectHeight: LongInt): Vector; overload;
  
  /// Returns a vector from the specified point to the specified rectangle.
  /// 
  /// @lib VectorFromPointToRectangle
  /// @sn vectorFromX:%s y:%s toRect:%s
  function VectorFromPointToRect(x, y: Single; const rect: Rectangle): Vector; overload;
  
  /// Returns a vector from a point to the specified rectangle.
  /// 
  /// @lib VectorFromPointPtToRectangle
  /// @sn vectorFromPt:%s to:%s
  function VectorFromPointToRect(const pt: Point2D; const rect: Rectangle): Vector; overload;
  
  
  
//---------------------------------------------------------------------------
// Functions to get a vector out of some bounded shape
//---------------------------------------------------------------------------
  
  /// Determines the vector needed to move from point ``pt`` out of rectangle ``rect`` given the velocity specified
  /// 
  /// @lib
  /// @sn vectorFromPt:%s outOfRect:%s givenHeading:%s
  /// 
  /// @class Point2D
  /// @method VectorOutOfRect
  /// @csn vectorOutOfRect:%s givenHeading:%s
  function VectorOutOfRectFromPoint(const pt: Point2D; const rect: Rectangle; const velocity: Vector): Vector; 
  
  /// Returns the vector needed to move rectangle ``src`` out of rectangle``bounds`` given the velocity specified.
  /// 
  /// @lib
  /// @sn vectorFromRect:%s outOfRect:%s givenHeading:%s
  /// 
  /// @class Rectangle
  /// @method VectorOutOfRect
  /// @csn vectorOutOfRect:%s givenHeading:%s
  function VectorOutOfRectFromRect(const src, bounds: Rectangle; const velocity: Vector): Vector;  
  
  /// Returns the vector out of a circle from a given point.
  /// 
  /// @lib
  /// @sn vectorFromPt:%s outOfCircle:%s givenHeading:%s
  /// 
  /// @class Point2D
  /// @method VectorOutOfCircleFromPoint
  /// @csn vectorOutOfCircle:%s givenHeading:%s
  function VectorOutOfCircleFromPoint(const pt: Point2D; const c: Circle; const velocity: Vector): Vector;
  
  /// Returns a vector out of a circle for a given circle.
  /// 
  /// @lib
  /// @sn vectorFromCircle:%s outOfCircle:%s givenHeading:%s
  /// 
  /// @class Circle
  /// @method VectorOutOfCircle
  /// @csn vectorOutOfCircle:%s givenHeading:%s
  function VectorOutOfCircleFromCircle(const src, bounds: Circle; const velocity: Vector): Vector;
  
  /// Returns a vector that can be used to move a circle out of a rectangle.
  /// 
  /// @lib
  /// @sn vectorFromCircle:%s outOfRect:%s givenHeading:%s
  /// 
  /// @class Circle
  /// @method VectorOutOfRect
  /// @csn vectorOutOfRect:%s givenHeading:%s
  function VectorOutOfRectFromCircle(const c: Circle; const rect: Rectangle; const velocity: Vector): Vector;
  
  /// Returns a vector that can be used to move a circle over the lines in the array.
  /// 
  /// @lib
  /// @sn vectorFromCircle:%s overLines:%s givenHeading:%s resultingMaxIdx:%s
  ///
  /// @class Circle
  /// @method VectorOverLines
  /// @csn vectorOverLines:%s givenHeading:%s resultingMaxIdx:%s
  function VectorOverLinesFromCircle(const c: Circle; const lines: LinesArray; const velocity: Vector; out maxIdx: LongInt): Vector;
  
  
  
//---------------------------------------------------------------------------
// Points functions and procedures
//---------------------------------------------------------------------------
  
  /// Returns the four points from the corners of a rectangle.
  /// 
  /// @lib PointsFromRect
  /// @fixed_result_size 4
  /// @sn pointsFromRect:%s
  /// 
  /// @class Rectangle
  /// @getter Points
  function PointsFrom(const rect: Rectangle): Point2DArray; overload;
  
  /// Returns the two points from the ends of a line segment.
  /// 
  /// @lib PointsFromLine
  /// @fixed_result_size 2
  /// @sn pointsFromLine:%s
  /// 
  /// @class LineSegment
  /// @getter Points
  function PointsFrom(const line: LineSegment): Point2DArray; overload;
  
  
  
//---------------------------------------------------------------------------
// Angle Calculation
//---------------------------------------------------------------------------
  
  /// Calculates the angle from x1,y1 to x2,y2.
  /// 
  /// @lib
  /// @sn calculateAngleFromX1:%s y1:%s toX2:%s y:%s
  function CalculateAngle(x1, y1, x2, y2: Single): Single; overload;
  
  /// Calculates the angle between two sprites.
  /// 
  /// @lib CalculateAngleBetweenSprites
  /// @sn sprite:%s angleToSprite:%s
  /// 
  /// @class Sprite
  /// @method AngleTo
  function CalculateAngle(s1, s2: Sprite): Single; overload;
  
  /// Calculates the angle from one vector to another.
  /// 
  /// @lib CalculateAngleBetweenVectors
  /// @sn vector:%s angleTo:%s
  /// 
  /// @class Vector
  /// @method AngleTo
  function CalculateAngle(const v1, v2: Vector): Single; overload;
  
  /// Calculates the angle between two points.
  /// 
  /// @lib
  /// @sn point:%s angleTo:%s
  ///
  /// @class Point2D
  /// @method AngleTo
  function CalculateAngleBetween(const pt1, pt2: Point2D): Single;
  
  /// Calculates the angle of a vector.
  /// 
  /// @lib
  ///
  /// @class Vector
  /// @getter Angle
  function VectorAngle(const v: Vector): Single;
  
  
  
//---------------------------------------------------------------------------
// Distance / Magnitude Calculation
//---------------------------------------------------------------------------
  
  /// Returns the squared magnitude of the line from the points given.
  /// 
  /// @lib
  /// @sn lineMagnitudeSqX1:%s y1:%s x2:%s y2:%s
  function LineMagnitudeSq(x1, y1, x2, y2: single): Single; overload;
  
  /// Returns the squared line magnitude.
  /// 
  /// @lib LineMagnitudeSqFromLine
  /// @sn lineMagnitudeSq:%s
  /// 
  /// @class LineSegment
  /// @getter MagnitudeSq
  function LineMagnitudeSq(const line: LineSegment): Single; overload;
  
  /// Returns the distance from point to point.
  /// 
  /// @lib
  /// @sn point:%s distanceToPoint:%s
  /// 
  /// @class Point2D
  /// @method DistanceTo
  function PointPointDistance(const pt1, pt2: Point2D): Single;
  
  
  
//---------------------------------------------------------------------------
// Point2D  Operations 
//---------------------------------------------------------------------------
  /// Returns the sum of pt1 and pt2
  ///
  /// @lib
  /// @sn point:%s plusPoint:%s
  ///
  /// @class Point2D
  /// @method Add
  function PointAdd(const pt1, pt2: Point2D): Point2D;
  
  
  
//---------------------------------------------------------------------------
// Matrix2D Creation and Operations
//---------------------------------------------------------------------------
  
  /// Returns the identity matrix. When a Matrix2D or Vector is multiplied by
  /// the identity matrix the result is the original matrix or vector.
  ///
  /// @lib
  ///
  /// @class Matrix2D
  /// @static
  /// @method IdentityMatrix
  function IdentityMatrix(): Matrix2D;
  
  /// Returns a matrix that can be used to translate 2d points. Moving them
  /// by dx and dy.
  /// 
  /// @lib
  /// @sn translationMatrixDx:%s dy:%s
  ///
  /// @class Matrix2D
  /// @static
  /// @method TranslationMatrix
  /// @csn translationMatrixDx:%s dy:%s
  function TranslationMatrix(dx, dy: Single): Matrix2D; overload;
  
  /// Returns a translation matric used to translate 2d points by the
  /// distance in the Point2D.
  /// 
  /// @lib TranslationMatrixPt
  /// @sn translationMatrix:%s
  /// 
  /// @class Matrix2D
  /// @static
  /// @overload TranslationMatrix TranslationMatrixWithPoint
  /// @csn translationMatrix:%s
  function TranslationMatrix(const pt: Point2D): Matrix2D; overload;
  
  /// Returns a matrix that can be used to scale 2d points (both x and y).
  /// 
  /// @lib
  /// @sn scaleMatrix:%s
  ///
  /// @class Matrix2D
  /// @static
  /// @method ScaleMatrix
  function ScaleMatrix(scale: Single): Matrix2D; overload;
  
  /// Create a scale matrix that scales x and y to
  /// different degrees.
  ///
  /// @lib ScaleMatrixByPoint
  /// @sn scaleMatricByPoint:%s
  ///
  /// @class Matrix2D
  /// @static
  /// @overload ScaleMatrix ScaleMatrixWithPoint
  /// @csn scaleMatrixByPoint:%s
  function ScaleMatrix(const scale: Point2D): Matrix2D; overload;
  
  /// Returns a rotation matrix that rotates 2d points by the angle.
  /// 
  /// @lib
  /// 
  /// @class Matrix2D
  /// @static
  /// @method RotationMatrix
  function RotationMatrix(deg: Single): Matrix2D;
  
  /// Create a matrix that can scale, rotate then translate geometry points.
  /// 
  /// @lib
  /// @sn matrixToScale:%s rotate:%s translate:%s
  /// 
  /// @class Matrix2D
  /// @static
  /// @method ScaleRotateTranslateMatrix
  /// @csn matrixToScale:%s rotate:%s translate:%s
  function ScaleRotateTranslateMatrix(const scale: Point2D; deg: Single; const translate: Point2D): Matrix2D;
  
  /// Multiplies the two `Matrix2D` parameters, ``m1`` by ``m2``, and returns
  /// the result as a new `Matrix2D`. Use this to combine the effects to two 
  /// matrix transformations.
  ///
  /// @lib
  /// @sn matrix:%s multiplyByMatrix:%s
  ///
  /// @class Matrix2D
  /// @method Multiply
  /// @csn multiplyByMatrix:%s
  function MatrixMultiply(const m1, m2: Matrix2D): Matrix2D; overload;
  
  /// Multiplies the `Vector` parameter ``v`` with the `Matrix2D` ``m`` and 
  /// returns the result as a `Vector`. Use this to transform the vector with 
  /// the matrix (to apply scaling, rotation or translation effects).
  /// 
  /// @lib MatrixMultiplyVector
  /// @sn matrix:%s multiplyByVector:%s
  /// 
  /// @class Matrix2D
  /// @overload Multiply MultiplyVector
  /// @csn multiplyByVector:%s
  function MatrixMultiply(const m: Matrix2D; const v: Vector): Vector; overload;
  
  /// Use a matrix to transform all of the points in a triangle.
  /// 
  /// @lib
  /// @sn matrix:%s applyToTriangle:%s
  ///
  /// @class Matrix2D
  /// @method ApplyTo
  /// @updatesArrayParam 2
  /// @csn applyToTriangle:%s
  procedure ApplyMatrix(const m: Matrix2D; tri: Triangle);
  
  /// Apply the passed in Matrix2D to all of the points in the 
  /// Point2DArray.
  /// 
  /// @lib ApplyMatrixToPoints
  /// @sn matrix:%s applyToPoints:%s
  /// 
  /// @class Matrix2D
  /// @overload ApplyTo ApplyToPoints
  /// @updatesArrayParam 2
  /// @csn applyToPoints:%s
  procedure ApplyMatrix(const m: Matrix2D; pts: Point2DArray);
  
  {$ifdef FPC}
  /// Multiply matrix by the vector.
  /// 
  /// @class Matrix2D
  /// @calls MatrixMultiplyVector
  operator * (const m: Matrix2D; const v: Vector) r : Vector;
  {$endif}
  
  {$ifdef FPC}
  /// Multiply the two matricies together.
  /// 
  /// @class Matrix2D
  /// @calls MatrixMultiply
  operator * (const m1, m2: Matrix2D) r : Matrix2D;
  {$endif}
  
  /// This function returns a string representation of a Matrix.
  ///
  /// @lib
  ///
  /// @class Matrix2D
  /// @method ToString
  /// @csn description
  function MatrixToString(const m: Matrix2D) : String;
  
  
  
//----------------------------------------------------------------------------
// Cosine/Sin/Tan accepting degrees
//----------------------------------------------------------------------------
  
  /// Returns the cosine of the passed in angle (in degrees).
  ///
  /// @lib
  function Cosine(angle: Single): Single;
  
  /// Returns the sine of the passed in angle (in degrees).
  ///
  /// @lib
  function Sine(angle: Single): Single;
  
  /// Returns the tangent of the passed in angle (in degrees).
  ///
  /// @lib
  function Tangent(angle: Single): Single;
  
  
//=============================================================================
implementation
//=============================================================================

  uses
    Classes, SysUtils, Math,  // system
    sgCamera, sgGraphics, sgSprites, sgPhysics, sgShared, sgTrace;     // SwinGame

  const
    DEG_TO_RAD = 0.0174532925199432957692369076848861271344287188854172545609;
  
  //
  // This internal function is used to calculate the vector and determine if a hit has occurred...
  //
  // pts  contains the array of points to search from. These represent the widest points of the shape being tested
  //      against the lines.
  //
  // lines      These are the lines being tested for collision.
  //
  // velocity   The direction/distance of the movement of the points (used to find impact point)
  //
  // maxIds     The index of the line that is the furthest colliding line back from the points.
  //
  function _VectorOverLinesFromLines(const srcLines, boundLines: LinesArray; const velocity: Vector; out maxIdx: LongInt): Vector;
  var
    ray, vOut: Vector;
    i, j, k: LongInt;
    maxDist: Single;
    lnPoints, boundLnPoints: Point2DArray;
    bothDidHit: Boolean;
    
    // Search from the startPt for the ray 
    function _RayFromPtHitLine(startPt: Point2D; const toLine: LineSegment; myRay: Vector): Boolean;
    var
      ptOnLine: Point2D;
      dist: Single;
    begin
      //DrawCircle(ColorWhite, pts[j], 2);
      result := False;
      
      // Cast myRay back from startPt to find line pts... out on ptOnLine
      // ptOnLine is then the point that the ray intersects with the line
      if RayIntersectionPoint(startPt, myRay, toLine, ptOnLine) then
      begin
        if not PointOnLine(ptOnLine, toLine) then exit; //this points ray misses the line
        result := True;

        // FillCircle(ColorRed, startPt, 2);
        // DrawCircle(ColorRed, ptOnLine, 2);
        // DrawLine(ColorRed, startPt, ptOnLine);
        
        // Calculate the distance from the point on the line to the point being tested
        dist := PointPointDistance(ptOnLine, startPt);
        
        // Check if the distance is the new max distance
        if (dist > maxDist) or (maxIdx = -1) then
        begin
          maxDist := dist;
          maxIdx := i;  //  We hit with the current line
          if VectorsEqual(myRay, ray) then // if we are searching back...
            vOut := VectorFromPoints(startPt, ptOnLine)
          else // if we are searching forward (using velocity)
            vOut := VectorFromPoints(ptOnLine, startPt);
          vOut := VectorMultiply(UnitVector(vOut), VectorMagnitude(vOut) + 1.42)
        end;      
      end;
    end;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', '_VectorOverLinesFromLines(const pts: Point2DArray', '');
    {$ENDIF}
    
    // Cast ray searching back from pts... looking for the impact point
    ray := InvertVector(velocity);  // the ray
    vOut := VectorTo(0,0);          // the vector out (from the point to over the line, i.e. moving the point along this vector moves it back over the line)

    // Indicate no find so far....
    maxIdx := -1;
    maxDist := -1;
    
    //
    //  Search all lines for hit points - cast ray back from line ends and find where these intersect with the bound lines
    //
    for i := 0 to High(boundLines) do
    begin
      //WriteLn('Testing bound line: ', LineToString(boundLines[i]));
      boundLnPoints := PointsFrom(boundLines[i]);
      
      // for all source lines...
      for j := 0 to High(srcLines) do
      begin
        //WriteLn('Testing src line: ', LineToString(srcLines[j]));
        
        // Get the points from the srcLine
        lnPoints := PointsFrom(srcLines[j]);
        bothDidHit := True;
        
        for k := 0 to High(lnPoints) do
        begin
          //WriteLn('Point ', k, ' in line is at ', PointToString(lnPoints[k]));
          bothDidHit := _RayFromPtHitLine(lnPoints[k], boundLines[i], ray) and bothDidHit;
        end;

        if bothDidHit then continue;
        
        // Search from the bound line to the source 
        
        for k := 0 to High(boundLnPoints) do
        begin
          _RayFromPtHitLine(boundLnPoints[k], srcLines[j], velocity);
        end;        
      end;
    end;

    result.x := Ceiling(vOut.x);
    result.y := Ceiling(vOut.y);

    {$IFDEF TRACE}
      TraceExit('sgGeometry', '_VectorOverLinesFromLines(const pts: Point2DArray', '');
    {$ENDIF}
  end;

  //
  // This internal function is used to calculate the vector and determine if a hit has occurred...
  //
  // pts  contains the array of points to search from. These represent the widest points of the shape being tested
  //      against the lines.
  //
  // lines      These are the lines being tested for collision.
  //
  // velocity   The direction/distance of the movement of the points (used to find impact point)
  //
  // maxIds     The index of the line that is the furthest colliding line back from the points.
  //
  function _VectorOverLinesFromPoints(const pts: Point2DArray; const lines: LinesArray; const velocity: Vector; out maxIdx: LongInt): Vector;
  var
    ptOnLine: Point2D;
    ray, vOut: Vector;
    i, j: LongInt;
    dist, maxDist: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', '_VectorOverLinesFromPoints(const pts: Point2DArray', '');
    {$ENDIF}
    
    // Cast ray searching back from pts... looking for the impact point
    ray := InvertVector(velocity);  // the ray
    vOut := VectorTo(0,0);          // the vector out (from the point to over the line, i.e. moving the point along this vector moves it back over the line)
    
    // Indicate no find so far....
    maxIdx := -1;
    maxDist := -1;
    
    //Search all lines for hit points
    for i := 0 to High(lines) do
    begin
      // for all points...
      for j := 0 to High(pts) do
      begin
        //DrawCircle(ColorWhite, pts[j], 2);

        // Cast a ray back from this point to find line pts... out on ptOnLine
        // ptOnLine is then the point that the ray intersects with the line
        if RayIntersectionPoint(pts[j], ray, lines[i], ptOnLine) then
        begin
          //DrawCircle(ColorRed, ptOnLine, 1);
          //DrawLine(ColorRed, pts[j], ptOnLine);
          
          if not PointOnLine(ptOnLine, lines[i]) then continue; //this points ray misses the line
          
          // Calculate the distance from the point on the line to the point being tested
          dist := PointPointDistance(ptOnLine, pts[j]);
          
          // Check if the distance is the new max distance
          if (dist > maxDist) or (maxIdx = -1) then
          begin
            maxDist := dist;
            maxIdx := i;
            vOut := VectorFromPoints(pts[j], ptOnLine);
            vOut := VectorMultiply(UnitVector(vOut), VectorMagnitude(vOut) + 1)
          end;      
        end;
      end;
    end;
    
    result.x := Ceiling(vOut.x);
    result.y := Ceiling(vOut.y);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', '_VectorOverLinesFromPoints(const pts: Point2DArray', '');
    {$ENDIF}
  end;
  
  //
  // This internal function is used to calculate the vector and determine if a hit has occurred...
  //
  function _VectorOverLinesFromPoint(const pt: Point2D; const lines: LinesArray; const velocity: Vector; out maxIdx: LongInt): Vector;
  var
    pts: Point2DArray;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', '_VectorOverLinesFromPoint(const pt: Point2D', '');
    {$ENDIF}
    
    SetLength(pts, 1);
    pts[0] := pt;
    result := _VectorOverLinesFromPoints(pts, lines, velocity, maxIdx);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', '_VectorOverLinesFromPoint(const pt: Point2D', '');
    {$ENDIF}
  end;

  //
  // This internal function is used to calculate the vector and determine if a hit has occurred...
  //
  function _VectorOverLinesFromCircle(const c: Circle; const lines: LinesArray; const velocity: Vector; out maxIdx: LongInt): Vector;
  type
    DoublePt = record ptOnCircle, ptOnLine: Point2D; end;
  var
    ptOnLine, ptOnCircle: Point2D;
    tmp: Array [0..3] of Point2D;
    chkPts: Array [0..3] of DoublePt;
    lineVec, normalMvmt, normalLine, toEdge, edge, ray, vOut: Vector;
    i, j, hits: LongInt;
    dotProd, dist, maxDist: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', '_VectorOverLinesFromCircle(const c: Circle', '');
    {$ENDIF}
    
    // Cast ray searching for points back from shape
    ray := InvertVector(velocity);
    normalMvmt := VectorNormal(velocity);
    vOut := VectorTo(0,0);
    ptOnCircle := PointAt(0,0);

    maxIdx := -1;
    maxDist := -1;

    // fix for tmp initialized warning
    for i := 0 to high(tmp) do tmp[i] := PointAt(0,0);

    //Search all lines for hit points
    for i := 0 to High(lines) do
    begin
      lineVec := LineAsVector(lines[i]);
      //Get the normal of the line we hit
      normalLine := VectorNormal(lineVec);
      hits := 0;

      //tmp 0 and tmp 1 are the widest points to detect collision with line
      WidestPoints(c, normalMvmt, tmp[0], tmp[1]);
      //tmp 2 and tmp 3 are the closest and furthest points from the line
      WidestPoints(c, normalLine, tmp[2], tmp[3]);
      
      // for both points...
      for j := 0 to 3 do
      begin
        //DrawCircle(ColorWhite, tmp[j], 2);
        
        // Cast a ray back from the test points to find line pts... out on ptOnLine
        if RayIntersectionPoint(tmp[j], ray, lines[i], ptOnLine) then
        begin
          //DrawCircle(ColorRed, ptOnLine, 1);
          //DrawLine(ColorRed, tmp[j], ptOnLine);
          
          chkPts[hits].ptOnLine := ptOnLine;
          chkPts[hits].ptOnCircle := tmp[j];
          hits := hits + 1;
        end;
      end;

      // for each of the hits on this line...
      // search for the longest hit.
      for j := 0 to hits - 1 do
      begin
        //if not maxLine then DrawCircle(ColorWhite, chkPts[j].ptOnCircle, 1);
        toEdge := VectorFromPoints(c.center, chkPts[j].ptOnCircle);
        //if not maxLine then DrawLine(ColorRed, c.center, chkPts[j].ptOnCircle);
        dotProd := DotProduct(toEdge, normalLine);

        // 2d: Most distant edge pt is on a line this distance (dotProd) from the center
        edge := AddVectors(c.center, VectorMultiply(normalLine, dotProd));
        //DrawPixel(ColorWhite, edge); // Draws pt on line to distant pt

        //  Find the point we hit on the line... ptOnLine receives intersection point...
        if not LineIntersectionPoint(LineFromVector(edge, velocity), lines[i], ptOnLine) then continue;
        // Move back onto line segment... linePt -> closest point on line to intersect point
        //if not maxLine then DrawCircle(ColorRed, ptOnLine, 1); // point on line, but not necessarily the line segment
        //if not maxLine then DrawLine(ColorWhite, edge, ptOnLine);

        ptOnLine := ClosestPointOnLine(ptOnLine, lines[i]);
        //if not maxLine then FillCircle(ColorBlue, ptOnLine, 1); // point on segment

        // Find the most distant point on the circle, given the velocity vector
        if not DistantPointOnCircleHeading(ptOnLine, c, velocity, ptOnCircle) then continue;
        // if not maxLine then FillCircle(ColorBlue, ptOnCircle, 2); // point on segment
        // if not maxLine then DrawLine(ColorBlue, ptOnLine, ptOnCircle);

        dist := PointPointDistance(ptOnLine, ptOnCircle);

        if (dist > maxDist) or (maxIdx = -1) then
        begin
          maxDist := dist;
          maxIdx := i;
          vOut := VectorFromPoints(ptOnCircle, ptOnLine);
          vOut := VectorMultiply(UnitVector(vOut), VectorMagnitude(vOut) + 1.42)
        end;
      end;
    end;
    
    result.x := Ceiling(vOut.x);
    result.y := Ceiling(vOut.y);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', '_VectorOverLinesFromCircle(const c: Circle', '');
    {$ENDIF}
  end;
  
  
  
//---------------------------------------------------------------------------
// Vector operations on Vectors (usally returning vectors)
//---------------------------------------------------------------------------
  
  function VectorTo(x, y: Single; invertY: boolean): Vector; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'VectorTo(x, y: Single', '');
    {$ENDIF}
    
    if invertY then y := y * -1;

    result.x := x;
    result.y := y;
    //result.w := 1;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'VectorTo(x, y: Single', '');
    {$ENDIF}
  end;

  function VectorTo(x, y: Single): Vector; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'VectorTo(x, y: Single): Vector', '');
    {$ENDIF}
    
    result := VectorTo(x, y, false);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'VectorTo(x, y: Single): Vector', '');
    {$ENDIF}
  end;

  function VectorToPoint(const p1: Point2D): Vector;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'VectorToPoint(const p1: Point2D): Vector', '');
    {$ENDIF}
    
    result := VectorTo(p1.x, p1.y);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'VectorToPoint(const p1: Point2D): Vector', '');
    {$ENDIF}
  end;

  function VectorFromPoints(const p1, p2: Point2D): Vector;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'VectorFromPoints(const p1, p2: Point2D): Vector', '');
    {$ENDIF}
    
    result := VectorTo(p2.x - p1.x, p2.y - p1.y, false);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'VectorFromPoints(const p1, p2: Point2D): Vector', '');
    {$ENDIF}
  end;

  function AddVectors(const v1, v2: Vector): Vector;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'AddVectors(const v1, v2: Vector): Vector', '');
    {$ENDIF}
    
    result.x := v1.x + v2.x;
    result.y := v1.y + v2.y;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'AddVectors(const v1, v2: Vector): Vector', '');
    {$ENDIF}
  end;

  {$ifdef FPC}
  operator + (const v1, v2: Vector) r : Vector;
  begin
    r := AddVectors(v1, v2);
  end;
  {$endif}

  function SubtractVectors(const v1, v2: Vector): Vector;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'SubtractVectors(const v1, v2: Vector): Vector', '');
    {$ENDIF}
    
    result.x := v1.x - v2.x;
    result.y := v1.y - v2.y;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'SubtractVectors(const v1, v2: Vector): Vector', '');
    {$ENDIF}
  end;

  {$ifdef FPC}
  operator - (const v1, v2: Vector) r : Vector;
  begin
    r := SubtractVectors(v1, v2);
  end;
  {$endif}

  function VectorMultiply(const v: Vector; s: Single): Vector;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'VectorMultiply(const v: Vector', '');
    {$ENDIF}
    
    result.x := v.x * s;
    result.y := v.y * s;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'VectorMultiply(const v: Vector', '');
    {$ENDIF}
  end;

  {$ifdef FPC}
  operator * (const v: Vector; s: Single) r : Vector;
  begin
    r := VectorMultiply(v, s);
  end;
  {$endif}
  
  function VectorsEqual(const v1, v2: Vector): Boolean;
  begin
    result := (v1.x = v2.x) and (v1.y = v2.y);
  end;
  
  {$ifdef FPC}
  operator = (const v1, v2: Vector) r : Boolean;
  begin
    r := VectorsEqual(v1, v2);
  end;
  {$endif}  

  function VectorsNotEqual(const v1, v2: Vector): Boolean;
  begin
    result := (v1.x <> v2.x) or (v1.y <> v2.y);
  end;
  
  function InvertVector(const v: Vector): Vector;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'InvertVector(const v: Vector): Vector', '');
    {$ENDIF}
    
    result.x := v.x * -1;
    result.y := v.y * -1;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'InvertVector(const v: Vector): Vector', '');
    {$ENDIF}
  end;

  function LimitVector(const v: Vector; limit: Single): Vector;
  var
    mag: Single;
    tmp: Vector;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'LimitVector(const v: Vector', '');
    {$ENDIF}
    
    mag := VectorMagnitude(v);
    if mag > limit then
    begin
      tmp := UnitVector(v);
      result.x := tmp.x * limit;
      result.y := tmp.y * limit;
    end
    else
      result := v;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'LimitVector(const v: Vector', '');
    {$ENDIF}
  end;

  function UnitVector(const v: Vector): Vector;
  var
    mag, tmp: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'UnitVector(const v: Vector): Vector', '');
    {$ENDIF}
    
    mag := VectorMagnitude(v);
  
    if mag = 0 then
      tmp := 0
    else
      tmp := 1 / mag;
  
    result.x := tmp * v.x;
    result.y := tmp * v.y;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'UnitVector(const v: Vector): Vector', '');
    {$ENDIF}
  end;

  function VectorIsZero(const v: Vector): Boolean;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'VectorIsZero(const v: Vector): Boolean', '');
    {$ENDIF}
    
    result := (v.x = 0) and (v.y = 0);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'VectorIsZero(const v: Vector): Boolean', '');
    {$ENDIF}
  end;

  function VectorMagnitude(const v: Vector): Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'VectorMagnitude(const v: Vector): Single', '');
    {$ENDIF}
    
    result := Sqrt(VectorMagnitudeSq(v));
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'VectorMagnitude(const v: Vector): Single', '');
    {$ENDIF}
  end;

  function VectorMagnitudeSq(const v: Vector): Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'VectorMagnitudeSq(const v: Vector): Single', '');
    {$ENDIF}
    result := (v.x * v.x) + (v.y * v.y);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'VectorMagnitudeSq(const v: Vector): Single', '');
    {$ENDIF}
  end;
  
  function DotProduct(const v1, v2: Vector): Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'DotProduct(const v1, v2: Vector): Single', '');
    {$ENDIF}
    
    result := (v1.x * v2.x) + (v1.y * v2.y);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'DotProduct(const v1, v2: Vector): Single', '');
    {$ENDIF}
  end;
  
  function VectorFromAngle(angle, magnitude: Single): Vector;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'VectorFromAngle(angle, magnitude: Single): Vector', '');
    {$ENDIF}
    
    result := VectorTo(magnitude * sgGeometry.Cosine(angle), magnitude * sgGeometry.Sine(angle));
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'VectorFromAngle(angle, magnitude: Single): Vector', '');
    {$ENDIF}
  end;
  
  function LineAsVector(const line: LineSegment): Vector;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'LineAsVector(const line: LineSegment): Vector', '');
    {$ENDIF}
    
    result.x := line.endPoint.x - line.startPoint.x;
    result.y := line.endPoint.y - line.startPoint.y;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'LineAsVector(const line: LineSegment): Vector', '');
    {$ENDIF}
  end;
  
  function VectorNormal(const v: Vector): Vector;
  var   
    tmp: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'VectorNormal(const v: Vector): Vector', '');
    {$ENDIF}
    
    tmp := Sqrt( (v.x * v.x) + (v.y * v.y) );
    result.x := -v.y / tmp;
    result.y :=  v.x / tmp;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'VectorNormal(const v: Vector): Vector', '');
    {$ENDIF}
  end;
  
  function LineNormal(const line: LineSegment): Vector;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'LineNormal(const line: LineSegment): Vector', '');
    {$ENDIF}
    
    result := VectorNormal(LineAsVector(line));
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'LineNormal(const line: LineSegment): Vector', '');
    {$ENDIF}
  end;
  
  
  
//---------------------------------------------------------------------------
// Angle Calculation 
//---------------------------------------------------------------------------
  
  function VectorAngle(const v: Vector): Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'VectorAngle(const v: Vector): Single', '');
    {$ENDIF}
    
    if v.x = 0 then
    begin
      if v.y < 0 then result := -90
      else result := 90; //Default to down screen if x and y are both 0
    end
    else if v.y = 0 then
    begin
      if v.x < 0 then result := 180
      else result := 0;   
    end
    else
    begin
      result := RadToDeg(arctan(v.y / v.x));
      
      if v.x < 0 then
      begin
        if v.y < 0 then result := result - 180
        else result := result + 180;
      end;
    end;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'VectorAngle(const v: Vector): Single', '');
    {$ENDIF}
  end;
  
  function CalculateAngle(x1, y1, x2, y2: Single): Single; overload;
  var
    o, a, oa, rads: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'CalculateAngle(x1, y1, x2, y2: Single): Single', '');
    {$ENDIF}
    
    if (x1 = x2) and (y2 < y1) then result := -90
    else if (x1 = x2) and (y2 >= y1) then result := 90
    else if (y1 = y2) and (x2 < x1) then result := 180
    else if (y1 = y2) and (x2 >= x1) then result := 0
    else
    begin
      o := (y2 - y1);
      a := (x2 - x1);
      oa := o / a;
      rads := arctan(oa);
      result := RadToDeg(rads);

      if x2 < x1 then
      begin
        if (y2 < y1) then result := result - 180
        else result := result + 180;
      end;
    end;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'CalculateAngle(x1, y1, x2, y2: Single): Single', '');
    {$ENDIF}
  end;

  function CalculateAngle(s1, s2: Sprite): Single; overload;
  var
    cx1, cy1, cx2, cy2: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'CalculateAngle(s1, s2: Sprite): Single', '');
    {$ENDIF}
    
    cx1 := s1^.position.x + SpriteWidth(s1) / 2;
    cy1 := s1^.position.y + SpriteHeight(s1) / 2;
    cx2 := s2^.position.x + SpriteWidth(s2) / 2;
    cy2 := s2^.position.y + SpriteHeight(s2) / 2;

    result := CalculateAngle(cx1, cy1, cx2, cy2);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'CalculateAngle(s1, s2: Sprite): Single', '');
    {$ENDIF}
  end;

  function CalculateAngle(const v1, v2: Vector): Single; overload;
  var
    t1, t2: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'CalculateAngle(const v1, v2: Vector): Single', '');
    {$ENDIF}
    
    t1 := CalculateAngle(0, 0, v1.x, v1.y);
    t2 := CalculateAngle(0, 0, v2.x, v2.y);
  
    result := t2 - t1;
  
    if result > 180 then result := result - 360
    else if result <= -180 then result := result + 360;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'CalculateAngle(const v1, v2: Vector): Single', '');
    {$ENDIF}
  end;
  
  function CalculateAngleBetween(const pt1, pt2: Point2D): Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'CalculateAngleBetween(const pt1, pt2: Point2D): Single', '');
    {$ENDIF}
    
     result := CalculateAngle(pt1.x, pt1.y, pt2.x, pt2.y);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'CalculateAngleBetween(const pt1, pt2: Point2D): Single', '');
    {$ENDIF}
  end;


//----------------------------------------------------------------------------
// Distance / Magnitude Calculations
//----------------------------------------------------------------------------

  //
  //  Returns the square of the magnitude of the line
  //    to cut down on unnecessary Sqrt when in many cases
  //    DistancePointLine() squares the result
  //
  function LineMagnitudeSq(const line: LineSegment): Single; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'LineMagnitudeSq(const line: LineSegment): Single', '');
    {$ENDIF}
    
    result := (line.endPoint.x - line.startPoint.x) * (line.endPoint.x - line.startPoint.x) +
              (line.endPoint.y - line.startPoint.y) * (line.endPoint.y - line.startPoint.y);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'LineMagnitudeSq(const line: LineSegment): Single', '');
    {$ENDIF}
  end;

  function LineMagnitudeSq(x1, y1, x2, y2: single): single; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'LineMagnitudeSq(x1, y1, x2, y2: single): single', '');
    {$ENDIF}
    
   result := (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'LineMagnitudeSq(x1, y1, x2, y2: single): single', '');
    {$ENDIF}
  end;

  function PointPointDistance(const pt1, pt2: Point2D): Single;
  var
    temp: Vector;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'PointPointDistance(const pt1, pt2: Point2D): Single', '');
    {$ENDIF}
    
    temp := VectorTo(pt2.x - pt1.x, pt2.y - pt1.y);
    result := VectorMagnitude(temp);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'PointPointDistance(const pt1, pt2: Point2D): Single', '');
    {$ENDIF}
  end;
  
//----------------------------------------------------------------------------
// Matrix2D Creation and Operation / Translation of Point/Vector Types
//----------------------------------------------------------------------------

  function RotationMatrix(deg: Single): Matrix2D;
  var
    rads: Extended;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'RotationMatrix(deg: Single): Matrix2D', '');
    {$ENDIF}
    
    rads := -deg * DEG_TO_RAD;

    result[0, 0] := System.Cos(rads);
    result[0, 1] := System.Sin(rads);
    result[0, 2] := 0;
  
    result[1, 0] := -System.Sin(rads);
    result[1, 1] := System.Cos(rads);
    result[1, 2] := 0;
  
    result[2, 0] := 0;
    result[2, 1] := 0;
    result[2, 2] := 1;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'RotationMatrix(deg: Single): Matrix2D', '');
    {$ENDIF}
  end;
  
  function ScaleMatrix(const scale: Point2D): Matrix2D; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'ScaleMatrix(const scale: Point2D): Matrix2D', '');
    {$ENDIF}
    
    result[0, 0] := scale.x;
    result[0, 1] := 0;
    result[0, 2] := 0;
    
    result[1, 0] := 0;
    result[1, 1] := scale.y;
    result[1, 2] := 0;
    
    result[2, 0] := 0;
    result[2, 1] := 0;
    result[2, 2] := 1;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'ScaleMatrix(const scale: Point2D): Matrix2D', '');
    {$ENDIF}
  end;
  
  function ScaleMatrix(scale: Single): Matrix2D; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'ScaleMatrix(scale: Single): Matrix2D', '');
    {$ENDIF}
    
    result[0, 0] := scale;
    result[0, 1] := 0;
    result[0, 2] := 0;

    result[1, 0] := 0;
    result[1, 1] := scale;
    result[1, 2] := 0;

    result[2, 0] := 0;
    result[2, 1] := 0;
    result[2, 2] := 1;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'ScaleMatrix(scale: Single): Matrix2D', '');
    {$ENDIF}
  end;
  
  function IdentityMatrix(): Matrix2D;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'IdentityMatrix(): Matrix2D', '');
    {$ENDIF}
    
    result[0, 0] := 1;
    result[0, 1] := 0;
    result[0, 2] := 0;

    result[1, 0] := 0;
    result[1, 1] := 1;
    result[1, 2] := 0;

    result[2, 0] := 0;
    result[2, 1] := 0;
    result[2, 2] := 1;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'IdentityMatrix(): Matrix2D', '');
    {$ENDIF}
  end;
  
  function TranslationMatrix(const pt: Point2D): Matrix2D; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'TranslationMatrix(const pt: Point2D): Matrix2D', '');
    {$ENDIF}
    
    result := TranslationMatrix(pt.x, pt.y);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'TranslationMatrix(const pt: Point2D): Matrix2D', '');
    {$ENDIF}
  end;
  
  function TranslationMatrix(dx, dy: Single): Matrix2D; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'TranslationMatrix(dx, dy: Single): Matrix2D', '');
    {$ENDIF}
    
    result := IdentityMatrix();

    result[0, 2] := dx;
    result[1, 2] := dy;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'TranslationMatrix(dx, dy: Single): Matrix2D', '');
    {$ENDIF}
  end;
  
  function ScaleRotateTranslateMatrix(const scale: Point2D; deg: Single; const translate: Point2D): Matrix2D;
  var
    rads: Extended;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'ScaleRotateTranslateMatrix(const scale: Point2D', '');
    {$ENDIF}
    
    rads := -deg * DEG_TO_RAD;
    
    result[0, 0] := System.Cos(rads) * scale.x;
    result[0, 1] := System.Sin(rads);
    result[0, 2] := translate.x;
    
    result[1, 0] := -System.Sin(rads);
    result[1, 1] := System.Cos(rads) * scale.y;
    result[1, 2] := translate.y;
    
    result[2, 0] := 0;
    result[2, 1] := 0;
    result[2, 2] := 1;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'ScaleRotateTranslateMatrix(const scale: Point2D', '');
    {$ENDIF}
  end;

  function MatrixMultiply(const m1, m2: Matrix2D): Matrix2D; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'MatrixMultiply(const m1, m2: Matrix2D): Matrix2D', '');
    {$ENDIF}
    
      //unwound for performance optimisation
    result[0, 0] := m1[0, 0] * m2[0, 0] +
                    m1[0, 1] * m2[1, 0] +
                    m1[0, 2] * m2[2, 0];
    result[0, 1] := m1[0, 0] * m2[0, 1] +
                    m1[0, 1] * m2[1, 1] +
                    m1[0, 2] * m2[2, 1];
    result[0, 2] := m1[0, 0] * m2[0, 2] +
                    m1[0, 1] * m2[1, 2] +
                    m1[0, 2] * m2[2, 2];

    result[1, 0] := m1[1, 0] * m2[0, 0] +
                    m1[1, 1] * m2[1, 0] +
                    m1[1, 2] * m2[2, 0];
    result[1, 1] := m1[1, 0] * m2[0, 1] +
                    m1[1, 1] * m2[1, 1] +
                    m1[1, 2] * m2[2, 1];
    result[1, 2] := m1[1, 0] * m2[0, 2] +
                    m1[1, 1] * m2[1, 2] +
                    m1[1, 2] * m2[2, 2];

    result[2, 0] := m1[2, 0] * m2[0, 0] +
                    m1[2, 1] * m2[1, 0] +
                    m1[2, 2] * m2[2, 0];
    result[2, 1] := m1[2, 0] * m2[0, 1] +
                    m1[2, 1] * m2[1, 1] +
                    m1[2, 2] * m2[2, 1];
    result[2, 2] := m1[2, 0] * m2[0, 2] +
                    m1[2, 1] * m2[1, 2] +
                    m1[2, 2] * m2[2, 2];
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'MatrixMultiply(const m1, m2: Matrix2D): Matrix2D', '');
    {$ENDIF}
  end;
  
  function MatrixToString(const m: Matrix2D) : String;
  var
    i, j: LongInt;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'MatrixToString(const m: Matrix2D) : String', '');
    {$ENDIF}
    
    result := '-------------------------------' + LineEnding;
    
    for i := 0 to 2 do
    begin
      result := result + '|';
      for j := 0 to 2 do
      begin
        result := result + ' ' + FormatFloat('###0.00', m[i,j]) + ' ';
      end;
      result := result + '|' + LineEnding;
    end;
    result := result + '-------------------------------';
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'MatrixToString(const m: Matrix2D) : String', '');
    {$ENDIF}
  end;  
  
  function MatrixMultiply(const m: Matrix2D; const v: Vector): Vector; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'MatrixMultiply(const m: Matrix2D', '');
    {$ENDIF}
    
    result.x := v.x * m[0,0]  +  v.y * m[0,1] + m[0,2]; 
    result.y := v.x * m[1,0]  +  v.y * m[1,1] + m[1,2]; 
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'MatrixMultiply(const m: Matrix2D', '');
    {$ENDIF}
  end;
  
  procedure ApplyMatrix(const m: Matrix2D; tri: Triangle);
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'ApplyMatrix(const m: Matrix2D', '');
    {$ENDIF}
    
    tri[0] := MatrixMultiply(m, tri[0]);
    tri[1] := MatrixMultiply(m, tri[1]);
    tri[2] := MatrixMultiply(m, tri[2]);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'ApplyMatrix(const m: Matrix2D', '');
    {$ENDIF}
  end;
  
  procedure ApplyMatrix(const m: Matrix2D; pts: Point2DArray);
  var
    i: LongInt;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'ApplyMatrix(const m: Matrix2D', '');
    {$ENDIF}
    
    for i := 0 to High(pts) do
    begin
      pts[i] := MatrixMultiply(m, pts[i]);
    end;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'ApplyMatrix(const m: Matrix2D', '');
    {$ENDIF}
  end;

  {$ifdef FPC}
  operator * (const m: Matrix2D; const v: Vector) r : Vector;
  begin
    r := MatrixMultiply(m, v);
  end;
  {$endif}

  {$ifdef FPC}
  operator * (const m1, m2: Matrix2D) r : Matrix2D;
  begin
    r := MatrixMultiply(m2, m1);
  end;
  {$endif}

//----------------------------------------------------------------------------
// Cosine/Sin/Tan accepting degrees
//----------------------------------------------------------------------------
  
  function Cosine(angle: Single): Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'Cosine(angle: Single): Single', '');
    {$ENDIF}
    
    result := System.Cos(DegToRad(angle));
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'Cosine(angle: Single): Single', '');
    {$ENDIF}
  end;
  
  function Sine(angle: Single): Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'Sine(angle: Single): Single', '');
    {$ENDIF}
    
    result := System.Sin(DegToRad(angle));
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'Sine(angle: Single): Single', '');
    {$ENDIF}
  end;
  
  function Tangent(angle: Single): Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'Tangent(angle: Single): Single', '');
    {$ENDIF}
    
    result := Math.Tan(DegToRad(angle));
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'Tangent(angle: Single): Single', '');
    {$ENDIF}
  end;
  
  const
    EPS    = 0.01;         // smallest positive value: less than that to be considered zero
    EPSEPS = EPS * EPS;        // and its square



  function PointLineDistance(x, y: Single; const line: LineSegment): Single; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'PointLineDistance(x, y: Single', '');
    {$ENDIF}
    
    result := PointLineDistance(PointAt(x, y), line);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'PointLineDistance(x, y: Single', '');
    {$ENDIF}
  end;

  function PointLineDistance(const pt: Point2D; const line: LineSegment): Single; overload;
  var
    sqLineMag, u: Single;
    intersect: Point2D;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'PointLineDistance(const pt: Point2D', '');
    {$ENDIF}
    
    // see Paul Bourke's original article(s)
    // square of line's magnitude (see note in function LineMagnitude)
    sqLineMag := LineMagnitudeSq(line);
    if SqLineMag < EPSEPS then
    begin
      RaiseException('Cannot determine intersection point on line, line is too short');
      exit;
    end;

    //squared unit vector
    u := ( (pt.x - line.startPoint.x) * (line.endPoint.x - line.startPoint.x) +
           (pt.y - line.startPoint.y) * (line.endPoint.y - line.startPoint.y) ) / sqLineMag;

    if (u < EPS) or (u > 1) then
    begin
      //  Closest point does not fall within the line segment,
      //    take the shorter distance to an endpoint
      intersect.x := LineMagnitudeSq(pt.x, pt.y, line.startPoint.x, line.startPoint.y);
      intersect.y := LineMagnitudeSq(pt.x, pt.y, line.endPoint.x, line.endPoint.y);
      result := min(intersect.x, intersect.y);
    end //  if (u < EPS) or (u > 1)
    else
    begin
      //  Intersecting point is on the line, use the formula
      intersect.x := line.startPoint.x + u * (line.endPoint.x - line.startPoint.x);
      intersect.y := line.startPoint.y + u * (line.endPoint.y - line.startPoint.y);
      result := LineMagnitudeSq(pt.x, pt.y, intersect.x, intersect.y);
    end; //  else NOT (u < EPS) or (u > 1)

    // finally convert to actual distance not its square
    result := sqrt(result);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'PointLineDistance(const pt: Point2D', '');
    {$ENDIF}
  end;
  
  function ClosestPointOnCircle(const fromPt: Point2D; const c: Circle): Point2D;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'ClosestPointOnCircle(const fromPt: Point2D', '');
    {$ENDIF}
    
    result := AddVectors(VectorMultiply(UnitVector(VectorFromPoints(c.center, fromPt)), c.radius), c.center);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'ClosestPointOnCircle(const fromPt: Point2D', '');
    {$ENDIF}
  end;
  
  function ClosestPointOnLine(x, y: Single; const line: LineSegment): Point2D; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'ClosestPointOnLine(x, y: Single', '');
    {$ENDIF}
    
    result := ClosestPointOnLine(PointAt(x, y), line);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'ClosestPointOnLine(x, y: Single', '');
    {$ENDIF}
  end;
  
  function ClosestPointOnLine(const fromPt: Point2D; const line: LineSegment): Point2D; overload;
  var
    sqLineMag, u: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'ClosestPointOnLine(const fromPt: Point2D', '');
    {$ENDIF}
    
    // see Paul Bourke's original article(s)
    // square of line's magnitude (see note in function LineMagnitude)
    sqLineMag := LineMagnitudeSq(line);
    if SqLineMag < EPSEPS then begin RaiseException('Cannot determine intersection point on line, line is too short'); exit; end;

    u := ( (fromPt.x - line.startPoint.x)*(line.endPoint.x - line.startPoint.x) + (fromPt.y - line.startPoint.y) * (line.endPoint.y - line.startPoint.y) ) / sqLineMag;

    if (u < EPS) or (u > 1) then
    begin
      //  Closest point does not fall within the line segment,
      //    take the shorter distance to an endpoint
      if LineMagnitudeSq(fromPt.x, fromPt.y, line.startPoint.x, line.startPoint.y) < LineMagnitudeSq(fromPt.x, fromPt.y, line.endPoint.x, line.endPoint.y) then
        result := line.startPoint
      else
        result := line.endPoint;
    end //  if (u < EPS) or (u > 1)
    else
    begin
      //  Intersecting point is on the line, use the formula
      result.x := line.startPoint.x + u * (line.endPoint.x - line.startPoint.x);
      result.y := line.startPoint.y + u * (line.endPoint.y - line.startPoint.y);
      
    end; //  else NOT (u < EPS) or (u > 1)
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'ClosestPointOnLine(const fromPt: Point2D', '');
    {$ENDIF}
  end;
  
  function ClosestPointOnRectFromCircle(const c: Circle; const rect: Rectangle): Point2D;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'ClosestPointOnRectFromCircle(const c: Circle', '');
    {$ENDIF}
    
    result := ClosestPointOnLinesFromCircle(c, LinesFrom(rect));  
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'ClosestPointOnRectFromCircle(const c: Circle', '');
    {$ENDIF}
  
  end;
  
  function ClosestPointOnLineFromCircle(const c: Circle; const line: LineSegment): Point2D;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'ClosestPointOnLineFromCircle(const c: Circle', '');
    {$ENDIF}
    
    result := ClosestPointOnLine(c.center, line);  
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'ClosestPointOnLineFromCircle(const c: Circle', '');
    {$ENDIF}
  end;
  
  function ClosestPointOnLinesFromCircle(const c: Circle; const lines: LinesArray): Point2D;
  var
    i: LongInt;
    dst, minDist: Single;
    pt: Point2D;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'ClosestPointOnLinesFromCircle(const c: Circle', '');
    {$ENDIF}
    
    minDist := -1;
    
    for i := Low(lines) to High(lines) do
    begin
      pt := ClosestPointOnLineFromCircle(c, lines[i]);
      dst := PointPointDistance(pt, c.center);
      
      if (minDist > dst) or (minDist < 0) then
      begin
        minDist := dst;
        result := pt;
      end;
    end;  
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'ClosestPointOnLinesFromCircle(const c: Circle', '');
    {$ENDIF}
  end;
  
  function InsetRectangle(const rect: Rectangle; insetAmount: LongInt): Rectangle;
  var
    dblAmt: LongInt;
  begin
    dblAmt := 2 * insetAmount;
    if (rect.width <= dblAmt) or (rect.height <= dblAmt) then begin result := rect; exit; end;
    
    result := RectangleFrom(rect.x + insetAmount, rect.y + insetAmount, rect.width - dblAmt, rect.height - dblAmt)
  end;
  
  function RectangleFrom(const c: Circle): Rectangle; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'RectangleFrom(const c: Circle): Rectangle', '');
    {$ENDIF}
    
    result.x := c.center.x - c.radius;
    result.y := c.center.y - c.radius;
    result.width := Ceiling(2 * c.radius);
    result.height := result.width;  
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'RectangleFrom(const c: Circle): Rectangle', '');
    {$ENDIF}
  end;
  
  function RectangleFrom(const pt1, pt2: Point2D): Rectangle; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'RectangleFrom(const pt1, pt2: Point2D): Rectangle', '');
    {$ENDIF}
    
    result.x := pt1.x;
    result.y := pt1.y;
    result.width := Ceiling(pt2.x - pt1.x);
    result.height := Ceiling(pt2.y - pt1.y);  
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'RectangleFrom(const pt1, pt2: Point2D): Rectangle', '');
    {$ENDIF}
  end;
  
  function RectangleFrom(const tri: Triangle): Rectangle; overload;
  var
    minX, minY, maxX, maxY: Single;
    i: LongInt;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'RectangleFrom(const tri: Triangle): Rectangle', '');
    {$ENDIF}
    
    minX := tri[0].x; maxX := tri[0].x;
    minY := tri[0].y; maxY := tri[0].y;
    
    for i := 1 to 2 do
    begin
      if tri[i].x < minX then minX := tri[i].x
      else if tri[i].x > maxX then maxX := tri[i].x;
      
      if tri[i].y < minY then minY := tri[i].y
      else if tri[i].y > maxY then maxY := tri[i].y;
    end;
    
    result.x := minX;
    result.y := minY;
    result.width := Ceiling(maxX - minX);
    result.height := Ceiling(maxY - minY);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'RectangleFrom(const tri: Triangle): Rectangle', '');
    {$ENDIF}
  end;
  
  function RectangleFrom(const lines: LinesArray): Rectangle; overload;
  var
    minX, minY, maxX, maxY: Single;
    i: LongInt;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'RectangleFrom(const lines: LinesArray): Rectangle', '');
    {$ENDIF}
    
    if Length(lines) = 0 then exit;
    
    minX := lines[0].startPoint.x; maxX := lines[0].startPoint.x;
    minY := lines[0].startPoint.y; maxY := lines[0].startPoint.y;
    
    for i := 0 to High(lines) do
    begin
      if lines[i].startPoint.x < minX then minX := lines[i].startPoint.x
      else if lines[i].startPoint.x > maxX then maxX := lines[i].startPoint.x;

      if lines[i].startPoint.y < minY then minY := lines[i].startPoint.y
      else if lines[i].startPoint.y > maxY then maxY := lines[i].startPoint.y;
      
      if lines[i].endPoint.x < minX then minX := lines[i].endPoint.x
      else if lines[i].endPoint.x > maxX then maxX := lines[i].endPoint.x;
      
      if lines[i].endPoint.y < minY then minY := lines[i].endPoint.y
      else if lines[i].endPoint.y > maxY then maxY := lines[i].endPoint.y;
    end;
    
    result.x := minX;
    result.y := minY;
    result.width := Ceiling(maxX - minX);
    result.height := Ceiling(maxY - minY);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'RectangleFrom(const lines: LinesArray): Rectangle', '');
    {$ENDIF}
  end;
  
  function RectangleFrom(const line: LineSegment): Rectangle; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'RectangleFrom(const line: LineSegment): Rectangle', '');
    {$ENDIF}
    
    result.x := Min(line.startPoint.x, line.endPoint.x);
    result.y := Min(line.startPoint.y, line.endPoint.y);
    result.width := Ceiling(Max(line.startPoint.x, line.endPoint.x) - result.x);
    result.height := Ceiling(Max(line.startPoint.y, line.endPoint.y) - result.y);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'RectangleFrom(const line: LineSegment): Rectangle', '');
    {$ENDIF}
  end;
    
  function PointOnLine(const pt: Point2D; const line: LineSegment): Boolean;
  const SMALL = 0.9;
    function SimpleComparisonXSame(): Boolean;
    var
      minY, maxY: Single;
    begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'PointOnLine(const pt: Point2D', '');
    {$ENDIF}
    
      minY := Min(line.startPoint.y, line.endPoint.Y);
      maxY := Max(line.startPoint.y, line.endPoint.Y);
      
      result := 
        (pt.x >= line.startPoint.x - SMALL) and (pt.x <= line.startPoint.x + SMALL) and
        (pt.y >= minY) and (pt.y <= maxY);
    end;
    
    function SimpleComparisonYSame(): Boolean;
    var
      minX, maxX: Single;
    begin
      minX := Min(line.startPoint.x, line.endPoint.x);
      maxX := Max(line.startPoint.x, line.endPoint.x);
      
      result := 
        (pt.y >= line.startPoint.y - SMALL) and (pt.y <= line.startPoint.y + SMALL) and
        (pt.x >= minX) and (pt.x <= maxX);
    end;
  var
    sqLineMag, lx, ly, m, c : Single;
  begin
    //Lines Magnitude must be at least 0.0001
    sqLineMag := LineMagnitudeSq(line);
    if SqLineMag < EPSEPS then
    begin
      RaiseException('Cannot determine if point is on line, line is too short'); exit;
    end;
          
    //Obtain the other variables for the Line Algorithm
    if line.endPoint.x = line.startPoint.x then
    begin
      result := SimpleComparisonXSame();
      exit;
    end;
    if line.endPoint.y = line.startPoint.y then
    begin
      result := SimpleComparisonYSame();
      exit;     
    end;
    
    m := (line.endPoint.y - line.startPoint.y) / (line.endPoint.x - line.startPoint.x);
    c := line.startPoint.y - (m * line.startPoint.x);

    ly := (m * pt.x) + c;
    lx := (pt.y - c) / m;

    result := (lx >= pt.x - SMALL) and
              (lx <= pt.x + SMALL) and
              (ly >= pt.y - SMALL) and
              (ly <= pt.y + SMALL) and
              PointInRect(pt, RectangleFrom(line));
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'PointOnLine(const pt: Point2D', '');
    {$ENDIF}
  end;

  function PointOnPoint(const pt1,pt2:Point2d):Boolean;
  begin
    if ((pt1.X = pt2.X) AND (pt1.Y = pt2.Y)) then
    begin
      result:=True;
    end
    else
    begin
      result:=False;
    end
  end;

  function PointOnLineList(const pt:point2d; const s:Shape):Boolean;
  var
  pts : Point2dArray;
  i :LongInt;
  begin
    pts := ShapePoints(s);
    result:=False;
    for i := 0 to Length(pts) div 2 - 1 do
    begin
      if PointOnLine(pt, Linefrom(pts[i * 2], pts[i * 2 + 1])) then
      begin
        result:=True;
        exit;
      end
    end;
  end;

  function PointOnLineStrip(const pt:point2d; const s:Shape):Boolean;
  var
  pts : Point2dArray;
  i :LongInt;
  begin
    pts := ShapePoints(s);
    result:=False;
    
    for i := 0 to Length(pts) - 2 do
    begin
      if PointOnLine(pt,Linefrom(pts[i], pts[i+ 1])) then
      begin
        result:=True;
        exit;
      end;
    end;
  end;

  function PointInTriangleList(const pt:point2d; const s:shape):Boolean;
  var
  pts : Point2dArray;
  i :LongInt;
  begin
    pts := ShapePoints(s);
    result:=False;
    for i := 0 to Length(pts) div 3 - 1 do
    begin
      if PointInTriangle(pt, TriangleFrom(
          pts[i * 3].x, pts[i * 3].y,
          pts[i * 3 + 1].x, pts[i * 3 + 1].y,
          pts[i * 3 + 2].x, pts[i * 3 + 2].y)) then
      begin
        result:=True;
        exit;
      end
    end;
  end;

  function PointInTriangleFan(const pt:point2d; const s:shape):Boolean;
  var
  pts : Point2dArray;
  i :LongInt;
  begin
    pts := ShapePoints(s);
    result:=False;
     for i := 0 to Length(pts) - 3 do
    begin
      if PointInTriangle(pt, TriangleFrom(
          pts[0].x,pts[0].y,
          pts[i + 1].x, pts[i + 1].y,
          pts[i + 2].x, pts[i + 2].y)) then
      begin
        result:=True;
        exit;
      end
    end;
  end;

  function PointInTriangleStrip(const pt:point2d; const s:shape):Boolean;
  var
  pts : Point2dArray;
  i :LongInt;
  begin
    pts := ShapePoints(s);
    result:=False;
    for i := 0 to Length(pts) - 3 do
    begin
      if PointInTriangle(pt, TriangleFrom(
        pts[i].x,pts[i].y,
        pts[i + 1].x, pts[i + 1].y,
        pts[i + 2].x, pts[i + 2].y)) then
      begin
        result:=True;
        exit;
      end
    end;
  end;

  function PointInShape(const pt: Point2d; s:Shape):Boolean;
  var
  k : ShapeKind;
  pts : Point2dArray;
  begin
    k := PrototypeKind(s^.prototype);
    pts := ShapePoints(s);
    
    case k of
      pkPoint: result := PointOnPoint(pt, s^.pt);
      pkCircle: result := PointInCircle(pt, CircleAt(s^.pt,round(PointPointDistance(pts[0], pts[1]))));
      // pkEllipse: result := 2;
      pkLine: result := PointOnLine(pt, Linefrom(pts[0],pts[1]));
      pkTriangle: result := PointInTriangle(pt,TriangleFrom(pts[0],pts[1],pts[2]));
      pkLineList: result := PointOnLineList(pt,s);
      pkLineStrip: result := PointOnLineStrip(pt,s);
      //pkPolygon: result := 3;
      pkTriangleStrip: result := PointInTriangleStrip(pt,s);
      pkTriangleFan: result := PointInTriangleFan(pt,s);
      pkTriangleList: result := PointInTriangleList(pt,s);
      else
        begin
          RaiseException('Shape not Recognized when checking if point is in shape.');
          exit;
        end;
    end;
  end;
  
  function LineFrom(const pt1, pt2: Point2D): LineSegment; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'LineFrom(const pt1, pt2: Point2D): LineSegment', '');
    {$ENDIF}
    
    result := LineFrom(pt1.x, pt1.y, pt2.x, pt2.y);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'LineFrom(const pt1, pt2: Point2D): LineSegment', '');
    {$ENDIF}
  end;
  
  function LineFrom(x1, y1, x2, y2: Single): LineSegment; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'LineFrom(x1, y1, x2, y2: Single): LineSegment', '');
    {$ENDIF}
    
    result.startPoint.x := x1;
    result.startPoint.y := y1;
    result.endPoint.x := x2;
    result.endPoint.y := y2;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'LineFrom(x1, y1, x2, y2: Single): LineSegment', '');
    {$ENDIF}
  end;
  
  function LinesFrom(const tri: Triangle): LinesArray; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'LinesFrom(const tri: Triangle): LinesArray', '');
    {$ENDIF}
    
    SetLength(result, 3);
    result[0] := LineFrom(tri[0], tri[1]);
    result[1] := LineFrom(tri[1], tri[2]);
    result[2] := LineFrom(tri[2], tri[0]);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'LinesFrom(const tri: Triangle): LinesArray', '');
    {$ENDIF}
  end;

  function LinesFromShapeAsTriangleStrip(s: shape): LinesArray; overload;
  var
    pts:Point2DArray;
    i:LongInt;
  begin
    pts:= ShapePoints(s);

    //WriteLn('ShapeLineCount: ', ShapeLineCount(s));
    SetLength(result, ShapeLineCount(s));
    if Length(result) <= 0 then exit;
    
    for i := 0 to Length(pts) - 3 do
    begin
      result[i * 2]     := LineFrom(pts[i], pts[i + 1]);
      result[i * 2 + 1] := LineFrom(pts[i], pts[i + 2]);
      //WriteLn('  Line ', LineToString(result[i*2]));
      //WriteLn('  Line ', LineToString(result[i*2 + 1]));
    end;

    result[high(result)] := LineFrom(pts[High(pts) - 1], pts[High(pts)]);
    //WriteLn('  Line ', LineToString(result[High(result)]));
  end;
  
  function LinesFrom(s: shape): LinesArray; overload;
  var
  k: ShapeKind;
  pts: Point2DArray;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'LinesFrom(const tri: Triangle): LinesArray', '');
    {$ENDIF}
    k := PrototypeKind(s^.prototype);
    pts := ShapePoints(s);
    
    case k of
      pkCircle: SetLength(result, 0);
      //pkLine: result := PointOnLine(pt, Linefrom(pts[0],pts[1]));
      pkTriangle: result := LinesFrom(TriangleFrom(pts[0],pts[1],pts[2]));
      //pkLineList: result := PointOnLineList(pt,s);
      //pkLineStrip: result := PointOnLineStrip(pt,s);
      pkTriangleStrip: result := LinesFromShapeAsTriangleStrip(s);
      //pkTriangleFan: result := PointInTriangleFan(pt,s);
      //pkTriangleList: result := PointInTriangleList(pt,s);
      else
        begin
          RaiseException('LinesFrom not implemented with this kind of shape.');
          exit;
        end;
    end;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'LinesFrom(const tri: Triangle): LinesArray', '');
    {$ENDIF}
  end;
  
  function LinesFrom(const rect: Rectangle): LinesArray; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'LinesFrom(const rect: Rectangle): LinesArray', '');
    {$ENDIF}
    
    SetLength(result, 4);
    
    with rect do
    begin
      result[0] := LineFrom(x, y, x + width, y);
      result[1] := LineFrom(x, y, x, y + height);
      result[2] := LineFrom(x + width, y, x + width, y + height);
      result[3] := LineFrom(x, y + height, x + width, y + height);
    end;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'LinesFrom(const rect: Rectangle): LinesArray', '');
    {$ENDIF}
  end;
  
  function RectangleCenter(const rect: Rectangle): Point2D;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'RectangleCenter(const rect: Rectangle): Point2D', '');
    {$ENDIF}
    
    result.x := rect.x + (rect.width / 2);
    result.y := rect.y + (rect.height / 2);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'RectangleCenter(const rect: Rectangle): Point2D', '');
    {$ENDIF}
  end;
  
  function PointAt(x, y: Single): Point2D; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'PointAt(x, y: Single): Point2D', '');
    {$ENDIF}
    
    result.x := x;
    result.y := y;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'PointAt(x, y: Single): Point2D', '');
    {$ENDIF}
  end;

  function PointToString(const pt: Point2D): String;
  begin
    result := 'Pt @' + FloatToStr(pt.x) + ':' + FloatToStr(pt.y);
  end;
  
  function TriangleToString(const tri: Triangle): String;
  begin
    result := 'Triangle @' + PointToString(tri[0]) + ' - ' + PointToString(tri[1]) + ' - ' + PointToString(tri[2]);
  end;
  
  function RectangleToString(const rect:Rectangle): String;
  begin
    result := 'Rect @' + FloatToStr(rect.x) + ':' + FloatToStr(rect.y) + ' ' + IntToStr(rect.width) + 'x' + IntToStr(rect.height);
  end;

  function LineToString(const ln: LineSegment): String;
  begin
    result := 'From ' + PointToString(ln.startPoint) + ' to ' + PointToString(ln.endPoint);
  end;
  
  function PointAt(const startPoint: Point2D; const offset: Vector): Point2D; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'PointAt(const startPoint: Point2D', '');
    {$ENDIF}
    
    result.x := startPoint.x + offset.x;
    result.y := startPoint.y + offset.y;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'PointAt(const startPoint: Point2D', '');
    {$ENDIF}
  end;

  function LineFromVector(const pt: Point2D; const mv: Vector): LineSegment; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'LineFromVector(const pt: Point2D', '');
    {$ENDIF}
    
    result := LineFromVector(pt.x, pt.Y, mv);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'LineFromVector(const pt: Point2D', '');
    {$ENDIF}
  end;
  
  function LineFromVector(x, y: Single; const mv: Vector): LineSegment; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'LineFromVector(x, y: Single', '');
    {$ENDIF}
    
    result.startPoint.x := x;
    result.startPoint.y := y;
    result.endPoint.x := x + mv.x;
    result.endPoint.y := y + mv.y;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'LineFromVector(x, y: Single', '');
    {$ENDIF}
  end;
  
  function LineFromVector(const mv: Vector): LineSegment; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'LineFromVector(const mv: Vector): LineSegment', '');
    {$ENDIF}
    
    result := LineFromVector(0, 0, mv);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'LineFromVector(const mv: Vector): LineSegment', '');
    {$ENDIF}
  end;

  function RectangleAfterMove(const rect: Rectangle; const mv: Vector): Rectangle;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'RectangleAfterMove(const rect: Rectangle', '');
    {$ENDIF}
    
    result := rect;
    result.x := result.x + mv.x;
    result.y := result.y + mv.y;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'RectangleAfterMove(const rect: Rectangle', '');
    {$ENDIF}
  end;
  
  function RectangleTop(const rect: Rectangle): Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'RectangleTop(const rect: Rectangle): Single', '');
    {$ENDIF}
    
    if rect.height > 0 then result := rect.y
    else result := rect.y + rect.height; //add negative height
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'RectangleTop(const rect: Rectangle): Single', '');
    {$ENDIF}
  end;
  
  function RectangleBottom(const rect: Rectangle): Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'RectangleBottom(const rect: Rectangle): Single', '');
    {$ENDIF}
    
    if rect.height > 0 then result := rect.y + rect.height
    else result := rect.y; //y is bottom most
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'RectangleBottom(const rect: Rectangle): Single', '');
    {$ENDIF}
  end;

  function RectangleLeft(const rect: Rectangle): Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'RectangleLeft(const rect: Rectangle): Single', '');
    {$ENDIF}
    
    if rect.width > 0 then result := rect.x
    else result := rect.x + rect.width; //add negative width
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'RectangleLeft(const rect: Rectangle): Single', '');
    {$ENDIF}
  end;

  function RectangleRight(const rect: Rectangle): Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'RectangleRight(const rect: Rectangle): Single', '');
    {$ENDIF}
    
    if rect.width > 0 then result := rect.x + rect.width
    else result := rect.x; //x is right most
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'RectangleRight(const rect: Rectangle): Single', '');
    {$ENDIF}
  end;
  
  function RectangleTopLeft(const rect: Rectangle): Point2D;
  begin
    result := PointAt(RectangleLeft(rect), RectangleTop(rect));
  end;
  
  function RectangleTopRight(const rect: Rectangle): Point2D;
  begin
    result := PointAt(RectangleRight(rect), RectangleTop(rect));
  end;
  
  function RectangleBottomLeft(const rect: Rectangle): Point2D;
  begin
    result := PointAt(RectangleLeft(rect), RectangleBottom(rect));
  end;
  
  function RectangleBottomRight(const rect: Rectangle): Point2D;
  begin
    result := PointAt(RectangleRight(rect), RectangleBottom(rect));
  end;
  
  function RectangleCenterTop(const rect: Rectangle): Point2D;
  begin
     result := PointAt((RectangleLeft(rect) + RectangleRight(rect)) / 2, RectangleTop(rect));
  end;
  
  function RectangleCenterBottom(const rect: Rectangle): Point2D;
  begin
    result := PointAt((RectangleLeft(rect) + RectangleRight(rect)) / 2, RectangleBottom(rect));
  end;

  function RectangleCenterLeft(const rect: Rectangle): Point2D;
  begin
    result := PointAt(RectangleLeft(rect), (RectangleTop(rect) + RectangleBottom(rect)) / 2);
  end;
  
  function RectangleCenterRight(const rect: Rectangle): Point2D;
  begin
    result := PointAt(RectangleRight(rect), (RectangleTop(rect) + RectangleBottom(rect)) / 2);
  end;
  
  function RectangleOffset(const rect: Rectangle; const vec: Vector): Rectangle;
  begin
    result := RectangleFrom(PointAdd(vec, RectangleTopLeft(rect)), rect.width, rect.height);
  end;
  
  function RectangleFrom(x, y: Single; w, h: LongInt): Rectangle; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'RectangleFrom(x, y: Single', '');
    {$ENDIF}
    
    result.x := x;
    result.y := y;
    result.width := w;
    result.height := h;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'RectangleFrom(x, y: Single', '');
    {$ENDIF}
  end;
  
  function RectangleFrom(const pt: Point2D; width, height: LongInt): Rectangle; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'RectangleFrom(const pt: Point2D', '');
    {$ENDIF}
    
    result := RectangleFrom(pt.x, pt.y, width, height);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'RectangleFrom(const pt: Point2D', '');
    {$ENDIF}
  end;
  
  function TriangleFrom(ax, ay, bx, by, cx, cy: Single): Triangle; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'TriangleFrom(ax, ay, bx, by, cx, cy: Single): Triangle', '');
    {$ENDIF}
    
    result := TriangleFrom(PointAt(ax, ay), PointAt(bx, by), PointAt(cx, cy));
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'TriangleFrom(ax, ay, bx, by, cx, cy: Single): Triangle', '');
    {$ENDIF}
  end;
  
  function TriangleFrom(const a, b, c: Point2D): Triangle; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'TriangleFrom(const a, b, c: Point2D): Triangle', '');
    {$ENDIF}
    
    result[0] := a;
    result[1] := b;
    result[2] := c;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'TriangleFrom(const a, b, c: Point2D): Triangle', '');
    {$ENDIF}
  end;
  
  function PointInTriangle(const pt : Point2D; const tri : Triangle): Boolean;
  var
    v0, v1, v2 : Vector;
    a, b, c, p: Vector;
    dot00, dot01, dot02, dot11, dot12 : Single;
    invDenom, u, v: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'PointInTriangle(const pt : Point2D', '');
    {$ENDIF}
    
    //Convert Points to vectors
    p := VectorToPoint(pt);
    a := VectorToPoint(tri[0]);
    b := VectorToPoint(tri[1]);
    c := VectorToPoint(tri[2]);
    
    // Compute vectors    
    v0 := SubtractVectors(c, a);
    v1 := SubtractVectors(b, a);
    v2 := SubtractVectors(p, a);

    // Compute dot products
    dot00 := DotProduct(v0, v0);
    dot01 := DotProduct(v0, v1);
    dot02 := DotProduct(v0, v2);
    dot11 := DotProduct(v1, v1);
    dot12 := DotProduct(v1, v2);

    // Compute barycentric coordinates
    if dot00 * dot11 - dot01 * dot01 = 0 then
    begin
      result := false;
      exit;
    end;
    
    invDenom := 1 / (dot00 * dot11 - dot01 * dot01);
    u := (dot11 * dot02 - dot01 * dot12) * invDenom;
    v := (dot00 * dot12 - dot01 * dot02) * invDenom;

    // Check if point is in triangle
    result := ((u > 0) and (v > 0) and (u + v < 1));
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'PointInTriangle(const pt : Point2D', '');
    {$ENDIF}
  end;

  function PointInCircle(const pt: Point2D; const c: Circle): Boolean;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'PointInCircle(const pt: Point2D', '');
    {$ENDIF}
    
    result := PointPointDistance(pt, c.center) <= c.radius;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'PointInCircle(const pt: Point2D', '');
    {$ENDIF}
  end;

  function TriangleBarycenter(const tri: Triangle): Point2D;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'TriangleBarycenter(const tri: Triangle): Point2D', '');
    {$ENDIF}
    
    result.x := (tri[0].x + tri[1].x + tri[2].x) / 3;
    result.y := (tri[0].y + tri[1].y + tri[2].y) / 3;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'TriangleBarycenter(const tri: Triangle): Point2D', '');
    {$ENDIF}
  end;

  function LineMidPoint(const line: LineSegment): Point2D;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'LineMidPoint(const line: LineSegment): Point2D', '');
    {$ENDIF}
    
    result.x := line.startPoint.x + (line.endPoint.x - line.startPoint.x) / 2;
    result.y := line.startPoint.y + (line.endPoint.y - line.startPoint.y) / 2;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'LineMidPoint(const line: LineSegment): Point2D', '');
    {$ENDIF}
  end;
  
  function ShapeAABB(s: Shape): Rectangle;
  var
    pts : Point2DArray;
    minPt, maxPt: Point2D;
    i, max: LongInt;
    r, l, t, b, temp: Single;
    subRect: Rectangle;
    kind: ShapeKind;
  begin
    pts :=    ShapePoints(s);
    result := RectangleFrom(0,0,0,0);
    kind :=   ShapeShapeKind(s);
    if (LongInt(kind) = -1) or (Length(pts) < MinimumPointsForKind(kind)) then exit;
    
    minPt := pts[0];
    maxPt := minPt;
    
    if kind = pkPoint then 
      //do nothing as the point is the maximum
    else if kind = pkCircle then
    begin
      temp := PointPointDistance(pts[0], pts[1]);
      minPt.x := minPt.x - temp;
      minPt.y := minPt.y - temp;
      maxPt.x := maxPt.x + temp;
      maxPt.y := maxPt.y + temp;
    end
    else
    begin
      
      case kind of
        pkLine: max := 1;         // line from 0 to 1
        pkTriangle: max := 2;     // triangle 0,1,2
        pkLineList: max := High(pts) - (Length(pts) mod 2); // even numbers
        pkLineStrip: max := High(pts); // all points in strip
        pkTriangleList: max := High(pts) - (Length(pts) mod 3); //groups of 3
        pkTriangleStrip: max := High(pts);
        pkTriangleFan: max := High(pts);
      else max := High(pts);
      end;
      
      for i := low(pts) + 1 to max do
      begin
        if pts[i].x < minPt.x then minPt.x := pts[i].x
        else if pts[i].x > maxPt.x then  maxPt.x := pts[i].x;
      
        if pts[i].y < minPt.y then minPt.y := pts[i].y
        else if pts[i].Y > maxPt.y then maxPt.y := pts[i].y;
      end;
    end;
    
    for i := Low(s^.subShapes) to High(s^.subShapes) do
    begin
      subRect := ShapeAABB(s^.subShapes[i]);
      
      r := RectangleRight(subRect);
      l := RectangleLeft(subRect);
      t := RectangleTop(subRect);
      b := RectangleBottom(subRect);
      
      if l < minPt.x then minPt.x := l;
      if r > maxPt.x then maxPt.x := r;
      
      if t < minPt.y then minPt.y := t;
      if b > maxPt.y then maxPt.y := b;
    end;
    
    result := RectangleFrom(minPt, maxPt);
    //DrawRectangle(colorYellow, result);
  end;
  
  function TriangleAABB(const tri: Triangle): Rectangle;
  var
    minPt, maxPt: Point2D;
    i: LongInt;
  begin
    minPt := tri[0];
    maxPt := tri[0];
    
    for i := 1 to 2 do
    begin
      if minPt.x > tri[i].x then minPt.x := tri[i].x
      else if maxPt.x < tri[i].x then maxPt.x := tri[i].x;
      
      if minPt.y > tri[i].y then minPt.y := tri[i].y
      else if maxPt.y < tri[i].y then maxPt.y := tri[i].y;
    end;
    
    result := RectangleFrom(minPt, maxPt);
  end;
  
  function TrianglesRectangleIntersect(const tri: TriangleArray; const rect: Rectangle): Boolean;
  var
    i: LongInt;
  begin
    for i := Low(tri) to High(tri) do
    begin
      if TriangleRectangleIntersect(tri[i], rect) then
      begin
        result := true;
        exit;
      end;
    end;
    
    // none intersect
    result := false;
  end;
  
  function TriangleRectangleIntersect(const tri: Triangle; const rect: Rectangle): Boolean;
  var
    r, l, t, b: Single;
    
    function _TriLineRectangleTest(const pt1, pt2: Point2D): Boolean;
    var
      m,c: Single;
      top_intersection, bottom_intersection: Single;
      toptrianglepoint, bottomtrianglepoint: Single;
      topoverlap, botoverlap: Single;
    begin
      if (pt2.x - pt1.x = 0) then m := 0
      else
      m := (pt2.y - pt1.y) / (pt2.x - pt1.x);
      
      c := pt1.y - (m * pt1.x);
      
      // if the line is going up from right to left then the top intersect point is on the left
      if m > 0 then
      begin
        top_intersection    := m * l + c;
        bottom_intersection := m * r + c;
      end
      // otherwise it's on the right
      else
      begin
        top_intersection    := m * l + c;
        bottom_intersection := m * l + c;
      end;
      
      // work out the top and bottom extents for the triangle
      if pt1.y < pt2.y then
      begin
        toptrianglepoint    := pt1.y;
        bottomtrianglepoint := pt2.y;
      end
      else
      begin
        toptrianglepoint    := pt2.y;
        bottomtrianglepoint := pt1.y;
      end;
      
      // and calculate the overlap between those two bounds
      topoverlap := iif(top_intersection>toptrianglepoint, top_intersection, toptrianglepoint);
      botoverlap := iif(bottom_intersection<bottomtrianglepoint, bottom_intersection, bottomtrianglepoint);
      
      // (topoverlap<botoverlap) :
      // if the intersection isn't the right way up then we have no overlap
      
      // (!((botoverlap<t) || (topoverlap>b)) :
      // If the bottom overlap is higher than the top of the rectangle or the top overlap is
      // lower than the bottom of the rectangle we don't have intersection. So return the negative
      // of that. Much faster than checking each of the points is within the bounds of the rectangle.
      result := (topoverlap<botoverlap) and (not((botoverlap<t) or (topoverlap>b)));
    end;
  begin
    result := false;
    // Perform bounding box check
    if not RectanglesIntersect(rect, TriangleAABB(tri)) then exit;
    
    r := RectangleRight(rect);
    l := RectangleLeft(rect);
    t := RectangleTop(rect);
    b := RectangleBottom(rect);
    
    // Check line intersects see http://sebleedelisle.com/2009/05/super-fast-trianglerectangle-intersection-test/
    result := _TriLineRectangleTest(tri[0],tri[1])
              or _TriLineRectangleTest(tri[1],tri[2])
              or _TriLineRectangleTest(tri[2],tri[0]);
    
    if not result then
    begin
      //check rect points in triangle
      result := PointInTriangle(PointAt(l, t), tri) or 
                PointInTriangle(PointAt(l, b), tri) or 
                PointInTriangle(PointAt(r, t), tri) or
                PointInTriangle(PointAt(r, b), tri);
    end;
  end;
  
  function RectanglesIntersect(const rect1, rect2: Rectangle): Boolean;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'RectanglesIntersect(const rect1, rect2: Rectangle): Boolean', '');
    {$ENDIF}
    
    if RectangleBottom(rect1) < RectangleTop(rect2) then result := false
    else if RectangleTop(rect1) > RectangleBottom(rect2) then result := false
    else if RectangleRight(rect1) < RectangleLeft(rect2) then result := false
    else if RectangleLeft(rect1) > RectangleRight(rect2) then result := false
    else result := true;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'RectanglesIntersect(const rect1, rect2: Rectangle): Boolean', '');
    {$ENDIF}
  end;
  
  function ShapeCircle(s: Shape): Circle;
  begin
    result := ShapeCircle(s, PointAt(0,0));
  end;
  
  function ShapeCircle(s: Shape; const offset: Point2D): Circle;
  var
    pts: Point2DArray;
  begin
    pts := ShapePoints(s);
    if length(pts) < 2 then begin result := CircleAt(0,0,0); exit; end;
      
    result := CircleAt(PointAdd(pts[0], offset), Round(PointPointDistance(pts[0], pts[1])));
  end;
  
  function ShapeLine(s: Shape): LineSegment;
  begin
    result := ShapeLine(s, PointAt(0,0));
  end;
  
  function ShapeLine(s: Shape; const offset: Point2D): LineSegment;
  var
    pts: Point2DArray;
  begin
    pts := ShapePoints(s);
    if length(pts) < 2 then begin result := LineFrom(0,0,0,0); exit; end;
      
    result := LineFrom(PointAdd(pts[0], offset), PointAdd(pts[1], offset));
  end;
  
  function ShapeTriangle(s: Shape): Triangle;
  begin
    result := ShapeTriangle(s, PointAt(0,0));
  end;
  
  function ShapeTriangle(s: Shape; const offset: Point2D): Triangle;
  var
    pts: Point2DArray;
  begin
    pts := ShapePoints(s);
    if length(pts) < 3 then begin result := TriangleFrom(0,0,0,0,0,0); exit; end;
      
    result := TriangleFrom( PointAdd(pts[0], offset), 
                            PointAdd(pts[1], offset), 
                            PointAdd(pts[2], offset));
  end;
  
  function ShapeTriangleCount(s: Shape): LongInt;
  begin
    result := ShapeKindTriangleCount(s, ShapeShapeKind(s));
  end;
  
  function ShapeKindTriangleCount(s: Shape; kind: ShapeKind): LongInt;
  var
    pts: LongInt;
  begin
    result := 0;

    pts := ShapePointCount(s);

    case kind of
      pkCircle: result := 0;
      pkLine: result := 0;
      pkTriangle: result := 1;
      pkLineList: result := 0;
      pkLineStrip: result := 0;
      //pkPolygon: result := 3;
      pkTriangleStrip, pkTriangleFan: result := pts - 2;
      pkTriangleList: result := pts div 3;
      else
        begin
          RaiseException('Shape not Recognized when getting line out of shape.');
          exit;
        end;
    end;
  end;
  
  function ShapeLineCount(s: Shape): LongInt;
  begin
    if assigned(s) then
      result := ShapeKindLineCount(s, ShapeShapeKind(s))
    else
      result := 0;
  end;
  
  function ShapeKindLineCount(s: Shape; kind: ShapeKind): LongInt;
  var
    pts: LongInt;
  begin
    result := 0;
    
    pts := ShapePointCount(s);
    
    case kind of
      pkCircle: result := 0;
      pkLine: result := 1;
      pkTriangle: result := 3;
      pkLineList: result := pts div 2;
      pkLineStrip: result := pts - 1;
      //pkPolygon: result := 3;
      pkTriangleStrip, pkTriangleFan: 
      begin
        result := 2 * pts - 3;
        if result < 0 then result := 0;
      end;
      pkTriangleList: result := (pts div 3) * 3;
      else
        begin
          RaiseException('Shape not Recognized when getting line out of shape.');
          exit;
        end;
    end;
  end;
  
  function ShapeLines(s: Shape): LinesArray;
  begin
    if assigned(s) then
      result := ShapeLines(s, ShapeShapeKind(s), PointAt(0,0))
    else
      result := nil;
  end;
  
  function ShapeLines(s: Shape; kind: ShapeKind): LinesArray;
  begin
    result := ShapeLines(s, kind, PointAt(0,0));
  end;
  
  function ShapeLines(s: Shape; kind: ShapeKind; const offset: Point2D): LinesArray;
  var
    pts,pts1: Point2DArray;
    i: LongInt;
  begin
    pts := ShapePoints(s);
    SetLength(pts1, length(pts));
    
    if length(pts) < 2 then begin SetLength(result, 0); exit; end;
    if not ((kind = pkLineList) or (kind = pkLineStrip)) then exit;
    
    if kind = pkLineList then SetLength(result, Length(pts) div 2)
    else SetLength(result, Length(pts) - 1);
    
    for i := 0 to high(pts1) do pts1[i] := PointAdd(pts[i], offset);
    
    for i := 0 to High(result) do
    begin
      if kind = pkLineList then
        result[i] := LineFrom(pts1[i * 2], pts1[i * 2 + 1])
      else // strip
        result[i] := LineFrom(pts1[i], pts1[i + 1]);
    end;
  end;
  
  function ShapeTriangles(s: Shape): TriangleArray; overload;
  begin
    if assigned(s) then
      result := ShapeTriangles(s, ShapeShapeKind(s))
    else
      result := nil;
  end;
  
  function ShapeTriangles(s: Shape; kind: ShapeKind): TriangleArray;
  begin
    result := ShapeTriangles(s, kind, PointAt(0,0));
  end;
  
  function ShapeTriangles(s: Shape; kind: ShapeKind; const offset: Point2D): TriangleArray;
  var
    pts, pts1: Point2DArray;
    i: LongInt;
  begin
    SetLength(result, 0);
    if not ((kind = pkTriangleStrip) or (kind = pkTriangleFan) or (kind = pkTriangleList) or (kind = pkTriangle)) then exit;
    pts := ShapePoints(s);
    if length(pts) < 3 then begin exit; end;
    
    if kind = pkTriangleList then SetLength(result, Length(pts) div 3)
    else if kind = pkTriangle then SetLength(result, 1)
    else SetLength(result, Length(pts) - 2);
    
    SetLength(pts1, Length(pts));
    for i := 0 to high(pts) do pts1[i] := PointAdd(pts[i], offset);
    
    for i := 0 to High(result) do
    begin
      case kind of
        pkTriangle:     result[i] := TriangleFrom(pts1[i],     pts1[i + 1],     pts1[i + 2]);
        pkTriangleList: result[i] := TriangleFrom(pts1[i * 3], pts1[i * 3 + 1], pts1[i * 3 + 2]);
        pkTriangleStrip:result[i] := TriangleFrom(pts1[i],     pts1[i + 1],     pts1[i + 2]);
        pkTriangleFan:  result[i] := TriangleFrom(pts1[0],     pts1[i + 1],     pts1[i + 2]);
      end;
    end;
  end;
  
  function LinesIntersect(const lines, lines1: LinesArray): Boolean;
  var
    i: LongInt;
  begin
    result := false;
    if (Length(lines) = 0) or (Length(lines1) = 0) then exit;
    
    for i := 0 to High(lines) do
    begin
      if LineIntersectsLines(lines[i], lines1) then 
      begin
        result := True;
        exit;
      end;
    end;
  end;
  
  function LinesRectIntersect(const lines: LinesArray; const r: Rectangle): Boolean;
  begin
    result := LinesIntersect(lines, LinesFrom(r));
  end;
  
  function ShapeRectangleIntersect(s: Shape; const rect: Rectangle): Boolean; overload;
  var
    kind: ShapeKind;
    i: LongInt;
  begin
    result := false;
    
    if not assigned(s) then exit;
    if not RectanglesIntersect(rect, ShapeAABB(s)) then exit;
    
    kind := ShapeShapeKind(s);
    
    case kind of
      pkPoint:      result := true;
      pkCircle:     result := CircleRectCollision(ShapeCircle(s), rect);
      pkLine:       result := RectLineCollision(rect, ShapeLine(s));
      pkTriangle:   result := TriangleRectangleIntersect(ShapeTriangle(s), rect);
      pkLineList, pkLineStrip:  
                    result := LinesRectIntersect(ShapeLines(s, kind), rect);
      pkTriangleStrip, pkTriangleFan, pkTriangleList:
                    result := TrianglesRectangleIntersect(ShapeTriangles(s, kind), rect);
    end;
    
    if not result then
    begin
      //Check sub shapes
      for i := 0 to High(s^.subShapes) do
      begin
        // if we have an intersection then return true
        if ShapeRectangleIntersect(s^.subShapes[i], rect) then
        begin
          result := true;
          exit;
        end;
      end;
    end;
  end;
  
  function Intersection(const rect1, rect2: Rectangle): Rectangle;
  var
    r, l, b, t: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'Intersection(const rect1, rect2: Rectangle): Rectangle', '');
    {$ENDIF}
    
    if RectangleBottom(rect1) > RectangleBottom(rect2) then b := RectangleBottom(rect2)
    else b := RectangleBottom(rect1);
      
    if RectangleTop(rect1) < RectangleTop(rect2) then t := RectangleTop(rect2)
    else t := RectangleTop(rect1);
      
    if RectangleRight(rect1) > RectangleRight(rect2) then r := RectangleRight(rect2)
    else r := RectangleRight(rect1);
      
    if RectangleLeft(rect1) < RectangleLeft(rect2) then l := RectangleLeft(rect2)
    else l := RectangleLeft(rect1);
    
    if (r < l) or (b < t) then
    begin
      result := RectangleFrom(0, 0, 0, 0);
      exit;
    end;

    result := RectangleFrom(l, t, Ceiling(r - l), Ceiling(b - t));
    
    // WriteLn();
    // WriteLn('b ', b);
    // WriteLn('t ', t);
    // WriteLn(RectangleToString(rect1));
    // WriteLn(RectangleToString(rect2));
    // WriteLn(RectangleToString(result));
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'Intersection(const rect1, rect2: Rectangle): Rectangle', '');
    {$ENDIF}
  end;

  function RayCircleIntersectDistance(const ray_origin: Point2D; const ray_heading:Vector; const c: Circle): Single;
  var
    to_circle, unit_heading: Vector;
    length, v, d: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'RayCircleIntersectDistance(const ray_origin: Point2D', '');
    {$ENDIF}
    
      unit_heading := UnitVector(ray_heading);
      to_circle := VectorFromPoints(ray_origin, c.center);
      length := VectorMagnitude(to_circle);

      v := DotProduct(to_circle, unit_heading);
      d := c.radius*c.radius - (length*length - v*v);
      // if there was no intersection, return -1
      if d < 0.0 then
          result := -1.0
      // return the distance to the (first) intersection point
      else
          result := (v - sqrt(d));
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'RayCircleIntersectDistance(const ray_origin: Point2D', '');
    {$ENDIF}
  end;


  procedure WidestPoints(const c: Circle; const along: Vector; out pt1, pt2: Point2D);
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'WidestPoints(const c: Circle', '');
    {$ENDIF}
    
    pt1 := AddVectors(c.center, VectorMultiply(UnitVector(along), c.radius));
    pt2 := AddVectors(c.center, VectorMultiply(UnitVector(along), -c.radius));
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'WidestPoints(const c: Circle', '');
    {$ENDIF}
  end;

  {
  ''' Given a point P and a circle of radius R centered at C, determine the
      two points T1, T2 on the circle that intersect with the tangents from P
      to the circle. Returns False if P is within the circle '''
  }
  function TangentPoints(const fromPt: Point2D; const c: Circle; out p1, p2: Point2D): Boolean;
  var
    pmC: Vector;
    sqr_len, r_sqr, inv_sqr_len, root: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'TangentPoints(const fromPt: Point2D', '');
    {$ENDIF}
    
    pmC := VectorFromPoints(fromPt, c.center);

    sqr_len := VectorMagnitudeSq(PmC);
    r_sqr := c.radius*c.radius;

    // Quick check for P inside the circle, return False if so
    if sqr_len <= r_sqr then
    begin
        result := False; // tangent objects are not returned.
        exit;
    end;

    // time to work out the real tangent points then
    inv_sqr_len := 1.0 / sqr_len;
    root := sqrt(abs(sqr_len - r_sqr));

    p1.x := c.center.x + c.radius*(c.radius*pmC.x - pmC.y*root)*inv_sqr_len;
    p1.y := c.center.y + c.radius*(c.radius*pmC.y + pmC.x*root)*inv_sqr_len;
    p2.x := c.center.x + c.radius*(c.radius*pmC.x + pmC.y*root)*inv_sqr_len;
    p2.y := c.center.y + c.radius*(c.radius*pmC.y - pmC.x*root)*inv_sqr_len;

    result := True;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'TangentPoints(const fromPt: Point2D', '');
    {$ENDIF}
  end;

  function RayIntersectionPoint(const fromPt: Point2D; const heading: Vector; const line: LineSegment; out pt: Point2D) : boolean;
  var
    rayLine: LineSegment;
    combMag: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'RayIntersectionPoint(const fromPt: Point2D', '');
    {$ENDIF}
    
    result := False;
    rayLine := LineFromVector(fromPt, heading);

    // Get where the line intersect
    if not LineIntersectionPoint(rayLine, line, pt) then exit;
    //DrawLine(ColorWhite, fromPt, pt);

    combMag := VectorMagnitude(AddVectors(UnitVector(VectorFromPoints(fromPt, pt)), UnitVector(heading)));
    // WriteLn(combMag:4:2);
    // Test that pt is forward of fromPt (given heading)
    if combMag < 1 then exit; //behind point

    result := True;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'RayIntersectionPoint(const fromPt: Point2D', '');
    {$ENDIF}
  end;

  function LineIntersectionPoint(const line1, line2: LineSegment; out pt: Point2D) : boolean;
  var
    // convert lines to the eqn
    // c = ax + by
    a1, b1, c1: Single;
    a2, b2, c2: Single;
    det: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'LineIntersectionPoint(const line1, line2: LineSegment', '');
    {$ENDIF}
    
    pt.x := 0;
    pt.y := 0;

    //Convert lines to eqn c = ax + by
    a1 := line1.endPoint.y - line1.startPoint.y; //y12 - y11;
    b1 := line1.startPoint.x - line1.endPoint.x; //x11 - x12;
    c1 := a1 * line1.startPoint.x + b1 * line1.startPoint.y; //a1 * x11 + b1 * y11;

    a2 := line2.endPoint.y - line2.startPoint.y; //y22 - y21;
    b2 := line2.startPoint.x - line2.endPoint.x; //x21 - x22;
    c2 := a2 * line2.startPoint.x + b2 * line2.startPoint.y; //a2 * x21 + b2 * y21;

    det := (a1 * b2) - (a2 * b1);

    if det = 0 then
      result := false
    else
    begin
      pt.x := (b2*c1 - b1*c2) / det;
      pt.y := (a1*c2 - a2*c1) / det;
      result := true;
    end;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'LineIntersectionPoint(const line1, line2: LineSegment', '');
    {$ENDIF}
  end;

  function LineSegmentsIntersect(const line1, line2: LineSegment): boolean;
  var
    pt: Point2D;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'LineSegmentsIntersect(const line1, line2: LineSegment): boolean', '');
    {$ENDIF}
    
    result := LineIntersectionPoint(line1, line2, pt) and PointOnLine(pt, line2) and PointOnLine(pt, line1);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'LineSegmentsIntersect(const line1, line2: LineSegment): boolean', '');
    {$ENDIF}
  end;

  function LineIntersectsLines(const line: LineSegment; const lines: LinesArray): boolean;
  var
    i: LongInt;
    pt: Point2D;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'LineIntersectsLines(const line: LineSegment', '');
    {$ENDIF}
    
    for i := 0 to High(lines) do
    begin
      if LineIntersectionPoint(line, lines[i], pt) and PointOnLine(pt, lines[i]) and PointOnLine(pt, line) then
      begin
        result := true;
        exit;
      end;
    end;
    result := false;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'LineIntersectsLines(const line: LineSegment', '');
    {$ENDIF}
  end;

  function LineIntersectsRect(const line: LineSegment; const rect: Rectangle): boolean;
  var
    lines: LinesArray;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'LineIntersectsRect(const line: LineSegment', '');
    {$ENDIF}
    
    lines := LinesFrom(rect);
    result := LineIntersectsLines(line, lines);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'LineIntersectsRect(const line: LineSegment', '');
    {$ENDIF}
  end;

  function PointInRect(const pt: Point2D; x, y, w, h: Single): Boolean; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'PointInRect(const pt: Point2D', '');
    {$ENDIF}
    
    if pt.x < x then result := false
    else if pt.x > x + w then result := false
    else if pt.y < y then result := false
    else if pt.y > y + h then result := false
    else result := true;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'PointInRect(const pt: Point2D', '');
    {$ENDIF}
  end;

  function PointInRect(const pt: Point2D; const rect: Rectangle): Boolean; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'PointInRect(const pt: Point2D', '');
    {$ENDIF}
    
    result := PointInRect(pt, rect.x, rect.y, rect.width, rect.height);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'PointInRect(const pt: Point2D', '');
    {$ENDIF}
  end;

  function PointInRect(ptX, ptY, x, y, w, h: Single): Boolean; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'PointInRect(ptX, ptY, x, y, w, h: Single): Boolean', '');
    {$ENDIF}
    
    if ptX < x then result := false
    else if ptX > x + w then result := false
    else if ptY < y then result := false
    else if ptY > y + h then result := false
    else result := true;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'PointInRect(ptX, ptY, x, y, w, h: Single): Boolean', '');
    {$ENDIF}
  end;

  function PointInRect(x, y: Single; const rect: Rectangle): Boolean; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'PointInRect(x, y: Single', '');
    {$ENDIF}
    
    result := PointInRect(x, y, rect.x, rect.y, rect.width, rect.height);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'PointInRect(x, y: Single', '');
    {$ENDIF}
  end;

  function VectorFromPointToRect(x, y, rectX, rectY: Single; rectWidth, rectHeight: LongInt): Vector; overload;
  var
    px, py: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'VectorFromPointToRect(x, y, rectX, rectY: Single', '');
    {$ENDIF}
    
    if x < rectX then px := rectX
    else if x > (rectX + rectWidth) then px := rectX + rectWidth
    else px := x;

    if y < rectY then py := rectY
    else if y > (rectY + rectHeight) then py := rectY + rectHeight
    else py := y;

    result := VectorTo(px - x, py - y);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'VectorFromPointToRect(x, y, rectX, rectY: Single', '');
    {$ENDIF}
  end;

  function VectorFromPointToRect(x, y: Single; const rect: Rectangle): Vector; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'VectorFromPointToRect(x, y: Single', '');
    {$ENDIF}
    
    result := VectorFromPointToRect(x, y, rect.x, rect.y, rect.width, rect.height);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'VectorFromPointToRect(x, y: Single', '');
    {$ENDIF}
  end;

  function VectorFromPointToRect(const pt: Point2D; const rect: Rectangle): Vector; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'VectorFromPointToRect(const pt: Point2D', '');
    {$ENDIF}
    
    result := VectorFromPointToRect(pt.x, pt.y, rect.x, rect.y, rect.width, rect.height);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'VectorFromPointToRect(const pt: Point2D', '');
    {$ENDIF}
  end;

  function VectorOutOfRectFromPoint(const pt: Point2D; const rect: Rectangle; const velocity: Vector): Vector;
  var
    maxIdx: LongInt;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'VectorOutOfRectFromPoint(const pt: Point2D', '');
    {$ENDIF}
    
    result := _VectorOverLinesFromPoint(pt, LinesFrom(rect), velocity, maxIdx);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'VectorOutOfRectFromPoint(const pt: Point2D', '');
    {$ENDIF}
  end;

  function VectorOutOfCircleFromPoint(const pt: Point2D; const c: Circle; const velocity: Vector): Vector;
  var
    dx, dy, cx, cy: Single;
    a, b, c1, det, t, mvOut: single;
    ipt2: Point2D;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'VectorOutOfCircleFromPoint(const pt: Point2D', '');
    {$ENDIF}
    
    // If the point is not in the radius of the circle, return a zero vector
    if PointPointDistance(pt, CenterPoint(c)) > c.radius then
    begin
      result := VectorTo(0, 0);
      exit;
    end;

    // Calculate the determinant (and components) from the center circle and
    // the point+velocity details
    cx := c.center.x;
    cy := c.center.y;
    dx := velocity.x;
    dy := velocity.y;

    a := dx * dx + dy * dy;
    b := 2 * (dx * (pt.x - cx) + dy * (pt.y - cy));
    c1 := (pt.x - cx) * (pt.x - cx) + (pt.y - cy) * (pt.y - cy) - c.radius * c.radius;

    det := b * b - 4 * a * c1;

    // If the determinate is very small, return a zero vector
    if (det <= 0) or (a = 0) then
      result := VectorTo(0, 0)
    else
    begin
      // Calculate the vector required to "push" the vector out of the circle
      t := (-b - Sqrt(det)) / (2 * a);
      ipt2.x := pt.x + t * dx;
      ipt2.y := pt.y + t * dy;

      mvOut := PointPointDistance(pt, ipt2) + 1.42; // sqrt 2
      result := VectorMultiply(UnitVector(InvertVector(velocity)), mvOut);
    end;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'VectorOutOfCircleFromPoint(const pt: Point2D', '');
    {$ENDIF}
  end;

  function VectorOutOfCircleFromCircle(const src, bounds: Circle; const velocity: Vector): Vector;
  var
    c: Circle;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'VectorOutOfCircleFromCircle(const src, bounds: Circle', '');
    {$ENDIF}
    
    c := CircleAt(CenterPoint(bounds), bounds.radius + src.radius);
    result := VectorOutOfCircleFromPoint(CenterPoint(Src), c, velocity);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'VectorOutOfCircleFromCircle(const src, bounds: Circle', '');
    {$ENDIF}
  end;

  function ShapeVectorOutOfRect(s: shape; const bounds: Rectangle; const velocity: Vector  ): vector;
  var
  maxIdx :LongInt;
  begin
   result := _VectorOverLinesFromLines(LinesFrom(bounds),LinesFrom(s), velocity, maxIdx);
  end;
    
  

  function VectorOutOfRectFromRect(const src, bounds: Rectangle; const velocity: Vector): Vector;
  var
    maxIDx: LongInt;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'VectorOutOfRectFromRect(const src, bounds: Rectangle', '');
    {$ENDIF}
    
    result := _VectorOverLinesFromLines(LinesFrom(src), LinesFrom(bounds), velocity, maxIdx);
    //result := _VectorOverLinesFromPoints(PointsFrom(src), LinesFrom(bounds), velocity, maxIdx);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'VectorOutOfRectFromRect(const src, bounds: Rectangle', '');
    {$ENDIF}
  end;

  // function VectorIntoRectFromRect(const src, bounds: Rectangle; const velocity: Vector): Vector;
  // var
  //   maxIDx: LongInt;
  // begin
  //   result := _VectorOverLinesFromPoints(PointsFrom(src), LinesFrom(bounds), velocity, False, maxIdx);
  // end;

  // function VectorIntoRectFromCircle(const c: Circle; bounds: Rectangle; const velocity: Vector): Vector;
  // var
  //   maxIdx: LongInt;
  // begin
  //   result := _VectorOverLinesFromCircle(c, LinesFrom(bounds), velocity, False, maxIdx);
  // end;

  function VectorInRect(const v: Vector; x, y, w, h: Single): Boolean; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'VectorInRect(const v: Vector', '');
    {$ENDIF}
    
    if v.x < x then result := false
    else if v.x > x + w then result := false
    else if v.y < y then result := false
    else if v.y > y + h then result := false
    else result := true;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'VectorInRect(const v: Vector', '');
    {$ENDIF}
  end;

  function VectorInRect(const v: Vector; const rect: Rectangle): Boolean; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'VectorInRect(const v: Vector', '');
    {$ENDIF}
    
    result := VectorInRect(v, rect.x, rect.y, rect.width, rect.height);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'VectorInRect(const v: Vector', '');
    {$ENDIF}
  end;

  function LineCircleHit(const c: Circle; const velocity: Vector; const lines: LinesArray; out found: LineSegment): Boolean;
  var
    hitIdx: LongInt;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'LineCircleHit(const c: Circle', '');
    {$ENDIF}
    
    _VectorOverLinesFromCircle(c, lines, velocity, hitIdx);
    if hitIdx >= 0 then
    begin
      found := lines[hitIdx];
      result := True;
    end
    else result := False;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'LineCircleHit(const c: Circle', '');
    {$ENDIF}
  end;

  function DistantPointOnCircle(const pt: Point2D; const c: Circle): Point2D;
  var
    ptOnCircle: Point2D;
    toCircle: Vector;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'DistantPointOnCircle(const pt: Point2D', '');
    {$ENDIF}
    
    //Get the closest point
    ptOnCircle := ClosestPointOnCircle(pt, c);

    // Get other side... follow toCircle vector 2 * radius
    toCircle := VectorFromPoints(pt, ptOnCircle);
    result := AddVectors(ptOnCircle, VectorMultiply(UnitVector(toCircle), c.radius * 2));
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'DistantPointOnCircle(const pt: Point2D', '');
    {$ENDIF}
  end;

  function DistantPointOnCircleHeading(const pt: Point2D; const c: Circle; const heading: Vector; out oppositePt: Point2D): Boolean;
  var
    dist, dotProd: Single;
    ptOnCircle, chkPt: Point2D;
    toCenter: Vector;
    head: Vector;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'DistantPointOnCircleHeading(const pt: Point2D', '');
    {$ENDIF}
    
    result := False;
    head := UnitVector(heading);

    //Move pt back 2 * radius to ensure it is outside of the circle...
    //  but still on same alignment
    chkPt := AddVectors(pt, VectorMultiply(InvertVector(head), 2 * c.radius));
    //DrawCircle(ColorBlue, chkPt, 1);

    dist := RayCircleIntersectDistance(chkPt, head, c);
    if dist < 0 then exit;

    // Get point on circle by moving from chkPt dist distance in heading direction
    ptOnCircle := AddVectors(chkPt, VectorMultiply(head, dist));
    //DrawLine(ColorMagenta, chkPt, ptOnCircle);
    //DrawCircle(ColorMagenta, ptOnCircle, 2);

    //Project the ray to the other side of the circle
    toCenter := VectorFromPoints(ptOnCircle, c.center);
    dotProd := DotProduct(toCenter, head);
    //WriteLn(dotProd:4:2);

    result := True;
    oppositePt := AddVectors(ptOnCircle, VectorMultiply(head, 2 * dotProd));
    //FillCircle(ColorRed, oppositePt, 2);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'DistantPointOnCircleHeading(const pt: Point2D', '');
    {$ENDIF}
  end;

  function VectorOutOfRectFromCircle(const c: Circle; const rect: Rectangle; const velocity: Vector): Vector;
  var
    maxIdx: LongInt;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'VectorOutOfRectFromCircle(const c: Circle', '');
    {$ENDIF}
    
    result := VectorOverLinesFromCircle(c, LinesFrom(rect), velocity, maxIdx);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'VectorOutOfRectFromCircle(const c: Circle', '');
    {$ENDIF}
  end;

  function VectorOverLinesFromCircle(const c: Circle; const lines: LinesArray; const velocity: Vector; out maxIdx: LongInt): Vector;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'VectorOverLinesFromCircle(const c: Circle', '');
    {$ENDIF}
    
    result := _VectorOverLinesFromCircle(c, lines, velocity, maxIDx);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'VectorOverLinesFromCircle(const c: Circle', '');
    {$ENDIF}
  end;

  // function VectorInLinesFromCircle(const c: Circle; lines: LinesArray; velocity: Vector; out maxIdx: LongInt): Vector;
  // begin
  //   result := _VectorOverLinesFromCircle(c, lines, velocity, False, maxIDx);
  // end;

  
  
//---------------------------------------------------------------------------
// Points functions and procedures
//---------------------------------------------------------------------------
  
  function PointsFrom(const rect: Rectangle): Point2DArray; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'PointsFrom(const rect: Rectangle): Point2DArray', '');
    {$ENDIF}
    
    SetLength(result, 4);
    result[0] := PointAt(rect.x, rect.y);
    result[1] := PointAt(rect.x + rect.width, rect.y);
    result[2] := PointAt(rect.x, rect.y + rect.height);
    result[3] := PointAt(rect.x + rect.width, rect.y + rect.height);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'PointsFrom(const rect: Rectangle): Point2DArray', '');
    {$ENDIF}
  end;
  
  function PointsFrom(const line: LineSegment): Point2DArray; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'PointsFrom(line): Point2DArray', '');
    {$ENDIF}
    
    SetLength(result, 2);
    result[0] := line.startPoint;
    result[1] := line.endPoint;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'PointsFrom(line): Point2DArray', '');
    {$ENDIF}
  end;
  
  function CenterPoint(const c: Circle): Point2D; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'CenterPoint(const c: Circle): Point2D', '');
    {$ENDIF}
    
    result := c.center;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'CenterPoint(const c: Circle): Point2D', '');
    {$ENDIF}
  end;
  
  function CircleAt(const pt: Point2D; radius: LongInt): Circle; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'CircleAt(const pt: Point2D', '');
    {$ENDIF}
    
    result.center := pt;
    result.radius := radius;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'CircleAt(const pt: Point2D', '');
    {$ENDIF}
  end;
  
  function CircleAt(x, y: Single; radius: LongInt): Circle; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'CircleAt(x, y: Single', '');
    {$ENDIF}
    
    result.center := PointAt(x, y);
    result.radius := radius;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'CircleAt(x, y: Single', '');
    {$ENDIF}
  end;
  
  function CircleX(const c: Circle): Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'CircleX(const c: Circle): Single', '');
    {$ENDIF}
    
    result := c.center.x;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'CircleX(const c: Circle): Single', '');
    {$ENDIF}
  end;
  
  function CircleY(const c: Circle): Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'CircleY(const c: Circle): Single', '');
    {$ENDIF}
    
    result := c.center.y;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'CircleY(const c: Circle): Single', '');
    {$ENDIF}
  end;
  
  function CircleRadius(const c: Circle): LongInt;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'CircleRadius(const c: Circle): LongInt', '');
    {$ENDIF}
    
    result := c.radius;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'CircleRadius(const c: Circle): LongInt', '');
    {$ENDIF}
  end;
  
  function CircleWithinRect(const c: Circle; const rect: Rectangle): Boolean;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'CircleWithinRect(const c: Circle', '');
    {$ENDIF}
    
    if CircleLinesCollision(c, LinesFrom(rect)) then result := False
    else result := PointInRect(c.center, rect.x, rect.y, rect.width, rect.height);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'CircleWithinRect(const c: Circle', '');
    {$ENDIF}
  end;
  
  function LineIntersectsCircle(const l: LineSegment; const c: Circle): Boolean;
  var
    pt: Point2D;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'LineIntersectsCircle(const l: LineSegment', '');
    {$ENDIF}
    
    pt := ClosestPointOnLineFromCircle(c, l);
    result := PointInCircle(pt, c);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'LineIntersectsCircle(const l: LineSegment', '');
    {$ENDIF}
  end;
  
  //=============================================================================
  
  function PointPrototypeFrom(const pt: Point2D): ShapePrototype;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'PointPrototypeFrom(const pt: Point2D): ShapePrototype', '');
    {$ENDIF}
    
    New(result);
    
    SetLength(result^.points, 1);
    result^.points[0] := pt;
    result^.kind := pkPoint;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'PointPrototypeFrom(const pt: Point2D): ShapePrototype', '');
    {$ENDIF}
  end;
  
  function CirclePrototypeFrom(const pt: Point2D; r: Single): ShapePrototype;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'CirclePrototypeFrom(const pt: Point2D', '');
    {$ENDIF}
    
    New(result);
    
    SetLength(result^.points, 2);
    result^.points[0] := pt;
    result^.points[1] := PointAt(r, r);
    result^.kind := pkCircle;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'CirclePrototypeFrom(const pt: Point2D', '');
    {$ENDIF}
  end;
  
  // function EllipsePrototypeFrom(const pt: Point2D; w, h: Single): ShapePrototype;
  // begin
  //   New(result);
  //   
  //   SetLength(result^.points, 2);
  //   result^.points[0] := pt;
  //   result^.points[1] := PointAt(w, h);
  //   result^.kind := pkEllipse;
  // end;
  
  function LinePrototypeFrom(const startPt, endPt: Point2D): ShapePrototype;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'LinePrototypeFrom(const startPt, endPt: Point2D): ShapePrototype', '');
    {$ENDIF}
    
    New(result);
    
    SetLength(result^.points, 2);
    result^.points[0] := startPt;
    result^.points[1] := endPt;
    result^.kind := pkLine;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'LinePrototypeFrom(const startPt, endPt: Point2D): ShapePrototype', '');
    {$ENDIF}
  end;
  
  function TrianglePrototypeFrom(const pt1, pt2, pt3: Point2D): ShapePrototype;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'TrianglePrototypeFrom(const pt1, pt2, pt3: Point2D): ShapePrototype', '');
    {$ENDIF}
    
    New(result);
    
    SetLength(result^.points, 3);
    result^.points[0] := pt1;
    result^.points[1] := pt2;
    result^.points[2] := pt3;
    result^.kind := pkTriangle;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'TrianglePrototypeFrom(const pt1, pt2, pt3: Point2D): ShapePrototype', '');
    {$ENDIF}
  end;
  
  function LineListPrototypeFrom(const points: Point2DArray): ShapePrototype;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'LineListPrototypeFrom(const points: Point2DArray): ShapePrototype', '');
    {$ENDIF}
    
    result := PrototypeFrom(points, pkLineList);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'LineListPrototypeFrom(const points: Point2DArray): ShapePrototype', '');
    {$ENDIF}
  end;
  
  function LineStripPrototypeFrom(const points: Point2DArray): ShapePrototype;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'LineStripPrototypeFrom(const points: Point2DArray): ShapePrototype', '');
    {$ENDIF}
    
    result := PrototypeFrom(points, pkLineStrip);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'LineStripPrototypeFrom(const points: Point2DArray): ShapePrototype', '');
    {$ENDIF}
  end;
  
  // function PolygonPrototypeFrom(const points: Point2DArray): ShapePrototype;
  // begin
  //   {$IFDEF TRACE}
  //     TraceEnter('sgGeometry', 'PolygonPrototypeFrom(const points: Point2DArray): ShapePrototype', '');
  //   {$ENDIF}
  //   
  //   result := PrototypeFrom(points, pkPolygon);
  //   
  //   {$IFDEF TRACE}
  //     TraceExit('sgGeometry', 'PolygonPrototypeFrom(const points: Point2DArray): ShapePrototype', '');
  //   {$ENDIF}
  // end;
  
  function TriangleStripPrototypeFrom(const points: Point2DArray): ShapePrototype;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'TriangleStripPrototypeFrom(const points: Point2DArray): ShapePrototype', '');
    {$ENDIF}
    
    result := PrototypeFrom(points, pkTriangleStrip);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'TriangleStripPrototypeFrom(const points: Point2DArray): ShapePrototype', '');
    {$ENDIF}
  end;
  
  function TriangleFanPrototypeFrom(const points: Point2DArray): ShapePrototype;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'TriangleFanPrototypeFrom(const points: Point2DArray): ShapePrototype', '');
    {$ENDIF}
    
    result := PrototypeFrom(points, pkTriangleFan);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'TriangleFanPrototypeFrom(const points: Point2DArray): ShapePrototype', '');
    {$ENDIF}
  end;
  
  function TriangleListPrototypeFrom(const points: Point2DArray): ShapePrototype;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'TriangleListPrototypeFrom(const points: Point2DArray): ShapePrototype', '');
    {$ENDIF}
    
    result := PrototypeFrom(points, pkTriangleList);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'TriangleListPrototypeFrom(const points: Point2DArray): ShapePrototype', '');
    {$ENDIF}
  end;
  
  function PrototypeFrom(const points: Point2DArray; kind:ShapeKind): ShapePrototype;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'PrototypeFrom(const points: Point2DArray', '');
    {$ENDIF}
    
    if Length(points) < MinimumPointsForKind(kind) then
    begin
      RaiseException('Insufficient points assigned to shape given its kind. Min is ' 
        + IntToStr(MinimumPointsForKind(kind))
        + ' was supplied ' 
        + IntToStr(Length(points)));
      exit;
    end;
    
    New(result);
    
    PrototypeSetKind(result, kind);
    PrototypeSetPoints(result, points);
    result^.shapeCount := 0;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'PrototypeFrom(const points: Point2DArray', '');
    {$ENDIF}
  end;
  
  procedure FreePrototype(var p: ShapePrototype);
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'FreePrototype(var p: ShapePrototype)', '');
    {$ENDIF}
    
    if not Assigned(p) then exit;
    if p^.shapeCount > 0 then begin RaiseWarning('Freeing prototype while it is still used by ' + IntToStr(p^.shapeCount) + ' shapes.'); end;
    
    SetLength(p^.points, 0);
    Dispose(p);
    CallFreeNotifier(p);
    p := nil;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'FreePrototype(var p: ShapePrototype)', '');
    {$ENDIF}
  end;
  
  //=============================================================================
  
  function MinimumPointsForKind(k: ShapeKind): LongInt;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'MinimumPointsForKind(k: ShapeKind): LongInt', '');
    {$ENDIF}
    
    case k of
      pkPoint: result := 1;
      pkCircle: result := 2;
      // pkEllipse: result := 2;
      pkLine: result := 2;
      pkTriangle: result := 3;
      pkLineList: result := 2;
      pkLineStrip: result := 2;
      // pkPolygon: result := 3;
      pkTriangleStrip: result := 3;
      pkTriangleFan: result := 3;
      pkTriangleList: result := 3;
    end;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'MinimumPointsForKind(k: ShapeKind): LongInt', '');
    {$ENDIF}
  end;
  
  function PrototypePointCount(p: ShapePrototype): LongInt;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'PrototypePointCount(p: ShapePrototype): LongInt', '');
    {$ENDIF}
    
    if not assigned(p) then result := 0
    else result := Length(p^.points);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'PrototypePointCount(p: ShapePrototype): LongInt', '');
    {$ENDIF}
  end;
  
  procedure PrototypeSetPoints(p: ShapePrototype; const points: Point2DArray);
  var
    i: LongInt;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'PrototypeSetPoints(p: ShapePrototype', '');
    {$ENDIF}
    
    if not assigned(p) then exit;
    
    if Length(points) < MinimumPointsForKind(p^.kind) then
    begin
      RaiseException('Insufficient points assigned to shape given its kind. Min is ' 
        + IntToStr(MinimumPointsForKind(p^.kind)) 
        + ' was supplied ' 
        + IntToStr(Length(points)));
      exit;
    end;
    
    SetLength(p^.points, Length(points));
    
    for i := 0 to High(points) do
    begin
      p^.points[i] := points[i];
    end;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'PrototypeSetPoints(p: ShapePrototype', '');
    {$ENDIF}
  end;
  
  function PrototypePoints(p: ShapePrototype): Point2DArray;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'PrototypePoints(p: ShapePrototype): Point2DArray', '');
    {$ENDIF}
    
    if not assigned(p) then SetLength(result,0)
    else result := p^.points;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'PrototypePoints(p: ShapePrototype): Point2DArray', '');
    {$ENDIF}
  end;
  
  procedure PrototypeSetKind(p: ShapePrototype; kind: ShapeKind);
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'PrototypeSetKind(p: ShapePrototype', '');
    {$ENDIF}
    
    if not assigned(p) then
    begin
      RaiseException('No shape prototype supplied to set kind.');
      exit;
    end;
    
    p^.kind := kind;
    case kind of
      pkPoint: p^.drawWith := @DrawShapeAsPoint;
      pkCircle: p^.drawWith := @DrawShapeAsCircle;
      // pkEllipse: p^.drawWith := @DrawShapeAsEllipse;
      pkLine: p^.drawWith := @DrawShapeAsLine;
      pkTriangle: p^.drawWith := @DrawShapeAsTriangle;
      pkLineList: p^.drawWith := @DrawShapeAsLineList;
      pkLineStrip: p^.drawWith := @DrawShapeAsLineStrip;
      // pkPolygon: p^.drawWith := @DrawShapeAsPolygon;
      pkTriangleStrip: p^.drawWith := @DrawShapeAsTriangleStrip;
      pkTriangleFan: p^.drawWith := @DrawShapeAsTriangleFan;
      pkTriangleList: p^.drawWith := @DrawShapeAsTriangleList;
      else p^.drawWith := nil; 
    end;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'PrototypeSetKind(p: ShapePrototype', '');
    {$ENDIF}
  end;
  
  function PrototypeKind(p: ShapePrototype): ShapeKind;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'PrototypeKind(p: ShapePrototype): ShapeKind', '');
    {$ENDIF}
    
    if not assigned(p) then result := ShapeKind(-1)
    else result := p^.kind;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'PrototypeKind(p: ShapePrototype): ShapeKind', '');
    {$ENDIF}
  end;
  
  //=============================================================================
  
  function ShapeAtPoint(p: ShapePrototype; const pt: Point2D): Shape;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'ShapeAtPoint(p: ShapePrototype', '');
    {$ENDIF}
    
    New(result);
    result^.prototype := p;
    result^.pt := pt;
    result^.angle := 0.0;
    result^.scale := PointAt(1,1);
    result^.color := ColorWhite;
    SetLength(result^.subShapes, 0);
    
    if Assigned(p) then p^.shapeCount += 1;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'ShapeAtPoint(p: ShapePrototype', '');
    {$ENDIF}
  end;
  
  procedure FreeShape(var s: Shape);
  var
    i: LongInt;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'FreeShape(var s: Shape)', '');
    {$ENDIF}
    
    if not Assigned(s) then exit;
    if Assigned(s^.prototype) then s^.prototype^.shapeCount -= 1;
    
    for i := 0 to High(s^.subShapes) do
    begin
      FreeShape(s^.subShapes[i])
    end;
    SetLength(s^.subShapes, 0);
    
    Dispose(s);
    CallFreeNotifier(s);
    s := nil;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'FreeShape(var s: Shape)', '');
    {$ENDIF}
  end;
  
  procedure UpdateSubShapePoints(s: Shape; const parentM: Matrix2D);
  var
    m: Matrix2D;
    i: LongInt;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'UpdateSubShapePoints(s: Shape', '');
    {$ENDIF}
    
    if not Assigned(s) then begin exit; end;
    
    // Copy the points from the prototype
    s^.ptBuffer := Copy(s^.prototype^.points, 0, Length(s^.prototype^.points));
    
    // Scale, angle and translate based on this shape's position * parent's matrix
    //m := ScaleMatrix(s.scale) * RotationMatrix(s.angle) * TranslationMatrix(s.pt);
    m := ScaleRotateTranslateMatrix(s^.scale, s^.angle, s^.pt) * parentM;
    ApplyMatrix(m, s^.ptBuffer);
    
    for i := 0 to High(s^.subShapes) do
    begin
      UpdateSubShapePoints(s^.subShapes[i], m);
    end;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'UpdateSubShapePoints(s: Shape', '');
    {$ENDIF}
  end;
  
  procedure UpdateShapePoints(s: Shape);
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'UpdateShapePoints(s: Shape)', '');
    {$ENDIF}
    
    UpdateSubShapePoints(s, IdentityMatrix());
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'UpdateShapePoints(s: Shape)', '');
    {$ENDIF}
  end;
  
  function ShapePoints(s: Shape): Point2DArray;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'ShapePoints(s: Shape): Point2DArray', '');
    {$ENDIF}
    
    if not Assigned(s) then begin SetLength(result, 0); exit; end;
    if not Assigned(s^.ptBuffer) then
    begin
      UpdateShapePoints(s);
    end;
    result := s^.ptBuffer;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'ShapePoints(s: Shape): Point2DArray', '');
    {$ENDIF}
  end;
  
  function ShapePointCount(s: Shape): LongInt;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'ShapePointCount(s: Shape): LongInt', '');
    {$ENDIF}
    
    if not Assigned(s) then begin result := 0; exit; end;
    result := PrototypePointCount(s^.prototype);
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'ShapePointCount(s: Shape): LongInt', '');
    {$ENDIF}
  end;
  
  procedure ShapeSetAngle(s: Shape; angle: Single);
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'ShapeSetAngle(s: Shape', '');
    {$ENDIF}
    
    if not Assigned(s) then begin exit; end;
    s^.angle := angle;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'ShapeSetAngle(s: Shape', '');
    {$ENDIF}
  end;
  
  function ShapeAngle(s: Shape): Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'ShapeAngle(s: Shape): Single', '');
    {$ENDIF}
    
    if not Assigned(s) then begin result := 0; exit; end;
    result := s^.angle;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'ShapeAngle(s: Shape): Single', '');
    {$ENDIF}
  end;
  
  procedure ShapeSetScale(s: Shape; const scale: Point2D);
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'ShapeSetScale(s: Shape', '');
    {$ENDIF}
    
    if not Assigned(s) then begin exit; end;
    s^.scale := scale;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'ShapeSetScale(s: Shape', '');
    {$ENDIF}
  end;
  
  function ShapeScale(s: Shape): Point2D;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'ShapeScale(s: Shape): Point2D', '');
    {$ENDIF}
    
    if not Assigned(s) then begin result := PointAt(0,0); exit; end;
    result := s^.scale;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'ShapeScale(s: Shape): Point2D', '');
    {$ENDIF}
  end;
  
  function ShapeShapePrototype(s: Shape): ShapePrototype;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'ShapeShapePrototype(s: Shape): ShapePrototype', '');
    {$ENDIF}
    
    if not Assigned(s) then begin result := nil; exit; end;
    result := s^.prototype;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'ShapeShapePrototype(s: Shape): ShapePrototype', '');
    {$ENDIF}
  end;
  
  procedure ShapeSetPrototype(s: Shape; p: ShapePrototype);
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'ShapeSetPrototype(s: Shape', '');
    {$ENDIF}
    
    if not Assigned(s) then exit;
    if Assigned(s^.prototype) then s^.prototype^.shapeCount -= 1;
    
    s^.prototype := p;
    
    if Assigned(s^.prototype) then s^.prototype^.shapeCount += 1;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'ShapeSetPrototype(s: Shape', '');
    {$ENDIF}
  end;
  
  procedure ShapeAddSubShape(parent, child: Shape);
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'ShapeAddSubShape(parent, child: Shape)', '');
    {$ENDIF}
    
    if not Assigned(parent) then exit;
    if not Assigned(child) then exit;
      
    SetLength(parent^.subShapes, Length(parent^.subShapes) + 1);
    parent^.subShapes[High(parent^.subShapes)] := child;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'ShapeAddSubShape(parent, child: Shape)', '');
    {$ENDIF}
  end;
  
  function ShapeColor(s: Shape): Color;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'ShapeColor(s: Shape): Color', '');
    {$ENDIF}
    
    if not Assigned(s) then begin result := ColorBlack; exit; end;
    result := s^.color;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'ShapeColor(s: Shape): Color', '');
    {$ENDIF}
  end;
  
  procedure ShapeSetColor(s: Shape; c: Color);
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGeometry', 'ShapeSetColor(s: Shape', '');
    {$ENDIF}
    
    if not Assigned(s) then begin exit; end;
    
    s^.color := c;
    
    {$IFDEF TRACE}
      TraceExit('sgGeometry', 'ShapeSetColor(s: Shape', '');
    {$ENDIF}
  end;
  
  function ShapeShapeKind(s: Shape): ShapeKind;
  begin
    if not assigned(s) then begin result := ShapeKind(-1); exit; end;
      
    result := PrototypeKind(ShapeShapePrototype(s));
  end;
  
  procedure FixRectangle(var rect: Rectangle);
  begin
    FixRectangle(rect.x, rect.y, rect.width, rect.height);
  end;
  
  procedure FixRectangle(var x, y: Single; width,height: LongInt);
  begin
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
  end;

  function PointAdd(const pt1, pt2: Point2D): Point2D;
  begin
    result.X := pt1.X + pt2.X;
    result.Y := pt1.Y + pt2.Y;
  end;
    
end.
