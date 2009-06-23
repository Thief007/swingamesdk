//=============================================================================
//          sgShapes.pas
//=============================================================================
//
// Contains the code for the bascic shapes available in the SGDK :
//  * Point2D, LineSegment, and Rectangle,
// and the code to create and manipulate these.
//
// Change History:
//
// Version 3.0:
// - 2009-06-23: Clinton: Renamed PointFrom to PointAt
//                      : Moved LineMagnitudeSq to sgMath
//                      : Renamed RectangleAroundLine to RectangleFrom
//                      : Moved ApplyMatrix to sgMath
// - 2009-06-22: Clinton: Comment format, cleanup and new additions.
//                      : Renamed CreateRectangle to RectangleFrom
//                      : Renamed CreateLine to LineFrom
//                      : Renamed CreateTriangle to TriangleFrom
//                      : Renamed IsPointInTriangle to PointInTriangle
//                      : Renamed IsPointOnLine to PointOnLine
//                      : Renamed PointIsWithinRect to PointInRect
//                      : Renamed LineIntersectsWithLines to LineIntersectsLines
//                      : Renamed LineIntersectsWithRect to LineIntersectsRect
//                      : Renamed MidPoint to LineMidPoint
//                      : Renamed SqLineMagnitude to _LineMagnitudeSq (private)
//                      : Renamed DistancePointToLine to PointLineDistance
//                      : Renamed DistanceBetween to PointPointDistance
//
// - 2009-06-15: Andrew: Added meta tags
//
// Version 2.0:
// - 2008-12-17: Andrew: Moved all integers to LongInt
// - 2008-12-10: Andrew: Fixed version history
//                     : Changed Triangle to Array
//                     : Added TriangleBarycenter
//                     : Added ApplyMatrix to Triangle
// Version 1.1.6:
// - 2008-04-08: Stephen: Added Triangle Record, and CreateTriangle()x2
//
// Version 1.1:
// - 2008-01-31: Andrew: Fixed IsPointOnLine
//                     : Added RectangleAroundLine
// - 2008-01-31: Stephen: Partial fix of IsPointOnLine
// - 2008-01-21: Andrew: General refactoring, adding const and
//                       some extra overloads.
// - 2008-01-18: Aki, Andrew, Stephen: Created initial version
//=============================================================================

/// @module Shapes
unit sgShapes;

//=============================================================================
interface
//=============================================================================

  uses sgTypes;

  //  Returns distance from the line, or if the intersecting point on the line nearest
  //    the point tested is outside the endpoints of the line, the distance to the
  //    nearest endpoint.
  //
  //  Returns -1 on zero-valued denominator conditions to return an illegal distance. (
  //    modification of Brandon Crosby's VBA code)

  /// @lib PointLineDistance
  function PointLineDistance(const pt: Point2D; const line: LineSegment): Single; overload;
  /// @lib PointXYLineDistance
  function PointLineDistance(x, y: Single; const line: LineSegment): Single; overload;
  /// @lib ClosestPointOnLineXY
  function ClosestPointOnLine(x, y: Single; const line: LineSegment): Point2D; overload;
  /// @lib ClosestPointOnLine
  function ClosestPointOnLine(const fromPt: Point2D; const line: LineSegment): Point2D; overload;

  /// @lib CenterPoint
  function CenterPoint(s: Sprite): Point2D;

  /// @lib PointOnLine
  function PointOnLine(const pt: Point2D; const line: LineSegment): Boolean;

  /// @lib PointAt
  function PointAt(x, y: Single): Point2D;

  /// @lib LinesFromRect
  function LinesFromRect(const rect: Rectangle): LinesArray;

  /// @lib
  function LineFrom(x1, y1, x2, y2: Single): LineSegment;

  /// @lib LineFromVectorWithStartPoint
  function LineFromVector(const pt: Point2D; const mv: Vector): LineSegment; overload;
  /// @lib LineFromVectorWithStartXY
  function LineFromVector(x, y: Single; const mv: Vector): LineSegment; overload;
  /// @lib
  function LineFromVector(const mv: Vector): LineSegment; overload;

  /// @lib
  function LineMidPoint(const line: LineSegment): Point2D;

  /// @lib
  function RectangleFrom(x, y: Single; w, h: LongInt): Rectangle; overload;
  /// @lib RectangleForBitmap
  function RectangleFrom(bmp: Bitmap): Rectangle; overload;
  /// @lib RectangleForBitmapAtXY
  function RectangleFrom(x, y: Single; bmp: Bitmap): Rectangle; overload;
  /// @lib RectangleForBitmapAtPoint
  function RectangleFrom(const pt: Point2D; bmp: Bitmap): Rectangle; overload;
  /// @lib RectangleAtPoint
  function RectangleFrom(const pt: Point2D; width, height: LongInt): Rectangle; overload;
  /// @lib RectangleFromSprite
  function RectangleFrom(s: Sprite): Rectangle; overload;
  /// @lib RectangleFromLine
  function RectangleFrom(const line: LineSegment): Rectangle; overload;

  /// @lib
  function TriangleFrom(ax, ay, bx, by, cx, cy: Single): Triangle; overload;

  /// @lib TriangleFromPoints
  function TriangleFrom(const a, b, c: Point2D): Triangle; overload;

  /// @lib
  function TriangleBarycenter(const tri: Triangle): Point2D;

  /// @lib
  function PointInTriangle(const pt: Point2D; const tri: Triangle): Boolean;

  /// @lib
  function RectangleAfterMove(const rect: Rectangle; const mv: Vector): Rectangle;

  /// @lib
  function RectangleTop (const rect: Rectangle): Single;
  /// @lib
  function RectangleBottom(const rect: Rectangle): Single;
  /// @lib
  function RectangleLeft  (const rect: Rectangle): Single;
  /// @lib
  function RectangleRight (const rect: Rectangle): Single;

  /// @lib
  function RectanglesIntersect(const rect1, rect2: Rectangle): Boolean;
  /// @lib
  function Intersection(const rect1, rect2: Rectangle): Rectangle;

  /// @lib
  function GetLineIntersectionPoint(const line1, line2: LineSegment; out pt: Point2D) : boolean;
  /// @lib
  function LineIntersectsLines(const line: LineSegment; const lines: LinesArray): boolean;
  /// @lib
  function LineIntersectsRect(const line: LineSegment; const rect: Rectangle): boolean;

  /// @lib PointInRectXY
  function PointInRect(const pt: Point2D; x, y, w, h: Single): Boolean; overload;
  /// @lib
  function PointInRect(const pt: Point2D; const rect: Rectangle): Boolean; overload;


//=============================================================================
implementation
//=============================================================================

  uses math, sysutils, classes, sgGraphics, sgPhysics, sgMath;

  const
    EPS    = 0.01;         // smallest positive value: less than that to be considered zero
    EPSEPS = EPS * EPS;        // and its square



  function PointLineDistance(x, y: Single; const line: LineSegment): Single; overload;
  begin
    result := PointLineDistance(PointAt(x, y), line);
  end;

  function PointLineDistance(const pt: Point2D; const line: LineSegment): Single; overload;
  var
    sqLineMag, u: Single;
    intersect: Point2D;
  begin
    // see Paul Bourke's original article(s)
    // square of line's magnitude (see note in function LineMagnitude)
    sqLineMag := LineMagnitudeSq(line);
    if SqLineMag < EPSEPS then
    begin
      raise Exception.Create('Cannot determine intersection point on line, line is too short');
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
  end;
  
  function ClosestPointOnLine(x, y: Single; const line: LineSegment): Point2D; overload;
  begin
    result := ClosestPointOnLine(PointAt(x, y), line);
  end;
  
  function ClosestPointOnLine(const fromPt: Point2D; const line: LineSegment): Point2D; overload;
  var
    sqLineMag, u: Single;
  begin
    // see Paul Bourke's original article(s)
    // square of line's magnitude (see note in function LineMagnitude)
    sqLineMag := LineMagnitudeSq(line);
    if SqLineMag < EPSEPS then
    begin
      raise Exception.Create('Cannot determine intersection point on line, line is too short');
    end;

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
  end;

  function RectangleFrom(const line: LineSegment): Rectangle; overload
  begin
    result.x := Min(line.startPoint.x, line.endPoint.x);
    result.y := Min(line.startPoint.y, line.endPoint.y);
    result.width := Round(Max(line.startPoint.x, line.endPoint.x) - result.x);
    result.height := Round(Max(line.startPoint.y, line.endPoint.y) - result.y);
  end;
    
  function PointOnLine(const pt: Point2D; const line: LineSegment): Boolean;
  const SMALL = 0.9;
    function SimpleComparisonXSame(): Boolean;
    var
      minY, maxY: Single;
    begin
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
      raise Exception.Create('Cannot determine if point is on line, line is too short');
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
  end;
    
  function LineFrom(x1, y1, x2, y2: Single): LineSegment;
  begin
    result.startPoint.x := x1;
    result.startPoint.y := y1;
    result.endPoint.x := x2;
    result.endPoint.y := y2;
  end;
  
  function LinesFromRect(const rect: Rectangle): LinesArray;
  begin
    SetLength(result, 4);
    with rect do
    begin
      result[0] := LineFrom(x, y, x + width, y);
      result[1] := LineFrom(x, y, x, y + height);
      result[2] := LineFrom(x + width, y, x + width, y + height);
      result[3] := LineFrom(x, y + height, x + width, y + height);
    end;
  end;
  
  function PointAt(x, y: Single): Point2D;
  begin
    result.x := x;
    result.y := y;
  end;

  function CenterPoint(s: Sprite): Point2D;
  begin
    result.x := s.x + CurrentWidth(s) / 2;
    result.y := s.y + CurrentHeight(s) / 2;
  end;
  
  function LineFromVector(const pt: Point2D; const mv: Vector): LineSegment; overload;
  begin
    result := LineFromVector(pt.x, pt.Y, mv);
  end;
  
  function LineFromVector(x, y: Single; const mv: Vector): LineSegment; overload;
  begin
    result.startPoint.x := x;
    result.startPoint.y := y;
    result.endPoint.x := x + mv.x;
    result.endPoint.y := y + mv.y;
  end;
  
  function LineFromVector(const mv: Vector): LineSegment; overload;
  begin
    result := LineFromVector(0, 0, mv);
  end;

  function RectangleAfterMove(const rect: Rectangle; const mv: Vector): Rectangle;
  begin
    result := rect;
    result.x := result.x + mv.x;
    result.y := result.y + mv.y;
  end;
  
  function RectangleTop(const rect: Rectangle): Single;
  begin
    if rect.height > 0 then result := rect.y
    else result := rect.y + rect.height - 1; //add negative height
  end;
  
  function RectangleBottom(const rect: Rectangle): Single;
  begin
    if rect.height > 0 then result := rect.y + rect.height - 1
    else result := rect.y; //y is bottom most
  end;

  function RectangleLeft(const rect: Rectangle): Single;
  begin
    if rect.width > 0 then result := rect.x
    else result := rect.x + rect.width - 1; //add negative width
  end;

  function RectangleRight(const rect: Rectangle): Single;
  begin
    if rect.width > 0 then result := rect.x + rect.width - 1
    else result := rect.x; //x is right most
  end;

  function RectangleFrom(x, y: Single; w, h: LongInt): Rectangle; overload;
  begin
    result.x := x;
    result.y := y;
    result.width := w;
    result.height := h;
  end;
  
  function RectangleFrom(s: Sprite): Rectangle;
  begin
    result := RectangleFrom(s.x, s.y, CurrentWidth(s), CurrentHeight(s));
  end;
  
  function RectangleFrom(const pt: Point2D; width, height: LongInt): Rectangle; overload;
  begin
    result := RectangleFrom(pt.x, pt.y, width, height);
  end;
  
  function RectangleFrom(const pt: Point2D; bmp: Bitmap): Rectangle; overload;
  begin
    result := RectangleFrom(pt.x, pt.y, bmp.width, bmp.height);
  end;
  
  function RectangleFrom(x, y: Single; bmp: Bitmap): Rectangle; overload;
  begin
    result := RectangleFrom(x, y, bmp.width, bmp.height);
  end;
  
  function RectangleFrom(bmp: Bitmap): Rectangle; overload;
  begin
    result := RectangleFrom(0, 0, bmp);
  end;
  
  function TriangleFrom(ax, ay, bx, by, cx, cy: Single): Triangle; overload;
  begin
    result := TriangleFrom(PointAt(ax, ay), PointAt(bx, by), PointAt(cx, cy));
  end;
  
  function TriangleFrom(const a, b, c: Point2D): Triangle; overload;
  begin
    result[0] := a;
    result[1] := b;
    result[2] := c;
  end;
  
  function PointInTriangle(const pt : Point2D; const tri : Triangle): Boolean;
  var
    v0, v1, v2 : Vector;
    a, b, c, p: Vector;
    dot00, dot01, dot02, dot11, dot12 : Single;
    invDenom, u, v: Single;
  begin
    //Convert Points to vectors
    p := VectorFromPoint(pt);
    a := VectorFromPoint(tri[0]);
    b := VectorFromPoint(tri[1]);
    c := VectorFromPoint(tri[2]);
    
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
    invDenom := 1 / (dot00 * dot11 - dot01 * dot01);
    u := (dot11 * dot02 - dot01 * dot12) * invDenom;
    v := (dot00 * dot12 - dot01 * dot02) * invDenom;
    
    // Check if point is in triangle
    result := ((u > 0) and (v > 0) and (u + v < 1));
  end;
  

  
  function TriangleBarycenter(const tri: Triangle): Point2D;
  begin
    result.x := (tri[0].x + tri[1].x + tri[2].x) / 3;
    result.y := (tri[0].y + tri[1].y + tri[2].y) / 3;
  end;
  
  function LineMidPoint(const line: LineSegment): Point2D;
  begin
    result.x := line.startPoint.x + (line.endPoint.x - line.startPoint.x) / 2;
    result.y := line.startPoint.y + (line.endPoint.y - line.startPoint.y) / 2;
  end;
  
  function RectanglesIntersect(const rect1, rect2: Rectangle): Boolean;
  begin
    if RectangleBottom(rect1) < RectangleTop(rect2) then result := false
    else if RectangleTop(rect1) > RectangleBottom(rect2) then result := false
    else if RectangleRight(rect1) < RectangleLeft(rect2) then result := false
    else if RectangleLeft(rect1) > RectangleRight(rect2) then result := false
    else result := true;
  end;

  function Intersection(const rect1, rect2: Rectangle): Rectangle;
  var
    r, l, b, t: Single;
  begin
    if RectangleBottom(rect1) > RectangleBottom(rect2) then b := RectangleBottom(rect2)
    else b := RectangleBottom(rect1);

    if RectangleTop(rect1) < RectangleTop(rect2) then t := RectangleTop(rect2)
    else t := RectangleTop(rect1);

    if RectangleRight(rect1) > RectangleRight(rect2) then r := RectangleRight(rect2)
    else r := RectangleRight(rect1);

    if RectangleLeft(rect1) < RectangleLeft(rect2) then l := RectangleLeft(rect2)
    else l := RectangleLeft(rect1);
    
    if (r < l) or (b > t) then
    begin
      result := RectangleFrom(0, 0, 0, 0);
      exit;
    end;
    
    result := RectangleFrom(l, t, Round(r - l), Round(b - t));
  end;
  


  function GetLineIntersectionPoint(const line1, line2: LineSegment; out pt: Point2D) : boolean;
  var
    // convert lines to the eqn
    // c = ax + by
    a1, b1, c1: Single;
    a2, b2, c2: Single;
    det: Single;
  begin
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
  end;

  function LineIntersectsLines(const line: LineSegment; const lines: LinesArray): boolean;
  var
    i: LongInt;
    pt: Point2D;
  begin
    for i := 0 to High(lines) do
    begin     
      if GetLineIntersectionPoint(line, lines[i], pt) and PointOnLine(pt, lines[i]) then
      begin
        result := true;
        exit;
      end;
    end;
    result := false;
  end;
  
  function LineIntersectsRect(const line: LineSegment; const rect: Rectangle): boolean;
  var
    lines: LinesArray;
  begin
    lines := LinesFromRect(rect);
    result := LineIntersectsLines(line, lines);
  end;
  
  function PointInRect(const pt: Point2D; x, y, w, h: Single): Boolean; overload;
  begin
    if pt.x < x then result := false
    else if pt.x > x + w then result := false
    else if pt.y < y then result := false
    else if pt.y > y + h then result := false
    else result := true;
  end;

  function PointInRect(const pt: Point2D; const rect: Rectangle): Boolean; overload;
  begin
    result := PointInRect(pt, rect.x, rect.y, rect.width, rect.height);
  end;
end.