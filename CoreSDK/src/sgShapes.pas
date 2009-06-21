//---------------------------------------------------/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
//          sgShapes.pas
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// The Shapes unit contains the code for the bascic shapes
// available in the SGDK : Point2D, LineSegment, and Rectangle,
// and the code to create and manipulate these.
//
// Change History:
//
// Version 2.0:
// - 2008-12-17: Andrew: Moved all integers to LongInt
// - 2008-12-10: Andrew: Fixed version history
//             Changed Triangle to Array
//             Added TriangleBarycenter
//             Added ApplyMatrix to Triangle
// Version 1.1.6:
// - 2008-04-08: Stephen: Added Triangle Record, and CreateTriangle()x2
//
// Version 1.1:
// - 2008-01-31: Andrew: Fixed IsPointOnLine
//                     : Added RectangleAroundLine
// - 2008-01-31: Stephen: Partial fix of IsPointOnLine
// - 2008-01-21: Andrew: General refactoring, adding const and
//    some extra overloads.
// - 2008-01-18: Aki, Andrew, Stephen: Created initial version

/// @module Shapes
unit sgShapes;

interface
  uses sgTypes;
  
  /// @lib DistancePointToLine
  function DistancePointToLine(const pnt: Point2D; const line: LineSegment): Single; overload;
  /// @lib DistanceXYToLine
  function DistancePointToLine(x, y: Single; const line: LineSegment): Single; overload;
  /// @lib ClosestPointOnLineXY
  function ClosestPointOnLine(x, y: Single; const line: LineSegment): Point2D; overload;
  /// @lib ClosestPointOnLine
  function ClosestPointOnLine(const fromPt: Point2D; const line: LineSegment): Point2D; overload;
  
  /// @lib CenterPoint
  function CenterPoint(sprt: Sprite): Point2D;
  
  /// @lib IsPointOnLine
  function IsPointOnLine(const pnt: Point2D; const line: LineSegment): Boolean;
  
  /// @lib CreatePoint
  function CreatePoint(x, y: Single): Point2D;
  
  /// @lib LinesFromRect
  function LinesFromRect(const rect: Rectangle): LinesArray;
  
  /// @lib
  function CreateLine(x1, y1, x2, y2: Single): LineSegment;
  
  /// @lib LineFromVectorWithStartPoint
  function LineFromVector(const pnt: Point2D; const mvt: Vector): LineSegment; overload;
  /// @lib LineFromVectorWithStartXY
  function LineFromVector(x, y: Single; const mvt: Vector): LineSegment; overload;
  /// @lib
  function LineFromVector(const mvt: Vector): LineSegment; overload;
  
  /// @lib
  function MidPoint(const line: LineSegment): Point2D;
  
  /// @lib
  function CreateRectangle(x, y: Single; w, h: LongInt): Rectangle; overload;
  /// @lib CreateRectangleForBitmap
  function CreateRectangle(bmp: Bitmap): Rectangle; overload;
  /// @lib CreateRectangleForBitmapAtXY
  function CreateRectangle(x, y: Single; bmp: Bitmap): Rectangle; overload;
  /// @lib CreateRectangleForBitmapAtPoint
  function CreateRectangle(const pt: Point2D; bmp: Bitmap): Rectangle; overload;
  /// @lib CreateRectangleAtPoint
  function CreateRectangle(const pt: Point2D; width, height: LongInt): Rectangle; overload;
  /// @lib CreateRectangleForSprite
  function CreateRectangle(sprt: Sprite): Rectangle; overload;
  
  /// @lib CreateTriangle
  function CreateTriangle(ax, ay, bx, by, cx, cy: Single): Triangle; overload;
  
  /// @lib CreateTriangleFromPoints
  function CreateTriangle(const a, b, c: Point2D): Triangle; overload;
  
  /// @lib
  function TriangleBarycenter(const aTriangle: Triangle): Point2D;
  
  /// @lib
  procedure ApplyMatrix(const m: Matrix2D; var toTrangle: Triangle);
  
  /// @lib
  function IsPointInTriangle(const point : Point2D; const theTriangle : Triangle): Boolean;
  
  /// @lib
  function RectangleAroundLine(const line: LineSegment): Rectangle;
  
  /// @lib
  function RectangleAfterMove(const rect: Rectangle; const move: Vector): Rectangle;
  
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
  function DistanceBetween(const pt1, pt2: Point2D): Single;
  
  /// @lib
  function GetLineIntersectionPoint(const line1, line2: LineSegment; out pnt: Point2D) : boolean;
  /// @lib
  function LineIntersectsWithLines(const target: LineSegment; const lines: LinesArray): boolean;
  /// @lib
  function LineIntersectsWithRect(const line: LineSegment; const rect: Rectangle): boolean;

  /// @lib PointIsWithinRectXY
  function PointIsWithinRect(const v: Point2D; x, y, w, h: Single): Boolean; overload;
  /// @lib
  function PointIsWithinRect(const v: Point2D; const rect: Rectangle): Boolean; overload;

implementation
  uses math, sysutils, classes, sgGraphics, sgPhysics;

  const
    EPS    = 0.01;         // smallest positive value: less than that to be considered zero
    EPSEPS = EPS * EPS;        // and its square

  //
  //  Returns the square of the magnitude of the line
  //    to cut down on unnecessary Sqrt when in many cases
  //    DistancePointLine() squares the result
  //
  //function SqLineMagnitude(const line.startPoint.x, line.startPoint.y, line.endPoint.x, line.endPoint.y: extended): extended;
  function SqLineMagnitude(const line: LineSegment): Single; overload;
  begin
    result := (line.endPoint.x - line.startPoint.x) * (line.endPoint.x - line.startPoint.x) + 
          (line.endPoint.y - line.startPoint.y) * (line.endPoint.y - line.startPoint.y);
  end;
  
  function SqLineMagnitude(x1, y1, x2, y2: single): single; overload;
  begin
   result := (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1);
  end;

  function DistancePointToLine(x, y: Single; const line: LineSegment): Single; overload;
  begin
    result := DistancePointToLine(CreatePoint(x, y), line);
  end;

  //  pnt.x, pnt.y is the point to test.
  //  line.startPoint.x, line.startPoint.y, line.endPoint.x, line.endPoint.y is the line to check distance.
  //
  //  Returns distance from the line, or if the intersecting point on the line nearest
  //    the point tested is outside the endpoints of the line, the distance to the
  //    nearest endpoint.
  //
  //  Returns -1 on zero-valued denominator conditions to return an illegal distance. (
  //    modification of Brandon Crosby's VBA code)
  //function DistancePointLine(const pnt.x, pnt.y, line.startPoint.x, line.startPoint.y, line.endPoint.x, line.endPoint.y: extended ): extended;
  function DistancePointToLine(const pnt: Point2D; const line: LineSegment): Single; overload;
  var
    sqLineMag,              // square of line's magnitude (see note in function LineMagnitude)
    u: Single;              // see Paul Bourke's original article(s)
    intersect: Point2D;
  begin
    sqLineMag := SqLineMagnitude(line);
    if SqLineMag < EPSEPS then
    begin
      raise Exception.Create('Cannot determine intersection point on line, line is too short');
    end;

    //squared unit vector
    u := ( (pnt.x - line.startPoint.x) * (line.endPoint.x - line.startPoint.x) + (pnt.y - line.startPoint.y) * (line.endPoint.y - line.startPoint.y) ) / sqLineMag;

    if (u < EPS) or (u > 1) then
    begin
      //  Closest point does not fall within the line segment,
      //    take the shorter distance to an endpoint
      intersect.x := SqLineMagnitude(pnt.x, pnt.y, line.startPoint.x, line.startPoint.y);
      intersect.y := SqLineMagnitude(pnt.x, pnt.y, line.endPoint.x, line.endPoint.y);
      result := min(intersect.x, intersect.y);
    end //  if (u < EPS) or (u > 1)
    else
    begin
      //  Intersecting point is on the line, use the formula
      intersect.x := line.startPoint.x + u * (line.endPoint.x - line.startPoint.x);
      intersect.y := line.startPoint.y + u * (line.endPoint.y - line.startPoint.y);
      result := SqlineMagnitude(pnt.x, pnt.y, intersect.x, intersect.y);
    end; //  else NOT (u < EPS) or (u > 1)

    // finally convert to actual distance not its square
    result := sqrt(result);
  end;
  
  function ClosestPointOnLine(x, y: Single; const line: LineSegment): Point2D; overload;
  begin
    result := ClosestPointOnLine(CreatePoint(x, y), line);
  end;
  
  function ClosestPointOnLine(const fromPt: Point2D; const line: LineSegment): Point2D; overload;
  var
    sqLineMag,              // square of line's magnitude (see note in function LineMagnitude)
    u: Single;              // see Paul Bourke's original article(s)
  begin
    sqLineMag := SqLineMagnitude(line);
    if SqLineMag < EPSEPS then
    begin
      raise Exception.Create('Cannot determine intersection point on line, line is too short');
    end;

    u := ( (fromPt.x - line.startPoint.x)*(line.endPoint.x - line.startPoint.x) + (fromPt.y - line.startPoint.y) * (line.endPoint.y - line.startPoint.y) ) / sqLineMag;

    if (u < EPS) or (u > 1) then
    begin
      //  Closest point does not fall within the line segment,
      //    take the shorter distance to an endpoint
      if SqLineMagnitude(fromPt.x, fromPt.y, line.startPoint.x, line.startPoint.y) < SqLineMagnitude(fromPt.x, fromPt.y, line.endPoint.x, line.endPoint.y) then
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

  function RectangleAroundLine(const line: LineSegment): Rectangle;
{ var
    x, y, width, height : Single;}
  begin
    result.x := Min(line.startPoint.x, line.endPoint.x);
    result.y := Min(line.startPoint.y, line.endPoint.y);
    result.width := Round(Max(line.startPoint.x, line.endPoint.x) - result.x);
    result.height := Round(Max(line.startPoint.y, line.endPoint.y) - result.y);
        
{   if line.startPoint.x < line.endpoint.x then
    begin
      x := line.startPoint.x;
      width := line.endPoint.x - line.startPoint.x;
    end
    else
    begin
      x := line.endPoint.x;
      width := line.startPoint.x - line.endPoint.x;
    end;
    
    if line.startPoint.y < line.endpoint.y then
    begin
      y := line.startPoint.y;
      height := line.endPoint.y - line.startPoint.y;
    end
    else
    begin
      y := line.endPoint.y;
      height := line.startPoint.y - line.endPoint.y;
    end;

    result := CreateRectangle(Round(x), Round(y), Round(width), Round(height));
    DrawRectangle(ColourWhite, result);}
  end;
    
  function IsPointOnLine(const pnt: Point2D; const line: LineSegment): Boolean;
  const SMALL = 0.9;
    function SimpleComparisonXSame(): Boolean;
    var
      minY, maxY: Single;
    begin
      minY := Min(line.startPoint.y, line.endPoint.Y);
      maxY := Max(line.startPoint.y, line.endPoint.Y);
      
      result := 
        (pnt.x >= line.startPoint.x - SMALL) and (pnt.x <= line.startPoint.x + SMALL) and
        (pnt.y >= minY) and (pnt.y <= maxY);
    end;
    
    function SimpleComparisonYSame(): Boolean;
    var
      minX, maxX: Single;
    begin
      minX := Min(line.startPoint.x, line.endPoint.x);
      maxX := Max(line.startPoint.x, line.endPoint.x);
      
      result := 
        (pnt.y >= line.startPoint.y - SMALL) and (pnt.y <= line.startPoint.y + SMALL) and
        (pnt.x >= minX) and (pnt.x <= maxX);
    end;
  var
    sqLineMag, lx, ly, m, c : Single;
  begin
    //Lines Magnitude must be at least 0.0001
    sqLineMag := SqLineMagnitude(line);
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

    ly := (m * pnt.x) + c;
    lx := (pnt.y - c) / m;
    
    //WriteLn('lx,ly = px,py', lx, ',', ly, ' = ', pnt.x, ',', pnt.y);
    
    result := (lx >= pnt.x - SMALL) and (lx <= pnt.x + SMALL) and (ly >= pnt.y - SMALL) and (ly <= pnt.y + SMALL)
     and PointIsWithinRect(pnt, RectangleAroundLine(line));
    
    {WriteLn(FloatToStr(line.startPoint.y) + ' = ' + FloatToStr(m) + ' * ' + FloatToStr(line.startPoint.x) + ' ' + FloatToStr(c));
    WriteLn(IntToStr(Round(lx)) + ',' + FloatToStr(pnt.y));
    WriteLn(FloatToStr(pnt.x) + ',' + IntToStr(Round(ly)));}

    
    
{   if PointIsWithinRect(CreatePoint(pnt.x, ly), RectFromLine()) and PointIsWithinRect(CreatePoint(lx, pnt.y), RectFromLine()) then
    begin
      //if (pnt.x < lx + 1) and (pnt.x > lx - 1) and (pnt.y < ly + 1) and (pnt.y > ly + 1) then result := true;
      
      if ((pnt.x - lx < 1) and (pnt.x - lx > -1)) or ((pnt.y - ly < 1) and (pnt.y - ly > -1)) then result := true 
    end;
}   
    {u := ( (pnt.x - line.startPoint.x)*(line.endPoint.x - line.startPoint.x) + (pnt.y - line.startPoint.y) * (line.endPoint.y - line.startPoint.y) ) / sqLineMag;
    
    result := (u >= EPS) and (u <= 1);}
    
  end;
    
  function CreateLine(x1, y1, x2, y2: Single): LineSegment;
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
      result[0] := CreateLine(x, y, x + width, y);
      result[1] := CreateLine(x, y, x, y + height);
      result[2] := CreateLine(x + width, y, x + width, y + height);
      result[3] := CreateLine(x, y + height, x + width, y + height);
    end;
    
//    writeln(hexstr(@rect));
//    writeln('rect ', rect.x, ':', rect.y, ' ', rect.width, 'x', rect.height);
//    writeln('[3] ', result[3].startPoint.x, ':' ,result[3].startPoint.y);
  end;
  
  function CreatePoint(x, y: Single): Point2D;
  begin
    result.x := x;
    result.y := y;
  end;
  
  function CenterPoint(sprt: Sprite): Point2D;
  begin
    result.x := sprt.x + CurrentWidth(sprt) / 2;
    result.y := sprt.y + CurrentHeight(sprt) / 2;
  end;
  
  function LineFromVector(const pnt: Point2D; const mvt: Vector): LineSegment; overload;
  begin
    result := LineFromVector(pnt.x, pnt.Y, mvt);
  end;
  
  function LineFromVector(x, y: Single; const mvt: Vector): LineSegment; overload;
  begin
    result.startPoint.x := x;
    result.startPoint.y := y;
    result.endPoint.x := x + mvt.x;
    result.endPoint.y := y + mvt.y;
  end;
  
  function LineFromVector(const mvt: Vector): LineSegment; overload;
  begin
    result := LineFromVector(0, 0, mvt);
  end;
  
  function RectangleAfterMove(const rect: Rectangle; const move: Vector): Rectangle;
  begin
    result := rect;
    result.x := result.x + move.x;
    result.y := result.y + move.y;
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

  function CreateRectangle(x, y: Single; w, h: LongInt): Rectangle; overload;
  begin
    result.x := x;
    result.y := y;
    result.width := w;
    result.height := h;
  end;
  
  function CreateRectangle(sprt: Sprite): Rectangle;
  begin
    result := CreateRectangle(sprt.x, sprt.y, CurrentWidth(sprt), CurrentHeight(sprt));
  end;
  
  function CreateRectangle(const pt: Point2D; width, height: LongInt): Rectangle; overload;
  begin
    result := CreateRectangle(pt.x, pt.y, width, height);
  end;
  
  function CreateRectangle(const pt: Point2D; bmp: Bitmap): Rectangle; overload;
  begin
    result := CreateRectangle(pt.x, pt.y, bmp.width, bmp.height);
  end;
  
  function CreateRectangle(x, y: Single; bmp: Bitmap): Rectangle; overload;
  begin
    result := CreateRectangle(x, y, bmp.width, bmp.height);
  end;
  
  function CreateRectangle(bmp: Bitmap): Rectangle; overload;
  begin
    result := CreateRectangle(0, 0, bmp);
  end;
  
  function CreateTriangle(ax, ay, bx, by, cx, cy: Single): Triangle; overload;
  begin
    result := CreateTriangle(CreatePoint(ax, ay), CreatePoint(bx, by), CreatePoint(cx, cy));
  end;
  
  function CreateTriangle(const a, b, c: Point2D): Triangle; overload;
  begin
    result[0] := a;
    result[1] := b;
    result[2] := c;
  end;
  
  function IsPointInTriangle(const point : Point2D; const theTriangle : Triangle): Boolean;
  var
    v0, v1, v2 : Vector;
    a, b, c, p: Vector;
    dot00, dot01, dot02, dot11, dot12 : Single;
    invDenom, u, v: Single;
  begin
    //Convert Points to vectors
    p := VectorFromPoint(point);
    a := VectorFromPoint(theTriangle[0]);
    b := VectorFromPoint(theTriangle[1]);
    c := VectorFromPoint(theTriangle[2]);
    
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
  
  procedure ApplyMatrix(const m: Matrix2D; var toTrangle: Triangle);
  begin
    toTrangle[0] := MatrixMultiply(m, toTrangle[0]);
    toTrangle[1] := MatrixMultiply(m, toTrangle[1]);
    toTrangle[2] := MatrixMultiply(m, toTrangle[2]);
  end;
  
  function TriangleBarycenter(const aTriangle: Triangle): Point2D;
  begin
    result.x := (aTriangle[0].x + aTriangle[1].x + aTriangle[2].x) / 3;
    result.y := (aTriangle[0].y + aTriangle[1].y + aTriangle[2].y) / 3;
  end;
  
  function MidPoint(const line: LineSegment): Point2D;
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
      result := CreateRectangle(0, 0, 0, 0);
      exit;
    end;
    
    result := CreateRectangle(l, t, Round(r - l), Round(b - t));
  end;
  
  function DistanceBetween(const pt1, pt2: Point2D): Single;
  var
    temp: Vector;
  begin
    temp := CreateVector(pt2.x - pt1.x, pt2.y - pt1.y);
    result := VectorMagnitude(temp);
  end;

  function GetLineIntersectionPoint(const line1, line2: LineSegment; out pnt: Point2D) : boolean;
  var
    // convert lines to the eqn
    // c = ax + by
    a1, b1, c1: Single;
    a2, b2, c2: Single;
    det: Single;
  begin
    pnt.x := 0; pnt.y := 0;
    
    //Convert lines to eqn c = ax + by
    a1 := line1.endPoint.y - line1.startPoint.y; //y12 - y11;
    b1 := line1.startPoint.x - line1.endPoint.x; //x11 - x12;
    c1 := a1 * line1.startPoint.x + b1 * line1.startPoint.y; //a1 * x11 + b1 * y11;

    a2 := line2.endPoint.y - line2.startPoint.y; //y22 - y21;
    b2 := line2.startPoint.x - line2.endPoint.x; //x21 - x22;
    c2 := a2 * line2.startPoint.x + b2 * line2.startPoint.y; //a2 * x21 + b2 * y21;
    
    det := (a1 * b2) - (a2 * b1);
    
    if det = 0 then result := false
    else
    begin
      pnt.x := (b2*c1 - b1*c2) / det;
          pnt.y := (a1*c2 - a2*c1) / det;
      result := true;
    end;    
  end;

  function LineIntersectsWithLines(const target: LineSegment; const lines: LinesArray): boolean;
  var
    i: LongInt;
    pnt: Point2D;
  begin
    for i := 0 to High(lines) do
    begin     
      if GetLineIntersectionPoint(target, lines[i], pnt) and IsPointOnLine(pnt, lines[i]) then
      begin
        result := true;
        exit;
      end;
    end;
    
    result := false;
  end;
  
  function LineIntersectsWithRect(const line: LineSegment; const rect: Rectangle): boolean;
  var
    lines: LinesArray;
  begin
    lines := LinesFromRect(rect);
    result := LineIntersectsWithLines(line, lines);
  end;
  
  function PointIsWithinRect(const v: Point2D; x, y, w, h: Single): Boolean; overload;
  begin
    if v.x < x then result := false
    else if v.x > x + w then result := false
    else if v.y < y then result := false
    else if v.y > y + h then result := false
    else result := true;
  end;
  
  function PointIsWithinRect(const v: Point2D; const rect: Rectangle): Boolean; overload;
  begin
    result := PointIsWithinRect(v, rect.x, rect.y, rect.width, rect.height);    
  end;
end.