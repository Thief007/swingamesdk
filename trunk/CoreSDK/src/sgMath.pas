//=============================================================================
//          sgMath.pas
//----------------------------------------------------------------------------
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
// - 2009-06-23: Clinton: Created unit. Moved math routines from physics and
//                        shapes units. 
//----------------------------------------------------------------------------


/// @module Math
/// @static
unit sgMath;

//----------------------------------------------------------------------------
interface
//----------------------------------------------------------------------------

  uses sgTypes;
  
  //---------------------------------------------------------------------------
  // Vector Creation and Operations
  //---------------------------------------------------------------------------
  
  /// Returns a new `Vector` using the ``x`` and ``y`` values provided. 
  ///
  /// @lib VectorFrom(x, y, False)
  /// @uname VectorFrom
  function VectorFrom(x, y: Single): Vector; overload;
  
  
  /// Creates a new `Vector` with the ``x`` and ``y`` values provided, and will 
  /// invert the ``y`` value if the `invertY`` parameter is True. The inversion 
  /// of the ``y`` value provides a convienient option for handling screen 
  /// related vectors.
  ///
  /// @lib
  /// @uname VectorFromWithInvertY
  function VectorFrom(x, y: Single; invertY: boolean): Vector; overload;
  
  /// Adds the two parameter vectors (``v1`` and ``v2``) together and returns 
  /// the result as a new `Vector`.
  ///
  /// @lib
  function AddVectors(const v1, v2: Vector): Vector;
  
  /// Subtracts the second vector parameter (``v2``) from the first vector 
  /// (``v1``) and returns the result as new `Vector`.
  ///
  /// @lib
  function SubtractVectors(const v1, v2: Vector): Vector;
  
  /// Multiplies each component (``x`` and ``y`` values) of the ``v1`` vector 
  /// by the ``s`` scalar value and returns the result as a new `Vector`.
  /// 
  /// @lib
  function VectorMultiply(const v: Vector; s: Single): Vector;
  
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
  function DotProduct(const v1, v2: Vector): Single;
  
  /// Returns a new `Vector` that is perpendicular ("normal") to the parameter
  /// vector ``v`` provided. The concept of a "normal" vector is usually 
  /// extracted from (or associated with) a line. See `LineNormal`. 
  ///
  /// @lib
  function VectorNormal(const v: Vector): Vector;
  
  /// Returns a unit vector (lenght is 1.0) that is "normal" (prependicular) to
  /// the ``line`` parameter. A normal vector is useful for calculating the
  /// result of a collision such as sprites bouncing off walls (lines).
  /// @lib
  function LineNormal(const line: LineSegment): Vector;
  
  /// Returns a new Vector that is an inverted version of the parameter 
  /// vector (v). In other words, the -/+ sign of the x and y values are changed.
  ///
  /// @lib
  function InvertVector(const v: Vector): Vector;
  
  /// Returns a new `Vector` that is a based on the parameter `v` however
  /// its magnitude (length) will be limited (truncated) if it exceeds the 
  /// specified limit value.
  ///
  /// @lib
  function LimitVector(const v: Vector; limit: Single): Vector;

  /// Returns the unit vector of the parameter vector (v). The unit vector has a
  /// magnitude of 1, resulting in a vector that indicates the direction of
  /// the original vector.
  ///
  /// @lib
  function UnitVector(const v: Vector): Vector;

  /// Test to see if the ``x`` and ``y`` components of the provided vector 
  /// parameter ``v`` are zero. 
  /// 
  /// @lib
  function VectorIsZero(const v: Vector): Boolean;

  /// Returns the magnitude (or "length") of the parameter vector (v) as a 
  /// scalar value.
  ///
  /// @lib
  function VectorMagnitude(const v: Vector): Single;
  
  /// Returns a new `Vector` created using the angle and magnitude (length). 
  /// The angle and magnitude are scalar values and the angle is in degrees.
  ///
  /// @lib
  function VectorFromAngle(angle, magnitude: Single): Vector;
  
  /// Returns a new `Vector` using the x and y value of a Point2D parameter.
  ///
  /// @lib
  function VectorFromPoint(const p1: Point2D): Vector;
  
  /// Returns a `Vector` created from the difference from the ``p1`` to 
  /// the second ``p2`` points (`Point2D`).
  ///
  /// @lib
  function VectorFromPoints(const p1, p2: Point2D): Vector;
  
  /// Returns a `Vector` that is the difference in the position of two sprites 
  /// (``s1`` and ``s2``). 
  ///
  /// @lib
  /// @class Sprite
  /// @method VectorTo
  function VectorFromTo(s1, s2: Sprite): Vector;
  
  /// Returns a `Vector` that is the difference in location from the center of
  /// the sprite ``s`` to the point ``pt``.
  ///
  /// @lib
  function VectorFromCenterSpriteToPoint(s: Sprite; const pt: Point2D): Vector;
  
  /// Returns a new `Vector` created from the start and end points of a 
  /// `lineSegment`. Useful for calculating angle vectors or extracting a 
  /// normal vector (see `LineNormal`) for the line.
  ///
  /// @lib
  function LineAsVector(const line: lineSegment): Vector;
  
  //---------------------------------------------------------------------------
  // Angle Calculation
  //---------------------------------------------------------------------------

  /// @lib
  function CalculateAngle(x1, y1, x2, y2: Single): Single; overload;

  /// @lib CalculateAngleFromPoints
  function CalculateAngle(const pt1, pt2: Point2D): Single; overload;

  /// @lib CalculateAngleFromSprites
  /// @class Sprite
  /// @method AngleTo
  function CalculateAngle(s1, s2: Sprite): Single; overload;

  /// @lib CalculateAngleFromVectors
  function CalculateAngle(const v1, v2: Vector): Single; overload;

  //---------------------------------------------------------------------------
  // Distance / Magnitude Calculation
  //---------------------------------------------------------------------------

  /// @lib
  function LineMagnitudeSq(x1, y1, x2, y2: single): Single; overload;

  /// @lib LineMagnitudeSqFromLine
  function LineMagnitudeSq(const line: LineSegment): Single; overload;

  /// @lib
  function PointPointDistance(const pt1, pt2: Point2D): Single;



  //---------------------------------------------------------------------------
  // Matrix2D Creation and Operations
  //---------------------------------------------------------------------------
  
  /// @lib
  /// @class Matrix2D
  /// @static
  /// @method TranslationMatrix
  function TranslationMatrix(dx, dy: Single): Matrix2D;
  
  /// @lib
  /// @class Matrix2D
  /// @static
  /// @method ScaleMatrix
  function ScaleMatrix(scale: Single): Matrix2D;

  /// @lib
  /// @class Matrix2D
  /// @static
  /// @method RotationMatrix
  function RotationMatrix(deg: Single): Matrix2D;
  
  /// Multiplies the two `Matrix2D` parameters, ``m1`` by ``m2``, and returns
  /// the result as a new `Matrix2D`. Use this to combine the effects to two 
  /// matrix transformations.
  ///
  /// @lib
  /// @class Matrix2D
  /// @method Multiply
  function MatrixMultiply(const m1, m2: Matrix2D): Matrix2D; overload;

  /// Multiplies the `Vector` parameter ``v`` with the `Matrix2D` ``m`` and 
  /// returns the result as a `Vector`. Use this to transform the vector with 
  /// the matrix (to apply scaling, rotation or translation effects).
  ///
  /// @lib MatrixMultiplyVector
  /// @class Matrix2D
  /// @overload Multiply MultiplyVector
  function MatrixMultiply(const m: Matrix2D; const v: Vector): Vector; overload;

  /// Multiplies the Point2D parameter `pt` with the Matrix2D `m` and returns 
  /// the result as a `Point2D`. Use this to transform the point with the 
  /// matrix (to apply scaling, rotation or translation effects).
  ///
  /// @lib MatrixMultiplyPoint
  /// @class Matrix2D
  /// @overload Multiply MultiplyPoint
  function MatrixMultiply(const m: Matrix2D; const pt: Point2D): Point2D; overload;
  
  /// @lib
  procedure ApplyMatrix(const m: Matrix2D; var tri: Triangle);
  
  
  
  //----------------------------------------------------------------------------
  // Cos/Sin/Tan accepting degrees
  //----------------------------------------------------------------------------
  
  /// Returns the cosine of the passed in angle (in degrees).
  ///
  /// @lib
  function Cos(angle: Single): Single;
  
  /// Returns the sine of the passed in angle (in degrees).
  ///
  /// @lib
  function Sin(angle: Single): Single;
  
  /// Returns the tangent of the passed in angle (in degrees).
  ///
  /// @lib
  function Tan(angle: Single): Single;
  
  
  
//----------------------------------------------------------------------------
implementation
//----------------------------------------------------------------------------

  uses Math, sgShapes, sgCamera, sgGraphics;
  
  const 
    DEG_TO_RAD = 0.0174532925;
  
  //---------------------------------------------------------------------------
  // Vector operations on Vectors (usally returning vectors)
  //---------------------------------------------------------------------------


  function VectorFrom(x, y: Single; invertY: boolean): Vector; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgPhysics, sgMath', 'VectorFrom');
    {$ENDIF}

    if invertY then y := y * -1;

    result.x := x;
    result.y := y;
    //result.w := 1;
  
    {$IFDEF TRACE}
      TraceExit('sgPhysics, sgMath', 'VectorFrom');
    {$ENDIF}
  end;

  function VectorFrom(x, y: Single): Vector; overload;
  begin
    result := VectorFrom(x, y, false);
  end;

  function VectorFromPoint(const p1: Point2D): Vector;
  begin
    result := VectorFrom(p1.x, p1.y);
  end;

  function VectorFromPoints(const p1, p2: Point2D): Vector;
  begin
    result := VectorFrom(p2.x - p1.x, p2.y - p1.y, false);
  end;

  function VectorFromCenterSpriteToPoint(s: Sprite; const pt: Point2D): Vector;
  begin
    result := VectorFromPoints(CenterPoint(s), pt);   
  end;


  function AddVectors(const v1, v2: Vector): Vector;
  begin
    result.x := v1.x + v2.x;
    result.y := v1.y + v2.y;
  end;

  function SubtractVectors(const v1, v2: Vector): Vector;
  begin
    result.x := v1.x - v2.x;
    result.y := v1.y - v2.y;
  end;

  function VectorMultiply(const v: Vector; s: Single): Vector;
  begin
    result.x := v.x * s;
    result.y := v.y * s;
  end;

  function InvertVector(const v: Vector): Vector;
  begin
    result.x := v.x * -1;
    result.y := v.y * -1;
  end;

  function LimitVector(const v: Vector; limit: Single): Vector;
  var
    mag: Single;
    tmp: Vector;
  begin
    mag := VectorMagnitude(v);
    if mag > limit then 
    begin
      tmp := UnitVector(v);
      result.x := tmp.x * limit;
      result.y := tmp.y * limit;
      //result := VectorMultiply(UnitVector(v), limit);
      //result := Multiply(ScaleMatrix(limit), GetUnitVector(v));
    end
    else
      result := v;
  end;

  function UnitVector(const v: Vector): Vector;
  var
    mag, tmp: Single; 
  begin
    mag := VectorMagnitude(v);
  
    if mag = 0 then
      tmp := 0
    else
      tmp := 1 / mag; //VectorMagnitude(v);
  
    result.x := tmp * v.x;
    result.y := tmp * v.y;
  end;

  function VectorIsZero(const v: Vector): Boolean;
  begin
    result := (v.x = 0) and (v.y = 0);
  end;

  function VectorMagnitude(const v: Vector): Single;
  begin
    result := Sqrt((v.x * v.x) + (v.y * v.y));
  end;

  function DotProduct(const v1, v2: Vector): Single;
  begin
    result := (v1.x * v2.x) + (v1.y * v2.y);
  end;

  function VectorFromTo(s1, s2: Sprite): Vector;
  begin
    result := VectorFromPoints(CenterPoint(s1), CenterPoint(s2));
  end;

  function VectorFromAngle(angle, magnitude: Single): Vector;
  begin
    result := VectorFrom(magnitude * sgMath.Cos(angle), magnitude * sgMath.Sin(angle));
  end;

  function LineAsVector(const line: lineSegment): Vector;
  begin
    result.x := line.endPoint.x - line.startPoint.x;
    result.y := line.endPoint.y - line.startPoint.y;
  end;

  function VectorNormal(const v: Vector): Vector;
  var   
    //sqrY, sqrX, 
    tmp: Single;
  begin
    //sqrX := vect.x * vect.x;
    //sqrY := vect.y * vect.y;
    tmp := Sqrt( (v.x * v.x) + (v.y * v.y) );
    result.x := -v.y / tmp; // -S2y / ::sqrt(S2y*S2y + S2x*S2x);
    result.y :=  v.x / tmp; //  S2x / ::sqrt(S2y*S2y + S2x*S2x);
  end;

  function LineNormal(const line: LineSegment): Vector;
  begin
    result := VectorNormal(LineAsVector(line));
  end;

  
  //---------------------------------------------------------------------------
  // Angle Calculation 
  //---------------------------------------------------------------------------


  function CalculateAngle(x1, y1, x2, y2: Single): Single; overload;
  var
    o, a, oa, rads: Single;
  begin
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
  end;

  function CalculateAngle(const pt1, pt2: Point2D): Single; overload;
  begin
    result := CalculateAngle(pt1.x, pt1.y, pt2.x, pt2.y);
  end;

  function CalculateAngle(s1, s2: Sprite): Single; overload;
  var
    cx1, cy1, cx2, cy2: Single;
  begin
    cx1 := s1^.x + CurrentWidth(s1) / 2;
    cy1 := s1^.y + CurrentHeight(s1) / 2;
    cx2 := s2^.x + CurrentWidth(s2) / 2;
    cy2 := s2^.y + CurrentHeight(s2) / 2;

    result := CalculateAngle(cx1, cy1, cx2, cy2);
  end;

  function CalculateAngle(const v1, v2: Vector): Single; overload;
  var
    t1, t2: Single;
  begin
    t1 := CalculateAngle(0, 0, v1.x, v1.y);
    t2 := CalculateAngle(0, 0, v2.x, v2.y);
  
    result := t2 - t1;
  
    if result > 180 then result := result - 360
    else if result <= -180 then result := result + 360
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
    result := (line.endPoint.x - line.startPoint.x) * (line.endPoint.x - line.startPoint.x) +
              (line.endPoint.y - line.startPoint.y) * (line.endPoint.y - line.startPoint.y);
  end;

  function LineMagnitudeSq(x1, y1, x2, y2: single): single; overload;
  begin
   result := (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1);
  end;

  function PointPointDistance(const pt1, pt2: Point2D): Single;
  var
    temp: Vector;
  begin
    temp := VectorFrom(pt2.x - pt1.x, pt2.y - pt1.y);
    result := VectorMagnitude(temp);
  end;
  
  //----------------------------------------------------------------------------
  // Matrix2D Creation and Operation / Translation of Point/Vector Types
  //----------------------------------------------------------------------------

  function RotationMatrix(deg: Single): Matrix2D;
  var
    rads: Double;
  begin
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
  end;

  function ScaleMatrix(scale: Single): Matrix2D;
  begin
    result[0, 0] := scale;
    result[0, 1] := 0;
    result[0, 2] := 0;

    result[1, 0] := 0;
    result[1, 1] := scale;
    result[1, 2] := 0;

    result[2, 0] := 0;
    result[2, 1] := 0;
    result[2, 2] := 1;
  end;

  function TranslationMatrix(dx, dy: Single): Matrix2D;
  begin
    result := ScaleMatrix(1);

    result[0, 2] := dx;
    result[1, 2] := dy;
  end;

  function MatrixMultiply(const m1, m2: Matrix2D): Matrix2D; overload;
    // procedure ShowMatrix(const m: Matrix2D);
    // var
    //   i, j: LongInt;
    // begin
    //   WriteLn('---');
    //   for i := 0 to 2 do
    //   begin
    //     Write('|');
    //     for j := 0 to 2 do
    //     begin
    //       Write(' ', m[i,j]);
    //     end;
    //     WriteLn('|');
    //   end;
    //   WriteLn('---');
    // end;
  begin
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
  end;

  function MatrixMultiply(const m: Matrix2D; const v: Vector): Vector; overload;
  begin
    result.x := v.x * m[0,0]  +  v.y * m[0,1] + m[0,2]; 
    result.y := v.x * m[1,0]  +  v.y * m[1,1] + m[1,2]; 
  end;

  function MatrixMultiply(const m: Matrix2D; const pt: Point2D): Point2D; overload;
  begin
    result.x := pt.x * m[0,0] + pt.y * m[0,1] + m[0,2];
    result.y := pt.x * m[1,0] + pt.y * m[1,1] + m[1,2];
  end;


  procedure ApplyMatrix(const m: Matrix2D; var tri: Triangle);
  begin
    tri[0] := MatrixMultiply(m, tri[0]);
    tri[1] := MatrixMultiply(m, tri[1]);
    tri[2] := MatrixMultiply(m, tri[2]);
  end;
  
  //----------------------------------------------------------------------------
  // Cos/Sin/Tan accepting degrees
  //----------------------------------------------------------------------------
  
  function Cos(angle: Single): Single;
  begin
    result := System.Cos(DegToRad(angle));
  end;
  
  function Sin(angle: Single): Single;
  begin
    result := System.Sin(DegToRad(angle));
  end;
  
  function Tan(angle: Single): Single;
  begin
    result := Math.Tan(DegToRad(angle));
  end;
  

end.