unit SGSDK_Shapes;

interface
uses SGSDK_Core;

	type
		Point2D = record
				x, y: Single;
			end;
			
		Rectangle = record
				x, y: Single;
				width, height: Integer;
			end;
			
		LineSegment = record
				startPoint: Point2D;
				endPoint: Point2D;
			end;

		LinesArray = Array of LineSegment;
		RectArray = ARray of Rectangle;

	function DistancePointToLine(const pnt: Point2D; const line: LineSegment): Single; overload;
	function DistancePointToLine(x, y: Single; const line: LineSegment): Single; overload;

	function IsPointOnLine(const pnt: Point2D; const line: LineSegment): Boolean;

	function CreatePoint(x, y: Single): Point2D;

	function LinesFromRect(const rect: Rectangle): LinesArray;
	
	function CreateLine(x1, y1, x2, y2: Single): LineSegment;
	function LineFromVector(const pnt: Point2D; const mvt: Vector): LineSegment; overload;
	function LineFromVector(x, y: Single; const mvt: Vector): LineSegment; overload;

	function VectorNormal(const vect: Vector): Vector;
	function LineNormal(const line: LineSegment): Vector;
	function LineAsVector(const line: lineSegment): Vector;
	
	function MidPoint(const line: LineSegment): Point2D;

	function CreateRectangle(x, y: Single; w, h: Integer): Rectangle;
	
	function RectangleAfterMove(const rect: Rectangle; const move: Vector): Rectangle;

	function RectangleTop	(const rect: Rectangle): Single;
	function RectangleBottom(const rect: Rectangle): Single;
	function RectangleLeft	(const rect: Rectangle): Single;
	function RectangleRight	(const rect: Rectangle): Single;
	
implementation
	uses math, sysutils, classes;

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

		u := ( (pnt.x - line.startPoint.x)*(line.endPoint.x - line.startPoint.x) + (pnt.y - line.startPoint.y) * (line.endPoint.y - line.startPoint.y) ) / sqLineMag;

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
		
		
	function IsPointOnLine(const pnt: Point2D; const line: LineSegment): Boolean;
	var
		sqLineMag,              // square of line's magnitude (see note in function LineMagnitude)
		u: Single;              // see Paul Bourke's original article(s)
	begin
		sqLineMag := SqLineMagnitude(line);
		if SqLineMag < EPSEPS then
		begin
			raise Exception.Create('Cannot determine if point is on line, line is too short');
		end;

		u := ( (pnt.x - line.startPoint.x)*(line.endPoint.x - line.startPoint.x) + (pnt.y - line.startPoint.y) * (line.endPoint.y - line.startPoint.y) ) / sqLineMag;
		
		result := (u >= EPS) and (u <= 1);
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
	end;
	
	function CreatePoint(x, y: Single): Point2D;
	begin
		result.x := x;
		result.y := y;
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
	
	function RectangleAfterMove(const rect: Rectangle; const move: Vector): Rectangle;
	begin
		result := rect;
		result.x := result.x + move.x;
		result.y := result.y + move.y;
	end;
	
	function RectangleTop(const rect: Rectangle): Single;
	begin
		if rect.height > 0 then result := rect.y
		else result := rect.y + rect.height; //add negative height
	end;
	
	function RectangleBottom(const rect: Rectangle): Single;
	begin
		if rect.height > 0 then result := rect.y + rect.height
		else result := rect.y; //y is bottom most
	end;

	function RectangleLeft(const rect: Rectangle): Single;
	begin
		if rect.width > 0 then result := rect.x
		else result := rect.x + rect.width; //add negative width
	end;

	function RectangleRight(const rect: Rectangle): Single;
	begin
		if rect.width > 0 then result := rect.x + rect.width
		else result := rect.x; //x is right most
	end;

	function CreateRectangle(x, y: Single; w, h: Integer): Rectangle;
	begin
		result.x := x;
		result.y := y;
		result.width := w;
		result.height := h;
	end;
	
	function LineAsVector(const line: lineSegment): Vector;
	begin
		result.x := line.endPoint.x - line.startPoint.x;
		result.y := line.endPoint.y - line.startPoint.y;
	end;
	
	function VectorNormal(const vect: Vector): Vector;
	var		
		sqrY, sqrX: Single;
	begin
		sqrX := vect.x * vect.x;
		sqrY := vect.y * vect.y;
		
	   result.x := -vect.y / Sqrt(sqrY + sqrX);  // -S2y / ::sqrt(S2y*S2y + S2x*S2x);
	   result.y := vect.x / Sqrt(sqrY + sqrX); // S2x / ::sqrt(S2y*S2y + S2x*S2x);
	end;
	
	function LineNormal(const line: LineSegment): Vector;
	begin
		result := VectorNormal(LineAsVector(line));
	end;
	
	function MidPoint(const line: LineSegment): Point2D;
	begin
		result.x := line.startPoint.x + (line.endPoint.x - line.startPoint.x) / 2; 
		result.y := line.startPoint.y + (line.endPoint.y - line.startPoint.y) / 2;
	end;
end.