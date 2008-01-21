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
	function ClosestPointOnLine(x, y: Single; const line: LineSegment): Point2D; overload;
	function ClosestPointOnLine(const fromPt: Point2D; const line: LineSegment): Point2D; overload;
	
	function CenterPoint(sprt: Sprite): Point2D;
	
	function IsPointOnLine(const pnt: Point2D; const line: LineSegment): Boolean;

	function CreatePoint(x, y: Single): Point2D;

	function LinesFromRect(const rect: Rectangle): LinesArray;
	
	function CreateLine(x1, y1, x2, y2: Single): LineSegment;
	function LineFromVector(const pnt: Point2D; const mvt: Vector): LineSegment; overload;
	function LineFromVector(x, y: Single; const mvt: Vector): LineSegment; overload;
	
	function MidPoint(const line: LineSegment): Point2D;

	function CreateRectangle(x, y: Single; w, h: Integer): Rectangle; overload;
	function CreateRectangle(bmp: Bitmap): Rectangle; overload;
	function CreateRectangle(x, y: Single; bmp: Bitmap): Rectangle; overload;
	function CreateRectangle(const pt: Point2D; bmp: Bitmap): Rectangle; overload;
	function CreateRectangle(const pt: Point2D; width, height: Integer): Rectangle; overload;
	function CreateRectangle(sprt: Sprite): Rectangle; overload;
		
	function RectangleAfterMove(const rect: Rectangle; const move: Vector): Rectangle;

	function RectangleTop	(const rect: Rectangle): Single;
	function RectangleBottom(const rect: Rectangle): Single;
	function RectangleLeft	(const rect: Rectangle): Single;
	function RectangleRight	(const rect: Rectangle): Single;
	
	function RectanglesIntersect(const rect1, rect2: Rectangle): Boolean;
	function Intersection(const rect1, rect2: Rectangle): Rectangle;
	
	function DistanceBetween(const pt1, pt2: Point2D): Single;
implementation
	uses math, sysutils, classes, SGSDK_Graphics, SGSDK_Physics;

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

	function CreateRectangle(x, y: Single; w, h: Integer): Rectangle; overload;
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
	
	function CreateRectangle(const pt: Point2D; width, height: Integer): Rectangle; overload;
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
		temp := CreateVector(pt2.x - pt1.x, pt2.y - pt2.x);
		result := Magnitude(temp);
	end;

end.