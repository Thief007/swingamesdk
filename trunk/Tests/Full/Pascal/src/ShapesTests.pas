unit ShapesTests;

interface
	uses TestFramework;

	procedure AddShapesSuite(var suites: TestSuites); 

implementation
	uses sgImages, GameResources, sgAudio, sgInput, sgGraphics, sgTypes,sgGeometry,sgPhysics, sgText,SysUtils, sgCamera, sgSprites;
	
	var
		staticRect1, staticRect2, movRect: Rectangle;
		movVec: Vector;
		ball: Sprite;
		movLine: LineSegment;
		rectCol: Color;
	
	procedure TestShapes1(const drawIn: Rectangle);
	var
		i: Integer;
	begin
		movVec := VectorTo(0, 0);
		
		if KeyDown(VK_LEFT) then movVec := AddVectors(movVec, VectorTo(-2, 0));
		if KeyDown(VK_RIGHT) then movVec := AddVectors(movVec, VectorTo(2, 0));
		if KeyDown(VK_UP) then movVec := AddVectors(movVec, VectorTo(0, -2));
		if KeyDown(VK_DOWN) then movVec := AddVectors(movVec, VectorTo(0, 	2));
		
		movRect := RectangleAfterMove(movRect, movVec);
		
		DrawSprite(ball);
		DrawCircle(ColorWhite, true, CenterPoint(ball), 5);
		
		for i := 0 to 3 do
		begin
			DrawLine(ColorWhite, LinesFrom(staticRect1)[i]);
			DrawPixel(ColorBlack, LineMidPoint(LinesFrom(staticRect1)[i]));
		end;
		
		if RectanglesIntersect(movRect, staticRect1) or RectanglesIntersect(movRect, staticRect2) then
		begin
			DrawText('The green rectangle is intersecting with a', ColorWhite, FontNamed('Courier'), 0, 0);
			DrawText('rectangle', ColorWhite, FontNamed('Courier'), 0, 15);
		end;
		
		DrawRectangle(ColorYellow, staticRect2);
		DrawRectangle(ColorGreen, movRect);
		
		DrawLine(ColorWhite, ClosestPointOnLine(movRect.x, movRect.y, LinesFrom(staticRect1)[2]).x, ClosestPointOnLine(movRect.x, movRect.y, LinesFrom(staticRect1)[2]).y, movRect.x, movRect.y);
		DrawLine(ColorWhite, ClosestPointOnLine(PointAt(movRect.x, movRect.y), LinesFrom(staticRect1)[3]).x, ClosestPointOnLine(movRect.x, movRect.y, LinesFrom(staticRect1)[3]).y, movRect.x, movRect.y);
		DrawLine(ColorRed, ClosestPointOnLine(movRect.x, movRect.y, LinesFrom(staticRect1)[0]).x, ClosestPointOnLine(movRect.x, movRect.y, LinesFrom(staticRect1)[0]).y, movRect.x, movRect.y);
		DrawText('The magnitude of the red line: ' + FloatToStr(PointLineDistance(PointAt(movRect.x, movRect.y), LinesFrom(staticRect1)[0])), ColorRed, FontNamed('Courier'), PointAt(0, 30));
		DrawLine(ColorBlue, ClosestPointOnLine(PointAt(movRect.x, movRect.y), LinesFrom(staticRect1)[1]).x, ClosestPointOnLine(movRect.x, movRect.y, LinesFrom(staticRect1)[1]).y, movRect.x, movRect.y);
		DrawText('The magnitude of the blue line: ' + FloatToStr(PointLineDistance(movRect.x, movRect.y, LinesFrom(staticRect1)[1])), ColorBlue, FontNamed('Courier'), PointAt(0, 45));
	end;
	
	procedure TestShapes2(const drawIn: Rectangle);
		var
			tempPoint: Point2D;
			i: Integer;
			points: Array of Point2D;
			mp: Point2D; //mouse
			tempLine, diag: LineSegment;
		
		procedure MoveLine(xOffset, yOffset: Integer);
		begin
			movLine.startPoint.x := movLine.startPoint.x + xOffset;
			movLine.startPoint.y := movLine.startPoint.y + yOffset;
			movLine.endPoint.x := movLine.endPoint.x + xOffset;
			movLine.endPoint.y := movLine.endPoint.y + yOffset;
		end;
		
		procedure ShowDistance(num: Integer);
		begin
			DrawLine(ColorBlue, points[num].x, points[num].y, movLine.startPoint.x, movLine.startPoint.y);
			DrawText('m = ' + FloatToStr(PointPointDistance(points[num], movLine.startPoint)), ColorWhite, FontNamed('Courier'), points[num].x, points[num].y);
		end;
	begin
		SetLength(points, 4);
		
		mp := PointAt(ToWorldX(Round(MousePosition().X)), ToWorldY(Round(MousePosition().Y)));
		
		if KeyDown(VK_LEFT) then MoveLine(-2, 0);
		if KeyDown(VK_RIGHT) then MoveLine(2, 0);
		if KeyDown(VK_UP) then MoveLine(0, -2);
		if KeyDown(VK_DOWN) then MoveLine(0, 2);
		
		if PointInRect(movLine.startPoint, staticRect1) or PointInRect(movLine.endPoint, staticRect1) then rectCol := ColorRed
		else rectCol := ColorWhite;
		
		if LineIntersectsLines(movLine, LinesFrom(staticRect1)) and LineIntersectsRect(movLine, staticRect1) then
		begin
			DrawText('The line intersects with the rectangle!', ColorWhite, FontNamed('Courier'), 0, 0);
		end;
		
		DrawLine(ColorWhite, movLine);
		If PointOnLine(mp, movLine) then DrawCircle(ColorGreen, mp, 5);
		
		DrawHorizontalLine(ColorGreen, RectangleTop(staticRect1), 0, ScreenWidth());
		DrawHorizontalLine(ColorGreen, RectangleBottom(staticRect1), 0, ScreenWidth());
		DrawVerticalLine(ColorGreen, RectangleLeft(staticRect1), 0, ScreenHeight());
		DrawVerticalLine(ColorGreen, RectangleRight(staticRect1), 0, ScreenHeight());
		
		DrawRectangle(rectCol, staticRect1);

		for i := 0 to 3 do
		begin
			tempLine := LinesFrom(staticRect1)[i];
			
			if LineIntersectionPoint(movLine, tempLine, tempPoint) then
			begin
				points[i] := tempPoint;
				
				if PointOnLine(tempPoint, movLine) then	DrawCircle(ColorGreen, tempPoint, 5)
				else if PointOnLine(tempPoint, tempLine) then DrawCircle(ColorWhite, tempPoint, 5)
				else DrawCircle(ColorRed, tempPoint, 5);
				
				if PointOnLine(mp, tempLine) then	DrawCircle(ColorGreen, mp, 5);
			end;
			
{			if PointOnLine(movLine.startPoint, tempLine) or PointOnLine(movLine.endPoint, tempLine) then
			begin
				DrawLine(ColorYellow, tempLine);
				DrawCircle(ColorWhite, )
			end;}
		end;
		
		diag := LineFrom(RectangleLeft(staticRect1), RectangleTop(staticRect1), RectangleRight(staticRect1), RectangleBottom(staticRect1));
		
		DrawLine(rectCol, diag);
		if PointOnLine(mp, diag) then
		begin
			DrawCircle(ColorGreen, mp, 5);
		end;
		
		if KeyDown(VK_1) then ShowDistance(0);
		if KeyDown(VK_2) then ShowDistance(1);
		if KeyDown(VK_3) then ShowDistance(2);
		if KeyDown(VK_4) then ShowDistance(3);
	end;
	
	procedure TestTriangle(const drawIn: Rectangle);
	var
	  tri: Triangle;
	  baryPnt: Point2D;
	begin
	  tri[0] := PointAt(100, 100);
	  tri[1] := PointAt(100, 150);
	  tri[2] := PointAt(150, 150);
	  
	  baryPnt := TriangleBarycenter(tri);
	  
	  DrawTriangle(ColorWhite, tri);
	  DrawCircle(ColorRed, baryPnt, 2);
	end;
	
	function GetShapesTests(): TestSuite;
	var
		i: Integer;
	begin
		result.Title := 'Shapes Tests';
		SetLength(result.Tests, 3);
		
		for i := 0 to High(result.Tests) do
		begin
			InitTest(result.Tests[i]);
		end;
		
		with result.Tests[0] do
		begin
			MethodBeingTested := 'Shapes test part1';
			Instructions := 'Use the arrow keys to move' + LineEnding + 'the rectange';
			staticRect1 := RectangleFrom(100, 100, 100, 150);
			staticRect2 := RectangleFrom(250, 100, 100, 150);
			movRect := RectangleFrom(100, 300, 70, 60);
			ball := CreateSprite(BitmapNamed('BallImage1'), 250, 270);
			ToRun := @TestShapes1;
		end;
		
		with result.Tests[1] do
		begin
			MethodBeingTested := 'Shapes test part2';
			Instructions := 'Use the arrow keys to move' + LineEnding + 'the line';
			movLine := LineFrom(200, 200, 250, 250);
			ToRun := @TestShapes2;
		end;
		
    with result.Tests[2] do
		begin
			MethodBeingTested := 'Triangle Tests';
			Instructions := 'Red circle = Barycenter';
			ToRun := @TestTriangle;
		end;
	end;
	
	procedure AddShapesSuite(var suites: TestSuites);
	begin
		suites[High(suites)] := GetShapesTests();
	end;

end.