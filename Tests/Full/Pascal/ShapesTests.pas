unit ShapesTests;

interface
	uses TestFramework;

	procedure AddShapesSuite(var suites: TestSuites); 

implementation
	uses GameResources, SGSDK_Core, SGSDK_Audio, SGSDK_Input, SGSDK_Graphics, SGSDK_KeyCodes, SGSDK_Shapes, SGSDK_Physics, SGSDK_Font, SysUtils, SGSDK_Camera;
	
	var
		staticRect1, staticRect2, movRect: Rectangle;
		movVec: Vector;
		ball: Sprite;
		movLine: LineSegment;
		rectCol: Colour;
	
	procedure TestShapes1(const drawIn: Rectangle);
	var
		i: Integer;
	begin
		movVec := CreateVector(0, 0);
		
		if IsKeyPressed(VK_LEFT) then movVec := AddVectors(movVec, CreateVector(-2, 0));
		if IsKeyPressed(VK_RIGHT) then movVec := AddVectors(movVec, CreateVector(2, 0));
		if IsKeyPressed(VK_UP) then movVec := AddVectors(movVec, CreateVector(0, -2));
		if IsKeyPressed(VK_DOWN) then movVec := AddVectors(movVec, CreateVector(0, 	2));
		
		movRect := RectangleAfterMove(movRect, movVec);
		
		DrawSprite(ball);
		DrawCircle(ColourWhite, true, CenterPoint(ball), 5);
		
		for i := 0 to 3 do
		begin
			DrawLine(ColourWhite, LinesFromRect(staticRect1)[i]);
			DrawPixel(ColourBlack, MidPoint(LinesFromRect(staticRect1)[i]));
		end;
		
		if RectanglesIntersect(movRect, staticRect1) or RectanglesIntersect(movRect, staticRect2) then
		begin
			DrawText('The green rectangle is intersecting with a', ColourWhite, GameFont('Courier'), 0, 0);
			DrawText('rectangle', ColourWhite, GameFont('Courier'), 0, 15);
		end;
		
		DrawRectangle(ColourYellow, staticRect2);
		DrawRectangle(ColourGreen, movRect);
		
		DrawLine(ColourWhite, ClosestPointOnLine(movRect.x, movRect.y, LinesFromRect(staticRect1)[2]).x, ClosestPointOnLine(movRect.x, movRect.y, LinesFromRect(staticRect1)[2]).y, movRect.x, movRect.y);
		DrawLine(ColourWhite, ClosestPointOnLine(CreatePoint(movRect.x, movRect.y), LinesFromRect(staticRect1)[3]).x, ClosestPointOnLine(movRect.x, movRect.y, LinesFromRect(staticRect1)[3]).y, movRect.x, movRect.y);
		DrawLine(ColourRed, ClosestPointOnLine(movRect.x, movRect.y, LinesFromRect(staticRect1)[0]).x, ClosestPointOnLine(movRect.x, movRect.y, LinesFromRect(staticRect1)[0]).y, movRect.x, movRect.y);
		DrawText('The magnitude of the red line: ' + FloatToStr(DistancePointToLine(CreatePoint(movRect.x, movRect.y), LinesFromRect(staticRect1)[0])), ColourRed, GameFont('Courier'), CreatePoint(0, 30));
		DrawLine(ColourBlue, ClosestPointOnLine(CreatePoint(movRect.x, movRect.y), LinesFromRect(staticRect1)[1]).x, ClosestPointOnLine(movRect.x, movRect.y, LinesFromRect(staticRect1)[1]).y, movRect.x, movRect.y);
		DrawText('The magnitude of the blue line: ' + FloatToStr(DistancePointToLine(movRect.x, movRect.y, LinesFromRect(staticRect1)[1])), ColourBlue, GameFont('Courier'), CreatePoint(0, 45));
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
			DrawLine(ColourBlue, points[num].x, points[num].y, movLine.startPoint.x, movLine.startPoint.y);
			DrawText('m = ' + FloatToStr(DistanceBetween(points[num], movLine.startPoint)), ColourWhite, GameFont('Courier'), points[num].x, points[num].y);
		end;
	begin
		SetLength(points, 4);
		
		mp := CreatePoint(GameX(Round(GetMousePosition().X)), GameY(Round(GetMousePosition().Y)));
		
		if IsKeyPressed(VK_LEFT) then MoveLine(-2, 0);
		if IsKeyPressed(VK_RIGHT) then MoveLine(2, 0);
		if IsKeyPressed(VK_UP) then MoveLine(0, -2);
		if IsKeyPressed(VK_DOWN) then MoveLine(0, 2);
		
		if PointIsWithinRect(movLine.startPoint, staticRect1) or PointIsWithinRect(movLine.endPoint, staticRect1) then rectCol := ColourRed
		else rectCol := ColourWhite;
		
		if LineIntersectsWithLines(movLine, LinesFromRect(staticRect1)) and LineIntersectsWithRect(movLine, staticRect1) then
		begin
			DrawText('The line intersects with the rectangle!', ColourWhite, GameFont('Courier'), 0, 0);
		end;
		
		DrawLine(ColourWhite, movLine);
		If IsPointOnLine(mp, movLine) then DrawCircle(ColorGreen, mp, 5);
		
		DrawHorizontalLine(ColourGreen, RectangleTop(staticRect1), 0, ScreenWidth());
		DrawHorizontalLine(ColourGreen, RectangleBottom(staticRect1), 0, ScreenWidth());
		DrawVerticalLine(ColourGreen, RectangleLeft(staticRect1), 0, ScreenHeight());
		DrawVerticalLine(ColourGreen, RectangleRight(staticRect1), 0, ScreenHeight());
		
		DrawRectangle(rectCol, staticRect1);

		for i := 0 to 3 do
		begin
			tempLine := LinesFromRect(staticRect1)[i];
			
			if GetLineIntersectionPoint(movLine, tempLine, tempPoint) then
			begin
				points[i] := tempPoint;
				
				if IsPointOnLine(tempPoint, movLine) then	DrawCircle(ColorGreen, tempPoint, 5)
				else if IsPointOnLine(tempPoint, tempLine) then DrawCircle(ColorWhite, tempPoint, 5)
				else DrawCircle(ColourRed, tempPoint, 5);
				
				if IsPointOnLine(mp, tempLine) then	DrawCircle(ColorGreen, mp, 5);
			end;
			
{			if IsPointOnLine(movLine.startPoint, tempLine) or IsPointOnLine(movLine.endPoint, tempLine) then
			begin
				DrawLine(ColourYellow, tempLine);
				DrawCircle(ColorWhite, )
			end;}
		end;
		
		diag := CreateLine(RectangleLeft(staticRect1), RectangleTop(staticRect1), RectangleRight(staticRect1), RectangleBottom(staticRect1));
		
		DrawLine(rectCol, diag);
		if IsPointOnLine(mp, diag) then
		begin
			DrawCircle(ColorGreen, mp, 5);
		end;
		
		if IsKeyPressed(VK_1) then ShowDistance(0);
		if IsKeyPressed(VK_2) then ShowDistance(1);
		if IsKeyPressed(VK_3) then ShowDistance(2);
		if IsKeyPressed(VK_4) then ShowDistance(3);
	end;
	
	procedure TestTriangle(const drawIn: Rectangle);
	var
	  tri: Triangle;
	  baryPnt: Point2D;
	begin
	  tri[0] := CreatePoint(100, 100);
	  tri[1] := CreatePoint(100, 150);
	  tri[2] := CreatePoint(150, 150);
	  
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
			Instructions := 'Use the arrow keys to move' + EOL + 'the rectange';
			staticRect1 := CreateRectangle(100, 100, 100, 150);
			staticRect2 := CreateRectangle(250, 100, 100, 150);
			movRect := CreateRectangle(100, 300, 70, 60);
			ball := CreateSprite(GameImage('BallImage1'));
			ball.x := 250; ball.y := 270;
			ToRun := @TestShapes1;
		end;
		
		with result.Tests[1] do
		begin
			MethodBeingTested := 'Shapes test part2';
			Instructions := 'Use the arrow keys to move' + EOL + 'the line';
			movLine := CreateLine(200, 200, 250, 250);
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