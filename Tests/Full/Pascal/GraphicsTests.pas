unit GraphicsTests;

interface
	uses TestFramework;

	procedure AddGraphicsSuite(var suites: TestSuites); 

implementation
	uses GameResources, SGSDK_Core, SGSDK_Input, SGSDK_Graphics, SGSDK_KeyCodes, SGSDK_Shapes, SGSDK_Font, SGSDK_Physics, SysUtils;
	
	var
		//Rectangle test variables
		tempRect, tempRect2, tempRect3, tempRect4,
		tempRect5, tempRect6: Rectangle;
		smallScreen: Bitmap;
		filled: Boolean;
		//Circle test variables
		curRadius: Integer;
		tempPoint, tempPoint2, tempPoint3, tempPoint4,
		tempPoint5, tempPoint6: Point2D;
		//TestLines variables
		angle: Integer;
		tempLines: Array of LineSegment;
		//Pixel
		curCol: Colour;
		//BitmapTest
		frameB: Array of Bitmap;
		ship: Bitmap;
		framePos: Point2D;
	
	procedure TestBitmap(const drawIn: Rectangle);
	begin
		if IsKeyPressed(VK_RIGHT) then framePos.x := framePos.x - 2;
		if IsKeyPressed(VK_LEFT) then framePos.x := framePos.x + 2;
		if IsKeyPressed(VK_UP) then framePos.y := framePos.y + 2;
		if IsKeyPressed(VK_DOWN) then framePos.y := framePos.y - 2;
		
		ClearSurface(smallScreen, ColourBlack);
		
		DrawBitmap(smallScreen, frameB[0], CreatePoint(0, 0));
		DrawBitmap(smallScreen, frameB[2], 278, 0);
		
		DrawBitmapPart(smallScreen, ship, CreateRectangle(framePos.x + 29, framePos.y + 50, 100, 100), CreatePoint(29, 50));
		DrawBitmapPart(smallScreen, ship, CreateRectangle(framePos.x + 149, framePos.y + 50, 120, 100), 149, 50);
		DrawBitmapPart(smallScreen, ship, Round(framePos.x + 289), Round(framePos.y + 50), 100, 100, 289, 50);
		
		DrawBitmap(smallScreen, 0, 0);
		
		DrawBitmap(frameB[1], CreatePoint(139, 0));
		DrawBitmap(frameB[3], 0, 209);
		DrawBitmapOnScreen(frameB[4], CreatePoint(139 + drawIn.x, 209 + drawIn.y));
		DrawBitmapOnScreen(frameB[5], Round(278 + drawIn.x), Round(209 + drawIn.y));
		
		DrawBitmapPart(ship, CreateRectangle(framePos.x + 29, framePos.y + 159, 100, 100), 29, 159);
		DrawBitmapPart(ship, CreateRectangle(framePos.x + 149, framePos.y + 159, 120, 100), CreatePoint(149, 159));
		DrawBitmapPart(ship, Round(framePos.x + 289), Round(framePos.y + 159), 100, 100, 289, 159);
		
		DrawBitmapPartOnScreen(ship, CreateRectangle(framePos.x + 29, framePos.y + 268, 100, 100), Round(29 + drawIn.x), Round(268 + drawIn.y));
		DrawBitmapPartOnScreen(ship, CreateRectangle(framePos.x + 149, framePos.y + 268, 120, 100), CreatePoint(149 + drawIn.x, 268 + drawIn.y));
		DrawBitmapPartOnScreen(ship, Round(framePos.x + 289), Round(framePos.y + 268), 100, 100, Round(289 + drawIn.x), Round(268 + drawIn.y));
	end;
	
	procedure TestPixels(const drawIn: Rectangle);
	begin
		if IsKeyPressed(VK_Z) then 
		begin
			ClearSurface(smallScreen, ColourTransparent);
			ClearScreen(ColourTransparent);
			DrawBitmapOnScreen(GameImage('BGA'), Round(drawIn.x), Round(drawIn.y))
		end;
		
		if IsKeyPressed(VK_1) then curCol := GetColour(GetRed(curCol) + 5, GetGreen(curCol), GetBlue(curCol));
		if IsKeyPressed(VK_2) then curCol := GetColour(GetRed(curCol), GetGreen(curCol) + 5, GetBlue(curCol));
		if IsKeyPressed(VK_3) then curCol := GetColour(GetRed(curCol), GetGreen(curCol), GetBlue(curCol) + 5);
		
		FillRectangle(ColourBlack, 0, 0, 300, 45);
		DrawText('Red   : ' + IntToStr(GetRed(curCol)), ColourRed, GameFont('Courier'), 0, 0);
		DrawText('Green : ' + IntToStr(GetGreen(curCol)), ColourGreen, GameFont('Courier'), 0, 15);
		DrawText('Blue  : ' + IntToStr(GetBlue(curCol)), ColourBlue, GameFont('Courier'), 0, 30);
		
		If IsMouseDown(LeftButton) then
		begin
			DrawPixel(smallScreen, curCol, CreatePoint(GetMousePosition().x - drawIn.x - 10, GetMousePosition().y - drawIn.y - 10));
			DrawPixel(smallScreen, curCol, Round(GetMousePosition().x - drawIn.x + 10), Round(GetMousePosition().y - drawIn.y) - 10);
		
			DrawPixel(curCol, CreatePoint(GetMousePosition().x - 10 - drawIn.x, GetMousePosition().y - drawIn.y));
			DrawPixel(curCol, Round(GetMousePosition().x - drawIn.x + 10), Round(GetMousePosition().y - drawIn.y));
		
			DrawPixelOnScreen(curCol, CreatePoint(GetMousePosition().x - 10, GetMousePosition().y + 10));
			DrawPixelOnScreen(curCol, Round(GetMousePosition().x) + 10, Round(GetMousePosition().y) + 10);
		end;
		
		DrawBitmap(smallScreen, 0, 0);
	end;
	
	procedure TestLines(const drawIn: Rectangle);
	var
		i : Integer;
	begin
		ClearSurface(smallScreen, ColourTransparent);
		
		if IsKeyPressed(VK_RIGHT) then angle := +2;
		if IsKeyPressed(VK_LEFT) then angle := -2;
		
		if angle <> 0 then
		begin
			for i := 1 to 6 do
			begin
				tempLines[i] := LineFromVector(tempLines[i].startPoint.x, tempLines[i].startPoint.y, Multiply(RotationMatrix(angle), LineAsVector(tempLines[i])));
			end;
		end;
		
		DrawHorizontalLine(smallScreen, ColourGreen, 100, 100, 350);
		DrawVerticalLine(smallScreen, ColourGreen, 100, 100, 350);
		DrawBitmap(smallScreen, 0, 0);
		ClearSurface(smallScreen, ColourTransparent);
		DrawLine(smallScreen, ColourWhite, tempLines[1]);
		DrawLine(smallScreen, ColourWhite, Round(tempLines[3].startPoint.x), Round(tempLines[3].startPoint.y), Round(tempLines[3].endPoint.x), Round(tempLines[3].endPoint.y));
		
		DrawHorizontalLine(ColourGreen, 225, 100, 350);
		DrawVerticalLine(ColourGreen, 225, 100, 350);
		DrawHorizontalLineOnScreen(ColourGreen, Round(350 + drawIn.y), Round(100 + drawIn.x), Round(350 + drawIn.x));
		DrawVerticalLineOnScreen(ColourGreen, Round(350 + drawIn.x), Round(100 + drawIn.y), Round(350 + drawIn.y));
		
		DrawLine(ColourWhite, tempLines[2]);
		DrawLine(ColourWhite, tempLines[4].startPoint.x, tempLines[4].startPoint.y, tempLines[4].endPoint.x, tempLines[4].endPoint.y);
		
		DrawBitmap(smallScreen, 0, 0);
		
		DrawLineOnScreen(ColourWhite, tempLines[5]);
		DrawLineOnScreen(ColourWhite, Round(tempLines[6].startPoint.x), Round(tempLines[6].startPoint.y), Round(tempLines[6].endPoint.x), Round(tempLines[6].endPoint.y));
		
		angle := 0;
	end;
	
	procedure TestFill2(const drawIn: Rectangle);
	begin
		ClearSurface(smallScreen, ColourTransparent);
		
		if IsKeyPressed(VK_RIGHT) then tempPoint.x := tempPoint.x + 2;
		if IsKeyPressed(VK_LEFT) then tempPoint.x := tempPoint.x - 2;
		if IsKeyPressed(VK_UP) then tempPoint.y := tempPoint.y - 2;
		if IsKeyPressed(VK_DOWN) then tempPoint.y := tempPoint.y + 2;
		if IsKeyPressed(VK_Z) then curRadius := curRadius - 1;
		if IsKeyPressed(VK_X) then curRadius := curRadius + 1;
		
		if curRadius < 0 then curRadius := 0;
		if curRadius > 50 then curRadius := 50;
		
		FillCircle(smallScreen, ColourGreen, tempPoint, curRadius);
		FillCircle(smallScreen, ColourGreen, Round(tempPoint.x + curRadius * 2), Round(tempPoint.y + curRadius * 2), curRadius);
		DrawBitmap(smallScreen, 0, 0);
		
		FillCircle(ColourWhite, curRadius, curRadius, curRadius);
		tempPoint3.x := curRadius * 3;
		tempPoint3.y := curRadius * 3;
		FillCircle(ColourWhite, tempPoint3, curRadius);
		
		FillCircleOnScreen(ColourYellow, Round(curRadius + drawIn.x), Round(RectangleBottom(drawIn) - curRadius) - 1, curRadius);
		tempPoint5.x := curRadius * 3 + drawIn.x;
		tempPoint5.y := RectangleBottom(drawIn) - curRadius * 3 - 1;
		FillCircleOnScreen(ColourYellow, tempPoint5, curRadius);
	end;
	
	procedure TestFill1(const drawIn: Rectangle);
	begin
		ClearSurface(smallScreen, ColourTransparent);
		
		if IsKeyPressed(VK_RIGHT) then tempRect.x := tempRect.x + 2;
		if IsKeyPressed(VK_LEFT) then tempRect.x := tempRect.x - 2;
		if IsKeyPressed(VK_UP) then tempRect.y := tempRect.y - 2;
		if IsKeyPressed(VK_DOWN) then tempRect.y := tempRect.y + 2;
		if IsKeyPressed(VK_A) then tempRect.width := tempRect.width - 2;
		if IsKeyPressed(VK_S) then tempRect.width := tempRect.width + 2;
		if IsKeyPressed(VK_Z) then tempRect.height := tempRect.height - 2;
		if IsKeyPressed(VK_X) then tempRect.height := tempRect.height + 2;
		
		if tempRect.width < 0 then tempRect.width := 0;
		if tempRect.height < 0 then tempRect.height := 0;
		if tempRect.width > 100 then tempRect.width := 100;
		if tempRect.height > 100 then tempRect.height := 100;
		
		FillRectangle(smallScreen, ColourGreen, tempRect);
		FillRectangle(smallScreen, ColourGreen, Round(tempRect.width + tempRect.x), Round(tempRect.y + tempRect.height), tempRect.width, tempRect.height);
		FillEllipse(smallScreen, ColourGreen, Round(tempRect.x), Round(tempRect.y + tempRect.height), tempRect.width, tempRect.height);
		tempRect4.x := tempRect.x + tempRect.width;
		tempRect4.y := tempRect.y;
		tempRect4.width := tempRect.width;
		tempRect4.height := tempRect.height;
		FillEllipse(smallScreen, ColourGreen, tempRect4);
		DrawBitmap(smallScreen, 0, 0);
		
		FillRectangle(ColourWhite, 0, 0, tempRect.width, tempRect.height);
		tempRect2.x := tempRect.width;
		tempRect2.y := tempRect.height;
		tempRect2.width := tempRect.width;
		tempRect2.height := tempRect.height;
		FillRectangle(ColourWhite, tempRect2);
		FillEllipse(ColourWhite, 0, tempRect.height, tempRect.width, tempRect.height);
		tempRect3.x := tempRect.width;
		tempRect3.width := tempRect.width;
		tempRect3.height := tempRect.height;
		FillEllipse(ColourWhite, tempRect3);
		
		FillRectangleOnScreen(ColourYellow, Round(drawIn.x), Round(RectangleBottom(drawIn) - tempRect.height) - 1, tempRect.width, tempRect.height);
		tempRect5.x := tempRect.width + drawIn.x;
		tempRect5.y := RectangleBottom(drawIn) - tempRect.height * 2 - 1;
		tempRect5.width := tempRect.width;
		tempRect5.height := tempRect.height;
		FillRectangleOnScreen(ColourYellow, tempRect5);
		FillEllipseOnScreen(ColourYellow, Round(drawIn.x), Round(RectangleBottom(drawIn) - tempRect.height * 2) - 1, tempRect.width, tempRect.height);
		tempRect6.x := tempRect.width + drawIn.x;
		tempRect6.y := RectangleBottom(drawIn) - tempRect.height - 1;
		tempRect6.width := tempRect.width;
		tempRect6.height := tempRect.height;
		FillEllipseOnScreen(ColourYellow, tempRect6);
	end;
	
	procedure TestDrawEllipse(const drawIn: Rectangle);
	begin
		ClearSurface(smallScreen, ColourTransparent);
		if IsKeyPressed(VK_RIGHT) then tempRect.x := tempRect.x + 2;
		if IsKeyPressed(VK_LEFT) then tempRect.x := tempRect.x - 2;
		if IsKeyPressed(VK_UP) then tempRect.y := tempRect.y - 2;
		if IsKeyPressed(VK_DOWN) then tempRect.y := tempRect.y + 2;
		if IsKeyPressed(VK_A) then tempRect.width := tempRect.width - 2;
		if IsKeyPressed(VK_S) then tempRect.width := tempRect.width + 2;
		if IsKeyPressed(VK_Z) then tempRect.height := tempRect.height - 2;
		if IsKeyPressed(VK_X) then tempRect.height := tempRect.height + 2;
		if WasKeyTyped(VK_T) then filled := filled = false;

		if tempRect.width < 0 then tempRect.width := 0;
		if tempRect.height < 0 then tempRect.height := 0;
		if tempRect.width > 100 then tempRect.width := 100;
		if tempRect.height > 100 then tempRect.height := 100;

		DrawEllipse(smallScreen, ColourGreen, filled, tempRect);
		DrawEllipse(smallScreen, ColourGreen, filled, Round(tempRect.x + tempRect.width), Round(tempRect.y + tempRect.height), tempRect.width, tempRect.height);
		DrawEllipse(smallScreen, ColourGreen, Round(tempRect.x), Round(tempRect.y + tempRect.height), tempRect.width, tempRect.height);
		tempRect4.x := tempRect.x + tempRect.width;
		tempRect4.y := tempRect.y;
		tempRect4.width := tempRect.width;
		tempRect4.height := tempRect.height;
		DrawEllipse(smallScreen, ColourGreen, tempRect4);
		DrawBitmap(smallScreen, 0, 0);

		DrawEllipse(ColourWhite, filled, 0, 0, tempRect.width, tempRect.height);
		tempRect2.x := tempRect.width;
		tempRect2.y := tempRect.height;
		tempRect2.width := tempRect.width;
		tempRect2.height := tempRect.height;
		DrawEllipse(ColourWhite, filled, tempRect2);
		DrawEllipse(ColourWhite, 0, tempRect.height, tempRect.width, tempRect.height);
		tempRect3.x := tempRect.width;
		tempRect3.width := tempRect.width;
		tempRect3.height := tempRect.height;
		DrawEllipse(ColourWhite, tempRect3);

		DrawEllipseOnScreen(ColourYellow, filled, Round(drawIn.x), Round(RectangleBottom(drawIn) - tempRect.height), tempRect.width, tempRect.height);
		tempRect5.x := tempRect.width + drawIn.x;
		tempRect5.y := RectangleBottom(drawIn) - tempRect.height * 2;
		tempRect5.width := tempRect.width;
		tempRect5.height := tempRect.height;
		DrawEllipseOnScreen(ColourYellow, filled, tempRect5);
		DrawEllipseOnScreen(ColourYellow, Round(drawIn.x), Round(RectangleBottom(drawIn) - tempRect.height * 2), tempRect.width, tempRect.height);
		tempRect6.x := tempRect.width + drawIn.x;
		tempRect6.y := RectangleBottom(drawIn) - tempRect.height;
		tempRect6.width := tempRect.width;
		tempRect6.height := tempRect.height;
		DrawEllipseOnScreen(ColourYellow, tempRect6);
	end;
	
	procedure TestDrawCircle(const drawIn: Rectangle);
	begin
		ClearSurface(smallScreen, ColourTransparent);
		if IsKeyPressed(VK_RIGHT) then tempPoint.x := tempPoint.x + 2;
		if IsKeyPressed(VK_LEFT) then tempPoint.x := tempPoint.x - 2;
		if IsKeyPressed(VK_UP) then tempPoint.y := tempPoint.y - 2;
		if IsKeyPressed(VK_DOWN) then tempPoint.y := tempPoint.y + 2;
		if IsKeyPressed(VK_Z) then curRadius := curRadius - 1;
		if IsKeyPressed(VK_X) then curRadius := curRadius + 1;
		if WasKeyTyped(VK_T) then filled := filled = false;
		
		if curRadius < 0 then curRadius := 0;
		if curRadius > 50 then curRadius := 50;
		
		DrawCircle(smallScreen, ColourGreen, filled, tempPoint, curRadius);
		DrawCircle(smallScreen, ColourGreen, filled, Round(tempPoint.x + curRadius * 2), Round(tempPoint.y + curRadius * 2), curRadius);
		DrawCircle(smallScreen, ColourGreen, Round(tempPoint.x), Round(tempPoint.y + curRadius * 2), curRadius);
		tempPoint2.x := tempPoint.x + curRadius * 2;
		tempPoint2.y := tempPoint.y;
		DrawCircle(smallScreen, ColourGreen, tempPoint2, curRadius);
		DrawBitmap(smallScreen, 0, 0);
		
		DrawCircle(ColourWhite, filled, curRadius, curRadius, curRadius);
		tempPoint3.x := curRadius * 3;
		tempPoint3.y := curRadius * 3;
		DrawCircle(ColourWhite, filled, tempPoint3, curRadius);
		DrawCircle(ColourWhite, curRadius, curRadius * 3, curRadius);
		tempPoint4.x := curRadius * 3;
		tempPoint4.y := curRadius;
		DrawCircle(ColourWhite, tempPoint4, curRadius);
		
		DrawCircleOnScreen(ColourYellow, filled, Round(curRadius + drawIn.x), Round(RectangleBottom(drawIn) - curRadius) - 1, curRadius);
		tempPoint5.x := curRadius * 3 + drawIn.x;
		tempPoint5.y := RectangleBottom(drawIn) - curRadius * 3 - 1;
		DrawCircleOnScreen(ColourYellow, filled, tempPoint5, curRadius);
		DrawCircleOnScreen(ColourYellow, Round(curRadius + drawIn.x), Round(RectangleBottom(drawIn) - curRadius * 3) - 1, curRadius);
		tempPoint6.x := curRadius * 3 + drawIn.x;
		tempPoint6.y := RectangleBottom(drawIn) - curRadius - 1;
		DrawCircleOnScreen(ColourYellow, tempPoint6, curRadius);
	end;
	
	procedure TestDrawRectangle(const drawIn: Rectangle);
	begin
		ClearSurface(smallScreen, ColourTransparent);
		
		if IsKeyPressed(VK_RIGHT) then tempRect.x := tempRect.x + 2;
		if IsKeyPressed(VK_LEFT) then tempRect.x := tempRect.x - 2;
		if IsKeyPressed(VK_UP) then tempRect.y := tempRect.y - 2;
		if IsKeyPressed(VK_DOWN) then tempRect.y := tempRect.y + 2;
		if IsKeyPressed(VK_A) then tempRect.width := tempRect.width - 2;
		if IsKeyPressed(VK_S) then tempRect.width := tempRect.width + 2;
		if IsKeyPressed(VK_Z) then tempRect.height := tempRect.height - 2;
		if IsKeyPressed(VK_X) then tempRect.height := tempRect.height + 2;
		if WasKeyTyped(VK_T) then filled := filled = false;
		
		if tempRect.width < -100 then tempRect.width := -100;
		if tempRect.height < -100 then tempRect.height := -100;
		if tempRect.width > 100 then tempRect.width := 100;
		if tempRect.height > 100 then tempRect.height := 100;
		
		DrawRectangle(smallScreen, ColourGreen, filled, tempRect);
		DrawRectangle(smallScreen, ColourGreen, filled, Round(tempRect.width + tempRect.x), Round(tempRect.y + tempRect.height), tempRect.width, tempRect.height);
		DrawRectangle(smallScreen, ColourGreen, Round(tempRect.x), Round(tempRect.y + tempRect.height), tempRect.width, tempRect.height);
		tempRect4.x := tempRect.x + tempRect.width;
		tempRect4.y := tempRect.y;
		tempRect4.width := tempRect.width;
		tempRect4.height := tempRect.height;
		DrawRectangle(smallScreen, ColourGreen, tempRect4);
		DrawBitmap(smallScreen, 0, 0);
		
		DrawRectangle(ColourWhite, filled, 0, 0, tempRect.width, tempRect.height);
		tempRect2.x := tempRect.width;
		tempRect2.y := tempRect.height;
		tempRect2.width := tempRect.width;
		tempRect2.height := tempRect.height;
		DrawRectangle(ColourWhite, filled, tempRect2);
		DrawRectangle(ColourWhite, 0, tempRect.height, tempRect.width, tempRect.height);
		tempRect3.x := tempRect.width;
		tempRect3.width := tempRect.width;
		tempRect3.height := tempRect.height;
		DrawRectangle(ColourWhite, tempRect3);
		
		DrawRectangleOnScreen(ColourYellow, filled, Round(drawIn.x), Round(RectangleBottom(drawIn) - tempRect.height) - 1, tempRect.width, tempRect.height);
		tempRect5.x := tempRect.width + drawIn.x;
		tempRect5.y := RectangleBottom(drawIn) - tempRect.height * 2 - 1;
		tempRect5.width := tempRect.width;
		tempRect5.height := tempRect.height;
		DrawRectangleOnScreen(ColourYellow, filled, tempRect5);
		DrawRectangleOnScreen(ColourYellow, Round(drawIn.x), Round(RectangleBottom(drawIn) - tempRect.height * 2) - 1, tempRect.width, tempRect.height);
		tempRect6.x := tempRect.width + drawIn.x;
		tempRect6.y := RectangleBottom(drawIn) - tempRect.height - 1;
		tempRect6.width := tempRect.width;
		tempRect6.height := tempRect.height;
		DrawRectangleOnScreen(ColourYellow, tempRect6);
	end;
	
	function GetGraphicsTests(): TestSuite;
	var
		i: Integer;
	begin
		result.Title := 'Graphics Tests';
		SetLength(result.Tests, 8);
		
		for i := 0 to High(result.Tests) do
		begin
			InitTest(result.Tests[i]);
		end;
		
		with result.Tests[0] do
		begin
			MethodBeingTested := 'DrawRectangle, DrawRectangleOnScreen, ClearSurface, DrawBitmap';
			Instructions := 	'Use the arrow keys to move' + EOL + 'the green box.' + EOL + 'A : Shrink the box width' + EOL + 'S : Expand the box width'
								+ EOL + 'Z : Shrink the box height' + EOL + 'X : Expand the box height' + EOL + 'T : Toggle fill' + EOL
								+ 'White : Normal' + EOL + 'Green : On destination bitmap' + EOL + 'Yellow: On screen';
			tempRect := CreateRectangle(0, 0, 50, 50);
			tempRect4 := CreateRectangle(0, 0, 50, 50);
			smallScreen := CreateBitmap(418, 418);
			filled := false;
			ToRun := @TestDrawRectangle;
		end;
		
		with result.Tests[1] do
		begin
			MethodBeingTested := 'DrawCircle, DrawCircleOnScreen';
			Instructions :=  	'Use the arrow keys to move' + EOL + 'the green circles.' + EOL + 'Z : Shrink the circle radius' + EOL + 'X : Expand the circle radius'
								+ EOL + 'T : Toggle fill' + EOL + 'White : Normal' + EOL + 'Green : On destination bitmap' + EOL + 'Yellow: On screen';
			tempPoint := CreatePoint(15, 15);
			curRadius := 15;
			ToRun := @TestDrawCircle;
		end;
		
		with result.Tests[2] do
		begin
			MethodBeingTested := 'DrawEllipse, DrawEllipseOnScreen';
			Instructions := 'Use the arrow keys to move' + EOL + 'the green ellipses.' + EOL + 'A : Shrink the ellipse width' + EOL + 'S : Expand the ellipse width'
							+ EOL + 'Z : Shrink the ellipse height' + EOL + 'X : Expand the ellipse height' + EOL + 'T : Toggle fill' + EOL
							+ 'White : Normal' + EOL + 'Green : On destination bitmap' + EOL + 'Yellow: On screen';
			ToRun := @TestDrawEllipse;
		end;
		
		with result.Tests[3] do
		begin
			MethodBeingTested := 'FillRectangle, FillEllipse';
			Instructions := 'Use the arrow keys to move' + EOL + 'the green shapes.' + EOL + 'A : Shrink the shape width' + EOL + 'S : Expand the shape width'
							+ EOL + 'Z : Shrink the shape height' + EOL + 'X : Expand the shape height' + EOL
							+ 'White : Normal' + EOL + 'Green : On destination bitmap' + EOL + 'Yellow: On screen';
			ToRun := @TestFill1;
		end;
		
		with result.Tests[4] do
		begin
			MethodBeingTested := 'FillCircle, FillCircleOnScreen';
			Instructions := 'Use the arrow keys to move' + EOL + 'the green circles.' + EOL + 'A : Shrink the circle width' + EOL + 'S : Expand the circle width'
							+ EOL + 'Z : Shrink the circle height' + EOL + 'X : Expand the circle height' + EOL
							+ 'White : Normal' + EOL + 'Green : On destination bitmap' + EOL + 'Yellow: On screen';
			ToRun := @TestFill2;
		end;
		
		with result.Tests[5] do
		begin
			SetLength(tempLines, 7);
			MethodBeingTested := 'All routines related to line, RotateMatrix, Multiply';
			Instructions := 'Use the arrow keys to rotate' + EOL + 'the white lines';
			tempLines[1] := LineFromVector(225, 225, GetVectorFromAngle(0, 125));
			tempLines[2] := LineFromVector(225, 225, GetVectorFromAngle(60, 125));
			tempLines[3] := LineFromVector(225, 225, GetVectorFromAngle(120, 125));
			tempLines[4] := LineFromVector(225, 225, GetVectorFromAngle(180, 125));
			tempLines[5] := LineFromVector(248, 354, GetVectorFromAngle(240, 125));
			tempLines[6] := LineFromVector(248, 354, GetVectorFromAngle(300, 125));
			ToRun := @TestLines;
		end;
		
		with result.Tests[6] do
		begin
			MethodBeingTested := 'DrawPixel';
			Instructions := 'Click and drag to draw with' + EOL + 'your mouse cursor.' + EOL + EOL + '1: Increment Blue' + EOL + '2: Increment Green' + EOL + '3: Inrrement Blue';
			curCol := ColourWhite;
			result.Tests[6].ClearScreen := false;
			ToRun := @TestPixels;
		end;
		
		with result.Tests[7] do
		begin
			MethodBeingTested := 'Bitmap routines';
			Instructions := 'Use the arrow keys to move' + EOL + 'the ship.';
			SetLength(frameB, 6);
			frameB[0] := GameImage('Frame1');
			frameB[1] := GameImage('Frame2');
			frameB[2] := GameImage('Frame3');
			frameB[3] := GameImage('Frame4');
			frameB[4] := GameImage('Frame5');
			frameB[5] := GameImage('Frame6');
			ship := GameImage('enShip');
			framePos := CreatePoint(0, 0);
			ToRun := @TestBitmap;
		end;
	end;
	
	procedure AddGraphicsSuite(var suites: TestSuites);
	begin
		suites[High(suites)] := GetGraphicsTests();
	end;

end.