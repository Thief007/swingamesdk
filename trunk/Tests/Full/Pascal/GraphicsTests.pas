unit GraphicsTests;

interface
	uses TestFramework;

	procedure AddGraphicsSuite(var suites: TestSuites); 

implementation
	uses GameResources, SGSDK_Core, SGSDK_Input, SGSDK_Graphics, SGSDK_KeyCodes, SGSDK_Shapes, SGSDK_Font;
	
	const
		BOXDIM = 50;
	
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
		DrawBitmap(smallScreen, drawIn.x, drawIn.y);

		DrawEllipse(ColourWhite, filled, drawIn.x, drawIn.y, tempRect.width, tempRect.height);
		tempRect2.x := tempRect.width + drawIn.x;
		tempRect2.y := drawIn.y + tempRect.height;
		tempRect2.width := tempRect.width;
		tempRect2.height := tempRect.height;
		DrawEllipse(ColourWhite, filled, tempRect2);
		DrawEllipse(ColourWhite, drawIn.x, drawIn.y + tempRect.height, tempRect.width, tempRect.height);
		tempRect3.x := tempRect.width + drawIn.x;
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
		DrawBitmap(smallScreen, drawIn.x, drawIn.y);
		
		DrawCircle(ColourWhite, filled, curRadius + drawIn.x, drawIn.y + curRadius, curRadius);
		tempPoint3.x := curRadius * 3 + drawIn.x;
		tempPoint3.y := drawIn.y + curRadius * 3;
		DrawCircle(ColourWhite, filled, tempPoint3, curRadius);
		DrawCircle(ColourWhite, curRadius + drawIn.x, drawIn.y + curRadius * 3, curRadius);
		tempPoint4.x := curRadius * 3 + drawIn.x;
		tempPoint4.y := drawIn.y + curRadius;
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
		
		if tempRect.width < 0 then tempRect.width := 0;
		if tempRect.height < 0 then tempRect.height := 0;
		if tempRect.width > 100 then tempRect.width := 100;
		if tempRect.height > 100 then tempRect.height := 100;
		
		DrawRectangle(smallScreen, ColourGreen, filled, tempRect);
		DrawRectangle(smallScreen, ColourGreen, filled, Round(tempRect.x + tempRect.width), Round(tempRect.y + tempRect.height), tempRect.width, tempRect.height);
		DrawRectangle(smallScreen, ColourGreen, Round(tempRect.x), Round(tempRect.y + tempRect.height), tempRect.width, tempRect.height);
		tempRect4.x := tempRect.x + tempRect.width;
		tempRect4.y := tempRect.y;
		tempRect4.width := tempRect.width;
		tempRect4.height := tempRect.height;
		DrawRectangle(smallScreen, ColourGreen, tempRect4);
		DrawBitmap(smallScreen, drawIn.x, drawIn.y);
		
		DrawRectangle(ColourWhite, filled, drawIn.x, drawIn.y, tempRect.width, tempRect.height);
		tempRect2.x := tempRect.width + drawIn.x;
		tempRect2.y := drawIn.y + tempRect.height;
		tempRect2.width := tempRect.width;
		tempRect2.height := tempRect.height;
		DrawRectangle(ColourWhite, filled, tempRect2);
		DrawRectangle(ColourWhite, drawIn.x, drawIn.y + tempRect.height, tempRect.width, tempRect.height);
		tempRect3.x := tempRect.width + drawIn.x;
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
		SetLength(result.Tests, 3);
		
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
			tempRect3 := CreateRectangle(0, 129, 50, 50);
			tempRect4 := CreateRectangle(0, 0, 50, 50);
			smallScreen := CreateBitmap(500, 510);
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
	end;
	
	procedure AddGraphicsSuite(var suites: TestSuites);
	begin
		suites[High(suites)] := GetGraphicsTests();
	end;

end.