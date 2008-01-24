unit GraphicsTests;

interface
	uses TestFramework;

	procedure AddGraphicsSuite(var suites: TestSuites); 

implementation
	uses GameResources, SGSDK_Core, SGSDK_Input, SGSDK_Graphics, SGSDK_KeyCodes, SGSDK_Shapes;
	
	const
		BOXDIM = 50;
	
	var
		tempRect, tempRect2, tempRect3, tempRect4,
		tempRect5, tempRect6: Rectangle;
		smallScreen: Bitmap;
		filled: Boolean;
	
	procedure TestDrawRectangle(const drawIn: Rectangle);
	begin
		ClearSurface(smallScreen);
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
		DrawBitmap(smallScreen, 0, 70);
		
		DrawRectangle(ColourWhite, filled, 0, 70, tempRect.width, tempRect.height);
		tempRect2.x := tempRect.width;
		tempRect2.y := 70 + tempRect.height;
		tempRect2.width := tempRect.width;
		tempRect2.height := tempRect.height;
		DrawRectangle(ColourWhite, filled, tempRect2);
		DrawRectangle(ColourWhite, 0, 70 + tempRect.height, tempRect.width, tempRect.height);
		tempRect3.x := tempRect.width;
		tempRect3.width := tempRect.width;
		tempRect3.height := tempRect.height;
		DrawRectangle(ColourWhite, tempRect3);
		
		DrawRectangleOnScreen(ColourYellow, filled, 0, 580 - tempRect.height, tempRect.width, tempRect.height);
		tempRect5.x := tempRect.width;
		tempRect5.y := 580 - tempRect.height * 2;
		tempRect5.width := tempRect.width;
		tempRect5.height := tempRect.height;
		DrawRectangleOnScreen(ColourYellow, filled, tempRect5);
		DrawRectangleOnScreen(ColourYellow, 0, 580 - tempRect.height * 2, tempRect.width, tempRect.height);
		tempRect6.x := tempRect.width;
		tempRect6.y := 580 - tempRect.height;
		tempRect6.width := tempRect.width;
		tempRect6.height := tempRect.height;
		DrawRectangleOnScreen(ColourYellow, tempRect6);
	end;
	
	function GetGraphicsTests(): TestSuite;
	var
		i: Integer;
	begin
		result.Title := 'Graphics Tests';
		SetLength(result.Tests, 1);
		
		for i := 0 to High(result.Tests) do
		begin
			InitTest(result.Tests[i]);
		end;
		
		with result.Tests[0] do
		begin
			MethodBeingTested := 'DrawRectangle, DrawRectangleOnScreen, ClearSurface, DrawBitmap';
			Instructions :=  'Use the arrow keys to move' + EOL + 'the green boxe.' + EOL + 'A : Shrink the box width' + EOL + 'S : Expand the box width'
							 + EOL + 'Z : Shrink the box height' + EOL + 'X : Expand the box height' + EOL + 'T : Toggle filled';
			tempRect := CreateRectangle(0, 0, 50, 50);
			tempRect2 := CreateRectangle(0, 0, 50, 50);
			tempRect3 := CreateRectangle(0, 70, 50, 50);
			tempRect4 := CreateRectangle(0, 0, 50, 50);
			tempRect5 := CreateRectangle(0, 0, 50, 50);
			tempRect6 := CreateRectangle(0, 580, 50, 50);
			smallScreen := CreateBitmap(500, 510);
			filled := false;
			ToRun := @TestDrawRectangle;
		end;
	end;
	
	procedure AddGraphicsSuite(var suites: TestSuites);
	begin
		suites[High(suites)] := GetGraphicsTests();
	end;

end.