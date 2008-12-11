unit CameraTests;

interface
	uses TestFramework;

	procedure AddCameraSuite(var suites: TestSuites); 

implementation
	uses GameResources, SGSDK_Core, SGSDK_Camera, SGSDK_Input, SGSDK_Graphics, SGSDK_KeyCodes, SGSDK_Shapes, SGSDK_Font, SGSDK_Physics, SysUtils;
	
	var
		bgImage: Bitmap;
		shipSprite: Sprite;
		follow: Boolean;
	
	procedure TestCamera(const drawIn: Rectangle);
	var
	  mvmt: Vector;
	begin
	  mvmt := CreateVector(0,0);
	  
		if IsKeyPressed(VK_RIGHT) then mvmt.x := mvmt.x + 4;
		if IsKeyPressed(VK_DOWN) then mvmt.y := mvmt.y + 4;
		if IsKeyPressed(VK_UP) then mvmt.y := mvmt.y - 4;
		if IsKeyPressed(VK_LEFT) then mvmt.x := mvmt.x - 4;
		if IsKeyPressed(VK_U) then follow := false;
		if IsKeyPressed(VK_I) then follow := true;
		if IsKeyPressed(VK_A) then MoveVisualArea(CreateVector(-20, 0));
		if IsKeyPressed(VK_D) then MoveVisualArea(CreateVector(20, 0));
		if IsKeyPressed(VK_W) then MoveVisualArea(0, -20);
		if IsKeyPressed(VK_S) then MoveVisualArea(0, 20);
		  
	  MoveSprite(shipSprite, mvmt);	
		  
		if IsKeyPressed(VK_COMMA) then shipSprite.rotation += 5;
		if IsKeyPressed(VK_PERIOD) then shipSprite.rotation -= 5;
		if follow then FollowSprite(shipSprite, Round(400 - drawIn.x - drawIn.width / 2), -50);
		
		DrawBitmap(bgImage, 0, 0);
		DrawText('0, 0', ColourRed, GameFont('Courier'), 0, 0);
		DrawTextOnScreen('Ship Position: ' + FloatToStr(GameX(ScreenX(shipSprite.x))) + ', ' + FloatToStr(GameY(ScreenY(shipSprite.y))), ColourRed, GameFont('Courier'), Round(drawIn.x), Round(drawIn.y));
		DrawTextOnScreen('Ship Position On Screen: ' + FloatToStr(ScreenX(shipSprite.x)) + ', ' + FloatToStr(ScreenY(shipSprite.y)), ColourRed, GameFont('Courier'), Round(drawIn.x), Round(drawIn.y) + 15);
		DrawTextOnScreen('Camera Offset: ' + IntToStr(XOffset()) + ', ' + IntToStr(YOffset()), ColourRed, GameFont('Courier'), Round(drawIn.x), Round(drawIn.y) + 30);
		
		
		DrawSprite(shipSprite);
		UpdateSprite(shipSprite);
	end;

	function GetCameraTests(): TestSuite;
	var
		i: Integer;
	begin
		result.Title := 'Camera Test';
		SetLength(result.Tests, 1);
		
		for i := 0 to High(result.Tests) do
		begin
			InitTest(result.Tests[i]);
		end;
		
		with result.Tests[0] do
		begin
			MethodBeingTested := 'All camera routines';
			Instructions := 'Use the arrow keys to move' + EOL + 'the ship.' + EOL + 'I: Follow the ship' + EOL + 'U: Unfollow' + EOL + 'W: Move up the visual area' + EOL + 'S: Move down the visual area'
							 + EOL + 'A: Move the visual area to' + EOL + 'the left' + EOL + 'D: Move the visual area to' + EOL + 'the right';
			bgImage := GameImage('Sea');
			shipSprite := CreateSprite(GameImage('Ship'), 3, 2, 40, 43);
			shipSprite.x := 100; shipSprite.y := 100;
			follow := false;
			ToRun := @TestCamera;
		end;
	end;
	
	procedure AddCameraSuite(var suites: TestSuites);
	begin
		suites[High(suites)] := GetCameraTests();
	end;

end.