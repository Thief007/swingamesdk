unit CameraTests;

interface
	uses TestFramework;

	procedure AddCameraSuite(var suites: TestSuites); 

implementation
	uses GameResources, sgCamera, sgImages, sgInput, sgGraphics, sgTypes,sgGeometry,sgText,sgPhysics, SysUtils, sgSprites, sgAnimations;
	
	var
		bgImage: Bitmap;
		shipSprite, ship2: Sprite;
		follow: Boolean;
	
	procedure TestCamera(const drawIn: Rectangle);
	var
	  mvmt: Vector;
	begin
	  mvmt := VectorTo(0,0);
	  
		if KeyDown(VK_RIGHT) then mvmt.x := mvmt.x + 4;
		if KeyDown(VK_DOWN) then mvmt.y := mvmt.y + 4;
		if KeyDown(VK_UP) then mvmt.y := mvmt.y - 4;
		if KeyDown(VK_LEFT) then mvmt.x := mvmt.x - 4;
		if KeyDown(VK_U) then follow := false;
		if KeyDown(VK_I) then follow := true;
		if KeyDown(VK_A) then MoveCameraBy(VectorTo(-20, 0));
		if KeyDown(VK_D) then MoveCameraBy(VectorTo(20, 0));
		if KeyDown(VK_W) then MoveCameraBy(0, -20);
		if KeyDown(VK_S) then MoveCameraBy(0, 20);
		  
	  MoveSprite(shipSprite, mvmt);	
		  
		if KeyDown(VK_COMMA) then SpriteSetRotation(shipSprite, SpriteRotation(shipSprite) + 5);
		if KeyDown(VK_PERIOD) then SpriteSetRotation(shipSprite, SpriteRotation(shipSprite) - 5);
		if follow then CenterCameraOn(shipSprite, Round(400 - drawIn.x - drawIn.width / 2), -50);
		
		DrawBitmap(bgImage, 0, 0);
		DrawText('0, 0', ColorRed, FontNamed('Courier'), 0, 0);
		DrawTextOnScreen('Ship Position: ' + FloatToStr(ToWorldX(ToScreenX(SpriteX(shipSprite)))) + ', ' + FloatToStr(ToWorldY(ToScreenY(SpriteY(shipSprite)))), ColorRed, FontNamed('Courier'), Round(drawIn.x), Round(drawIn.y));
		DrawTextOnScreen('Ship Position On Screen: ' + FloatToStr(ToScreenX(SpriteX(shipSprite))) + ', ' + FloatToStr(ToScreenY(SpriteY(shipSprite))), ColorRed, FontNamed('Courier'), Round(drawIn.x), Round(drawIn.y) + 15);
		DrawTextOnScreen('Camera Offset: ' + FloatToStr(CameraX()) + ', ' + FloatToStr(CameraY()), ColorRed, FontNamed('Courier'), Round(drawIn.x), Round(drawIn.y) + 30);
		
		if SpriteCollision(shipSprite, ship2) then
		  DrawTextOnScreen('Collision', ColorWhite, FontNamed('Courier'), Round(drawIn.x), Round(drawIn.y) + 45);
		
		DrawSprite(ship2);
		DrawSprite(shipSprite);
		DrawBitmapOnScreen(shipSprite^.collisionBitmap, 100, 200);
		if assigned(shipSprite^.cacheImage) then
		begin
		  DrawBitmapOnScreen(shipSprite^.cacheImage, 100, 150);
		end;
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
			Instructions := 'Use the arrow keys to move' + LineEnding + 'the ship.' + LineEnding + 'I: Follow the ship' + LineEnding + 'U: Unfollow' + LineEnding + 'W: Move up the visual area' + LineEnding + 'S: Move down the visual area'
							 + LineEnding + 'A: Move the visual area to' + LineEnding + 'the left' + LineEnding + 'D: Move the visual area to' + LineEnding + 'the right';
			bgImage := BitmapNamed('Sea');
			shipSprite := CreateSprite(BitmapNamed('Ship'), LoadAnimationScript('ship_anim.txt'));
			SpriteStartAnimation(shipSprite, 0);
			SpriteSetX(shipSprite, 100); 
			SpriteSetY(shipSprite, 100);
			follow := false;
			
			ship2 := CreateSprite(BitmapNamed('Ship'), LoadAnimationScript('ship_anim.txt'));
			SpriteStartAnimation(shipSprite, 0);
			SpriteSetX(shipSprite, 50); 
			SpriteSetY(shipSprite, 50);
			
			ToRun := @TestCamera;
		end;
	end;
	
	procedure AddCameraSuite(var suites: TestSuites);
	begin
		suites[High(suites)] := GetCameraTests();
	end;

end.