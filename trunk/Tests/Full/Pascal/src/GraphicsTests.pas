unit GraphicsTests;

interface
	uses TestFramework;

	procedure AddGraphicsSuite(var suites: TestSuites); 

implementation
	uses GameResources, sgInput, sgImages, sgGraphics, sgTypes,sgGeometry,sgText,sgPhysics, SysUtils, sgSprites, sgAnimations;
	
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
		curCol: Color;
		//BitmapTest
		frameB: Array of Bitmap;
		ship: Bitmap;
		framePos: Point2D;
		//SpriteTest
		sprites: Array of Sprite;
		explodeAnim: Array of Bitmap;
		explodeAnimFinal: Bitmap;
		animExplode: AnimationScript;
		currentSpr: Integer;
		//AddBitmapToSprite
		numSprite: Sprite;
		curNum: Integer;
		//flag
		TP: Boolean = false;
	
	procedure TestAddBitmap(const drawIn: Rectangle);
	var
		tempBitmap: Bitmap;
	begin
		if KeyTyped(VK_SPACE) then
		begin
			curNum := curNum + 1;
			tempBitmap := CreateBitmap(121, 120);
			DrawCircle(tempBitmap, ColorWhite, true, 60, 60, 60);
			DrawTextLines(tempBitmap, IntToStr(curNum), ColorBlack, ColorTransparent, FontNamed('ArialLarge'), AlignCenter, BitmapRectangle(tempBitmap));
			SpriteAddLayer(numSprite, tempBitmap, 'Layer ' + IntToStr(curNum));
		end;
		DrawSprite(numSprite);
		UpdateSpriteAnimation(numSprite);
	end;
	
	procedure TestSprite(const drawIn: Rectangle);
		procedure ChangeSprite(changeTo: Integer);
		var
			vec: Vector;
			tempX, tempY: Single;
            // curAction: SpriteEndingAction;
		begin
            // curAction := sprites[currentSpr].endingAction;
			vec := SpriteVelocity(sprites[currentSpr]);
			//ReplayAnimation(sprites[currentSpr]);
			SpriteStartAnimation(sprites[currentSpr], 0);
			tempX := SpriteX(sprites[currentSpr]);
			tempY := SpriteY(sprites[currentSpr]);
			currentSpr := changeTo;
			SpriteSetVelocity(sprites[currentSpr], vec);
			SpriteSetX(sprites[currentSpr], tempX);
			SpriteSetY(sprites[currentSpr], tempY);
            // sprites[currentSpr].endingAction := curAction;
		end;
		
		procedure ResetPos();
		begin
			SpriteSetVelocity(sprites[currentSpr], VectorTo(0, 0));
			SpriteSetPosition(sprites[currentSpr], PointAt(0, 0));
		end;
		
        // procedure ChangeAnim(act: SpriteEndingAction);
        // begin
        //  sprites[currentSpr].endingAction := act;
        //  ReplayAnimation(sprites[currentSpr]);
        // end;
	begin
		if KeyDown(VK_1) then ChangeSprite(0);
		if KeyDown(VK_2) then ChangeSprite(1);
		if KeyDown(VK_3) then ChangeSprite(2);
		if KeyDown(VK_4) then ChangeSprite(3);
		if KeyDown(VK_5) then ChangeSprite(4);
		if KeyDown(VK_6) then ChangeSprite(5);
		if KeyDown(VK_7) then ChangeSprite(6);
		if KeyDown(VK_LEFT) then 
		    SpriteSetVelocity(sprites[currentSpr], AddVectors(SpriteVelocity(sprites[currentSpr]), VectorTo(-1, 0)));
		if KeyDown(VK_RIGHT) then 
		    SpriteSetVelocity(sprites[currentSpr], AddVectors(SpriteVElocity(sprites[currentSpr]), VectorTo(1, 0)));
		if KeyDown(VK_UP) then 
		    SpriteAddToVelocity(sprites[currentSpr], VectorTo(0, -1));
		if KeyDown(VK_DOWN) then 
		    SpriteAddToVelocity(sprites[currentSpr], VectorTo(0, 1));
		if KeyDown(VK_M) then ResetPos();
    // if KeyDown(VK_Q) then ChangeAnim(Loop);
    // if KeyDown(VK_W) then ChangeAnim(ReverseLoop);
    // if KeyDown(VK_E) then ChangeAnim(ReverseOnce);
    // if KeyDown(VK_R) then ChangeAnim(Stop);
		
		SpriteSetVelocity(sprites[currentSpr], VectorMultiply(SpriteVelocity(sprites[currentSpr]), 0.95));
		SpriteSetVElocity(sprites[currentSpr], LimitVector(SpriteVelocity(sprites[currentSpr]), 5));
		
		DrawSprite(sprites[currentSpr]);
		UpdateSprite(sprites[currentSpr]);
		
		if SpriteOffscreen(sprites[currentSpr]) then
		begin
			DrawText('The sprite is not on the screen', ColorWhite, FontNamed('Courier'), 0, 0);
		end;
	end;
	
	procedure TestBitmap(const drawIn: Rectangle);
	begin
		if KeyDown(VK_RIGHT) then framePos.x := framePos.x - 2;
		if KeyDown(VK_LEFT) then framePos.x := framePos.x + 2;
		if KeyDown(VK_UP) then framePos.y := framePos.y + 2;
		if KeyDown(VK_DOWN) then framePos.y := framePos.y - 2;
		
		MakeOpaque(frameB[0]);
		MakeOpaque(frameB[2]);
		MakeOpaque(ship);
		//ClearSurface(smallScreen, ColorBlack);
		
		DrawBitmap(smallScreen, frameB[0], PointAt(0, 0));
		DrawBitmap(smallScreen, frameB[2], 278, 0);
		
		DrawBitmapPart(smallScreen, ship, RectangleFrom(framePos.x + 29, framePos.y + 50, 100, 100), PointAt(29, 50));
		DrawBitmapPart(smallScreen, ship, RectangleFrom(framePos.x + 149, framePos.y + 50, 120, 100), 149, 50);
		DrawBitmapPart(smallScreen, ship, Round(framePos.x + 289), Round(framePos.y + 50), 100, 100, 289, 50);
		
		DrawBitmap(smallScreen, 0, 0);
		
		MakeTransparent(ship);
		
		DrawBitmap(frameB[1], PointAt(139, 0));
		DrawBitmap(frameB[3], 0, 209);
		DrawBitmapOnScreen(frameB[4], PointAt(139 + drawIn.x, 209 + drawIn.y));
		DrawBitmapOnScreen(frameB[5], Round(278 + drawIn.x), Round(209 + drawIn.y));
		
		DrawBitmapPart(ship, RectangleFrom(framePos.x + 29, framePos.y + 159, 100, 100), 29, 159);
		DrawBitmapPart(ship, RectangleFrom(framePos.x + 149, framePos.y + 159, 120, 100), PointAt(149, 159));
		DrawBitmapPart(ship, Round(framePos.x + 289), Round(framePos.y + 159), 100, 100, 289, 159);
		
		DrawBitmapPartOnScreen(ship, RectangleFrom(framePos.x + 29, framePos.y + 268, 100, 100), Round(29 + drawIn.x), Round(268 + drawIn.y));
		DrawBitmapPartOnScreen(ship, RectangleFrom(framePos.x + 149, framePos.y + 268, 120, 100), PointAt(149 + drawIn.x, 268 + drawIn.y));
		DrawBitmapPartOnScreen(ship, Round(framePos.x + 289), Round(framePos.y + 268), 100, 100, Round(289 + drawIn.x), Round(268 + drawIn.y));
	end;
	
	procedure TestPixels(const drawIn: Rectangle);
		procedure ClearStuff();
		begin
			ClearSurface(smallScreen, ColorTransparent);
			ClearScreen(ColorBlack);
			DrawBitmapOnScreen(BitmapNamed('BGA'), Round(drawIn.x), Round(drawIn.y));
		end;
	begin
		if not TP then
		begin
			ClearStuff();
			TP := true;
		end;
		
		if KeyDown(VK_Z) then ClearStuff();
		
		if KeyDown(VK_1) then curCol := RGBColor(RedOf(curCol) + 5, GreenOf(curCol), BlueOf(curCol));
		if KeyDown(VK_2) then curCol := RGBColor(RedOf(curCol), GreenOf(curCol) + 5, BlueOf(curCol));
		if KeyDown(VK_3) then curCol := RGBColor(RedOf(curCol), GreenOf(curCol), BlueOf(curCol) + 5);
		
		FillRectangle(ColorBlack, 0, 0, 100, 45);
		DrawText('Red   : ' + IntToStr(RedOf(curCol)), ColorRed, FontNamed('Courier'), 0, 0);
		DrawText('Green : ' + IntToStr(GreenOf(curCol)), ColorGreen, FontNamed('Courier'), 0, 15);
		DrawText('Blue  : ' + IntToStr(BlueOf(curCol)), ColorBlue, FontNamed('Courier'), 0, 30);
		
		If MouseDown(LeftButton) then
		begin
			DrawPixel(smallScreen, curCol, PointAt(MousePosition().x - drawIn.x - 10, MousePosition().y - drawIn.y - 10));
			DrawPixel(smallScreen, curCol, Round(MousePosition().x - drawIn.x + 10), Round(MousePosition().y - drawIn.y) - 10);
		
			DrawPixel(curCol, PointAt(MousePosition().x - 10 - drawIn.x, MousePosition().y - drawIn.y));
			DrawPixel(curCol, Round(MousePosition().x - drawIn.x + 10), Round(MousePosition().y - drawIn.y));
		
			DrawPixelOnScreen(curCol, PointAt(MousePosition().x - 10, MousePosition().y + 10));
			DrawPixelOnScreen(curCol, Round(MousePosition().x) + 10, Round(MousePosition().y) + 10);
		end;
		
		DrawBitmap(smallScreen, 0, 0);
	end;
	
	procedure TestLines(const drawIn: Rectangle);
	var
		i : Integer;
	begin
		ClearSurface(smallScreen, ColorTransparent);
		
		if KeyDown(VK_RIGHT) then angle := +2;
		if KeyDown(VK_LEFT) then angle := -2;
		
		if angle <> 0 then
		begin
			for i := 1 to 6 do
			begin
				tempLines[i] := LineFromVector(tempLines[i].startPoint.x, tempLines[i].startPoint.y, MatrixMultiply(RotationMatrix(angle), LineAsVector(tempLines[i])));
			end;
		end;
		
		DrawHorizontalLine(smallScreen, ColorGreen, 100, 100, 350);
		DrawVerticalLine(smallScreen, ColorGreen, 100, 100, 350);
		DrawBitmap(smallScreen, 0, 0);
		ClearSurface(smallScreen, ColorTransparent);
		DrawLine(smallScreen, ColorWhite, tempLines[1]);
		DrawLine(smallScreen, ColorWhite, Round(tempLines[3].startPoint.x), Round(tempLines[3].startPoint.y), Round(tempLines[3].endPoint.x), Round(tempLines[3].endPoint.y));
		
		DrawHorizontalLine(ColorGreen, 225, 100, 350);
		DrawVerticalLine(ColorGreen, 225, 100, 350);
		DrawHorizontalLineOnScreen(ColorGreen, Round(350 + drawIn.y), Round(100 + drawIn.x), Round(350 + drawIn.x));
		DrawVerticalLineOnScreen(ColorGreen, Round(350 + drawIn.x), Round(100 + drawIn.y), Round(350 + drawIn.y));
		
		DrawLine(ColorWhite, tempLines[2]);
		DrawLine(ColorWhite, tempLines[4].startPoint.x, tempLines[4].startPoint.y, tempLines[4].endPoint.x, tempLines[4].endPoint.y);
		
		DrawBitmap(smallScreen, 0, 0);
		
		DrawLineOnScreen(ColorWhite, tempLines[5]);
		DrawLineOnScreen(ColorWhite, Round(tempLines[6].startPoint.x), Round(tempLines[6].startPoint.y), Round(tempLines[6].endPoint.x), Round(tempLines[6].endPoint.y));
		
		for i := 1 to 4 do
		begin
			DrawLine(ColorWhite, LineFromVector(tempLines[i].endPoint, VectorMultiply(LineNormal(tempLines[i]), 10)));
		end;
		
		for i := 5 to 6 do
		begin
			DrawLineOnScreen(ColorWhite, LineFromVector(tempLines[i].endPoint, VectorMultiply(VectorNormal(LineAsVector(tempLines[i])), 10)));
		end;
		
		angle := 0;
	end;
	
	procedure TestFill2(const drawIn: Rectangle);
	begin
		ClearSurface(smallScreen, ColorTransparent);
		
		if KeyDown(VK_RIGHT) then tempPoint.x := tempPoint.x + 2;
		if KeyDown(VK_LEFT) then tempPoint.x := tempPoint.x - 2;
		if KeyDown(VK_UP) then tempPoint.y := tempPoint.y - 2;
		if KeyDown(VK_DOWN) then tempPoint.y := tempPoint.y + 2;
		if KeyDown(VK_Z) then curRadius := curRadius - 1;
		if KeyDown(VK_X) then curRadius := curRadius + 1;
		
		if curRadius < 0 then curRadius := 0;
		if curRadius > 50 then curRadius := 50;
		
		FillCircle(smallScreen, ColorGreen, tempPoint, curRadius);
		FillCircle(smallScreen, ColorGreen, Round(tempPoint.x + curRadius * 2), Round(tempPoint.y + curRadius * 2), curRadius);
		DrawBitmap(smallScreen, 0, 0);
		
		FillCircle(ColorWhite, curRadius, curRadius, curRadius);
		tempPoint3.x := curRadius * 3;
		tempPoint3.y := curRadius * 3;
		FillCircle(ColorWhite, tempPoint3, curRadius);
		
		FillCircleOnScreen(ColorYellow, Round(curRadius + drawIn.x), Round(RectangleBottom(drawIn) - curRadius) - 1, curRadius);
		tempPoint5.x := curRadius * 3 + drawIn.x;
		tempPoint5.y := RectangleBottom(drawIn) - curRadius * 3 - 1;
		FillCircleOnScreen(ColorYellow, tempPoint5, curRadius);
	end;
	
	procedure TestFill1(const drawIn: Rectangle);
	begin
		ClearSurface(smallScreen, ColorTransparent);
		
		if KeyDown(VK_RIGHT) then tempRect.x := tempRect.x + 2;
		if KeyDown(VK_LEFT) then tempRect.x := tempRect.x - 2;
		if KeyDown(VK_UP) then tempRect.y := tempRect.y - 2;
		if KeyDown(VK_DOWN) then tempRect.y := tempRect.y + 2;
		if KeyDown(VK_A) then tempRect.width := tempRect.width - 2;
		if KeyDown(VK_S) then tempRect.width := tempRect.width + 2;
		if KeyDown(VK_Z) then tempRect.height := tempRect.height - 2;
		if KeyDown(VK_X) then tempRect.height := tempRect.height + 2;
		
		if tempRect.width < 0 then tempRect.width := 0;
		if tempRect.height < 0 then tempRect.height := 0;
		if tempRect.width > 100 then tempRect.width := 100;
		if tempRect.height > 100 then tempRect.height := 100;
		
		FillRectangle(smallScreen, ColorGreen, tempRect);
		FillRectangle(smallScreen, ColorGreen, Round(tempRect.width + tempRect.x), Round(tempRect.y + tempRect.height), tempRect.width, tempRect.height);
		FillEllipse(smallScreen, ColorGreen, Round(tempRect.x), Round(tempRect.y + tempRect.height), tempRect.width, tempRect.height);
		tempRect4.x := tempRect.x + tempRect.width;
		tempRect4.y := tempRect.y;
		tempRect4.width := tempRect.width;
		tempRect4.height := tempRect.height;
		FillEllipse(smallScreen, ColorGreen, tempRect4);
		DrawBitmap(smallScreen, 0, 0);
		
		FillRectangle(ColorWhite, 0, 0, tempRect.width, tempRect.height);
		tempRect2.x := tempRect.width;
		tempRect2.y := tempRect.height;
		tempRect2.width := tempRect.width;
		tempRect2.height := tempRect.height;
		FillRectangle(ColorWhite, tempRect2);
		FillEllipse(ColorWhite, 0, tempRect.height, tempRect.width, tempRect.height);
		tempRect3.x := tempRect.width;
		tempRect3.width := tempRect.width;
		tempRect3.height := tempRect.height;
		FillEllipse(ColorWhite, tempRect3);
		
		FillRectangleOnScreen(ColorYellow, Round(drawIn.x), Round(RectangleBottom(drawIn) - tempRect.height) - 1, tempRect.width, tempRect.height);
		tempRect5.x := tempRect.width + drawIn.x;
		tempRect5.y := RectangleBottom(drawIn) - tempRect.height * 2 - 1;
		tempRect5.width := tempRect.width;
		tempRect5.height := tempRect.height;
		FillRectangleOnScreen(ColorYellow, tempRect5);
		FillEllipseOnScreen(ColorYellow, Round(drawIn.x), Round(RectangleBottom(drawIn) - tempRect.height * 2) - 1, tempRect.width, tempRect.height);
		tempRect6.x := tempRect.width + drawIn.x;
		tempRect6.y := RectangleBottom(drawIn) - tempRect.height - 1;
		tempRect6.width := tempRect.width;
		tempRect6.height := tempRect.height;
		FillEllipseOnScreen(ColorYellow, tempRect6);
	end;
	
	procedure TestDrawEllipse(const drawIn: Rectangle);
	begin
		ClearSurface(smallScreen, ColorTransparent);
		if KeyDown(VK_RIGHT) then tempRect.x := tempRect.x + 2;
		if KeyDown(VK_LEFT) then tempRect.x := tempRect.x - 2;
		if KeyDown(VK_UP) then tempRect.y := tempRect.y - 2;
		if KeyDown(VK_DOWN) then tempRect.y := tempRect.y + 2;
		if KeyDown(VK_A) then tempRect.width := tempRect.width - 2;
		if KeyDown(VK_S) then tempRect.width := tempRect.width + 2;
		if KeyDown(VK_Z) then tempRect.height := tempRect.height - 2;
		if KeyDown(VK_X) then tempRect.height := tempRect.height + 2;
		if KeyTyped(VK_T) then filled := filled = false;

		if tempRect.width < 0 then tempRect.width := 0;
		if tempRect.height < 0 then tempRect.height := 0;
		if tempRect.width > 100 then tempRect.width := 100;
		if tempRect.height > 100 then tempRect.height := 100;

		DrawEllipse(smallScreen, ColorGreen, filled, tempRect);
		DrawEllipse(smallScreen, ColorGreen, filled, Round(tempRect.x + tempRect.width), Round(tempRect.y + tempRect.height), tempRect.width, tempRect.height);
		DrawEllipse(smallScreen, ColorGreen, Round(tempRect.x), Round(tempRect.y + tempRect.height), tempRect.width, tempRect.height);
		tempRect4.x := tempRect.x + tempRect.width;
		tempRect4.y := tempRect.y;
		tempRect4.width := tempRect.width;
		tempRect4.height := tempRect.height;
		DrawEllipse(smallScreen, ColorGreen, tempRect4);
		DrawBitmap(smallScreen, 0, 0);

		DrawEllipse(ColorWhite, filled, 0, 0, tempRect.width, tempRect.height);
		tempRect2.x := tempRect.width;
		tempRect2.y := tempRect.height;
		tempRect2.width := tempRect.width;
		tempRect2.height := tempRect.height;
		DrawEllipse(ColorWhite, filled, tempRect2);
		DrawEllipse(ColorWhite, 0, tempRect.height, tempRect.width, tempRect.height);
		tempRect3.x := tempRect.width;
		tempRect3.width := tempRect.width;
		tempRect3.height := tempRect.height;
		DrawEllipse(ColorWhite, tempRect3);

		DrawEllipseOnScreen(ColorYellow, filled, Round(drawIn.x), Round(RectangleBottom(drawIn) - tempRect.height), tempRect.width, tempRect.height);
		tempRect5.x := tempRect.width + drawIn.x;
		tempRect5.y := RectangleBottom(drawIn) - tempRect.height * 2;
		tempRect5.width := tempRect.width;
		tempRect5.height := tempRect.height;
		DrawEllipseOnScreen(ColorYellow, filled, tempRect5);
		DrawEllipseOnScreen(ColorYellow, Round(drawIn.x), Round(RectangleBottom(drawIn) - tempRect.height * 2), tempRect.width, tempRect.height);
		tempRect6.x := tempRect.width + drawIn.x;
		tempRect6.y := RectangleBottom(drawIn) - tempRect.height;
		tempRect6.width := tempRect.width;
		tempRect6.height := tempRect.height;
		DrawEllipseOnScreen(ColorYellow, tempRect6);
	end;
	
	procedure TestDrawCircle(const drawIn: Rectangle);
	begin
		ClearSurface(smallScreen, ColorTransparent);
		if KeyDown(VK_RIGHT) then tempPoint.x := tempPoint.x + 2;
		if KeyDown(VK_LEFT) then tempPoint.x := tempPoint.x - 2;
		if KeyDown(VK_UP) then tempPoint.y := tempPoint.y - 2;
		if KeyDown(VK_DOWN) then tempPoint.y := tempPoint.y + 2;
		if KeyDown(VK_Z) then curRadius := curRadius - 1;
		if KeyDown(VK_X) then curRadius := curRadius + 1;
		if KeyTyped(VK_T) then filled := filled = false;
		
		if curRadius < 0 then curRadius := 0;
		if curRadius > 50 then curRadius := 50;
		
		DrawCircle(smallScreen, ColorGreen, filled, tempPoint, curRadius);
		DrawCircle(smallScreen, ColorGreen, filled, Round(tempPoint.x + curRadius * 2), Round(tempPoint.y + curRadius * 2), curRadius);
		DrawCircle(smallScreen, ColorGreen, Round(tempPoint.x), Round(tempPoint.y + curRadius * 2), curRadius);
		tempPoint2.x := tempPoint.x + curRadius * 2;
		tempPoint2.y := tempPoint.y;
		DrawCircle(smallScreen, ColorGreen, tempPoint2, curRadius);
		DrawBitmap(smallScreen, 0, 0);
		
		DrawCircle(ColorWhite, filled, curRadius, curRadius, curRadius);
		tempPoint3.x := curRadius * 3;
		tempPoint3.y := curRadius * 3;
		DrawCircle(ColorWhite, filled, tempPoint3, curRadius);
		DrawCircle(ColorWhite, curRadius, curRadius * 3, curRadius);
		tempPoint4.x := curRadius * 3;
		tempPoint4.y := curRadius;
		DrawCircle(ColorWhite, tempPoint4, curRadius);
		
		DrawCircleOnScreen(ColorYellow, filled, Round(curRadius + drawIn.x), Round(RectangleBottom(drawIn) - curRadius) - 1, curRadius);
		tempPoint5.x := curRadius * 3 + drawIn.x;
		tempPoint5.y := RectangleBottom(drawIn) - curRadius * 3 - 1;
		DrawCircleOnScreen(ColorYellow, filled, tempPoint5, curRadius);
		DrawCircleOnScreen(ColorYellow, Round(curRadius + drawIn.x), Round(RectangleBottom(drawIn) - curRadius * 3) - 1, curRadius);
		tempPoint6.x := curRadius * 3 + drawIn.x;
		tempPoint6.y := RectangleBottom(drawIn) - curRadius - 1;
		DrawCircleOnScreen(ColorYellow, tempPoint6, curRadius);
	end;
	
	procedure TestDrawRectangle(const drawIn: Rectangle);
	begin
		ClearSurface(smallScreen, ColorTransparent);
		
		if KeyDown(VK_RIGHT) then tempRect.x := tempRect.x + 2;
		if KeyDown(VK_LEFT) then tempRect.x := tempRect.x - 2;
		if KeyDown(VK_UP) then tempRect.y := tempRect.y - 2;
		if KeyDown(VK_DOWN) then tempRect.y := tempRect.y + 2;
		if KeyDown(VK_A) then tempRect.width := tempRect.width - 2;
		if KeyDown(VK_S) then tempRect.width := tempRect.width + 2;
		if KeyDown(VK_Z) then tempRect.height := tempRect.height - 2;
		if KeyDown(VK_X) then tempRect.height := tempRect.height + 2;
		if KeyTyped(VK_T) then filled := filled = false;
		
		if tempRect.width < -100 then tempRect.width := -100;
		if tempRect.height < -100 then tempRect.height := -100;
		if tempRect.width > 100 then tempRect.width := 100;
		if tempRect.height > 100 then tempRect.height := 100;
		
		DrawRectangle(smallScreen, ColorGreen, filled, tempRect);
		DrawRectangle(smallScreen, ColorGreen, filled, Round(tempRect.width + tempRect.x), Round(tempRect.y + tempRect.height), tempRect.width, tempRect.height);
		DrawRectangle(smallScreen, ColorGreen, Round(tempRect.x), Round(tempRect.y + tempRect.height), tempRect.width, tempRect.height);
		tempRect4.x := tempRect.x + tempRect.width;
		tempRect4.y := tempRect.y;
		tempRect4.width := tempRect.width;
		tempRect4.height := tempRect.height;
		DrawRectangle(smallScreen, ColorGreen, tempRect4);
		DrawBitmap(smallScreen, 0, 0);
		
		DrawRectangle(ColorWhite, filled, 0, 0, tempRect.width, tempRect.height);
		tempRect2.x := tempRect.width;
		tempRect2.y := tempRect.height;
		tempRect2.width := tempRect.width;
		tempRect2.height := tempRect.height;
		DrawRectangle(ColorWhite, filled, tempRect2);
		DrawRectangle(ColorWhite, 0, tempRect.height, tempRect.width, tempRect.height);
		tempRect3.x := tempRect.width;
		tempRect3.width := tempRect.width;
		tempRect3.height := tempRect.height;
		DrawRectangle(ColorWhite, tempRect3);
		
		DrawRectangleOnScreen(ColorYellow, filled, Round(drawIn.x), Round(RectangleBottom(drawIn) - tempRect.height) - 1, tempRect.width, tempRect.height);
		tempRect5.x := tempRect.width + drawIn.x;
		tempRect5.y := RectangleBottom(drawIn) - tempRect.height * 2 - 1;
		tempRect5.width := tempRect.width;
		tempRect5.height := tempRect.height;
		DrawRectangleOnScreen(ColorYellow, filled, tempRect5);
		DrawRectangleOnScreen(ColorYellow, Round(drawIn.x), Round(RectangleBottom(drawIn) - tempRect.height * 2) - 1, tempRect.width, tempRect.height);
		tempRect6.x := tempRect.width + drawIn.x;
		tempRect6.y := RectangleBottom(drawIn) - tempRect.height - 1;
		tempRect6.width := tempRect.width;
		tempRect6.height := tempRect.height;
		DrawRectangleOnScreen(ColorYellow, tempRect6);
	end;
	
  //*************************************
  //
  // Variables for testing rotate and zoom Drawing
  // 
  //*************************************
  var
    zoom, rotate: Single;

	procedure TestRotBmp(const drawIn: Rectangle);
	var
	  bmp: Bitmap;
	begin
		if KeyDown(VK_RIGHT) then rotate += 1;
		if KeyDown(VK_LEFT) then rotate -= 1;
		if KeyDown(VK_UP) then zoom += 0.1;
		if KeyDown(VK_DOWN) then zoom -= 0.1;
	  
	  bmp := nil;
	  bmp := RotateScaleBitmap(BitmapNamed('Ship'), rotate, zoom);
	  DrawBitmap(bmp, 200, 200);
	  FreeBitmap(bmp);
	end;

  //*************************************
  //
  // Variables for testing triangle Drawing
  // 
  //*************************************
  var
    greenTriangle, whiteTriangle, yellowTriangle: Triangle;
    triangleRotDeg, triangleScale: Single;

  procedure TestDrawTriangle(const drawIn: Rectangle);
  var
    tempTriangle: Triangle;
    m: Matrix2D;
    midPoint: Point2D;
    tempColor: Color;
    
    procedure OffsetTempTriangle(const forTriangle: Triangle);
    begin
  		tempTriangle := forTriangle;
  		m := MatrixMultiply(TranslationMatrix(tempTriangle[1].x, 0), m);
  		ApplyMatrix(m, tempTriangle);
    end;
    
    procedure SetupTempTriangle(const forTriangle: Triangle);
    begin
  		tempTriangle := forTriangle;
  		
      midPoint := TriangleBarycenter(tempTriangle);
  		m := TranslationMatrix(-midPoint.x, -midPoint.y);
  		m := MatrixMultiply(ScaleMatrix(triangleScale), m);
  		m := MatrixMultiply(RotationMatrix(triangleRotDeg), m);
  		m := MatrixMultiply(TranslationMatrix(midPoint.x + tempTriangle[1].x, midPoint.y), m);
  		//m := TranslationMatrix(tempTriangle[1].x, 0);
  		ApplyMatrix(m, tempTriangle);
    end;
	begin
	  tempColor := RGBAColor(0, 255, 0, 120);
		ClearSurface(smallScreen, ColorTransparent);
		//WriteLn(scr.surface.format^.amask);

    if KeyDown(VK_UP) then triangleScale += 0.1;
    if KeyDown(VK_DOWN) then triangleScale -= 0.1;
		if KeyTyped(VK_T) then filled := filled = false;
    if KeyDown(VK_LEFT) then triangleRotDeg += 5;
    if KeyDown(VK_RIGHT) then triangleRotDeg -= 5;

		DrawTriangle(smallScreen, ColorGreen, filled, greenTriangle);

    SetupTempTriangle(greenTriangle);
		DrawTriangle(smallScreen, tempColor, tempTriangle);

    OffsetTempTriangle(greenTriangle);
		DrawTriangle(smallScreen, tempColor, tempTriangle[0].x, tempTriangle[0].y, tempTriangle[1].x, tempTriangle[1].y, tempTriangle[2].x, tempTriangle[2].y);

    OffsetTempTriangle(greenTriangle);
		FillTriangle(smallScreen, tempColor, tempTriangle);

    OffsetTempTriangle(greenTriangle);
		FillTriangle(smallScreen, tempColor, tempTriangle[0].x, tempTriangle[0].y, tempTriangle[1].x, tempTriangle[1].y, tempTriangle[2].x, tempTriangle[2].y);

		DrawTriangle(ColorWhite, filled, whiteTriangle);
		
		tempColor := RGBAColor(255, 255, 255, 120);
		
		SetupTempTriangle(whiteTriangle);
		DrawTriangle(tempColor, tempTriangle);
		
		OffsetTempTriangle(whiteTriangle);
		DrawTriangle(tempColor, tempTriangle[0].x, tempTriangle[0].y, tempTriangle[1].x, tempTriangle[1].y, tempTriangle[2].x, tempTriangle[2].y);

    OffsetTempTriangle(whiteTriangle);
		FillTriangle(tempColor, tempTriangle);

    OffsetTempTriangle(whiteTriangle);
		FillTriangle(tempColor, tempTriangle[0].x, tempTriangle[0].y, tempTriangle[1].x, tempTriangle[1].y, tempTriangle[2].x, tempTriangle[2].y);
		DrawBitmap(smallScreen, 0, 0);
		
    DrawTriangleOnScreen(ColorYellow, filled, yellowTriangle);
		tempColor := RGBAColor(255, 255, 0, 120);
		
		SetupTempTriangle(yellowTriangle);
		DrawTriangleOnScreen(tempColor, tempTriangle);
		
		OffsetTempTriangle(yellowTriangle);
		DrawTriangleOnScreen(tempColor, tempTriangle[0].x, tempTriangle[0].y, tempTriangle[1].x, tempTriangle[1].y, tempTriangle[2].x, tempTriangle[2].y);

    OffsetTempTriangle(yellowTriangle);
		FillTriangleOnScreen(tempColor, tempTriangle);

    OffsetTempTriangle(yellowTriangle);
		FillTriangleOnScreen(tempColor, tempTriangle[0].x, tempTriangle[0].y, tempTriangle[1].x, tempTriangle[1].y, tempTriangle[2].x, tempTriangle[2].y);
	end;

	
	function GetGraphicsTests(): TestSuite;
	var
		i: Integer;
		fps: Array of Integer;
		tempBitmap: Array of Bitmap;
	begin
		result.Title := 'Graphics Tests';
		SetLength(result.Tests, 12);
		
		for i := 0 to High(result.Tests) do
		begin
			InitTest(result.Tests[i]);
		end;
		
		with result.Tests[0] do
		begin
			MethodBeingTested := 'DrawRectangle, DrawRectangleOnScreen, ClearSurface, DrawBitmap';
			Instructions := 	'Use the arrow keys to move' + LineEnding + 'the green box.' + LineEnding + 'A : Shrink the box width' + LineEnding + 'S : Expand the box width'
								+ LineEnding + 'Z : Shrink the box height' + LineEnding + 'X : Expand the box height' + LineEnding + 'T : Toggle fill' + LineEnding
								+ 'White : Normal' + LineEnding + 'Green : On destination bitmap' + LineEnding + 'Yellow: On screen';
			tempRect := RectangleFrom(0, 0, 50, 50);
			tempRect4 := RectangleFrom(0, 0, 50, 50);
			smallScreen := CreateBitmap(418, 418);
			filled := false;
			ToRun := @TestDrawRectangle;
		end;
		
		with result.Tests[1] do
		begin
			MethodBeingTested := 'DrawCircle, DrawCircleOnScreen';
			Instructions :=  	'Use the arrow keys to move' + LineEnding + 'the green circles.' + LineEnding + 'Z : Shrink the circle radius' + LineEnding + 'X : Expand the circle radius'
								+ LineEnding + 'T : Toggle fill' + LineEnding + 'White : Normal' + LineEnding + 'Green : On destination bitmap' + LineEnding + 'Yellow: On screen';
			tempPoint := PointAt(15, 15);
			curRadius := 15;
			ToRun := @TestDrawCircle;
		end;
		
		with result.Tests[2] do
		begin
			MethodBeingTested := 'DrawEllipse, DrawEllipseOnScreen';
			Instructions := 'Use the arrow keys to move' + LineEnding + 'the green ellipses.' + LineEnding + 'A : Shrink the ellipse width' + LineEnding + 'S : Expand the ellipse width'
							+ LineEnding + 'Z : Shrink the ellipse height' + LineEnding + 'X : Expand the ellipse height' + LineEnding + 'T : Toggle fill' + LineEnding
							+ 'White : Normal' + LineEnding + 'Green : On destination bitmap' + LineEnding + 'Yellow: On screen';
			ToRun := @TestDrawEllipse;
		end;
		
		with result.Tests[3] do
		begin
			MethodBeingTested := 'FillRectangle, FillEllipse';
			Instructions := 'Use the arrow keys to move' + LineEnding + 'the green shapes.' + LineEnding + 'A : Shrink the shape width' + LineEnding + 'S : Expand the shape width'
							+ LineEnding + 'Z : Shrink the shape height' + LineEnding + 'X : Expand the shape height' + LineEnding
							+ 'White : Normal' + LineEnding + 'Green : On destination bitmap' + LineEnding + 'Yellow: On screen';
			ToRun := @TestFill1;
		end;
		
		with result.Tests[4] do
		begin
			MethodBeingTested := 'FillCircle, FillCircleOnScreen';
			Instructions := 'Use the arrow keys to move' + LineEnding + 'the green circles.'
							+ LineEnding + 'Z : Shrink the circle height' + LineEnding + 'X : Expand the circle height' + LineEnding
							+ 'White : Normal' + LineEnding + 'Green : On destination bitmap' + LineEnding + 'Yellow: On screen';
			ToRun := @TestFill2;
		end;
		
		with result.Tests[5] do
		begin
			SetLength(tempLines, 7);
			MethodBeingTested := 'All routines related to line, RotateMatrix, Multiply';
			Instructions := 'Use the arrow keys to rotate' + LineEnding + 'the white lines';
			tempLines[1] := LineFromVector(225, 225, VectorFromAngle(0, 125));
			tempLines[2] := LineFromVector(225, 225, VectorFromAngle(60, 125));
			tempLines[3] := LineFromVector(225, 225, VectorFromAngle(120, 125));
			tempLines[4] := LineFromVector(225, 225, VectorFromAngle(180, 125));
			tempLines[5] := LineFromVector(248, 354, VectorFromAngle(240, 125));
			tempLines[6] := LineFromVector(248, 354, VectorFromAngle(300, 125));
			ToRun := @TestLines;
		end;
		
		with result.Tests[6] do
		begin
			MethodBeingTested := 'DrawPixel';
			Instructions := 'Click and drag to draw with' + LineEnding + 'your mouse cursor.' + LineEnding + LineEnding + '1: Increment Blue' + LineEnding + '2: Increment Green' + LineEnding + '3: Inrrement Blue';
			curCol := ColorWhite;
			result.Tests[6].ClearScreen := false;
			ToRun := @TestPixels;
		end;
		
		with result.Tests[7] do
		begin
			MethodBeingTested := 'Bitmap routines';
			Instructions := 'Use the arrow keys to move' + LineEnding + 'the ship.';
			SetLength(frameB, 6);
			frameB[0] := BitmapNamed('Frame1');
			frameB[1] := BitmapNamed('Frame2');
			frameB[2] := BitmapNamed('Frame3');
			frameB[3] := BitmapNamed('Frame4');
			frameB[4] := BitmapNamed('Frame5');
			frameB[5] := BitmapNamed('Frame6');
			ship := BitmapNamed('enShip');
			framePos := PointAt(0, 0);
			ToRun := @TestBitmap;
		end;
		
		with result.Tests[8] do
		begin
			SetLength(fps, 40);
			SetLength(explodeAnim, 40);
			for i := 0 to 39 do
			begin
				fps[i] := 2;
				explodeAnim[i] := BitmapNamed('Explode_' + IntToStr(i));
			end;
			
			explodeAnimFinal := CombineIntoGrid(explodeAnim, 10);
			
			MethodBeingTested := 'Sprite routines';
			Instructions := 'Use the number keys from' + LineEnding + '1 to 7 to change the sprite.' + LineEnding + 'Use the arrow keys to' + LineEnding + 'move the sprite.' + LineEnding + 'Use the M key to reset the' + LineEnding + 'position of the sprite.'
							+ LineEnding + 'Press Q to change the animation' + LineEnding + 'to Loop.'+ LineEnding + 'Press W to change the animation' + LineEnding + 'to ReverseLoop.'+ LineEnding + 'Press E to change the animation' + LineEnding + 'to ReverseOnce.'+ LineEnding + 'Press R to change the animation' + LineEnding + 'to Stop.';
			SetLength(sprites, 7);
			
			animExplode := LoadAnimationScript('blue_anim.txt');
			sprites[0] := CreateSprite(BitmapNamed('BlueExplosion'), animExplode, 180, 180);
			sprites[4] := CreateSprite(explodeAnimFinal, animExplode);
			  
			animExplode := LoadAnimationScript('blue_anim_loop.txt');
			sprites[1] := CreateSprite(BitmapNamed('BlueExplosion'), animExplode, 180, 180);
			sprites[5] := CreateSprite(explodeAnimFinal, animExplode);
			
			animExplode := LoadAnimationScript('blue_anim_cycle.txt');
			sprites[2] := CreateSprite(BitmapNamed('BlueExplosion'), animExplode, 180, 180);
			sprites[6] := CreateSprite(explodeAnimFinal, animExplode);
			
			sprites[3] := CreateSprite(BitmapNamed('BallImage1'));
			
			ToRun := @TestSprite;
		end;
		
		with result.Tests[9] do
		begin
			SetLength(tempBitmap, 1);
			MethodBeingTested := 'AddBitmapToSprite';
			Instructions := 'Press Space to add an' + LineEnding + 'another bitmap.';
			curNum := 1;
			tempBitmap[0] := CreateBitmap(121, 120);
			DrawCircle(tempBitmap[0], ColorWhite, true, 60, 60, 60);
			DrawTextLines(tempBitmap[0], '1', ColorBlack, ColorTransparent, FontNamed('ArialLarge'), AlignCenter, BitmapRectangle(tempBitmap[0]));
			numSprite := CreateSprite(tempBitmap);
			SpriteSetX(numSprite, 149);
			SpriteSetY(numSprite, 149);
			ToRun := @TestAddBitmap;
		end;
		
		with result.Tests[10] do
		begin
			MethodBeingTested := 'DrawTriangle, DrawTriangleOnScreen, ClearSurface, DrawBitmap';
			Instructions := 	'Use the arrow keys to move' + LineEnding + 'the green triangles.' + LineEnding + 'T : Toggle fill' + LineEnding
								+ 'White : Normal' + LineEnding + 'Green : On destination bitmap' + LineEnding + 'Yellow: On screen';
			greenTriangle := TriangleFrom(0, 200, 50, 200, 50, 150);
			whiteTriangle := TriangleFrom(0, 0, 50, 0, 50, 50);
			yellowTriangle := TriangleFrom(0, 400, 50, 400, 50, 450);
			triangleRotDeg := 0;
			triangleScale := 1;
			filled := false;
			ToRun := @TestDrawTriangle;
		end;
		
		with result.Tests[11] do
		begin
			MethodBeingTested := 'RotateZoomBitmap';
			Instructions := 	'Use the arrow keys to move';
			zoom := 1;
			rotate := 0;
			ToRun := @TestRotBmp;
		end;
	end;
	
	procedure AddGraphicsSuite(var suites: TestSuites);
	begin
		suites[High(suites)] := GetGraphicsTests();
	end;

end.