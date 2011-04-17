  unit PhysicsTests;

interface
	uses TestFramework;

	procedure AddPhysicsSuite(var suites: TestSuites); 

implementation
	uses GameResources, sgPhysics, sgInput, sgGraphics, sgTypes,sgGeometry,sgText,sgCamera, SysUtils, sgSprites, sgImages, sgAnimations;
	
	var
		ship, explosion, ball, bigball: Sprite;
		smallball, mediumball: Bitmap;
		rotShip: Bitmap;
		X, Y: Integer;
		v1, v2, v, movement, mvOut: Vector;
		SprSt: Boolean = false;
		VIWRT: Boolean = false;
		VA: Boolean = false;
		POOR: Boolean = false;
		ROOR: Boolean = false;
		POOC: Boolean = false;
		COOC: Boolean = false;
		TT: Boolean = false;
		point, point2, p, c, tempPoint: Point2D;
		rect, rect2, mvRect, tgtRect: Rectangle;
		px, py, rotF: Single;
		halfH, halfW: Integer;
		enterRectFrom: CollisionSide;
		mat: Matrix2D;
		
	procedure CollisionSpriteTest(const drawIn: Rectangle);
	begin
	    //ClearScreen(ColorWhite);
	    //ship^.velocity := VectorTo(1,1));
	    UpdateSpriteAnimation(ship);
	    //ship.UsePixelCollision := true;
	    //Drawship^.velocity.X + ', ' + ship^.velocity.Y, ColorWhite, FontNamed('Courier'), 10, 200);

	    if (KeyDown(VK_LEFT)) then
	    begin
	        SpriteSetX(ship, SpriteX(ship) - 1);
	    end;
	    if (KeyDown(VK_RIGHT)) then
	    begin
	        SpriteSetX(ship, SpriteX(ship) + 1);
	    end;
	    if (KeyDown(VK_DOWN)) then
	    begin
	        SpriteSetY(ship, SpriteY(ship) + 1);
	    end;
	    if (KeyDown(VK_UP)) then
	    begin
	        SpriteSetY(ship, SpriteY(ship) - 1);
	    end;

	    //HasSpriteCollidedX
      // DrawVerticalLine(ColorRed, 400, 0, 418);
      // if (HasSpriteCollidedX(ship, 400, CollisionRangeEquals)) then
      // begin
      //     DrawRectangle(ColorWhite, 395, 405, 10, 10);
      // end;
      // if (HasSpriteCollidedX(ship, 400, CollisionRangeGreaterThan)) then
      // begin
      //     DrawRectangle(ColorWhite, 407, 405, 10, 10);
      // end;
      // if (HasSpriteCollidedX(ship, 400, CollisionRangeLessThan)) then
      // begin
      //     DrawRectangle(ColorWhite, 383, 405, 10, 10);
      // end;

	    //HasSpriteCollidedY
      // DrawHorizontalLine(ColorBlue, 400, 0, 418);
      // if (HasSpriteCollidedY(ship, 400, CollisionRangeEquals)) then
      // begin
      //     DrawRectangle(ColorWhite, 10, 395, 10, 10);
      // end;
      // if (HasSpriteCollidedY(ship, 400, CollisionRangeGreaterThan)) then
      // begin
      //     DrawRectangle(ColorWhite, 10, 407, 10, 10);
      // end;
      // if (HasSpriteCollidedY(ship, 400, CollisionRangeLessThan)) then
      // begin
      //     DrawRectangle(ColorWhite, 10, 383, 10, 10);
      // end;

	    //SpriteRectCollision

	    DrawRectangle(ColorRed, 10, 10, 10, 10);
	    if (SpriteRectCollision(ship, 10, 10, 10, 10)) then
	    begin
	        FillRectangle(ColorGreen, 10, 10, 10, 10);
	    end;

	    DrawRectangle(ColorGreen, 10, 25, 10, 10);
	    if (SpriteRectCollision(ship, RectangleFrom(10, 25, 10, 10))) then
	    begin
	        FillRectangle(ColorGreen, 10, 25, 10, 10);
	    end;

	    //SpriteCollision
	    //explosion.UsePixelCollision := true;

	    //SpriteBitmapCollision
	    if (SpriteBitmapCollision(ship, smallball, 10,100)) then
	    begin
	        DrawRectangle(ColorRed, true, 5, 95, BitmapWidth(smallball) + 10, BitmapHeight(smallball) + 10);
	    end;
	    DrawBitmap(smallball, 10, 100);
	    DrawRectangle(ColorWhite, 10, 100, BitmapWidth(smallball), BitmapHeight(smallball));

	    if (SpriteBitmapCollision(ship, smallball, 10, 150)) then
	    begin
	        DrawRectangle(ColorBlue, true, 5, 145, BitmapWidth(smallball) + 10, BitmapHeight(smallball) + 10);
	    end;
	    DrawBitmap(smallball, 10, 150);
	    DrawRectangle(ColorWhite, 10, 150, BitmapWidth(smallball), BitmapHeight(smallball));
	    //using points
	    if SpriteBitmapCollision(ship, smallball, PointAt(70,100)) then
	    begin
	        DrawRectangle(ColorBlue, true, 65, 95, BitmapWidth(smallball) + 10, BitmapHeight(smallball) + 10);
	    end;
	    DrawBitmap(smallball, 70, 100);
	    DrawRectangle(ColorWhite, 70, 100, BitmapWidth(smallball), BitmapHeight(smallball));

	    if SpriteBitmapCollision(ship, smallball, PointAt(70, 150)) then
	    begin
	        DrawRectangle(ColorBlue, true, 65, 145, BitmapWidth(smallball) + 10, BitmapHeight(smallball) + 10);
	    end;
	    DrawBitmap(smallball, 70, 150);
	    DrawRectangle(ColorWhite, 70, 150, BitmapWidth(smallball), BitmapHeight(smallball));

	    SpriteSetX(explosion, 70);
	    SpriteSetY(explosion, 10);
	    
	    UpdateSprite(explosion);
	    // FillRectangle(ColorWhite, 64, 4, 45, 45);
	    if SpriteCollision(ship, explosion) then
	    begin
	        DrawRectangle(ColorBlue, 65, 5, 78, 78);
	        DrawRectangle(ColorBlue, 64, 4, 80, 80);
	    end;
	    DrawSprite(explosion);

	    DrawSprite(ship);
	    DrawRectangle(ColorWhite, SpriteX(ship), SpriteY(ship), SpriteWidth(ship), SpriteHeight(ship));
	end;
	
	procedure CollissionBitmapTest(const drawIn: Rectangle);
	begin
		if (KeyDown(VK_LEFT)) then
	    begin
	        X := X - 1;
	    end;
	    if (KeyDown(VK_RIGHT)) then
	    begin
	        X := X + 1;
	    end;
	    if (KeyDown(VK_DOWN)) then
	    begin
	        Y := Y + 1;
	    end;
	    if (KeyDown(VK_UP)) then
	    begin
	        Y := Y - 1;
	    end;
	    //BitmapCollision
	    DrawText('Default', ColorWhite, FontNamed('Courier'), 10, 5);
	    if (BitmapCollision(mediumball, 10, 30, smallball, X, Y)) then
	    begin
	        FillRectangle(ColorBlue, 5, 25, BitmapHeight(mediumball) + 10, BitmapWidth(mediumball) + 10);
	    end;
	    DrawBitmap(mediumball, 10, 30);
	    DrawRectangle(ColorWhite, 10, 30, BitmapWidth(mediumball), BitmapHeight(mediumball));

	    if (BitmapCollision(mediumball, PointAt(10, 150), smallball, PointAt(X, Y))) then
	    begin
	        FillRectangle(ColorPink, 5, 145, BitmapHeight(mediumball) + 10, BitmapWidth(mediumball) + 10);
	    end;
	    DrawBitmap(mediumball, 10, 150);
	    DrawRectangle(ColorWhite, 10, 150, BitmapWidth(mediumball), BitmapHeight(mediumball));

	    if BitmapCollision(mediumball, PointAt(10, 270), smallball, PointAt(X, Y)) then
	    begin
	        FillRectangle(ColorPink, 5, 265, BitmapHeight(mediumball) + 10, BitmapWidth(mediumball) + 10);
	    end;
	    DrawBitmap(mediumball, 10, 270);
	    DrawRectangle(ColorWhite, 10, 270, BitmapWidth(mediumball), BitmapHeight(mediumball));



	    DrawText('No Bound', ColorWhite, FontNamed('Courier'), 120, 5);
	    if (BitmapCollision(mediumball, 120, 30, smallball, X, Y)) then
	    begin
	        FillRectangle(ColorBlue, 115, 25,  BitmapHeight(mediumball) + 10, BitmapWidth(mediumball) + 10);
	    end;
	    DrawBitmap(mediumball, 120, 30);
	    DrawRectangle(ColorWhite, 120, 30, BitmapWidth(mediumball), BitmapHeight(mediumball));

	    if (BitmapCollision(mediumball, PointAt(120, 150), smallball, PointAt(X, Y))) then
	    begin
	        FillRectangle(ColorBlue, 115, 145, BitmapHeight(mediumball) + 10, BitmapWidth(mediumball) + 10);
	    end;
	    DrawBitmap(mediumball, 120, 150);
	    DrawRectangle(ColorWhite, 120, 150, BitmapWidth(mediumball), BitmapHeight(mediumball));

	    if (BitmapCollision(mediumball, PointAt(120, 270), smallball, PointAt(X, Y))) then
	    begin
	        FillRectangle(ColorBlue, 115, 265, BitmapHeight(mediumball) + 10, BitmapWidth(mediumball) + 10);
	    end;
	    DrawBitmap(mediumball, 120, 270);
	    DrawRectangle(ColorWhite, 120, 270, BitmapWidth(mediumball), BitmapHeight(mediumball));

	    DrawText('Big ball Bound', ColorWhite, FontNamed('Courier'), 240, 5);
	    if (BitmapCollision(mediumball, 240, 30, smallball, X, Y)) then
	    begin
	        FillRectangle(ColorBlue, 235, 25, BitmapHeight(mediumball) + 10, BitmapWidth(mediumball) + 10);
	    end;
	    DrawBitmap(mediumball, 240, 30);
	    DrawRectangle(ColorWhite, 240, 30, BitmapWidth(mediumball), BitmapHeight(mediumball));

	    DrawText('small ball Bound', ColorWhite, FontNamed('Courier'), 240, 150);
	    if (BitmapCollision(mediumball, PointAt(240, 170), smallball, PointAt(X, Y))) then
	    begin
	        FillRectangle(ColorBlue, 235, 165, BitmapHeight(mediumball) + 10, BitmapWidth(mediumball) + 10);
	    end;
	    DrawBitmap(mediumball, 240, 170);
	    DrawRectangle(ColorWhite, 240, 170, BitmapWidth(mediumball), BitmapHeight(mediumball));

	    //BitmapRectCollision
	    DrawRectangle(ColorGreen, 400, 350, 10, 10);
	    if (BitmapRectCollision(smallball, X, Y, 400, 350, 10, 10)) then
	    begin
	        FillRectangle(ColorGreen, 400, 350, 10, 10);
	    end;

	    DrawRectangle(ColorGreen, 400, 365, 10, 10);
	    if (BitmapRectCollision(smallball, X, Y, RectangleFrom(400, 365, 10, 10))) then
	    begin
	        FillRectangle(ColorGreen, 400, 365, 10, 10);
	    end;
	    
	    if (BitmapCollision(rotShip, 250, 300, smallball, X, Y)) then
	    begin
	        FillRectangle(ColorBlue, 245, 295, BitmapHeight(rotShip) + 10, BitmapWidth(rotShip) + 10);
	    end;
	    DrawBitmap(rotShip, 250, 300);
		
	    DrawBitmap(smallball,X,Y);
	    DrawRectangle(ColorWhite, X, Y, BitmapWidth(smallball), BitmapHeight(smallball));
	end;
	
	procedure CircleCollissionTest(const drawIn: Rectangle);
	var
		tempX, tempY: Single;
	begin
	   if (KeyDown(VK_LEFT)) then
	    begin
	        SpriteSetX(ball, SpriteX(ball) - 1);
	    end;
	    if (KeyDown(VK_RIGHT)) then
	    begin
	        SpriteSetX(ball, SpriteX(ball) + 1);
	    end;
		
	    if (KeyDown(VK_S)) then
	    begin
	        if (VectorMagnitude(ball^.velocity) > 2) then
	        begin
	            SpriteSetVelocity(ball, VectorMultiply(UnitVector(SpriteVelocity(ball)), VectorMagnitude(SpriteVelocity(ball)) - 1));
	        end;
	    end;
	    if (KeyDown(VK_DOWN)) then
	    begin
	        SpriteSetY(ball, SpriteY(ball) + 1);
	    end;
	    if (KeyDown(VK_UP)) then
	    begin
	        SpriteSetY(ball, SpriteY(ball) - 1);
	    end;
		
	    if (KeyDown(VK_W)) then
	    begin
	        ball^.velocity := VectorMultiply(UnitVector(ball^.velocity), VectorMagnitude(ball^.velocity) + 1);
	    end;
	    if (KeyDown(VK_A)) then
	    begin
	        ball^.velocity := MatrixMultiply(RotationMatrix(1),ball^.velocity);
	    end;
	    if (KeyDown(VK_D)) then
	    begin
	        ball^.velocity := MatrixMultiply(RotationMatrix(-1),ball^.velocity);
	    end;
	    //CircleLineCollision
	    if (CircleLineCollision(ball, LineFrom(300, 0, 300, 418))) then
	    begin
	        FillRectangle(ColorRed, 298, 0, 5, 418);
	    end;
	    DrawVerticalLine(ColorRed, 300, 0, 418);
		
	    //CircleLineCollision
	    if (CircleLineCollision(ball, LineFrom(300, 0, 300, 418)) and KeyDown(VK_SPACE)) then
	    begin
	        CollideCircleLine(ball, LineFrom(300, 0, 300, 418));
	    end;
		
	    DrawSprite(ball);
	    tempX := SpriteX(ball) + (SpriteWidth(ball) /2);
	    tempY := SpriteY(ball) + (SpriteHeight(ball) / 2);
	    DrawLine(ColorBlue, tempX, tempY, tempX + ball^.velocity.X, tempY + ball^.velocity.Y);
	    DrawRectangle(ColorWhite, SpriteX(ball), SpriteY(ball), SpriteWidth(ball), SpriteHeight(ball));
	end;
	
	var toggleRunCircle: Boolean = false;
	
	procedure CircleCollissionTest2(const drawIn: Rectangle);
	var
		tempX, tempY, bigtempX, bigtempY: Single;
		
		procedure MoveBallUsingVector(var ball : Sprite);
		begin
			MoveSprite(ball, ball^.velocity);
			
			if SpriteX(ball) > drawIn.width - SpriteWidth(ball) then
			begin
				ball^.velocity.x := -ball^.velocity.x;
				SpriteSetX(ball, drawIn.width - SpriteWidth(ball));
			end;
			if SpriteY(ball) > drawIn.height - SpriteHeight(ball) then
			begin
				ball^.velocity.y := -ball^.velocity.y;
				SpriteSetY(ball, drawIn.height - SpriteHeight(ball));
			end;
			if SpriteX(ball) < drawIn.x then
			begin
				ball^.velocity.x := -ball^.velocity.x;
				SpriteSetX(ball, drawIn.x);
			end;
			if SpriteY(ball) < 0 then
			begin
				ball^.velocity.y := -ball^.velocity.y;
				SpriteSetY(ball, 0);
			end;
		end;

	begin
		if KeyTyped(VK_T) then toggleRunCircle := not toggleRunCircle;
			
		if (KeyDown(VK_DOWN)) then
        begin
            if (VectorMagnitude(bigball^.velocity) > 2) then
            begin
                bigball^.velocity := VectorMultiply(UnitVector(bigball^.velocity), VectorMagnitude(bigball^.velocity) - 1);
            end;
        end;
        if (KeyDown(VK_UP)) then
        begin
            bigball^.velocity := VectorMultiply(UnitVector(bigball^.velocity), VectorMagnitude(bigball^.velocity) + 1);
        end;
        if (KeyDown(VK_LEFT)) then
        begin
            bigball^.velocity := MatrixMultiply(RotationMatrix(-1), bigball^.velocity);
        end;
        if (KeyDown(VK_RIGHT)) then
        begin
            bigball^.velocity := MatrixMultiply(RotationMatrix(1), bigball^.velocity);
        end;

        if (KeyDown(VK_S)) then
        begin
            if (VectorMagnitude(ball^.velocity) > 2) then
            begin
                ball^.velocity := VectorMultiply(UnitVector(ball^.velocity), VectorMagnitude(ball^.velocity) - 1);
            end;
        end;
        if (KeyDown(VK_W)) then
        begin
            ball^.velocity := VectorMultiply(UnitVector(ball^.velocity), VectorMagnitude(ball^.velocity) + 1);
        end;
        if (KeyDown(VK_A)) then
        begin
            ball^.velocity := MatrixMultiply(RotationMatrix(-1), ball^.velocity);
        end;
        if (KeyDown(VK_D)) then
        begin
            ball^.velocity := MatrixMultiply(RotationMatrix(1), ball^.velocity);
        end;
        if (not toggleRunCircle) and (KeyTyped(VK_SPACE)) then
        begin
            if (SpriteCollision(ball, bigball)) then
            begin
                CollideCircles(ball, bigball);
                UpdateSprite(ball);
                UpdateSprite(bigball);
            end
            else
            begin
                SpriteSetX(ball, 150);
                SpriteSetY(ball, 150);
                SpriteSetX(bigball, 155);
                SpriteSetY(bigball, 155);
            end;
        end;
        if toggleRunCircle then
		  begin
           MoveBallUsingVector(ball);
           MoveBallUsingVector(bigball);

           if (SpriteCollision(ball, bigball)) then
           begin
               CollideCircles(ball, bigball);
               UpdateSprite(ball);
               UpdateSprite(bigball);
           end;				
		  end;

        DrawSprite(bigball);
        DrawSprite(ball);
        tempX := SpriteX(ball) + (SpriteWidth(ball) / 2);
        tempY := SpriteY(ball) + (SpriteHeight(ball) / 2);
        bigtempX := SpriteX(bigball) + (SpriteWidth(bigball) / 2);
        bigtempY := SpriteY(bigball) + (SpriteHeight(bigball) / 2);
        DrawLine(ColorBlue, tempX, tempY, tempX + ball^.velocity.X, tempY + ball^.velocity.Y);
        DrawLine(ColorBlue, bigtempX, bigtempY, bigtempX + bigball^.velocity.X, bigtempY + bigball^.velocity.Y);
        //DrawRectangle(ColorWhite, ball.X, SpriteY(ball), SpriteWidth(ball), SpriteHeight(ball));
	end;
	
	procedure RectangleCollisionTest(const drawIn: Rectangle);
	begin
		if (KeyDown(VK_LEFT)) then
        begin
            X := X - 1;
        end;
        if (KeyDown(VK_RIGHT)) then
        begin
            X := X + 1;
        end;
        if (KeyDown(VK_DOWN)) then
        begin
            Y := Y + 1;
        end;
        if (KeyDown(VK_UP)) then
        begin
            Y := Y - 1;
        end;
        //RectLineCollision
        if (RectLineCollision(RectangleFrom(X,Y, 50,50), LineFrom(100, 0, 100, 418))) then
        begin
            FillRectangle(ColorRed, 98, 0, 5, 418);
        end;
        if (RectLineCollision(RectangleFrom(X, Y, 50, 50), LineFrom(200, 0, 200, 418))) then
        begin
            FillRectangle(ColorRed, 198, 0, 5, 418);
        end;
        DrawRectangle(ColorWhite, RectangleFrom(X, Y, 50, 50));
        DrawLine(ColorYellow, LineFrom(100, 0, 100, 418));
        DrawLine(ColorYellow, LineFrom(200, 0, 200, 418));
	end;
	
	procedure VectorMathsTest(const drawIn: Rectangle);
	var
		temp: Vector;
	begin
		if (KeyDown(VK_LEFT)) then
        begin
            v1.Y := v1.Y - 1;
        end;
        if (KeyDown(VK_RIGHT)) then
        begin
            v1.Y := v1.Y + 1;
        end;
        if (KeyDown(VK_DOWN)) then
        begin
            v1.X := v1.X - 1;
        end;
        if (KeyDown(VK_UP)) then
        begin
            v1.X := v1.X + 1;
        end;

        if (KeyDown(VK_A)) then
        begin
            v2.Y := v2.Y - 1;
        end;
        if (KeyDown(VK_D)) then
        begin
            v2.Y := v2.Y + 1;
        end;
        if (KeyDown(VK_S)) then
        begin
            v2.X := v2.X - 1;
        end;
        if (KeyDown(VK_W)) then
        begin
            v2.X := v2.X + 1;
        end;

        DrawText('Vector 1 := X:' + FloatToStr(v1.X) + ', Y:' + FloatToStr(v1.Y), ColorWhite, FontNamed('Courier'), 10, 10);
        DrawText('Vector 2 := X:' + FloatToStr(v2.X) + ', Y:' + FloatToStr(v2.Y), ColorWhite, FontNamed('Courier'), 10, 30);
        temp := AddVectors(v1, v2);
        DrawText('Vector 1 + Vector 2 := X:' + FloatToStr(temp.X) + ', Y:' + FloatToStr(temp.Y), ColorWhite, FontNamed('Courier'), 10, 50);
        temp := SubtractVectors(v1, v2);
        DrawText('Vector 1 - Vector 2 := X:' + FloatToStr(temp.X) + ', Y:' + FloatToStr(temp.Y), ColorWhite, FontNamed('Courier'), 10, 70);
        temp := LimitVector(v1, 20);
        DrawText('limt Vector 1 to 20 := X:' + FloatToStr(temp.X) + ', Y:' + FloatToStr(temp.Y), ColorWhite, FontNamed('Courier'), 10, 90);
        temp := InvertVector(v1);
        DrawText('Vector 1 inverted := X:' + FloatToStr(temp.X) + ', Y:' + FloatToStr(temp.Y), ColorWhite, FontNamed('Courier'), 10, 110);
        if (VectorIsZero(v1)) then
        begin
            DrawText('Vector 1 is zero', ColorWhite, FontNamed('Courier'), 10, 130);
        end
        else
        begin
            DrawText('Vector 1 is not zero', ColorWhite, FontNamed('Courier'), 10, 130);
        end;
	end;
	
	procedure SpriteTests(const drawIn: Rectangle);
	var
		temp: Single;
		tempV: Vector;
	begin
		if not SprSt then
		begin
			SpriteSetX(ball, 100);
	    SpriteSetY(ball, 300);
	        SpriteSetX(bigball, 200);
	        SpriteSetY(bigball, 300);
			SprSt := true;
		end;
		
		if (KeyDown(VK_UP)) then SpriteSetY(ball, SpriteY(ball) - 1);
        if (KeyDown(VK_DOWN)) then SpriteSetY(ball, SpriteY(ball) + 1);
        if (KeyDown(VK_LEFT)) then SpriteSetX(ball, SpriteX(ball) - 1);
        if (KeyDown(VK_RIGHT)) then SpriteSetX(ball, SpriteX(ball) + 1);

        if (KeyDown(VK_W)) then SpriteSetY(bigball, SpriteY(bigball) - 1);
        if (KeyDown(VK_S)) then SpriteSetY(bigball, SpriteY(bigball) + 1);
        if (KeyDown(VK_A)) then SpriteSetX(bigball, SpriteX(bigball) - 1);
        if (KeyDown(VK_D)) then SpriteSetX(bigball, SpriteX(bigball) + 1);

        DrawSprite(bigball);
        DrawSprite(ball);
        //SpriteOnScreenAt
        if (SpriteOnScreenAt(ball, PointAt(ToScreenX(209), ToScreenY(209)))) then
        begin
            DrawText('The small ball is on the screen ', ColorWhite, FontNamed('Courier'), 10, 10);
            DrawText('at X:209, Y:209    X:' + FloatToStr(SpriteX(ball)) + ', Y:' + FloatToStr(SpriteY(ball)), ColorWhite, FontNamed('Courier'), 10, 30);
        end
        else
        begin
            DrawText('The small ball is not on the screen ', ColorWhite, FontNamed('Courier'), 10, 10);
            DrawText('at X:209, Y:209    X:' + FloatToStr(SpriteX(ball)) + ', Y:' + FloatToStr(SpriteY(ball)), ColorWhite, FontNamed('Courier'), 10, 30);
        end;
        if (SpriteOnScreenAt(bigball, ToScreenX(209), ToScreenY(209))) then
        begin
            DrawText('The large ball is on the screen ', ColorWhite, FontNamed('Courier'), 10, 50);
            DrawText('at X:209, Y:209    X:' + FloatToStr(SpriteX(bigball)) + ', Y:' + FloatToStr(SpriteY(bigball)), ColorWhite, FontNamed('Courier'), 10, 70);
        end
        else
        begin
            DrawText('The large ball is not on the screen ', ColorWhite, FontNamed('Courier'), 10, 50);
            DrawText('at X:209, Y:209    X:' + FloatToStr(SpriteX(bigball)) + ', Y:' + FloatToStr(SpriteY(bigball)), ColorWhite, FontNamed('Courier'), 10, 70);
        end;
        DrawPixel(ColorWhite, 209, 209);
        //CalculateAngle
        temp := CalculateAngle(ball, bigball);
        //DrawText(CenterPoint(ball).X + ', ' + CenterPoint(ball).Y + ' '+ CenterPoint(bigball).X + ', ' + CenterPoint(bigball).Y, ColorWhite, FontNamed('Courier'), 10, 110);
        DrawText('The angle betwen the balls is ' + FloatToStr(temp), ColorWhite, FontNamed('Courier'), 10, 90);

        //VectorFromTo
        tempV := VectorFromTo(ball, bigball);
        DrawText('The vector from the small ball ', ColorWhite, FontNamed('Courier'), 10, 110);
        DrawText('to the large ball is X:' + FloatToStr(tempV.X) + ', Y:' + FloatToStr(tempV.Y), ColorWhite, FontNamed('Courier'), 10, 130);

        //VectorFromCenterSpriteToPoint
        tempV := VectorFromCenterSpriteToPoint(ball, PointAt(100, 200));
        DrawText('Vector from ball to point X:' + FloatToStr(tempV.X) + ', Y:' + FloatToStr(tempV.Y), ColorWhite, FontNamed('Courier'), 10, 150);
        FillRectangle(ColorRed, 100 - 1, 200 - 1, 3, 3);
	end;
	
	procedure PointTest(const drawIn: Rectangle);
	var
		temp: Vector;
	begin
		if (KeyDown(VK_UP)) then point.Y := point.Y - 1;
        if (KeyDown(VK_DOWN)) then point.Y := point.Y + 1;
        if (KeyDown(VK_LEFT)) then point.X := point.X - 1;
        if (KeyDown(VK_RIGHT)) then point.X := point.X + 1;

        temp := VectorFromPoints(point, point2);
        DrawText('Vector from Point to Point X:' + FloatToStr(temp.X) + ', Y:' + FloatToStr(temp.Y), ColorWhite, FontNamed('Courier'), 10, 10);

        temp := VectorFromPointToRect(point, rect);
        DrawText('Vector from Point to Rectangle X:' + FloatToStr(temp.X) + ', Y:' + FloatToStr(temp.Y), ColorWhite, FontNamed('Courier'), 10, 30);

        temp := VectorFromPointToRect(point.X, point.Y, rect);
        DrawText('Vector from Point to Rectangle X:' + FloatToStr(temp.X) + ', Y:' + FloatToStr(temp.Y), ColorWhite, FontNamed('Courier'), 10, 50);

        temp := VectorFromPointToRect(point.X,point.Y, rect.X,rect.Y,rect.Width,rect.Height);
        DrawText('Vector from Point to Rectangle X:' + FloatToStr(temp.X) + ', Y:' + FloatToStr(temp.Y), ColorWhite, FontNamed('Courier'), 10, 70);

        DrawRectangle(ColorRed, rect);
        FillRectangle(ColorRed, point2.X - 1, point2.Y - 1, 3, 3);
        FillRectangle(ColorWhite, point.X - 1, point.Y - 1, 3, 3);

        temp := VectorToPoint(point);
        DrawText('Vector from Point X:' + FloatToStr(temp.X) + ', Y:' + FloatToStr(temp.Y), ColorWhite, FontNamed('Courier'), 10, 90);
	end;
	
	procedure TranslationTest(const drawIn: Rectangle);
	begin
		if not TT then
		begin
			point := PointAt(209, 209);
		    point2 := PointAt(309, 209);
		    tempPoint := PointAt(309, 209);
			rotF := 0.0;
			TT := true;
		end;
		
        if (KeyTyped(VK_SPACE)) then
        begin
			rotF := Round(rotF + 45) mod 360;
            v := VectorToPoint(point2);
            mat := TranslationMatrix(-point.X,-point.Y);
			mat := MatrixMultiply(RotationMatrix(rotF), mat);
			mat := MatrixMultiply(TranslationMatrix(point.X,point.Y), mat);
			v := MatrixMultiply(mat, v);
            tempPoint := PointAt(v.X, v.Y);
        end;
		
		DrawLine(RGBColor(100, 100, 100), point.X, point.Y, point2.X, point2.Y);
		DrawLine(RGBColor(100, 100, 100), point.X, point.Y, tempPoint.X, tempPoint.Y);
								
        FillRectangle(ColorWhite, point.X - 1, point.Y - 1, 3, 3);
        FillRectangle(ColorRed, point2.X - 1, point2.Y - 1, 3, 3);
        FillRectangle(ColorBlue, tempPoint.X - 1, tempPoint.Y - 1, 3, 3);
	end;
	
	procedure VectorInRectTest(const drawIn: Rectangle);
	begin
		
		if not VIWRT then
		begin
			rect := RectangleFrom(100, 200, 25, 25);
		    rect2 := RectangleFrom(300, 200, 50, 50);
		    v := VectorTo(2,2);
			VIWRT := true;
		end;
		
		v := MousePositionAsVector();
		
        v.X := ToWorldX(Round(v.X));
        v.Y := ToWorldY(Round(v.Y));
        
        DrawRectangle(ColorRed, rect);
        DrawRectangle(ColorRed, rect2);
        if (VectorInRect(v, rect)) then
        begin
            FillRectangle(ColorYellow, rect);
        end;
        if (VectorInRect(v, rect2.X, rect2.Y, rect2.Width, rect2.Height)) then
        begin
            FillRectangle(ColorYellow, rect2);
        end;
        DrawLine(ColorWhite, 0, 0, v.X, v.Y);
	end;
	
	procedure VectorAngle(const drawIn: Rectangle);
	var
		angle, v1a, v2a, rot: Single;
		rm: Matrix2D;
	const
		CX = 209;
		CY = 209;
		RADIUS = 20;
		LINE_LENGTH = 100;
	begin
		if not VA then
		begin
			v1 := VectorTo(LINE_LENGTH, 0);
			v2 := VectorTo(0 , LINE_LENGTH);
			VA := true;
		end;
		
		FillCircle(ColorRed, CX, CY, RADIUS);
        DrawLine(ColorRed, CX, CY, CX + v1.X, CY + v1.Y);
		DrawLine(ColorWhite, CX, CY, CX + v2.X, CY + v2.Y);

		angle := CalculateAngle(v2, v1);
		v1a := CalculateAngle(0, 0, v1.X, v1.Y);
		v2a := CalculateAngle(0, 0, v2.X, v2.Y);

		DrawText('White: ' + FloatToStr(v2a), ColorWhite, FontNamed('Courier'), 5, 50);
		DrawText('Red: ' + FloatToStr(v1a), ColorRed, FontNamed('Courier'), 5, 70);
		DrawText('Between: ' + FloatToStr(angle), ColorWhite, FontNamed('Courier'), 5, 90);

		rot := 0;
		rm := ScaleMatrix(0);

		if (KeyDown(VK_LEFT)) then rot := 5
		else if (KeyDown(VK_RIGHT)) then rot := -5;

		if (rot <> 0) then
		begin
  		rm := RotationMatrix(rot);
  		v2 := MatrixMultiply(rm, v2);
		end;

		rot := 0;

		if (KeyDown(VK_UP)) then rot := 5
		else if (KeyDown(VK_DOWN)) then rot := -5;

		if (rot <> 0) then
		begin
  		rm := RotationMatrix(rot);
  		v1 := MatrixMultiply(rm, v1);
		end;

		if (KeyTyped(VK_SPACE)) then
		begin
  		rm := RotationMatrix(-angle);
  		v1 := MatrixMultiply(rm, v1);
		end;
	end;
	
	procedure PointOutOfRect(const drawIn: Rectangle);
	var
		r, r2: Integer;
	const
		RW = 100;
		RH = 100;
		RX = 159;
		RY = 159;
		LINE_LENGTH = 100;
	begin
		if not POOR then
		begin
			p := PointAt(100 / 2 + RX, 100 / 2 + RY);
			rect := RectangleFrom(RX, RY, RW, RH);
			movement := VectorTo(100, 0);
			POOR := true;
		end;
		
		r := 1;
		r2 := 2;
		
		if ( KeyDown(VK_A)) then movement := MatrixMultiply(RotationMatrix(-4.0), movement);
		if ( KeyDown(VK_Z)) then movement := MatrixMultiply(RotationMatrix(4.0), movement);

		if ( KeyDown(VK_UP)) then p.Y := p.Y - 5;
		if ( KeyDown(VK_DOWN)) then p.Y := p.Y + 5;
		if ( KeyDown(VK_LEFT)) then p.X := p.X - 5;
		if ( KeyDown(VK_RIGHT)) then p.X := p.X + 5;

		mvOut := VectorOutOfRectFromPoint(p, rect, movement);
		
		if ( KeyDown(VK_SPACE)) then
		begin
			p.Y += mvOut.Y;
			p.X += mvOut.X;
			mvOut := VectorTo(0, 0);
		end;

		ClearScreen(ColorBlack);

		DrawRectangle(ColorRed, RX, RY, RW, RH);			
		DrawRectangle(ColorWhite, p.X - r, p.Y - r, r2, r2);
		DrawLine(ColorWhite, p.X, p.Y, p.X + movement.X, p.Y + movement.Y);

		if (false = ((mvOut.X = 0) and (mvOut.Y = 0))) then
		begin
			DrawLine(ColorGreen, p.X, p.Y, p.X + mvOut.X, p.Y + mvOut.Y);
		end;
	end;
	
	procedure RectOutOfRect(const drawIn: Rectangle);
	const
		RW = 100;
    	RH = 100;
    	RX = 159;
    	RY = 159;
    	LINE_LENGTH = 100;
	begin
		if not ROOR then
		begin
			movement := VectorTo(100, 0);
         halfH := 10;
         halfW := 5;

	      movement := VectorTo(100, 0);
	
	      tgtRect := RectangleFrom(RX, RY, RW, RH);
         mvRect := RectangleFrom(100 + RX, 100 + RY, 10, 20);
			ROOR := true;
		end;

		if (KeyDown(VK_A)) then movement := MatrixMultiply(RotationMatrix(-4.0), movement);
		if (KeyDown(VK_Z)) then movement := MatrixMultiply(RotationMatrix(4.0), movement);

		if (KeyTyped(VK_0)) then movement := VectorTo(100, 0);
		if (KeyTyped(VK_9)) then movement := MatrixMultiply(RotationMatrix(90.0), VectorTo(100, 0));
		if (KeyTyped(VK_8)) then movement := MatrixMultiply(RotationMatrix(180.0), VectorTo(100, 0));
		if (KeyTyped(VK_2)) then movement := MatrixMultiply(RotationMatrix(-90.0), VectorTo(100, 0));

		if (KeyDown(VK_UP)) then mvRect.Y := mvRect.Y - 5;
		if (KeyDown(VK_DOWN)) then mvRect.Y := mvRect.Y + 5;
		if (KeyDown(VK_LEFT)) then mvRect.X := mvRect.X - 5;
		if (KeyDown(VK_RIGHT)) then mvRect.X := mvRect.X + 5;

		mvOut := VectorOutOfRectFromRect(mvRect, tgtRect, movement);
    enterRectFrom := SideForCollisionTest(movement);
        
	    px := RectangleLeft(mvRect); 
	    py := RectangleTop(mvRect);
	
       case enterRectFrom of
			BottomLeft: px := RectangleRight(mvRect);
			BottomRight: ;//px := RectangleRight(mvRect);
			TopLeft:
				begin
				    px := RectangleRight(mvRect);
				    py := RectangleBottom(mvRect);
				end;
			TopRight: py := RectangleBottom(mvRect);
			Bottom: if px < RX then px := RectangleRight(mvRect);
			Right: if py < RY then py := RectangleBottom(mvRect);
			Left:
				begin
				    px := RectangleRight(mvRect);
 					 if py < RY then py := RectangleBottom(mvRect);
				end;
			Top:
			 	begin
					py := RectangleBottom(mvRect);
					if px < RX then px := RectangleRight(mvRect);
			 	end;
        end;

        if (KeyDown(VK_SPACE)) then
        begin
            mvRect.Y := mvRect.Y + mvOut.Y;
            mvRect.X := mvRect.X + mvOut.X;
            mvOut := VectorTo(0, 0);
        end;

        ClearScreen(ColorBlack);

        DrawRectangle(ColorRed, RX, RY, RW, RH);
        DrawRectangle(ColorWhite, mvRect.X , mvRect.Y , mvRect.Width, mvRect.Height);
        DrawLine(ColorWhite, mvRect.X + halfW, mvRect.Y + halfH, mvRect.X + halfW + movement.X, mvRect.Y + halfH + movement.Y);

        if (false = ((mvOut.X = 0) and (mvOut.Y = 0))) then
        begin
            DrawLine(RGBColor(100, 100, 100), mvRect.X, mvRect.Y, mvRect.X + mvOut.X, mvRect.Y + mvOut.Y);
            DrawRectangle(ColorGreen, mvRect.X + mvOut.X, mvRect.Y + mvOut.Y, mvRect.Width, mvRect.Height);
            DrawCircle(ColorRed, px, py, 2);
        end;
	end;
	
	procedure PointOutOfCircle(const drawIn: Rectangle);
	var
		r, r2: Integer;
	const
		CR = 100;
        CX = 209;
        CY = 209;
        LINE_LENGTH = 100;
	begin
		r := 1; r2 := 2;
		
		if not POOC then
		begin
			p := PointAt(100 / 2 + CX, 100 / 2 + CY);
            c := PointAt(CX, CY);
            movement := VectorTo(100, 0);
			POOC := true;
		end;

        if (KeyDown(VK_A)) then movement := MatrixMultiply(RotationMatrix(-4.0), movement);
        if (KeyDown(VK_Z)) then movement := MatrixMultiply(RotationMatrix(4.0), movement);

        if (KeyDown(VK_UP)) then p.Y := p.Y - 5;
        if (KeyDown(VK_DOWN)) then p.Y := p.Y + 5;
        if (KeyDown(VK_LEFT)) then p.X := p.X - 5;
        if (KeyDown(VK_RIGHT)) then p.X := p.X + 5;

        mvOut := VectorOutOfCircleFromPoint(p, CircleAt(c, CR), movement);

        if (KeyDown(VK_SPACE)) then
        begin
            p.Y := p.Y + mvOut.Y;
            p.X := p.X + mvOut.X;
            mvOut := VectorTo(0, 0);
        end;

        //ClearScreen(ColorBlack);

        DrawCircle(ColorRed, CX, CY, CR);
        DrawRectangle(ColorWhite, p.X - r, p.Y - r, r2, r2);
        DrawLine(ColorWhite, p.X, p.Y, p.X + movement.X, p.Y + movement.Y);

        if (false = ((mvOut.X = 0) and (mvOut.Y = 0))) then
        begin
            DrawLine(ColorGreen, p.X, p.Y, p.X + mvOut.X, p.Y + mvOut.Y);
        end;
	end;
	
	procedure CircleOutOfCircle(const drawIn: Rectangle);
	const
		CR = 100;
	    CX = 209;
	    CY = 209;
	    LINE_LENGTH = 100;
	    PR = 10;
	begin
		if not COOC then
		begin
			p := PointAt(100 / 2 + CX, 100 / 2 + CY);
            c := PointAt(CX, CY);
            movement := VectorTo(100, 0);
			COOC := true;
		end;
		
        if (KeyDown(VK_A)) then movement := MatrixMultiply(RotationMatrix(-4.0), movement);
        if (KeyDown(VK_Z)) then movement := MatrixMultiply(RotationMatrix(4.0), movement);

        if (KeyDown(VK_UP)) then p.Y := p.Y - 5;
        if (KeyDown(VK_DOWN)) then p.Y := p.Y + 5;
        if (KeyDown(VK_LEFT)) then p.X := p.X - 5;
        if (KeyDown(VK_RIGHT)) then p.X := p.X + 5;

        mvOut := VectorOutOfCircleFromCircle(CircleAt(p,PR), CircleAt(c, CR), movement);

        if (KeyDown(VK_SPACE)) then
        begin
            p.Y += mvOut.Y;
            p.X += mvOut.X;
            mvOut := VectorTo(0, 0);
        end;

        DrawCircle(ColorRed, CX, CY, CR);
        DrawCircle(ColorWhite, p.X, p.Y, PR);
        DrawLine(ColorWhite, p.X, p.Y, p.X + movement.X, p.Y + movement.Y);

        if (false = ((mvOut.X = 0) and (mvOut.Y = 0))) then
        begin
            DrawLine(ColorGreen, p.X, p.Y, p.X + mvOut.X, p.Y + mvOut.Y);
            DrawCircle(ColorGreen, p.X + mvOut.X, p.Y + mvOut.Y, PR);
        end;
	end;
	
	function GetPhysicsTests(): TestSuite;
	var
		i: Integer;
	begin
		result.Title := 'Physics Tests';
		SetLength(result.Tests, 15);
		
		for i := 0 to High(result.Tests) do
		begin
			InitTest(result.Tests[i]);
		end;
		
		with result.Tests[0] do
		begin
			MethodBeingTested := 'HasSpriteCollidedWith..., SpriteCollision';
			Instructions := 'Use the arrow keys to move the' + LineEnding + 'sprite around.' + LineEnding +
							'The sprite with red rectangle' + LineEnding + 'uses bounded collisions.' + LineEnding +
							'The sprite with blue rectangle' + LineEnding + 'uses non-bounded collisions.';
			ship := CreateSprite(BitmapNamed('Ship'), AnimationScriptNamed('ship_anim.txt'));
			SpriteStartAnimation(ship, 0);
			explosion := CreateSprite(BitmapNamed('BlueExplosion'), LoadAnimationScript('blue_anim_loop.txt'));
			SpriteStartAnimation(explosion, 0);
			smallball := BitmapNamed('SmallBall');
			ToRun := @CollisionSpriteTest;
		end;
		
		with result.Tests[1] do
		begin
			MethodBeingTested := 'BitmapCollision, BitmapRectCollision';
			Instructions := 'Use the arrow keys to move' + LineEnding + 'the sprite.' + LineEnding + 
							'This test is used to check' + LineEnding + 'if the collision works properly.';
			mediumball := BitmapNamed('BallImage1');
			rotShip := RotateScaleBitmap(BitmapNamed('Ship'), 45, 1.5);
			SetupBitmapForCollisions(rotShip);
			X := 0; Y := 0;
			ToRun := @CollissionBitmapTest;
		end;
		
		with result.Tests[2] do
		begin
			MethodBeingTested := 'BitmapCollision, CircleLineCollision, UnitVector, RotationMatrix';
			Instructions := 'Use the arrow keys to move' + LineEnding + 'the sprite.' +LineEnding+ 'The blue line represents the' + LineEnding + 'vector of the sprite.' + LineEnding +
							'A, D: Rotate the vector.' + LineEnding + 'W, S: Increase or decrese the' + LineEnding + 'magnitude.' + LineEnding + 
							'Space: Move ball out of line' + LineEnding + 'if colliding.';
			ball := CreateSprite(BitmapNamed('SmallBall'));
			X := 0; Y := 0;
			ball^.velocity.X := 1; ball^.velocity.Y := 1;
			ToRun := @CircleCollissionTest;
		end;
		
		with result.Tests[3] do
		begin
			MethodBeingTested := 'CollideCircles';
			Instructions := 'Use the following keys to run' + LineEnding +'the test:' + LineEnding + LineEnding + 'UP, DOWN: Increase or decrease' + LineEnding + 'the vector of the big ball.' + LineEnding +
							'LEFT, RIGHT: Rotate the vector' + LineEnding + 'of the big ball.' + LineEnding + LineEnding + 'W, S: Increase or decrease' + LineEnding + 'the vector of the small ball.' + LineEnding +
							'A, D: Rotate the vector of' + LineEnding + 'the small ball' + LineEnding + LineEnding +
							'T: Toggle the collision test' + LineEnding + 'SPACE: See the effect';
			bigball := CreateSprite(BitmapNamed('BallImage1'));
			ball^.velocity.X := 1; ball^.velocity.Y := 1;
	        SpriteSetMass(ball, 1);
	        SpriteSetX(ball, 150); SpriteSetY(ball, 150);
	        bigball^.velocity.X := 1; bigball^.velocity.Y := 1;
	        SpriteSetMass(bigball, 5);
	        SpriteSetX(bigball, 155); SpriteSetY(bigball, 155);
			ToRun := @CircleCollissionTest2;
		end;
		
		with result.Tests[4] do
		begin
			MethodBeingTested := 'RectLineCollision';
			Instructions := 'This test is used to test the' + LineEnding + 'collision between the rectangle' + LineEnding + 'and a line.' + LineEnding + LineEnding + 'Use the arrow keys to move the' + LineEnding + 'rectangle around.';
			ToRun := @RectangleCollisionTest;
		end;
		
		with result.Tests[5] do
		begin
			MethodBeingTested := 'RectLineCollision';
			Instructions := 'This test is used to test the' + LineEnding + 'mathematical calculation of' + LineEnding + 'a vector.' + LineEnding + LineEnding +
							'UP, DOWN: Change Vector1 X' + LineEnding +
							'LEFT, RIGHT: Change Vector1 Y' + LineEnding + LineEnding +
							'W, S: Change Vector2 X' + LineEnding +
							'A, D: Change Vector2 Y';
			v1 := VectorTo(2,2);
			v2 := VectorTo(2, 2, true);
			ToRun := @VectorMathsTest;
		end;
		
		with result.Tests[6] do
		begin
			MethodBeingTested := 'Calculate angle and vector, SpriteOnScreenAt, VectorFromCenterSpriteToPoint';
			Instructions := 'Use the arrow keys to move' + LineEnding + 'the small ball.' + LineEnding + LineEnding +
							'Use the ASDW keys to move' + LineEnding + 'the large ball.';
			ToRun := @SpriteTests;
		end;
		
		with result.Tests[7] do
		begin
			MethodBeingTested := 'VectorFromPoints, VectorFromPointToRect';
			Instructions := 'Use the arrow keys to move' + LineEnding + 'the point around.';
			point := PointAt(100, 100);
		    point2 := PointAt(150, 150);
		    rect := RectangleFrom(200, 200, 50, 50);
			ToRun := @PointTest;
		end;
		
		with result.Tests[8] do
		begin
			MethodBeingTested := 'Matrix Multiplication, Translation';
			Instructions := 'Press Space to Rotate the vector' + LineEnding + 'by 45 degrees.';
			ToRun := @TranslationTest;
		end;
		
		with result.Tests[9] do
		begin
			MethodBeingTested := 'VectorInRect';
			Instructions := 'This test is used to check if' + LineEnding + 'a vector is within a rectangle.' + LineEnding + LineEnding + 
							'Use the Mouse to change' + LineEnding + 'the vector';
			ToRun := @VectorInRectTest;
		end;
		
		with result.Tests[10] do
		begin
			MethodBeingTested := 'CalculateAngle, Matrix Multiply, Rotation Matrix';
			Instructions := 'LEFT, RIGHT: Control the white' + LineEnding + 'line.' + LineEnding +
							'UP, DOWN: Control the red line' + LineEnding +
							'Space: Move the red over white';
			ToRun := @VectorAngle;
		end;
		
		with result.Tests[11] do
		begin
			MethodBeingTested := 'VectorOutOfRectFromPoint';
			Instructions := 'Use the arrow keys to move' + LineEnding + 'the point.' + LineEnding +
							'A, Z: Rotate the movement' + LineEnding +
							'Space: Move the point out';
			ToRun := @PointOutOfRect;
		end;
		
		with result.Tests[12] do
		begin
			MethodBeingTested := 'VectorOutOfRectFromRect';
			Instructions := 'Use the arrow keys to move' + LineEnding + 'the point.' + LineEnding +
							'A, Z: Rotate the movement' + LineEnding +
							'Space: Move the rectangle out' + LineEnding + LineEnding +
							'0 = set movement to 0 degrees' + LineEnding +
							'9 = set movement to 90 degrees' + LineEnding +
							'8 = set movement to 180 degrees' + LineEnding +
							'2 = set movement to 270 degrees' + LineEnding;
			ToRun := @RectOutOfRect;
		end;
		
		with result.Tests[13] do
		begin
			MethodBeingTested := 'VectorOutOfCircleFromPoint';
			Instructions := 'Use the arrow keys to move' + LineEnding + 'the point.' + LineEnding +
							'A, Z: Rotate the movement' + LineEnding +
							'Space: Move the point out';
			ToRun := @PointOutOfCircle;
		end;
		
		with result.Tests[14] do
		begin
			MethodBeingTested := 'VectorOutOfCircleFromCircle';
			Instructions := 'Use the arrow keys to move' + LineEnding + 'the point.' + LineEnding +
							'A, Z: Rotate the movement' + LineEnding +
							'Space: Move the point out';
			ToRun := @CircleOutOfCircle;
		end;
	end;
	
	procedure AddPhysicsSuite(var suites: TestSuites);
	begin
		suites[High(suites)] := GetPhysicsTests();
	end;

end.