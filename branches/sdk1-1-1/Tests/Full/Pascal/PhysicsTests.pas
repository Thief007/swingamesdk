unit PhysicsTests;

interface
	uses TestFramework;

	procedure AddPhysicsSuite(var suites: TestSuites); 

implementation
	uses GameResources, SGSDK_Core, SGSDK_Physics, SGSDK_Input, SGSDK_Graphics, SGSDK_KeyCodes, SGSDK_Shapes, SGSDK_Font, SGSDK_Camera, SysUtils;
	
	var
		ship, explosion, ball, bigball: Sprite;
		smallball, mediumball: Bitmap;
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
	    //ClearScreen(ColourWhite);
	    //ship.Movement := CreateVector(1,1));
	    UpdateSpriteAnimation(ship);
	    ship.UsePixelCollision := true;
	    //Drawship.Movement.X + ', ' + ship.Movement.Y, ColourWhite, GameFont('Courier'), 10, 200);

	    if (IsKeyPressed(VK_LEFT)) then
	    begin
	        ship.X := ship.X - 1;
	    end;
	    if (IsKeyPressed(VK_RIGHT)) then
	    begin
	        ship.X := ship.X + 1;
	    end;
	    if (IsKeyPressed(VK_DOWN)) then
	    begin
	        ship.Y := ship.Y + 1;
	    end;
	    if (IsKeyPressed(VK_UP)) then
	    begin
	        ship.Y := ship.Y - 1;
	    end;

	    //HasSpriteCollidedX
	    DrawVerticalLine(ColourRed, 400, 0, 418);
	    if (HasSpriteCollidedX(ship, 400, CollisionRangeEquals)) then
	    begin
	        DrawRectangle(ColourWhite, 395, 405, 10, 10);
	    end;
	    if (HasSpriteCollidedX(ship, 400, CollisionRangeGreaterThan)) then
	    begin
	        DrawRectangle(ColourWhite, 407, 405, 10, 10);
	    end;
	    if (HasSpriteCollidedX(ship, 400, CollisionRangeLessThan)) then
	    begin
	        DrawRectangle(ColourWhite, 383, 405, 10, 10);
	    end;

	    //HasSpriteCollidedY
	    DrawHorizontalLine(ColourBlue, 400, 0, 418);
	    if (HasSpriteCollidedY(ship, 400, CollisionRangeEquals)) then
	    begin
	        DrawRectangle(ColourWhite, 10, 395, 10, 10);
	    end;
	    if (HasSpriteCollidedY(ship, 400, CollisionRangeGreaterThan)) then
	    begin
	        DrawRectangle(ColourWhite, 10, 407, 10, 10);
	    end;
	    if (HasSpriteCollidedY(ship, 400, CollisionRangeLessThan)) then
	    begin
	        DrawRectangle(ColourWhite, 10, 383, 10, 10);
	    end;

	    //HasSpriteCollidedWithRect

	    DrawRectangle(ColourRed, 10, 10, 10, 10);
	    if (HasSpriteCollidedWithRect(ship, 10, 10, 10, 10)) then
	    begin
	        FillRectangle(ColourGreen, 10, 10, 10, 10);
	    end;

	    DrawRectangle(ColourGreen, 10, 25, 10, 10);
	    if (HasSpriteCollidedWithRect(ship, CreateRectangle(10, 25, 10, 10))) then
	    begin
	        FillRectangle(ColourGreen, 10, 25, 10, 10);
	    end;

	    //HaveSpritesCollided
	    explosion.UsePixelCollision := true;

	    explosion.X := 70;
	    explosion.Y := 10;
	    UpdateSpriteAnimation(explosion);
	    //FillRectangle(ColourWhite, 64, 4, 45, 45);
	    if (HaveSpritesCollided(ship, explosion)) then
	    begin
	        DrawRectangle(ColourBlue, 65, 5, 78, 78);
	        DrawRectangle(ColourBlue, 64, 4, 80, 80);
	    end;
	    DrawSprite(explosion);

	    //HasSpriteCollidedWithBitmap
	    if (HasSpriteCollidedWithBitmap(ship, smallball, 10,100)) then
	    begin
	        DrawRectangle(ColourRed, true, 5, 95, smallball.Width + 10, smallball.Height + 10);
	    end;
	    DrawBitmap(smallball, 10, 100);
	    DrawRectangle(ColourWhite, 10, 100, smallball.Width, smallball.Height);

	    if (HasSpriteCollidedWithBitmap(ship, smallball, 10, 150, false)) then
	    begin
	        DrawRectangle(ColourBlue, true, 5, 145, smallball.Width + 10, smallball.Height + 10);
	    end;
	    DrawBitmap(smallball, 10, 150);
	    DrawRectangle(ColourWhite, 10, 150, smallball.Width, smallball.Height);
	    //using points
	    if (HasSpriteCollidedWithBitmap(ship, smallball, CreatePoint(70,100),CreateRectangle(smallball), false)) then
	    begin
	        DrawRectangle(ColourBlue, true, 65, 95, smallball.Width + 10, smallball.Height + 10);
	    end;
	    DrawBitmap(smallball, 70, 100);
	    DrawRectangle(ColourWhite, 70, 100, smallball.Width, smallball.Height);

	    if (HasSpriteCollidedWithBitmap(ship, smallball, CreatePoint(70, 150), false)) then
	    begin
	        DrawRectangle(ColourBlue, true, 65, 145, smallball.Width + 10, smallball.Height + 10);
	    end;
	    DrawBitmap(smallball, 70, 150);
	    DrawRectangle(ColourWhite, 70, 150, smallball.Width, smallball.Height);

	    DrawSprite(ship);
	    DrawRectangle(ColourWhite, ship.X, ship.Y, CurrentWidth(ship), CurrentHeight(ship));
	end;
	
	procedure CollissionBitmapTest(const drawIn: Rectangle);
	begin
		if (IsKeyPressed(VK_LEFT)) then
	    begin
	        X := X - 1;
	    end;
	    if (IsKeyPressed(VK_RIGHT)) then
	    begin
	        X := X + 1;
	    end;
	    if (IsKeyPressed(VK_DOWN)) then
	    begin
	        Y := Y + 1;
	    end;
	    if (IsKeyPressed(VK_UP)) then
	    begin
	        Y := Y - 1;
	    end;
	    //HaveBitmapsCollided
	    DrawText('Default', ColourWhite, GameFont('Courier'), 10, 5);
	    if (HaveBitmapsCollided(mediumball, 10, 30, smallball, X, Y)) then
	    begin
	        FillRectangle(ColourBlue, 5, 25, mediumball.Height + 10, mediumball.Width + 10);
	    end;
	    DrawBitmap(mediumball, 10, 30);

	    DrawRectangle(ColourWhite, 10, 30, mediumball.Width, mediumball.Height);

	    if (HaveBitmapsCollided(mediumball, CreatePoint(10, 150), smallball, CreatePoint(X, Y))) then
	    begin
	        FillRectangle(ColourPink, 5, 145, mediumball.Height + 10, mediumball.Width + 10);
	    end;
	    DrawBitmap(mediumball, 10, 150);
	    DrawRectangle(ColourWhite, 10, 150, mediumball.Width, mediumball.Height);

	    if (HaveBitmapsCollided(mediumball, CreatePoint(10, 270), CreateRectangle(mediumball), smallball, CreatePoint(X, Y), CreateRectangle(smallball))) then
	    begin
	        FillRectangle(ColourPink, 5, 265, mediumball.Height + 10, mediumball.Width + 10);
	    end;
	    DrawBitmap(mediumball, 10, 270);
	    DrawRectangle(ColourWhite, 10, 270, mediumball.Width, mediumball.Height);



	    DrawText('No Bound', ColourWhite, GameFont('Courier'), 120, 5);
	    if (HaveBitmapsCollided(mediumball, 120, 30, false, smallball, X, Y, false)) then
	    begin
	        FillRectangle(ColourBlue, 115, 25,  mediumball.Height + 10, mediumball.Width + 10);
	    end;
	    DrawBitmap(mediumball, 120, 30);
	    DrawRectangle(ColourWhite, 120, 30, mediumball.Width, mediumball.Height);

	    if (HaveBitmapsCollided(mediumball, CreatePoint(120, 150),false, smallball, CreatePoint(X, Y), false)) then
	    begin
	        FillRectangle(ColourBlue, 115, 145, mediumball.Height + 10, mediumball.Width + 10);
	    end;
	    DrawBitmap(mediumball, 120, 150);
	    DrawRectangle(ColourWhite, 120, 150, mediumball.Width, mediumball.Height);

	    if (HaveBitmapsCollided(mediumball, CreatePoint(120, 270),CreateRectangle(mediumball), false, smallball, CreatePoint(X, Y),  CreateRectangle(smallball),false)) then
	    begin
	        FillRectangle(ColourBlue, 115, 265, mediumball.Height + 10, mediumball.Width + 10);
	    end;
	    DrawBitmap(mediumball, 120, 270);
	    DrawRectangle(ColourWhite, 120, 270, mediumball.Width, mediumball.Height);

	    DrawText('Big ball Bound', ColourWhite, GameFont('Courier'), 240, 5);
	    if (HaveBitmapsCollided(mediumball, 240, 30, true, smallball, X, Y, false)) then
	    begin
	        FillRectangle(ColourBlue, 235, 25, mediumball.Height + 10, mediumball.Width + 10);
	    end;
	    DrawBitmap(mediumball, 240, 30);
	    DrawRectangle(ColourWhite, 240, 30, mediumball.Width, mediumball.Height);

	    DrawText('small ball Bound', ColourWhite, GameFont('Courier'), 240, 150);
	    if (HaveBitmapsCollided(mediumball, CreatePoint(240, 170), false, smallball, CreatePoint(X, Y), true)) then
	    begin
	        FillRectangle(ColourBlue, 235, 165, mediumball.Height + 10, mediumball.Width + 10);
	    end;
	    DrawBitmap(mediumball, 240, 170);
	    DrawRectangle(ColourWhite, 240, 170, mediumball.Width, mediumball.Height);

	    //HasBitmapCollidedWithRect
	    DrawRectangle(ColourGreen, 400, 350, 10, 10);
	    if (HasBitmapCollidedWithRect(smallball, X, Y, 400, 350, 10, 10)) then
	    begin
	        FillRectangle(ColourGreen, 400, 350, 10, 10);
	    end;

	    DrawRectangle(ColourGreen, 400, 365, 10, 10);
	    if (HasBitmapCollidedWithRect(smallball, X, Y, CreateRectangle(400, 365, 10, 10))) then
	    begin
	        FillRectangle(ColourGreen, 400, 365, 10, 10);
	    end;
		
	    DrawBitmap(smallball,X,Y);
	    DrawRectangle(ColourWhite, X, Y, smallball.Width, smallball.Height);
	end;
	
	procedure CircleCollissionTest(const drawIn: Rectangle);
	var
		tempX, tempY: Single;
	begin
	   if (IsKeyPressed(VK_LEFT)) then
	    begin
	        ball.X := ball.X - 1;
	    end;
	    if (IsKeyPressed(VK_RIGHT)) then
	    begin
	        ball.X := ball.X + 1;
	    end;
		
	    if (IsKeyPressed(VK_S)) then
	    begin
	        if (Magnitude(ball.Movement) > 2) then
	        begin
	            ball.Movement := MultiplyVector(GetUnitVector(ball.Movement), Magnitude(ball.Movement) - 1);
	        end;
	    end;
	    if (IsKeyPressed(VK_DOWN)) then
	    begin
	        ball.Y := ball.Y + 1;
	    end;
	    if (IsKeyPressed(VK_UP)) then
	    begin
	        ball.Y := ball.Y - 1;
	    end;
		
	    if (IsKeyPressed(VK_W)) then
	    begin
	        ball.Movement := MultiplyVector(GetUnitVector(ball.Movement), Magnitude(ball.Movement) + 1);
	    end;
	    if (IsKeyPressed(VK_A)) then
	    begin
	        ball.Movement := Multiply(RotationMatrix(1),ball.Movement);
	    end;
	    if (IsKeyPressed(VK_D)) then
	    begin
	        ball.Movement := Multiply(RotationMatrix(-1),ball.Movement);
	    end;
	    //CircleHasCollidedWithLine
	    if (CircleHasCollidedWithLine(ball, CreateLine(300, 0, 300, 418))) then
	    begin
	        FillRectangle(ColourRed, 298, 0, 5, 418);
	    end;
	    DrawVerticalLine(ColourRed, 300, 0, 418);
		
	    //CircleHasCollidedWithLine
	    if (CircleHasCollidedWithLine(ball, CreateLine(300, 0, 300, 418)) and IsKeyPressed(VK_SPACE)) then
	    begin
	        CircleCollisionWithLine(ball, CreateLine(300, 0, 300, 418));
	        UpdateSprite(ball);
	    end;
		
	    DrawSprite(ball);
	    tempX := ball.X + (CurrentWidth(ball) /2);
	    tempY := ball.Y + (CurrentHeight(ball) / 2);
	    DrawLine(ColourBlue, tempX, tempY, tempX + ball.Movement.X, tempY + ball.Movement.Y);
	    DrawRectangle(ColourWhite, ball.X, ball.Y, CurrentWidth(ball), CurrentHeight(ball));
	end;
	
	var toggleRunCircle: Boolean = false;
	
	procedure CircleCollissionTest2(const drawIn: Rectangle);
	var
		tempX, tempY, bigtempX, bigtempY: Single;
		
		procedure MoveBallUsingVector(var ball : Sprite);
		begin
			MoveSprite(ball, ball.movement);
			
			if ball.x > drawIn.width - CurrentWidth(ball) then
			begin
				ball.movement.x := -ball.movement.x;
				ball.x := drawIn.width - CurrentWidth(ball);
			end;
			if ball.y > drawIn.height - CurrentHeight(ball) then
			begin
				ball.movement.y := -ball.movement.y;
				ball.y := drawIn.height - CurrentHeight(ball);
			end;
			if ball.x < drawIn.x then
			begin
				ball.movement.x := -ball.movement.x;
				ball.x := drawIn.x;
			end;
			if ball.y < 0 then
			begin
				ball.movement.y := -ball.movement.y;
				ball.y := 0;
			end;
		end;

	begin
		if WasKeyTyped(VK_T) then toggleRunCircle := not toggleRunCircle;
			
		if (IsKeyPressed(VK_DOWN)) then
        begin
            if (Magnitude(bigball.Movement) > 2) then
            begin
                bigball.Movement := MultiplyVector(GetUnitVector(bigball.Movement), Magnitude(bigball.Movement) - 1);
            end;
        end;
        if (IsKeyPressed(VK_UP)) then
        begin
            bigball.Movement := MultiplyVector(GetUnitVector(bigball.Movement), Magnitude(bigball.Movement) + 1);
        end;
        if (IsKeyPressed(VK_LEFT)) then
        begin
            bigball.Movement := Multiply(RotationMatrix(-1), bigball.Movement);
        end;
        if (IsKeyPressed(VK_RIGHT)) then
        begin
            bigball.Movement := Multiply(RotationMatrix(1), bigball.Movement);
        end;

        if (IsKeyPressed(VK_S)) then
        begin
            if (Magnitude(ball.Movement) > 2) then
            begin
                ball.Movement := MultiplyVector(GetUnitVector(ball.Movement), Magnitude(ball.Movement) - 1);
            end;
        end;
        if (IsKeyPressed(VK_W)) then
        begin
            ball.Movement := MultiplyVector(GetUnitVector(ball.Movement), Magnitude(ball.Movement) + 1);
        end;
        if (IsKeyPressed(VK_A)) then
        begin
            ball.Movement := Multiply(RotationMatrix(-1), ball.Movement);
        end;
        if (IsKeyPressed(VK_D)) then
        begin
            ball.Movement := Multiply(RotationMatrix(1), ball.Movement);
        end;
        if (not toggleRunCircle) and (WasKeyTyped(VK_SPACE)) then
        begin
            if (HaveSpritesCollided(ball, bigball)) then
            begin
                CircularCollision(ball, bigball);
                UpdateSprite(ball);
                UpdateSprite(bigball);
            end
            else
            begin
                ball.X := 150;
                ball.Y := 150;
                bigball.X := 155;
                bigball.Y := 155;
            end;
        end;
        if toggleRunCircle then
		  begin
           MoveBallUsingVector(ball);
           MoveBallUsingVector(bigball);

           if (HaveSpritesCollided(ball, bigball)) then
           begin
               CircularCollision(ball, bigball);
               UpdateSprite(ball);
               UpdateSprite(bigball);
           end;				
		  end;

        DrawSprite(bigball);
        DrawSprite(ball);
        tempX := ball.X + (CurrentWidth(ball) / 2);
        tempY := ball.Y + (CurrentHeight(ball) / 2);
        bigtempX := bigball.X + (CurrentWidth(bigball) / 2);
        bigtempY := bigball.Y + (CurrentHeight(bigball) / 2);
        DrawLine(ColourBlue, tempX, tempY, tempX + ball.Movement.X, tempY + ball.Movement.Y);
        DrawLine(ColourBlue, bigtempX, bigtempY, bigtempX + bigball.Movement.X, bigtempY + bigball.Movement.Y);
        //DrawRectangle(ColourWhite, ball.X, ball.Y, CurrentWidth(ball), CurrentHeight(ball));
	end;
	
	procedure RectangleCollisionTest(const drawIn: Rectangle);
	begin
		if (IsKeyPressed(VK_LEFT)) then
        begin
            X := X - 1;
        end;
        if (IsKeyPressed(VK_RIGHT)) then
        begin
            X := X + 1;
        end;
        if (IsKeyPressed(VK_DOWN)) then
        begin
            Y := Y + 1;
        end;
        if (IsKeyPressed(VK_UP)) then
        begin
            Y := Y - 1;
        end;
        //RectangleHasCollidedWithLine
        if (RectangleHasCollidedWithLine(CreateRectangle(X,Y, 50,50), CreateLine(100, 0, 100, 418))) then
        begin
            FillRectangle(ColourRed, 98, 0, 5, 418);
        end;
        if (RectangleHasCollidedWithLine(CreateRectangle(X, Y, 50, 50), CreateLine(200, 0, 200, 418))) then
        begin
            FillRectangle(ColourRed, 198, 0, 5, 418);
        end;
        DrawRectangle(ColourWhite, CreateRectangle(X, Y, 50, 50));
        DrawLine(ColourYellow, CreateLine(100, 0, 100, 418));
        DrawLine(ColourYellow, CreateLine(200, 0, 200, 418));
	end;
	
	procedure VectorMathsTest(const drawIn: Rectangle);
	var
		temp: Vector;
	begin
		if (IsKeyPressed(VK_LEFT)) then
        begin
            v1.Y := v1.Y - 1;
        end;
        if (IsKeyPressed(VK_RIGHT)) then
        begin
            v1.Y := v1.Y + 1;
        end;
        if (IsKeyPressed(VK_DOWN)) then
        begin
            v1.X := v1.X - 1;
        end;
        if (IsKeyPressed(VK_UP)) then
        begin
            v1.X := v1.X + 1;
        end;

        if (IsKeyPressed(VK_A)) then
        begin
            v2.Y := v2.Y - 1;
        end;
        if (IsKeyPressed(VK_D)) then
        begin
            v2.Y := v2.Y + 1;
        end;
        if (IsKeyPressed(VK_S)) then
        begin
            v2.X := v2.X - 1;
        end;
        if (IsKeyPressed(VK_W)) then
        begin
            v2.X := v2.X + 1;
        end;

        DrawText('Vector 1 := X:' + FloatToStr(v1.X) + ', Y:' + FloatToStr(v1.Y), ColourWhite, GameFont('Courier'), 10, 10);
        DrawText('Vector 2 := X:' + FloatToStr(v2.X) + ', Y:' + FloatToStr(v2.Y), ColourWhite, GameFont('Courier'), 10, 30);
        temp := AddVectors(v1, v2);
        DrawText('Vector 1 + Vector 2 := X:' + FloatToStr(temp.X) + ', Y:' + FloatToStr(temp.Y), ColourWhite, GameFont('Courier'), 10, 50);
        temp := SubtractVectors(v1, v2);
        DrawText('Vector 1 - Vector 2 := X:' + FloatToStr(temp.X) + ', Y:' + FloatToStr(temp.Y), ColourWhite, GameFont('Courier'), 10, 70);
        temp := LimitMagnitude(v1, 20);
        DrawText('limt Vector 1 to 20 := X:' + FloatToStr(temp.X) + ', Y:' + FloatToStr(temp.Y), ColourWhite, GameFont('Courier'), 10, 90);
        temp := InvertVector(v1);
        DrawText('Vector 1 inverted := X:' + FloatToStr(temp.X) + ', Y:' + FloatToStr(temp.Y), ColourWhite, GameFont('Courier'), 10, 110);
        if (IsZeroVector(v1)) then
        begin
            DrawText('Vector 1 is zero', ColourWhite, GameFont('Courier'), 10, 130);
        end
        else
        begin
            DrawText('Vector 1 is not zero', ColourWhite, GameFont('Courier'), 10, 130);
        end;
	end;
	
	procedure SpriteTests(const drawIn: Rectangle);
	var
		temp: Single;
		tempV: Vector;
	begin
		if not SprSt then
		begin
			ball.xPos := 100;
	        ball.yPos := 300;
	        bigball.xPos := 200;
	        bigball.yPos := 300;
			SprSt := true;
		end;
		
		if (IsKeyPressed(VK_UP)) then ball.Y := ball.Y - 1;
        if (IsKeyPressed(VK_DOWN)) then ball.Y := ball.Y + 1;
        if (IsKeyPressed(VK_LEFT)) then ball.X := ball.X - 1;
        if (IsKeyPressed(VK_RIGHT)) then ball.X := ball.X + 1;

        if (IsKeyPressed(VK_W)) then bigball.Y := bigball.Y - 1;
        if (IsKeyPressed(VK_S)) then bigball.Y := bigball.Y + 1;
        if (IsKeyPressed(VK_A)) then bigball.X := bigball.X - 1;
        if (IsKeyPressed(VK_D)) then bigball.X := bigball.X + 1;

        DrawSprite(bigball);
        DrawSprite(ball);
        //IsSpriteOnScreenAt
        if (IsSpriteOnScreenAt(ball, CreatePoint(ScreenX(209), ScreenY(209)))) then
        begin
            DrawText('The small ball is on the screen ', ColourWhite, GameFont('Courier'), 10, 10);
            DrawText('at X:209, Y:209    X:' + FloatToStr(ball.x) + ', Y:' + FloatToStr(ball.y), ColourWhite, GameFont('Courier'), 10, 30);
        end
        else
        begin
            DrawText('The small ball is not on the screen ', ColourWhite, GameFont('Courier'), 10, 10);
            DrawText('at X:209, Y:209    X:' + FloatToStr(ball.x) + ', Y:' + FloatToStr(ball.y), ColourWhite, GameFont('Courier'), 10, 30);
        end;
        if (IsSpriteOnScreenAt(bigball, ScreenX(209), ScreenY(209))) then
        begin
            DrawText('The large ball is on the screen ', ColourWhite, GameFont('Courier'), 10, 50);
            DrawText('at X:209, Y:209    X:' + FloatToStr(bigball.x) + ', Y:' + FloatToStr(bigball.y), ColourWhite, GameFont('Courier'), 10, 70);
        end
        else
        begin
            DrawText('The large ball is not on the screen ', ColourWhite, GameFont('Courier'), 10, 50);
            DrawText('at X:209, Y:209    X:' + FloatToStr(bigball.x) + ', Y:' + FloatToStr(bigball.y), ColourWhite, GameFont('Courier'), 10, 70);
        end;
        DrawPixel(ColourWhite, 209, 209);
        //CalculateAngle
        temp := CalculateAngle(ball, bigball);
        //DrawText(CenterPoint(ball).X + ', ' + CenterPoint(ball).Y + ' '+ CenterPoint(bigball).X + ', ' + CenterPoint(bigball).Y, ColourWhite, GameFont('Courier'), 10, 110);
        DrawText('The angle betwen the balls is ' + FloatToStr(temp), ColourWhite, GameFont('Courier'), 10, 90);

        //CalculateVectorFromTo
        tempV := CalculateVectorFromTo(ball, bigball);
        DrawText('The vector from the small ball ', ColourWhite, GameFont('Courier'), 10, 110);
        DrawText('to the large ball is X:' + FloatToStr(tempV.X) + ', Y:' + FloatToStr(tempV.Y), ColourWhite, GameFont('Courier'), 10, 130);

        //VectorFromCenterSpriteToPoint
        tempV := VectorFromCenterSpriteToPoint(ball, CreatePoint(100, 200));
        DrawText('Vector from ball to point X:' + FloatToStr(tempV.X) + ', Y:' + FloatToStr(tempV.Y), ColourWhite, GameFont('Courier'), 10, 150);
        FillRectangle(ColourRed, 100 - 1, 200 - 1, 3, 3);
	end;
	
	procedure PointTest(const drawIn: Rectangle);
	var
		temp: Vector;
	begin
		if (IsKeyPressed(VK_UP)) then point.Y := point.Y - 1;
        if (IsKeyPressed(VK_DOWN)) then point.Y := point.Y + 1;
        if (IsKeyPressed(VK_LEFT)) then point.X := point.X - 1;
        if (IsKeyPressed(VK_RIGHT)) then point.X := point.X + 1;

        temp := VectorFromPoints(point, point2);
        DrawText('Vector from Point to Point X:' + FloatToStr(temp.X) + ', Y:' + FloatToStr(temp.Y), ColourWhite, GameResources.GameFont('Courier'), 10, 10);

        temp := VectorFromPointToRectangle(point, rect);
        DrawText('Vector from Point to Rectangle X:' + FloatToStr(temp.X) + ', Y:' + FloatToStr(temp.Y), ColourWhite, GameResources.GameFont('Courier'), 10, 30);

        temp := VectorFromPointToRectangle(point.X, point.Y, rect);
        DrawText('Vector from Point to Rectangle X:' + FloatToStr(temp.X) + ', Y:' + FloatToStr(temp.Y), ColourWhite, GameResources.GameFont('Courier'), 10, 50);

        temp := VectorFromPointToRectangle(point.X,point.Y, rect.X,rect.Y,rect.Width,rect.Height);
        DrawText('Vector from Point to Rectangle X:' + FloatToStr(temp.X) + ', Y:' + FloatToStr(temp.Y), ColourWhite, GameResources.GameFont('Courier'), 10, 70);

        DrawRectangle(ColourRed, rect);
        FillRectangle(ColourRed, point2.X - 1, point2.Y - 1, 3, 3);
        FillRectangle(ColourWhite, point.X - 1, point.Y - 1, 3, 3);

        temp := PointToVector(point);
        DrawText('Vector from Point X:' + FloatToStr(temp.X) + ', Y:' + FloatToStr(temp.Y), ColourWhite, GameResources.GameFont('Courier'), 10, 90);
	end;
	
	procedure TranslationTest(const drawIn: Rectangle);
	begin
		if not TT then
		begin
			point := CreatePoint(209, 209);
		    point2 := CreatePoint(309, 209);
		    tempPoint := CreatePoint(309, 209);
			rotF := 0.0;
			TT := true;
		end;
		
        if (WasKeyTyped(VK_SPACE)) then
        begin
			rotF := Round(rotF + 45) mod 360;
            v := PointToVector(point2);
            mat := TranslationMatrix(-point.X,-point.Y);
			mat := Multiply(RotationMatrix(rotF), mat);
			mat := Multiply(TranslationMatrix(point.X,point.Y), mat);
			v := Multiply(mat, v);
            tempPoint := CreatePoint(v.X, v.Y);
        end;
		
		DrawLine(GetColor(100, 100, 100), point.X, point.Y, point2.X, point2.Y);
		DrawLine(GetColor(100, 100, 100), point.X, point.Y, tempPoint.X, tempPoint.Y);
								
        FillRectangle(ColourWhite, point.X - 1, point.Y - 1, 3, 3);
        FillRectangle(ColourRed, point2.X - 1, point2.Y - 1, 3, 3);
        FillRectangle(ColourBlue, tempPoint.X - 1, tempPoint.Y - 1, 3, 3);
	end;
	
	procedure VectorIsWithinRectTest(const drawIn: Rectangle);
	begin
		
		if not VIWRT then
		begin
			rect := CreateRectangle(100, 200, 25, 25);
		    rect2 := CreateRectangle(300, 200, 50, 50);
		    v := CreateVector(2,2);
			VIWRT := true;
		end;
		
		v := GetMousePositionAsVector();
		
        v.X := GameX(Round(v.X));
        v.Y := GameY(Round(v.Y));
        
        DrawRectangle(ColourRed, rect);
        DrawRectangle(ColourRed, rect2);
        if (VectorIsWithinRect(v, rect)) then
        begin
            FillRectangle(ColourYellow, rect);
        end;
        if (VectorIsWithinRect(v, rect2.X, rect2.Y, rect2.Width, rect2.Height)) then
        begin
            FillRectangle(ColourYellow, rect2);
        end;
        DrawLine(ColourWhite, 0, 0, v.X, v.Y);
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
			v1 := CreateVector(LINE_LENGTH, 0);
			v2 := CreateVector(0 , LINE_LENGTH);
			VA := true;
		end;
		
		FillCircle(ColourRed, CX, CY, RADIUS);
        DrawLine(ColourRed, CX, CY, CX + v1.X, CY + v1.Y);
		DrawLine(ColourWhite, CX, CY, CX + v2.X, CY + v2.Y);

		angle := CalculateAngleBetween(v1, v2);
		v1a := CalculateAngle(0, 0, v1.X, v1.Y);
		v2a := CalculateAngle(0, 0, v2.X, v2.Y);

		DrawText('White: ' + FloatToStr(v2a), ColourWhite, GameResources.GameFont('Courier'), 5, 50);
		DrawText('Red: ' + FloatToStr(v1a), ColourRed, GameResources.GameFont('Courier'), 5, 70);
		DrawText('Between: ' + FloatToStr(angle), ColourWhite, GameResources.GameFont('Courier'), 5, 90);

		rot := 0;
		rm := ScaleMatrix(0);

		if (IsKeyPressed(VK_LEFT)) then rot := 5
		else if (IsKeyPressed(VK_RIGHT)) then rot := -5;

		if (rot <> 0) then
		begin
		rm := RotationMatrix(rot);
		v2 := Multiply(rm, v2);
		end;

		rot := 0;

		if (IsKeyPressed(VK_UP)) then rot := 5
		else if (IsKeyPressed(VK_DOWN)) then rot := -5;

		if (rot <> 0) then
		begin
		rm := RotationMatrix(rot);
		v1 := Multiply(rm, v1);
		end;

		if (WasKeyTyped(VK_SPACE)) then
		begin
		rm := RotationMatrix(angle);
		v1 := Multiply(rm, v1);
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
			p := CreatePoint(100 / 2 + RX, 100 / 2 + RY);
			rect := CreateRectangle(RX, RY, RW, RH);
			movement := CreateVector(100, 0);
			POOR := true;
		end;
		
		r := 1;
		r2 := 2;
		
		if ( IsKeyPressed(VK_A)) then movement := Multiply(RotationMatrix(-4.0), movement);
		if ( IsKeyPressed(VK_Z)) then movement := Multiply(RotationMatrix(4.0), movement);

		if ( IsKeyPressed(VK_UP)) then p.Y := p.Y - 5;
		if ( IsKeyPressed(VK_DOWN)) then p.Y := p.Y + 5;
		if ( IsKeyPressed(VK_LEFT)) then p.X := p.X - 5;
		if ( IsKeyPressed(VK_RIGHT)) then p.X := p.X + 5;

		mvOut := VectorOutOfRectFromPoint(p, rect, movement);
		
		if ( IsKeyPressed(VK_SPACE)) then
		begin
			p.Y += mvOut.Y;
			p.X += mvOut.X;
			mvOut := CreateVector(0, 0);
		end;

		ClearScreen(ColourBlack);

		DrawRectangle(ColourRed, RX, RY, RW, RH);			
		DrawRectangle(ColourWhite, p.X - r, p.Y - r, r2, r2);
		DrawLine(ColourWhite, p.X, p.Y, p.X + movement.X, p.Y + movement.Y);

		if (false = ((mvOut.X = 0) and (mvOut.Y = 0))) then
		begin
			DrawLine(ColourGreen, p.X, p.Y, p.X + mvOut.X, p.Y + mvOut.Y);
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
			movement := CreateVector(100, 0);
         halfH := 10;
         halfW := 5;

	      movement := CreateVector(100, 0);
	
	      tgtRect := CreateRectangle(RX, RY, RW, RH);
         mvRect := CreateRectangle(100 + RX, 100 + RY, 10, 20);
			ROOR := true;
		end;

		if (IsKeyPressed(VK_A)) then movement := Multiply(RotationMatrix(-4.0), movement);
		if (IsKeyPressed(VK_Z)) then movement := Multiply(RotationMatrix(4.0), movement);

		if (WasKeyTyped(VK_0)) then movement := CreateVector(100, 0);
		if (WasKeyTyped(VK_9)) then movement := Multiply(RotationMatrix(90.0), CreateVector(100, 0));
		if (WasKeyTyped(VK_8)) then movement := Multiply(RotationMatrix(180.0), CreateVector(100, 0));
		if (WasKeyTyped(VK_2)) then movement := Multiply(RotationMatrix(-90.0), CreateVector(100, 0));

		if (IsKeyPressed(VK_UP)) then mvRect.Y := mvRect.Y - 5;
		if (IsKeyPressed(VK_DOWN)) then mvRect.Y := mvRect.Y + 5;
		if (IsKeyPressed(VK_LEFT)) then mvRect.X := mvRect.X - 5;
		if (IsKeyPressed(VK_RIGHT)) then mvRect.X := mvRect.X + 5;

		mvOut := VectorOutOfRectFromRect(mvRect, tgtRect, movement);
		enterRectFrom := GetSideForCollisionTest(movement);
        
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

        if (IsKeyPressed(VK_SPACE)) then
        begin
            mvRect.Y := mvRect.Y + mvOut.Y;
            mvRect.X := mvRect.X + mvOut.X;
            mvOut := CreateVector(0, 0);
        end;

        ClearScreen(ColourBlack);

        DrawRectangle(ColourRed, RX, RY, RW, RH);
        DrawRectangle(ColourWhite, mvRect.X , mvRect.Y , mvRect.Width, mvRect.Height);
        DrawLine(ColourWhite, mvRect.X + halfW, mvRect.Y + halfH, mvRect.X + halfW + movement.X, mvRect.Y + halfH + movement.Y);

        if (false = ((mvOut.X = 0) and (mvOut.Y = 0))) then
        begin
            DrawLine(GetColor(100, 100, 100), mvRect.X, mvRect.Y, mvRect.X + mvOut.X, mvRect.Y + mvOut.Y);
            DrawRectangle(ColourGreen, mvRect.X + mvOut.X, mvRect.Y + mvOut.Y, mvRect.Width, mvRect.Height);
            DrawCircle(ColourRed, px, py, 2);
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
			p := CreatePoint(100 / 2 + CX, 100 / 2 + CY);
            c := CreatePoint(CX, CY);
            movement := CreateVector(100, 0);
			POOC := true;
		end;

        if (IsKeyPressed(VK_A)) then movement := Multiply(RotationMatrix(-4.0), movement);
        if (IsKeyPressed(VK_Z)) then movement := Multiply(RotationMatrix(4.0), movement);

        if (IsKeyPressed(VK_UP)) then p.Y := p.Y - 5;
        if (IsKeyPressed(VK_DOWN)) then p.Y := p.Y + 5;
        if (IsKeyPressed(VK_LEFT)) then p.X := p.X - 5;
        if (IsKeyPressed(VK_RIGHT)) then p.X := p.X + 5;

        mvOut := VectorOutOfCircleFromPoint(p, c, CR, movement);

        if (IsKeyPressed(VK_SPACE)) then
        begin
            p.Y := p.Y + mvOut.Y;
            p.X := p.X + mvOut.X;
            mvOut := CreateVector(0, 0);
        end;

        //ClearScreen(ColourBlack);

        DrawCircle(ColourRed, CX, CY, CR);
        DrawRectangle(ColourWhite, p.X - r, p.Y - r, r2, r2);
        DrawLine(ColourWhite, p.X, p.Y, p.X + movement.X, p.Y + movement.Y);

        if (false = ((mvOut.X = 0) and (mvOut.Y = 0))) then
        begin
            DrawLine(ColourGreen, p.X, p.Y, p.X + mvOut.X, p.Y + mvOut.Y);
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
			p := CreatePoint(100 / 2 + CX, 100 / 2 + CY);
            c := CreatePoint(CX, CY);
            movement := CreateVector(100, 0);
			COOC := true;
		end;
		
        if (IsKeyPressed(VK_A)) then movement := Multiply(RotationMatrix(-4.0), movement);
        if (IsKeyPressed(VK_Z)) then movement := Multiply(RotationMatrix(4.0), movement);

        if (IsKeyPressed(VK_UP)) then p.Y := p.Y - 5;
        if (IsKeyPressed(VK_DOWN)) then p.Y := p.Y + 5;
        if (IsKeyPressed(VK_LEFT)) then p.X := p.X - 5;
        if (IsKeyPressed(VK_RIGHT)) then p.X := p.X + 5;

        mvOut := VectorOutOfCircleFromCircle(p,PR, c, CR, movement);

        if (IsKeyPressed(VK_SPACE)) then
        begin
            p.Y += mvOut.Y;
            p.X += mvOut.X;
            mvOut := CreateVector(0, 0);
        end;

        DrawCircle(ColourRed, CX, CY, CR);
        DrawCircle(ColourWhite, p.X, p.Y, PR);
        DrawLine(ColourWhite, p.X, p.Y, p.X + movement.X, p.Y + movement.Y);

        if (false = ((mvOut.X = 0) and (mvOut.Y = 0))) then
        begin
            DrawLine(ColourGreen, p.X, p.Y, p.X + mvOut.X, p.Y + mvOut.Y);
            DrawCircle(ColourGreen, p.X + mvOut.X, p.Y + mvOut.Y, PR);
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
			MethodBeingTested := 'HasSpriteCollidedWith..., HaveSpritesCollided';
			Instructions := 'Use the arrow keys to move the' + EOL + 'sprite around.' + EOL +
							'The sprite with red rectangle' + EOL + 'uses bounded collisions.' + EOL +
							'The sprite with blue rectangle' + EOL + 'uses non-bounded collisions.';
			ship := CreateSprite(GameImage('Ship'),1,2,40,43);
			explosion := CreateSprite(LoadBitmap(GetPathToResource('explosion_blue.jpg', ImageResource), true, ColourBlack), 20, 40, 72, 72);
			smallball := GameImage('SmallBall');
			ToRun := @CollisionSpriteTest;
		end;
		
		with result.Tests[1] do
		begin
			MethodBeingTested := 'HaveBitmapsCollided, HasBitmapCollidedWithRect';
			Instructions := 'Use the arrow keys to move' + EOL + 'the sprite.' + EOL + 
							'This test is used to check' + EOL + 'if the collision works properly.';
			mediumball := GameImage('BallImage1');
			X := 0; Y := 0;
			ToRun := @CollissionBitmapTest;
		end;
		
		with result.Tests[2] do
		begin
			MethodBeingTested := 'HaveBitmapsCollided, CircleHasCollidedWithLine, GetUnitVector, RotationMatrix';
			Instructions := 'Use the arrow keys to move' + EOL + 'the sprite.' +EOL+ 'The blue line represents the' + EOL + 'vector of the sprite.' + EOL +
							'A, D: Rotate the vector.' + EOL + 'W, S: Increase or decrese the' + EOL + 'magnitude.' + EOL + 
							'Space: Move ball out of line' + EOL + 'if colliding.';
			ball := CreateSprite(GameImage('SmallBall'));
			X := 0; Y := 0;
			ball.Movement.X := 1; ball.Movement.Y := 1;
			ToRun := @CircleCollissionTest;
		end;
		
		with result.Tests[3] do
		begin
			MethodBeingTested := 'CircularCollision';
			Instructions := 'Use the following keys to run' + EOL +'the test:' + EOL + EOL + 'UP, DOWN: Increase or decrease' + EOL + 'the vector of the big ball.' + EOL +
							'LEFT, RIGHT: Rotate the vector' + EOL + 'of the big ball.' + EOL + EOL + 'W, S: Increase or decrease' + EOL + 'the vector of the small ball.' + EOL +
							'A, D: Rotate the vector of' + EOL + 'the small ball' + EOL + EOL +
							'T: Toggle the collision test' + EOL + 'SPACE: See the effect';
			bigball := CreateSprite(GameImage('BallImage1'));
			ball.Movement.X := 1; ball.Movement.Y := 1;
	        ball.Mass := 1;
	        ball.X := 150; ball.Y := 150;
	        bigball.Movement.X := 1; bigball.Movement.Y := 1;
	        bigball.Mass := 5;
	        bigball.X := 155; bigball.Y := 155;
			ToRun := @CircleCollissionTest2;
		end;
		
		with result.Tests[4] do
		begin
			MethodBeingTested := 'RectangleHasCollidedWithLine';
			Instructions := 'This test is used to test the' + EOL + 'collision between the rectangle' + EOL + 'and a line.' + EOL + EOL + 'Use the arrow keys to move the' + EOL + 'rectangle around.';
			ToRun := @RectangleCollisionTest;
		end;
		
		with result.Tests[5] do
		begin
			MethodBeingTested := 'RectangleHasCollidedWithLine';
			Instructions := 'This test is used to test the' + EOL + 'mathematical calculation of' + EOL + 'a vector.' + EOL + EOL +
							'UP, DOWN: Change Vector1 X' + EOL +
							'LEFT, RIGHT: Change Vector1 Y' + EOL + EOL +
							'W, S: Change Vector2 X' + EOL +
							'A, D: Change Vector2 Y';
			v1 := CreateVector(2,2);
			v2 := CreateVector(2, 2, true);
			ToRun := @VectorMathsTest;
		end;
		
		with result.Tests[6] do
		begin
			MethodBeingTested := 'Calculate angle and vector, IsSpriteOnScreenAt, VectorFromCenterSpriteToPoint';
			Instructions := 'Use the arrow keys to move' + EOL + 'the small ball.' + EOL + EOL +
							'Use the ASDW keys to move' + EOL + 'the large ball.';
			ToRun := @SpriteTests;
		end;
		
		with result.Tests[7] do
		begin
			MethodBeingTested := 'VectorFromPoints, VectorFromPointToRectangle';
			Instructions := 'Use the arrow keys to move' + EOL + 'the point around.';
			point := CreatePoint(100, 100);
		    point2 := CreatePoint(150, 150);
		    rect := CreateRectangle(200, 200, 50, 50);
			ToRun := @PointTest;
		end;
		
		with result.Tests[8] do
		begin
			MethodBeingTested := 'Matrix Multiplication, Translation';
			Instructions := 'Press Space to Rotate the vector' + EOL + 'by 45 degrees.';
			ToRun := @TranslationTest;
		end;
		
		with result.Tests[9] do
		begin
			MethodBeingTested := 'VectorIsWithinRect';
			Instructions := 'This test is used to check if' + EOL + 'a vector is within a rectangle.' + EOL + EOL + 
							'Use the Mouse to change' + EOL + 'the vector';
			ToRun := @VectorIsWithinRectTest;
		end;
		
		with result.Tests[10] do
		begin
			MethodBeingTested := 'CalculateAngle, Matrix Multiply, Rotation Matrix';
			Instructions := 'LEFT, RIGHT: Control the white' + EOL + 'line.' + EOL +
							'UP, DOWN: Control the red line' + EOL +
							'Space: Move the red over white';
			ToRun := @VectorAngle;
		end;
		
		with result.Tests[11] do
		begin
			MethodBeingTested := 'VectorOutOfRectFromPoint';
			Instructions := 'Use the arrow keys to move' + EOL + 'the point.' + EOL +
							'A, Z: Rotate the movement' + EOL +
							'Space: Move the point out';
			ToRun := @PointOutOfRect;
		end;
		
		with result.Tests[12] do
		begin
			MethodBeingTested := 'VectorOutOfRectFromRect';
			Instructions := 'Use the arrow keys to move' + EOL + 'the point.' + EOL +
							'A, Z: Rotate the movement' + EOL +
							'Space: Move the rectangle out' + EOL + EOL +
							'0 = set movement to 0 degrees' + EOL +
							'9 = set movement to 90 degrees' + EOL +
							'8 = set movement to 180 degrees' + EOL +
							'2 = set movement to 270 degrees' + EOL;
			ToRun := @RectOutOfRect;
		end;
		
		with result.Tests[13] do
		begin
			MethodBeingTested := 'VectorOutOfCircleFromPoint';
			Instructions := 'Use the arrow keys to move' + EOL + 'the point.' + EOL +
							'A, Z: Rotate the movement' + EOL +
							'Space: Move the point out';
			ToRun := @PointOutOfCircle;
		end;
		
		with result.Tests[14] do
		begin
			MethodBeingTested := 'VectorOutOfCircleFromCircle';
			Instructions := 'Use the arrow keys to move' + EOL + 'the point.' + EOL +
							'A, Z: Rotate the movement' + EOL +
							'Space: Move the point out';
			ToRun := @CircleOutOfCircle;
		end;
	end;
	
	procedure AddPhysicsSuite(var suites: TestSuites);
	begin
		suites[High(suites)] := GetPhysicsTests();
	end;

end.