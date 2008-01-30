unit GameLogic;

interface
	procedure Main();

implementation
	uses
	GameResources,
	SysUtils,
	SGSDK_Core, SGSDK_Font, SGSDK_Audio, SGSDK_Graphics, SGSDK_Input, SGSDK_Physics,
	SGSDK_KeyCodes, SGSDK_Camera, SGSDK_MappyLoader, SGSDK_Shapes;
	
	procedure DrawOverlay(title : String);
	begin
		FillRectangleOnScreen(ColorBlack, 0, 0, 800, 50);
        DrawTextOnScreen(title, ColorWhite, GameFont('Courier'), Round((ScreenWidth() / 2) - ((Length(title) / 2) * 10)), 20);
	end;
	
	function GetRandomColor(): Color;
	begin
		case Random(9) of
			0: result := ColourBlue;
			1: result := ColourGreen;
			2: result := ColourRed;
			3: result := ColourWhite;
			4: result := ColourBlack;
			5: result := ColourYellow;
			6: result := ColourPink;
			7: result := ColourTurquoise;
			8: result := ColourGrey;
			9: result := ColourMagenta;
		end;
	end;
	
	procedure DrawLines();
	begin
		ClearScreen();
		repeat
			DrawLine(GetRandomColor(), Random(800), Random(800), Random(800), Random(800));
			DrawOverlay('Drawing Lines Example');
			ProcessEvents();
			RefreshScreen();
			if WindowCloseRequested() then break;
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;
	
	procedure DrawRectangles();
	begin
		ClearScreen();
		repeat
			DrawRectangle(GetRandomColor(), Random(800), Random(800), Random(800), Random(800));
			FillRectangle(GetRandomColor(), Random(800), Random(800), Random(800), Random(800));
			DrawOverlay('Drawing Rectangles Example');
			ProcessEvents();
			RefreshScreen();
			if WindowCloseRequested() then break;
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;
	
	procedure DrawCircles();
	begin
		ClearScreen();
		repeat
			DrawCircle(GetRandomColor(), Random(800) + 1, Random(800) + 1, Random(400) + 1);
			FillCircle(GetRandomColor(), Random(800) + 1, Random(800) + 1, Random(400) + 1);
			DrawOverlay('Drawing Circles Example');
			ProcessEvents();
			RefreshScreen();
			if WindowCloseRequested() then break;
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;
	
	procedure DrawEllipses();
	begin
		ClearScreen();
		repeat
			DrawEllipse(GetRandomColor(), Random(800) + 1, Random(800) + 1, Random(400) + 1, Random(400) + 1);
			FillEllipse(GetRandomColor(), Random(800) + 1, Random(800) + 1, Random(400) + 1, Random(400) + 1);
			DrawOverlay('Drawing Ellipses Example');
			ProcessEvents();
			RefreshScreen();
			if WindowCloseRequested() then break;
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;
	
	procedure DrawBitmaps();
	var
		i : Integer;
		tempBitmap, tempBitmap2 : Bitmap;
	begin
		i := 0;
		ClearScreen();
		tempBitmap := GameImage('BallImage1');
		tempBitmap2 := GameImage('BallImage2');
		repeat
			ClearScreen();
			DrawBitmap(tempBitmap, round(SGSDK_Core.sin(i) * 100) + 250, round(SGSDK_Core.cos(i) * 100) + 200);
			DrawBitmap(tempBitmap2, round(SGSDK_Core.cos(i) * 100) + 250, round(SGSDK_Core.sin(i) * 100) + 200);
			DrawOverlay('Drawing Bitmap Example');
			ProcessEvents();
			RefreshScreen();
			i := i + 1;
			if WindowCloseRequested() then break;
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;
	
	procedure DrawSpriteCaption(sprite : Sprite; caption : String);
	begin
		DrawText(caption, ColorWhite, GameFont('Courier'), 
			Round((sprite.x + CurrentWidth(sprite) / 2) - ((Length(caption) / 2) * 10)), 
			Round(sprite.y + CurrentHeight(sprite)));
	end;
	
	procedure DrawSprites();
	var
		loopSprite, reverseSprite, stopSprite, reverseOnceSprite : Sprite;
	begin
		loopSprite := CreateSprite(GameImage('Running'), 5, 15, 80, 94);
		loopSprite.endingAction := Loop;
		reverseSprite := CreateSprite(GameImage('Running'), 5, 15, 80, 94);
		reverseSprite.endingAction := ReverseLoop;
		stopSprite := CreateSprite(GameImage('Running'), 5, 15, 80, 94);
		stopSprite.endingAction := Stop;
		reverseOnceSprite := CreateSprite(GameImage('Running'), 5, 15, 80, 94);
		reverseOnceSprite.endingAction := ReverseOnce;
		loopSprite.x := 50;
		loopSprite.y := 200;
		reverseSprite.x := 150;
		reverseSprite.y := 200;
		stopSprite.x := 350;
		stopSprite.y := 200;
		reverseOnceSprite.x := 450;
		reverseOnceSprite.y := 200;
		repeat
			ClearScreen();
			DrawSprite(loopSprite);
			DrawSprite(reverseSprite);
			DrawSprite(stopSprite);
			DrawSprite(reverseOnceSprite);
			DrawSpriteCaption(loopSprite, 'Loop');
			DrawSpriteCaption(reverseSprite, 'ReverseLoop');
			DrawSpriteCaption(stopSprite, 'Stop');
			DrawSpriteCaption(reverseOnceSprite, 'ReverseOnce');
			UpdateSprite(loopSprite);
			UpdateSprite(reverseSprite);
			UpdateSprite(stopSprite);
			UpdateSprite(reverseOnceSprite);
			DrawOverlay('Drawing Sprite Example');
			ProcessEvents();
			RefreshScreen();
			if WindowCloseRequested() then break;
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;
	
	procedure MoveBall(var ball : Sprite; var xSpeed, ySpeed : Integer);
	begin
		ball.x := ball.x + xSpeed;
		ball.y := ball.y + ySpeed;
		if ball.x > ScreenWidth() - CurrentWidth(ball) then
		begin
			ball.x := ScreenWidth() - CurrentWidth(ball);
			xSpeed := -1 * xSpeed;
		end;
		if ball.y > ScreenHeight() - CurrentHeight(ball) then
		begin
			ball.y := ScreenHeight() - CurrentHeight(ball);
			ySpeed := -1 * ySpeed;
		end;
		if ball.x < 0 then
		begin
			ball.x := 0;
			xSpeed := -1 * xSpeed;
		end;
		if ball.y < 0 then
		begin
			ball.y := 0;
			ySpeed := -1 * ySpeed;
		end;
	end;
	
	procedure DrawCollisionDetection();
	var
		ball1, ball2 : Sprite;
		xSpeed1, xSpeed2, ySpeed1, ySpeed2 : Integer;
	begin
		ClearScreen();
		xSpeed1 := 3;
		xSpeed2 := 3;
		ySpeed1 := 3;
		ySpeed2 := 3;
		ball1 := CreateSprite(GameImage('BallImage1'));
		ball2 := CreateSprite(GameImage('BallImage2'));
		ball1.x := 0;
		ball1.y := 0;
		ball2.x := ScreenWidth() - CurrentWidth(ball2);
		ball2.y := ScreenHeight() - CurrentHeight(ball2);
		ball1.usePixelCollision := true;
		ball2.usePixelCollision := true;
		repeat
			ClearScreen();
			DrawSprite(ball1);
			DrawSprite(ball2);
			if HaveSpritesCollided(ball1, ball2) then
				DrawText('Collided!', ColorWhite, GameFont('Courier'), ScreenWidth() - 90, ScreenHeight() - 20);
			MoveBall(ball1, xSpeed1, ySpeed1);
			MoveBall(ball2, xSpeed2, ySpeed2);
			DrawOverlay('Collision Detection Example');
			ProcessEvents();
			RefreshScreen();
			if WindowCloseRequested() then break;
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;
	
	procedure PlayMusicExample();
	var
		musicSource : Music;
	begin
		ClearScreen();
		musicSource := GameMusic('Fast');
		PlayMusic(musicSource);
		repeat
			DrawOverlay('Music Playback Example');
			RefreshScreen();
			ProcessEvents();
			if WindowCloseRequested() then break;
		until IsKeyPressed(VK_N);
		StopMusic();
		Sleep(500);
		ProcessEvents();
	end;
	
	function GetRandomFontStyle(): FontStyle;
	begin
		case Random(3) of
			0: result := NormalFont;
			1: result := BoldFont;
			2: result := ItalicFont;
			3: result := UnderlineFont;
		end;
	end;
	
	procedure DrawRandomText();
	begin
		ClearScreen();
		repeat
			SetFontStyle(GameFont('Courier'), GetRandomFontStyle());
			DrawText('SwinGameSDK!', GetRandomColor(), GameFont('Courier'), Random(ScreenWidth()), Random(ScreenHeight()));
			SetFontStyle(GameFont('Courier'), GetRandomFontStyle());
			DrawText('SwinGameSDK!', GetRandomColor(), GameFont('Courier'), Random(ScreenWidth()), Random(ScreenHeight()));
			SetFontStyle(GameFont('Courier'), NormalFont);
			DrawOverlay('Drawing Random Texts');
			RefreshScreen();
			ProcessEvents();
			if WindowCloseRequested() then break;
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;
	
	procedure MoveBallUsingVector(var ball : Sprite);
	begin
		MoveSprite(ball, ball.movement);
		if ball.x > ScreenWidth() - CurrentWidth(ball) then
		begin
			ball.movement.x := ball.movement.x * -1;
			ball.x := ScreenWidth() - CurrentWidth(ball);
		end;
		if ball.y > ScreenHeight() - CurrentHeight(ball) then
		begin
			ball.movement.y := ball.movement.y * -1;
			ball.y := ScreenHeight() - CurrentHeight(ball);
		end;
		if ball.x < 0 then
		begin
			ball.movement.x := ball.movement.x * -1;
			ball.x := 0;
		end;
		if ball.y < 0 then
		begin
			ball.movement.y := ball.movement.y * -1;
			ball.y := 0;
		end;
	end;
	
	procedure DrawVectorCollision();
	var
		ball1, ball2 : Sprite;
	begin
		ClearScreen();
		ball1 := CreateSprite(GameImage('BallImage1'));
		ball2 := CreateSprite(GameImage('BallImage2'));
		ball1.movement := CreateVector(3, 3, true);
		ball2.movement := CreateVector(3, 3, true);
		ball1.mass := 1;
		ball2.mass := 5;
		ball1.x := 0;
		ball1.y := 0;
		ball2.x := ScreenWidth() - CurrentWidth(ball2);
		ball2.y := ScreenHeight() - CurrentHeight(ball2);
		ball1.usePixelCollision := true;
		ball2.usePixelCollision := true;
		repeat
			ClearScreen();
			DrawSprite(ball1);
			DrawSprite(ball2);
			if HaveSpritesCollided(ball1, ball2) then VectorCollision(ball1, ball2);
			MoveBallUsingVector(ball1);
			MoveBallUsingVector(ball2);
			
			DrawOverlay('Vector Collision Example');
			ProcessEvents();
			RefreshScreen();
			if WindowCloseRequested() then break;
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;
	
	procedure MoveSpriteWithInput();
	var
		ball : Sprite;
		xSpeed, ySpeed : Integer;
	begin
		ball := CreateSprite(GameImage('BallImage1'));
	    ball.x := 400;
	    ball.y := 300;

	    repeat
	        xSpeed := 0;
	        ySpeed := 0;

	        if IsKeyPressed(VK_UP) then
	            ySpeed := -3;
	        if IsKeyPressed(VK_DOWN) then
	            ySpeed := 3;
	        if IsKeyPressed(VK_LEFT) then
	            xSpeed := -3;
	        if IsKeyPressed(VK_RIGHT) then
	            xSpeed := 3;

	        DrawSprite(ball);
	        ball.x := ball.x + xSpeed;
			ball.y := ball.y + ySpeed;

	        DrawOverlay('Move Sprite with Arrow Keys Example');
	        ProcessEvents();
	        RefreshScreen();
	        ClearScreen();
			if WindowCloseRequested() then exit;
	    until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;
	
	procedure MouseCursor();
	var
		ball : Sprite;
		position : Point2D;
	begin
		ball := CreateSprite(GameImage('BallImage1'));
        repeat
			ProcessEvents();
			
            position := GetMousePosition();
			
            DrawHorizontalLine(ColorWhite, Round(position.Y), 0, 800);
            DrawVerticalLine(ColorWhite, Round(position.X), 0, 600);
			
            if MouseWasClicked(LeftButton) then
            begin
                ball.x := position.X - (CurrentWidth(ball) / 2);
                ball.y := position.Y - (CurrentHeight(ball) / 2);
                DrawSprite(ball);
            end;

            DrawOverlay('Mouse Cursor Example');
            RefreshScreen();
            ClearScreen();
			if WindowCloseRequested() then exit;
        until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;
	
	procedure TextReadExample();
	begin
		StartReadingText(ColorGreen, 50, GameFont('Courier'), 0, 65);
		while IsReadingText() do
		begin
			ClearScreen();
			DrawText('Please enter a message:', ColorGreen, GameFont('Courier'), 0, 50);
			DrawOverlay('Text Reading Example');
			RefreshScreen();
			ProcessEvents();
			if WindowCloseRequested() then exit;
		end;
		DrawText('You have entered ' + TextReadAsASCII(), ColorGreen, GameFont('Courier'), 0, 80);
		RefreshScreen();
		repeat
			Sleep(20);
			ProcessEvents();
			if WindowCloseRequested() then exit;
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;

    procedure SoundInput();
	var
		sound : SoundEffect;
		_Font : Font;
    begin
		sound := GameSound('Shock');
		_Font := GameFont('Courier');
        repeat
            if IsKeyPressed(VK_SPACE) and (not IsSoundEffectPlaying(sound)) then
                PlaySoundEffect(sound);

            DrawText('Press Space to play a Sound Effect', ColorWhite, _Font, 210, 300);

            DrawOverlay('Play Sound Effect when hitting a key Example');
            ProcessEvents();
            RefreshScreen();
            ClearScreen();
			
            if WindowCloseRequested() then break;
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
    end;
	
	procedure DroppingBall();
	var
		ball: Sprite;
		GravityConst: Vector;
	begin
		GravityConst := CreateVector(0, 0.5);

		ball := CreateSprite(GameImage('SmallBall'));
		ball.movement := CreateVector(5, 0);
		ball.mass := 1;

		ball.x := 0;
		ball.y := 0;
		repeat
			ClearScreen();

			ball.movement := AddVectors(ball.movement, GravityConst);
			ball.movement := MultiplyVector(ball.movement, 0.995);

			MoveSprite(ball, ball.movement);

			if ball.x > ScreenWidth() - CurrentWidth(ball) then
			begin
				ball.movement.x := ball.movement.x * -1;
				ball.x := ScreenWidth() - CurrentWidth(ball);
			end;
			if ball.y > ScreenHeight() - CurrentHeight(ball) then
			begin
				if ball.movement.y < 1 then
					ball.movement.y := 0;
				ball.movement.y := ball.movement.y * -1;
				ball.y := ScreenHeight() - CurrentHeight(ball);
			end;
			if ball.x < 0 then
			begin
				ball.movement.x := ball.movement.x * -1;
				ball.x := 0;
			end;
			if ball.y < 0 then
			begin
				ball.movement.y := ball.movement.y * -1;
				ball.y := 0;
			end;
			DrawSprite(ball);

			ProcessEvents();
			DrawOverlay('Drapping Ball Example');
			RefreshScreen();
			if WindowCloseRequested() then break;
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;
	
	procedure MultiBitmapSprite();
	var
		sp1, sp2: Sprite;
	begin
		sp1 := CreateSprite(GameImage('Explosion'), 5, 15, 38, 38);
		sp2 := CreateSprite(GameImage('Ship'), 3, 2, 40, 43);
		sp1.x := 70;
		sp1.y := 100;
		sp2.x := 80;
		sp2.y := 110;
		repeat
			ProcessEvents();
			ClearScreen(ColorBlack);
			DrawSprite(sp1);
			DrawSprite(sp2);
			UpdateSprite(sp1);
			UpdateSprite(sp2);
			DrawBitmap(GameImage('Explosion'), 70, 170);
			DrawText('Explosion Bitmap', ColorWhite, GameFont('Courier'), 90 + GameImage('Explosion').width, 190);
			DrawBitmap(GameImage('Ship'), 70, 250);
			DrawText('Ship Bitmap', ColorWhite, GameFont('Courier'), 90 + GameImage('Ship').width, 260);
			if HaveSpritesCollided(sp1, sp2) then
				DrawText('Collided...', ColorWhite, GameFont('Courier'), 125, 120);
			DrawOverlay('Multi-bitmap Collision Detection');
			RefreshScreen();
			if WindowCloseRequested() then exit;
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;
	
	procedure FollowSpriteExample();
	var
		Sea: Bitmap;
		Ship: Sprite;
	begin
		Sea := GameImage('Sea');
		Ship := CreateSprite(GameImage('Ship'), 3, 2, 40, 43);
		Ship.x := 0;
		Ship.y := 0;
		repeat
			if IsKeyPressed(VK_RIGHT) then
				Ship.x := Ship.x + 4;
			if IsKeyPressed(VK_DOWN) then
				Ship.y := Ship.y + 4;
			if IsKeyPressed(VK_UP) then
				Ship.y := Ship.y - 4;
			if IsKeyPressed(VK_LEFT) then
				Ship.x := Ship.x - 4;
			FollowSprite(Ship, 0, -150);
			ProcessEvents();
			ClearScreen();
			DrawBitmap(Sea, 0, 0);
			DrawSprite(Ship);
			UpdateSprite(Ship);
			DrawOverlay('Follow Sprite Example');
			RefreshScreen();
			if WindowCloseRequested() then exit;
		until IsKeyPressed(VK_N);
		SetScreenOffset(0,0);
		Sleep(500);
		ProcessEvents();
	end;
	
	procedure DrawCircleWithLines();
	begin
		ClearScreen();
		repeat
			if IsKeyPressed(VK_RIGHT) then
				MoveVisualArea(4, 0);
			if IsKeyPressed(VK_DOWN) then
				MoveVisualArea(0, 4);
			if IsKeyPressed(VK_UP) then
				MoveVisualArea(0, -4);
			if IsKeyPressed(VK_LEFT) then
				MoveVisualArea(-4, 0);
			ClearScreen();
			FillCircle(ColorGreen, ScreenWidth() div 2, ScreenHeight() div 2, 200);
			DrawCircle(ColorWhite, ScreenWidth() div 2, ScreenHeight() div 2, 200);
			FillCircleOnScreen(ColorRed, ScreenWidth() div 2, ScreenHeight() div 2, 100);
			DrawCircleOnScreen(ColorWhite, ScreenWidth() div 2, ScreenHeight() div 2, 100);
			DrawHorizontalLine(ColorWhite, ScreenHeight() div 2, ScreenWidth(), 0);
			DrawVerticalLine(ColorWhite, ScreenWidth() div 2, ScreenHeight(), 0);
			DrawOverlay('Camera Example');
			RefreshScreen();
			ProcessEvents();
			if WindowCloseRequested() then exit;
		until IsKeyPressed(VK_N);
		SetScreenOffset(0,0);
		Sleep(500);
		ProcessEvents();
	end;
	

	
	procedure MapExample();
	var
		m : Map;
		balls: Array of Sprite;
		tempString: CollisionSide;
		i, j, tx, ty : Integer;
		gravity: Vector;
	begin
		m := GameMap('test');
		
		SetLength(balls, 3);
		
		Randomize();
		
		for i := 0 to High(balls) do
		begin
			balls[i] := CreateSprite(GameImage('SmallBall'));
			balls[i].movement := CreateVector((i-1)*4, 0);
			balls[i].x := EventPositionX(m, Event1, i);
			balls[i].y := EventPositionY(m, Event1, i);
			
			balls[i].mass := 1;
		end;
		
		gravity := CreateVector(0, 0.7);
		
		WriteLn('Event Count (Event1): ', EventCount(m, Event1));
		
		repeat			
			if IsKeyPressed(VK_RIGHT) then
				MoveVisualArea(2, 0);
			if IsKeyPressed(VK_LEFT) then
				MoveVisualArea(-2, 0);
			if IsKeyPressed(VK_UP) then
				MoveVisualArea(0, -2);
			if IsKeyPressed(VK_DOWN) then
				MoveVisualArea(0, 2);
			
			DrawMap(m);
			
			for i := 0 to  High(balls) - 1 do
			begin
				for j := i + 1 to High(balls) do
				begin
					if i <> j then
					begin
						if HaveSpritesCollided(balls[i], balls[j]) then
						begin
							{MoveSprite(balls[i], InvertVector(balls[i].movement));
							MoveSprite(balls[j], InvertVector(balls[j].movement));
							tempVector1 := balls[i].movement;
							tempVector2 := balls[j].movement;
							repeat
								MoveSprite(balls[i], tempVector1);
								MoveSprite(balls[j], tempVector2);
								if HaveSpritesCollided(balls[i], balls[j]) then
								begin
									MoveSprite(balls[i], InvertVector(tempVector1));
									MoveSprite(balls[j], InvertVector(tempVector2));
									tempVector1 := MultiplyVector(tempVector1, 0.5);
									tempVector2 := MultiplyVector(tempVector2, 0.5);
								end;
							until (Magnitude(tempVector1) < 0.05) and (Magnitude(tempVector2) < 0.5);}
							VectorCollision(balls[i], balls[j]);
						end;
					end;
				end;
			end;
			
			for i := 0 to High(balls) do
			begin
				balls[i].movement := AddVectors(balls[i].movement, gravity);
				balls[i].movement := MultiplyVector(balls[i].movement, 0.99);
				
				MoveSprite(balls[i], balls[i].movement);
			end;
			
			for i := 0 to High(balls) do
			begin
			
				if SpriteHasCollidedWithMapTile(m, balls[i], tx, ty) then
				begin
					MoveSpriteOutOfTile(m, balls[i], tx, ty);
					tempString := WillCollideOnSide(m, balls[i]);
					if (tempString = Right) or (tempString = Left) or (tempString = TopLeft) or (tempString = TopRight) or (tempString = BottomLeft) or (tempString = BottomRight) then
						balls[i].movement.x := balls[i].movement.x * -1;
					if (tempString = Top) or (tempString = Bottom) or (tempString = TopLeft) or (tempString = TopRight) or (tempString = BottomLeft) or (tempString = BottomRight) then
						balls[i].movement.y := balls[i].movement.y * -1;
						
					if tempString <> None then
					begin
						balls[i].movement := MultiplyVector(balls[i].movement, 0.9);
						if Magnitude(balls[i].movement) < 3 then 
						begin
							balls[i].movement.x := 0; 
							balls[i].movement.y := 0;
						end;
					end;
				end;
			end;
			
			for i := 0 to High(balls) do
			begin
				DrawSprite(balls[i]);
			end;
			DrawOverlay('MappyLoader Example');
			DrawFramerate(0, 0, GameFont('Courier'));
			ProcessEvents();
			RefreshScreen(60);
			ClearScreen(ColorBlack);
			if WindowCloseRequested() then exit;
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;
	
	procedure VectorAngleExample();
	var
		pd: Sprite;
		mousePos: Point2D;
	begin
		pd := CreateSprite(GameImage('BallImage1'));
		repeat
			ProcessEvents();
			mousePos := GetMousePosition();
			pd.movement := GetVectorFromAngle(CalculateAngle(pd.x + CurrentWidth(pd) / 2, pd.y + CurrentHeight(pd) / 2, mousePos.X, mousePos.Y), 2);
			//pd.movement := LimitMagnitude(VectorFromCenterSpriteToPoint(pd, mousePos), 2);
			MoveSprite(pd, pd.movement);
			ClearScreen(ColorBlack);
			DrawSprite(pd);
			DrawOverlay('Get Vector From Angle Example (The ball will follow your cursor)');
			RefreshScreen();
			if WindowCloseRequested() then exit;
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;
	
	procedure MouseExample();
	var
		temp : Integer;
	begin
		temp := 0;
		repeat
			temp := temp + 1;
			ProcessEvents();
			if temp /50 = 1 then
			begin
				MoveMouse(ScreenWidth() div 2, ScreenHeight() div 2);
				//writeln(GetMouseMovement.x, GetMouseMovement.y);
			end;
			if temp / 100 = 1 then
			begin
				HideMouse();
			end;
			if temp / 100 = 2 then
			begin
				ShowMouse();
				temp := 0;
			end;
			
			//writeln(temp);
			
			
			ClearScreen(ColorBlack);
			DrawOverlay('Hiding, Showing and Moving the mouse');
			RefreshScreen();

			if WindowCloseRequested() then exit;
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;
	
	procedure VectorExample1();
	const
		CX = 400;
		CY = 300;
		RADIUS = 10;
		LINE_LENGTH = 100;
	var
		v1, v2: Vector;
		angle, rot, v1a, v2a: Single;
		rm : Matrix2D;
	begin
		v1 := CreateVector(LINE_LENGTH, 0);
		v2 := CreateVector(0 , LINE_LENGTH);

		repeat
			ProcessEvents();			
			ClearScreen(ColorBlack);
			
			DrawOverlay('Vector Calculations - Angle');

			DrawCircle(ColorRed, CX, CY, RADIUS);
			DrawLine(ColorRed, CX, CY, CX + v1.x, CY + v1.y);
			DrawLine(ColorWhite, CX, CY, CX + v2.x, CY + v2.y);			
			
			angle := CalculateAngleBetween(v1, v2);
			v1a := CalculateAngle(0, 0, v1.x, v1.y);
			v2a := CalculateAngle(0, 0, v2.x, v2.y);
			
			DrawTextLines('Left/Right control White.' + EOL + 'Up/Down control Red.' + EOL + 'Space to align red with white.', ColorWhite, ColorBlack, GameFont('Courier'), AlignLeft, 10, 50, 200, 200);
			
			DrawText('White: ' + FloatToStr(v2a), ColorWhite, GameFont('Courier'), 650, 50);
			DrawText('Red: ' + FloatToStr(v1a), ColorRed, GameFont('Courier'), 650, 70);
			DrawText('Between: ' + FloatToStr(angle), ColorWhite, GameFont('Courier'), 650, 90);
						
			rot := 0;
			
			if IsKeyPressed(VK_LEFT) then
				rot := 5
			else if IsKeyPressed(VK_RIGHT) then
				rot := -5;
			
			if rot <> 0 then
			begin
				rm := RotationMatrix(rot);
				v2 := Multiply(rm, v2);
			end;

			rot := 0;

			if IsKeyPressed(VK_UP) then
				rot := 5
			else if IsKeyPressed(VK_DOWN) then
				rot := -5;
			
			if rot <> 0 then
			begin
				rm := RotationMatrix(rot);
				v1 := Multiply(rm, v1);
			end;
			
			if WasKeyTyped(VK_SPACE) then
			begin
				rm := RotationMatrix(angle);
				v1 := Multiply(rm, v1);
			end;
			
			RefreshScreen();
			if WindowCloseRequested() then exit;
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;

	procedure VectorExample2();
	const
		RW = 200;
		RH = 200;		
		RX = (800 - RW) div 2;
		RY = (600 - RH) div 2;
	var
		p: Point2D;
		rect: Rectangle;
		r, r2: Integer;
		movement, mvOut: Vector;
	begin
		r := 1;
		r2 := 2;		
		
		p := CreatePoint(100 + RX, 100 + RY);
		rect := CreateRectangle(RX, RY, RW, RH);
		
		movement := CreateVector(100, 0);
		
		repeat
			ProcessEvents();			
			
			if IsKeyPressed(VK_A) then movement := Multiply(RotationMatrix(-4.0), movement);
			if IsKeyPressed(VK_Z) then movement := Multiply(RotationMatrix(4.0), movement);
			
			if IsKeyPressed(VK_UP) then p.y := p.y - 5;
			if IsKeyPressed(VK_DOWN) then p.y := p.y + 5;
			if IsKeyPressed(VK_LEFT) then p.x := p.x - 5;
			if IsKeyPressed(VK_RIGHT) then p.x := p.x + 5;
							
			mvOut := VectorOutOfRectFromPoint(p, rect, movement);
//			WriteLn('after Rotat: ', movement.x:4:2, ',', movement.y:4:2);			
//			WriteLn('after mvOut: ', mvOut.x:4:2, ':', mvOut.y:4:2);

			ClearScreen(ColorBlack);
			DrawOverlay('Vector Calculations - Getting A Point Out Of A Rectangle');

			DrawRectangle(ColorRed, RX, RY, RW, RH);			
			DrawRectangle(ColorWhite, p.x - r, p.y - r, r2, r2);
			DrawLine(ColorWhite, p.x, p.y, p.x + movement.x, p.y + movement.y);
			
			if not ((mvOut.x = 0) and (mvOut.y = 0)) then
			begin
				DrawLine(ColorGreen, p.x, p.y, p.x + mvOut.x, p.y + mvOut.y);
			end;
			
			RefreshScreen();
			if WindowCloseRequested() then exit;
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;

	procedure VectorOutOfCircleExample();
	const
		RADIUS = 100;		
		RX = 400;
		RY = 300;
	var
		p, center: Point2D;
		r, r2: Integer;
		movement, mvOut: Vector;
	begin
		r := 1;
		r2 := 2;		

		p := CreatePoint(100 + RX, 100 + RY);
		center := CreatePoint(RX, RY);
		
		movement := CreateVector(100, 0);

		repeat
			ProcessEvents();			

			if IsKeyPressed(VK_A) then movement := Multiply(RotationMatrix(-4.0), movement);
			if IsKeyPressed(VK_Z) then movement := Multiply(RotationMatrix(4.0), movement);

			if IsKeyPressed(VK_UP) then p.y := p.y - 5;
			if IsKeyPressed(VK_DOWN) then p.y := p.y + 5;
			if IsKeyPressed(VK_LEFT) then p.x := p.x - 5;
			if IsKeyPressed(VK_RIGHT) then p.x := p.x + 5;

			mvOut := VectorOutOfCircleFromPoint(p, center, RADIUS, movement);
			
			ClearScreen(ColorBlack);
			DrawOverlay('Vector Calculations - Getting A Point Out Of A Circle');

			DrawCircle(ColorRed, center, RADIUS);			
			DrawRectangle(ColorWhite, p.x - r, p.y - r, r2, r2);
			DrawLine(ColorWhite, p.x, p.y, p.x + movement.x, p.y + movement.y);

			if not ((mvOut.x = 0) and (mvOut.y = 0)) then
			begin
				DrawLine(ColorGreen, p.x, p.y, p.x + mvOut.x, p.y + mvOut.y);
			end;

			RefreshScreen();
			if WindowCloseRequested() then exit;
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;

	procedure CircleOutOfCircleExample();
	const
		RADIUS = 100;
		RADIUS2 = 10;		
		RX = 400;
		RY = 300;
	var
		p, center: Point2D;
		r, r2: Integer;
		movement, mvOut: Vector;
	begin
		r := 1;
		r2 := 2;		

		p := CreatePoint(100 + RX, 100 + RY);
		center := CreatePoint(RX, RY);
		
		movement := CreateVector(100, 0);

		repeat
			ProcessEvents();			

			if IsKeyPressed(VK_A) then movement := Multiply(RotationMatrix(-4.0), movement);
			if IsKeyPressed(VK_Z) then movement := Multiply(RotationMatrix(4.0), movement);

			if IsKeyPressed(VK_UP) then p.y := p.y - 5;
			if IsKeyPressed(VK_DOWN) then p.y := p.y + 5;
			if IsKeyPressed(VK_LEFT) then p.x := p.x - 5;
			if IsKeyPressed(VK_RIGHT) then p.x := p.x + 5;

			mvOut := VectorOutOfCircleFromCircle(p, RADIUS2, center, RADIUS, movement);
			
			ClearScreen(ColorBlack);
			DrawOverlay('Vector Calculations - Getting A Circle Out Of A Circle');

			DrawCircle(ColorRed, center, RADIUS);			
			DrawCircle(ColorWhite, p, RADIUS2);
			DrawLine(ColorWhite, p.x, p.y, p.x + movement.x, p.y + movement.y);

			if not ((mvOut.x = 0) and (mvOut.y = 0)) then
			begin
				DrawLine(ColorGreen, p.x, p.y, p.x + mvOut.x, p.y + mvOut.y);
				DrawCircle(ColorGreen, p.x + mvOut.x, p.y + mvOut.y, RADIUS2);
			end;

			RefreshScreen();
			if WindowCloseRequested() then exit;
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;


	procedure VectorExample3();
	const
		RW = 200;
		RH = 200;		
		RX = (800 - RW) div 2;
		RY = (600 - RH) div 2;
	var
		mvRect, tgtRect: Rectangle;
		
		px, py: Single;
		halfH, halfW: Integer;
		movement, mvOut: Vector;
		enterRectFrom: CollisionSide;
	begin
		halfW := 5; halfH := 10;
		movement := CreateVector(100, 0);
		
		tgtRect := CreateRectangle(RX, RY, RW, RH);
		mvRect := CreateRectangle(100 + RX, 100 + RY, 10, 20);
		
		repeat
			ProcessEvents();			
			
			if IsKeyPressed(VK_A) then movement := Multiply(RotationMatrix(-4.0), movement);			
			if IsKeyPressed(VK_Z) then movement := Multiply(RotationMatrix(4.0), movement);
						
			if IsKeyPressed(VK_UP) then mvRect.y := mvRect.y - 5;
			if IsKeyPressed(VK_DOWN) then mvRect.y := mvRect.y + 5;
			if IsKeyPressed(VK_LEFT) then mvRect.x := mvRect.x - 5;
			if IsKeyPressed(VK_RIGHT) then mvRect.x := mvRect.x + 5;
			
			mvOut := VectorOutOfRectFromRect(mvRect, tgtRect, movement);
			enterRectFrom := GetSideForCollisionTest(movement);
			
			// furthest point... default top-left
			px := RectangleLeft(mvRect); 
			py := RectangleTop(mvRect);
			
			case enterRectFrom of
				//Hit top or left of wall... bottom right in
				TopLeft: 		begin px := RectangleRight(mvRect); py := RectangleBottom(mvRect);  end;
				//Hit top or right of wall... bottom left in
				TopRight:		py := RectangleBottom(mvRect);
				//Hit bottom or left of wall... top right in
				BottomLeft:		px := RectangleRight(mvRect);
				//Hit bottom or right of wall... top left is in
				BottomRight: 	;
				//Hit left of wall... right in
				Left: 			px := RectangleRight(mvRect);
				Right: 			;                                   
				//Hit top of wall... bottom in
				Top: 			py := RectangleBottom(mvRect);
				Bottom: 		;
			end;
			
			ClearScreen(ColorBlack);
			DrawOverlay('Vector Calculations - Getting A Rectangle Out Of A Rectangle');
			
			DrawRectangle(ColorRed, RX, RY, RW, RH);			
			DrawRectangle(ColorWhite, mvRect.x, mvRect.y, mvRect.width, mvRect.height);
			DrawLine(ColorWhite, mvRect.x + halfW, mvRect.y + halfH, mvRect.x + halfW + movement.x, mvRect.y + halfH + movement.y);

			if not ((mvOut.x = 0) and (mvOut.y = 0)) then
			begin
				DrawLine(ColorGrey, mvRect.x, mvRect.y, mvRect.x + mvOut.x, mvRect.y + mvOut.y);
				DrawRectangle(ColorGreen, mvRect.x + mvOut.x, mvRect.y + mvOut.y, mvRect.width, mvRect.height);			
				DrawLine(ColorGreen, px, py, px + mvOut.x, py + mvOut.y);
			end;
			
			RefreshScreen();
			if WindowCloseRequested() then exit;
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;

	procedure CollisionExample2();
	const
		LCX = 400;
		LCY = 300;
	var
		ball: Sprite;
		line, lineToPoint, normalLine: LineSegment;
		normal, toLine, lineVec, tempVec: Vector;
		rot, x, y, dist: Single;
		intersect: Point2D;
		pause : Boolean;
	
		procedure ResetBall();
		begin
			ball.x := 400 - CurrentWidth(ball) div 2;
			ball.y := 100;
			ball.movement := CreateVector(0, 3);
		end;
		
		procedure UpdateLine();
		begin
			line := LineFromVector(x, y, Multiply(ScaleMatrix(2), lineVec));
			normal := MultiplyVector(LineNormal(line), 50);			
			normalLine := LineFromVector(MidPoint(line), normal);
		end;
	
	begin
		ball := CreateSprite(GameImage('SmallBall'));
		ball.mass := 1;

		ResetBall();
		
		lineVec := CreateVector(100, 0);				
		x := LCX - lineVec.x;
		y := LCY - lineVec.y;

		UpdateLine();
		
		rot := 0;
		repeat
			pause := false;
			ProcessEvents();			
			
			if IsKeyPressed(VK_LEFT) then rot := -3;			
			if IsKeyPressed(VK_RIGHT) then rot := 3;

			if rot <> 0 then
			begin
				intersect := ClosestPointOnLine(CenterPoint(ball), line);
				dist := Sqrt(Abs(DistanceBetween(intersect, MidPoint(line)))) / 4;
				tempVec := Multiply(RotationMatrix(rot), CreateVector(rot * dist, 0));
				lineVec := Multiply(RotationMatrix(rot), lineVec);
				
				x := LCX - lineVec.x;
				y := LCY - lineVec.y;

				UpdateLine();
				
				if CircleHasCollidedWithLine(ball, line) then
				begin
					CircleCollisionWithLine(ball, line);
					UpdateSprite(ball);
					ball.movement := AddVectors(ball.movement, tempVec);
					pause := true;
				end;
				rot := 0;
			end;

			UpdateSprite(ball);
			
			intersect := ClosestPointOnLine(CenterPoint(ball), line);
			toLine := VectorFromCenterSpriteToPoint(ball, intersect);
			lineToPoint := LineFromVector(CenterPoint(ball), toLine);
			
			if IsSpriteOffscreen(ball) then ResetBall();

			if CircleHasCollidedWithLine(ball, line) then
			begin
				CircleCollisionWithLine(ball, line);
				UpdateSprite(ball);
				pause := true;
			end;
			
			ClearScreen(ColorBlack);
			DrawOverlay('Circle Collision with Line');

			DrawLine(ColorWhite, line);
			DrawLine(ColorRed, normalLine);
			DrawLine(ColorGrey, lineToPoint);
			DrawSprite(ball);
			RefreshScreen();
			
			if pause then Sleep(1000);
			
			if WindowCloseRequested() then exit;
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;
	
	//The main procedure that controlls the game logic.
	//
	// SIDE EFFECTS:
	// - Creates the screen, and displays a message
	procedure Main();
	begin
		OpenGraphicsWindow('SwinGameSDK Showcase', 800, 600);
		LoadResources();
		Randomize();
		ProcessEvents();
		DrawLines();
		if WindowCloseRequested() then exit;
		DrawRectangles();
		if WindowCloseRequested() then exit;
		DrawCircles();
		if WindowCloseRequested() then exit;
		DrawEllipses();
		if WindowCloseRequested() then exit;
		DrawBitmaps();
		if WindowCloseRequested() then exit;
		DrawSprites();
		if WindowCloseRequested() then exit;
		DrawCollisionDetection();
		if WindowCloseRequested() then exit;
		PlayMusicExample();
		if WindowCloseRequested() then exit;
		DrawRandomText();
		if WindowCloseRequested() then exit;
		DrawVectorCollision();
		if WindowCloseRequested() then exit;
		MoveSpriteWithInput();
		if WindowCloseRequested() then exit;
		MouseCursor();
		if WindowCloseRequested() then exit;
		TextReadExample();
		if WindowCloseRequested() then exit;
		SoundInput();
		if WindowCloseRequested() then exit;
		DroppingBall();
		if WindowCloseRequested() then exit;
		VectorAngleExample();
		if WindowCloseRequested() then exit;
		MultiBitmapSprite();
		If WindowCloseRequested() then exit;
		FollowSpriteExample();
		If WindowCloseRequested() then exit;
		DrawCircleWithLines();
		If WindowCloseRequested() then exit;
		MapExample();
		If WindowCloseRequested() then exit;
		MouseExample();	
		If WindowCloseRequested() then exit;
		VectorExample1();		
		If WindowCloseRequested() then exit;
		VectorExample2();

		If WindowCloseRequested() then exit;
		VectorExample3();
	
		CollisionExample2();
	
		VectorOutOfCircleExample();
		CircleOutOfCircleExample();
		
		FreeResources();
	end;
end.