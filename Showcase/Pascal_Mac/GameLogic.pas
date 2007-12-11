unit GameLogic;

interface
	procedure Main();

implementation
	uses
	GameResources,
	SysUtils,
	SGSDK_Core, SGSDK_Font, SGSDK_Audio, SGSDK_Graphics, SGSDK_Input, SGSDK_Physics,
	SGSDK_KeyCodes;
	
	procedure DrawOverlay(title : String);
	begin
		FillRectangle(ColorBlack, 0, 0, 800, 50);
        DrawText(title, ColorWhite, GameFont('Courier'), Round((ScreenWidth() / 2) - ((Length(title) / 2) * 10)), 20);
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
			RefreshScreen(60);
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
			RefreshScreen(60);
			if WindowCloseRequested() then break;
			Sleep(100);
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;
	
	procedure DrawCircles();
	begin
		ClearScreen();
		repeat
			DrawCircle(GetRandomColor(), Random(800), Random(800), Random(800));
			FillCircle(GetRandomColor(), Random(800), Random(800), Random(800));
			DrawOverlay('Drawing Circles Example');
			Sleep(100);
			ProcessEvents();
			RefreshScreen(60);
			if WindowCloseRequested() then break;
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;
	
	procedure DrawEllipses();
	begin
		ClearScreen();
		repeat
			DrawEllipse(GetRandomColor(), Random(800), Random(800), Random(400), Random(400));
			FillEllipse(GetRandomColor(), Random(800), Random(800), Random(400), Random(400));
			DrawOverlay('Drawing Ellipses Example');
			Sleep(100);
			ProcessEvents();
			RefreshScreen(60);
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
			RefreshScreen(60);
			i := i + 1;
			if WindowCloseRequested() then break;
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;
	
	procedure DrawSpriteCaption(sprite : Sprite; caption : String);
	begin
		DrawText(caption, ColorWhite, GameFont('Courier'), 
			Round((sprite.xPos + CurrentWidth(sprite) / 2) - ((Length(caption) / 2) * 10)), 
			Round(sprite.yPos + CurrentHeight(sprite)));
	end;
	
	procedure DrawSprites();
	var
		loopSprite, reverseSprite, stopSprite, reverseOnceSprite : Sprite;
	begin
		loopSprite := CreateSprite(GameImage('Running'), 2, 15, 80, 94);
		loopSprite.endingAction := Loop;
		reverseSprite := CreateSprite(GameImage('Running'), 2, 15, 80, 94);
		reverseSprite.endingAction := ReverseLoop;
		stopSprite := CreateSprite(GameImage('Running'), 2, 15, 80, 94);
		stopSprite.endingAction := Stop;
		reverseOnceSprite := CreateSprite(GameImage('Running'), 2, 15, 80, 94);
		reverseOnceSprite.endingAction := ReverseOnce;
		loopSprite.xPos := 50;
		loopSprite.yPos := 200;
		reverseSprite.xPos := 150;
		reverseSprite.yPos := 200;
		stopSprite.xPos := 350;
		stopSprite.yPos := 200;
		reverseOnceSprite.xPos := 450;
		reverseOnceSprite.yPos := 200;
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
			RefreshScreen(60);
			if WindowCloseRequested() then break;
			Sleep(50);
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;
	
	procedure MoveBall(var ball : Sprite; var xSpeed, ySpeed : Integer);
	begin
		ball.xPos := ball.xPos + xSpeed;
		ball.yPos := ball.yPos + ySpeed;
		if ball.xPos > ScreenWidth() - CurrentWidth(ball) then
		begin
			ball.xPos := ScreenWidth() - CurrentWidth(ball);
			xSpeed := -1 * xSpeed;
		end;
		if ball.yPos > ScreenHeight() - CurrentHeight(ball) then
		begin
			ball.yPos := ScreenHeight() - CurrentHeight(ball);
			ySpeed := -1 * ySpeed;
		end;
		if ball.xPos < 0 then
		begin
			ball.xPos := 0;
			xSpeed := -1 * xSpeed;
		end;
		if ball.yPos < 0 then
		begin
			ball.yPos := 0;
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
		ball1.xPos := 0;
		ball1.yPos := 0;
		ball2.xPos := ScreenWidth() - CurrentWidth(ball2);
		ball2.yPos := ScreenHeight() - CurrentHeight(ball2);
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
			RefreshScreen(60);
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
			Sleep(10);
			RefreshScreen(60);
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
			Sleep(10);
			RefreshScreen(60);
			ProcessEvents();
			if WindowCloseRequested() then break;
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;
	
	procedure MoveBallUsingVector(var ball : Sprite);
	begin
		MoveSprite(ball, ball.movement);
		if ball.xPos > ScreenWidth() - CurrentWidth(ball) then
		begin
			ball.movement.x := ball.movement.x * -1;
			ball.xPos := ScreenWidth() - CurrentWidth(ball);
		end;
		if ball.yPos > ScreenHeight() - CurrentHeight(ball) then
		begin
			ball.movement.y := ball.movement.y * -1;
			ball.yPos := ScreenHeight() - CurrentHeight(ball);
		end;
		if ball.xPos < 0 then
		begin
			ball.movement.x := ball.movement.x * -1;
			ball.xPos := 0;
		end;
		if ball.yPos < 0 then
		begin
			ball.movement.y := ball.movement.y * -1;
			ball.yPos := 0;
		end;
	end;
	
	procedure DrawVectorCollision();
	var
		ball1, ball2 : Sprite;
	begin
		ClearScreen();
		ball1 := CreateSprite(GameImage('BallImage1'));
		ball2 := CreateSprite(GameImage('BallImage2'));
		ball1.movement := CreateVector(3, 3);
		ball2.movement := CreateVector(3, 3);
		ball1.mass := 1;
		ball2.mass := 1;
		ball1.xPos := 0;
		ball1.yPos := 0;
		ball2.xPos := ScreenWidth() - CurrentWidth(ball2);
		ball2.yPos := ScreenHeight() - CurrentHeight(ball2);
		ball1.usePixelCollision := true;
		ball2.usePixelCollision := true;
		repeat
			ClearScreen();
			DrawSprite(ball1);
			DrawSprite(ball2);
			if HaveSpritesCollided(ball1, ball2) then
				VectorCollision(ball1, ball2);
			MoveBallUsingVector(ball1);
			MoveBallUsingVector(ball2);
			DrawOverlay('Vector Collision Example');
			ProcessEvents();
			RefreshScreen(60);
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
	    ball.xPos := 400;
	    ball.yPos := 300;

	    repeat
	        xSpeed := 0;
	        ySpeed := 0;

	        if IsKeyPressed(VK_UP) then
	            ySpeed := -1;
	        if IsKeyPressed(VK_DOWN) then
	            ySpeed := 1;
	        if IsKeyPressed(VK_LEFT) then
	            xSpeed := -1;
	        if IsKeyPressed(VK_RIGHT) then
	            xSpeed := 1;

	        DrawSprite(ball);
	        ball.xPos := ball.xPos + xSpeed;
			ball.yPos := ball.yPos + ySpeed;

	        DrawOverlay('Move Sprite with Arrow Keys Example');
	        ProcessEvents();
	        RefreshScreen(60);
	        ClearScreen();
			if WindowCloseRequested() then exit;
	    until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
	end;
	
	procedure MouseCursor();
	var
		ball : Sprite;
		position : Vector;
	begin
		ball := CreateSprite(GameImage('BallImage1'));
        repeat
			ProcessEvents();
			
            position := GetMousePosition();
			
            DrawHorizontalLine(ColorWhite, Round(position.Y), 0, 800);
            DrawVerticalLine(ColorWhite, Round(position.X), 0, 600);
			
            if MouseWasClicked(LeftButton) then
            begin
                ball.xPos := position.X - (CurrentWidth(ball) / 2);
                ball.yPos := position.Y - (CurrentHeight(ball) / 2);
                DrawSprite(ball);
            end;

            DrawOverlay('Mouse Cursor Example');
            RefreshScreen(60);
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
			RefreshScreen(60);
			ProcessEvents();
			if WindowCloseRequested() then exit;
		end;
		DrawText('You have entered ' + TextReadAsASCII(), ColorGreen, GameFont('Courier'), 0, 80);
		RefreshScreen(60);
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
            RefreshScreen(60);
            ClearScreen();
			
            if WindowCloseRequested() then break;
		until IsKeyPressed(VK_N);
		Sleep(500);
		ProcessEvents();
    end;
	
	procedure DroppingBall();
	var
		ball: Sprite;
		GravityConst, AirResistanceV, AirResistanceH: Vector;
		Rotate: Matrix2D;
		falling: Boolean;
	begin
		GravityConst := CreateVector(0, 0.5);
		AirResistanceV := CreateVector(0, 0.1);
		AirResistanceH := CreateVector(0.01, 0);

		Rotate := RotationMatrix(180);

		ball := CreateSprite(GameImage('SmallBall'));
		ball.movement := CreateVector(5, 0);
		ball.mass := 1;

		ball.xPos := 0;
		ball.yPos := 0;

		falling := false;

		repeat
			ClearScreen();
			if (not falling) and (ball.movement.y >= 0) then
			begin
				AirResistanceV := Multiply(Rotate, AirResistanceV);
				falling := true;
			end;

	        if (ball.Movement.x < 0) and (AirResistanceH.x < 0) then
	        begin
	            AirResistanceH := InvertVector(AirResistanceH);
	        end;

	        if (ball.Movement.x > 0) and (AirResistanceH.x > 0) then
	        begin
	            AirResistanceH := InvertVector(AirResistanceH);
	        end;

			ball.movement := AddVectors(ball.movement, GravityConst);
			ball.movement := AddVectors(ball.movement, AirResistanceV);
			ball.movement := AddVectors(ball.movement, AirResistanceH);

			MoveSprite(ball, ball.movement);

			if ball.xPos > ScreenWidth() - CurrentWidth(ball) then
			begin
				ball.movement.x := ball.movement.x * -1;
				ball.xPos := ScreenWidth() - CurrentWidth(ball);
				AirResistanceH := InvertVector(AirResistanceH);
			end;
			if ball.yPos > ScreenHeight() - CurrentHeight(ball) then
			begin
				if ball.movement.y < 1 then
					ball.movement.y := 0;
				ball.movement.y := ball.movement.y * -1;
				ball.yPos := ScreenHeight() - CurrentHeight(ball);
				AirResistanceV := Multiply(Rotate, AirResistanceV);
				falling := false;
			end;
			if ball.xPos < 0 then
			begin
				ball.movement.x := ball.movement.x * -1;
				ball.xPos := 0;
			end;
			if ball.yPos < 0 then
			begin
				ball.movement.y := ball.movement.y * -1;
				ball.yPos := 0;
			end;
			DrawSprite(ball);

			ProcessEvents();
			RefreshScreen(60);
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
		sp1.xPos := 70;
		sp1.yPos := 100;
		sp2.xPos := 80;
		sp2.yPos := 110;
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
			RefreshScreen(60);
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
		Ship.xPos := 0;
		Ship.yPos := 0;
		repeat
			if IsKeyPressed(VK_RIGHT) then
				Ship.xPos := Ship.xPos + 4;
			if IsKeyPressed(VK_DOWN) then
				Ship.yPos := Ship.yPos + 4;
			if IsKeyPressed(VK_UP) then
				Ship.yPos := Ship.yPos - 4;
			if IsKeyPressed(VK_LEFT) then
				Ship.xPos := Ship.xPos - 4;
			FollowSprite(Ship, 0, -150);
			ProcessEvents();
			ClearScreen();
			DrawBitmap(Sea, 0, 0);
			DrawSprite(Ship);
			UpdateSprite(Ship);
			RefreshScreen(60);
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
		
		repeat
			ProcessEvents();
			DrawLines();
			if WindowCloseRequested() then break;
			DrawRectangles();
			if WindowCloseRequested() then break;
			DrawCircles();
			if WindowCloseRequested() then break;
			DrawEllipses();
			if WindowCloseRequested() then break;
			DrawBitmaps();
			if WindowCloseRequested() then break;
			DrawSprites();
			if WindowCloseRequested() then break;
			DrawCollisionDetection();
			if WindowCloseRequested() then break;
			PlayMusicExample();
			if WindowCloseRequested() then break;
			DrawRandomText();
			if WindowCloseRequested() then break;
			DrawVectorCollision();
			if WindowCloseRequested() then break;
			MoveSpriteWithInput();
			if WindowCloseRequested() then break;
			MouseCursor();
			if WindowCloseRequested() then break;
			TextReadExample();
			if WindowCloseRequested() then break;
			SoundInput();
			if WindowCloseRequested() then break;
			DroppingBall();
			if WindowCloseRequested() then break;
			MultiBitmapSprite();
			If WindowCloseRequested() then break;
			FollowSpriteExample();
		until WindowCloseRequested();
		
		FreeResources();
	end;
end.