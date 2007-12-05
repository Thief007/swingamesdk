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
        DrawText(title, ColorWhite, GameFont(Courier), Round((ScreenWidth() / 2) - ((Length(title) / 2) * 10)), 20);
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
	var
		i : Integer;
	begin
		ClearScreen();
		for i := 0 to 999 do
		begin
			DrawLine(GetRandomColor(), Random(800), Random(800), Random(800), Random(800));
			DrawOverlay('Drawing Lines Example');
			ProcessEvents();
			RefreshScreen();
			if WindowCloseRequested() then break;
		end;
	end;
	
	procedure DrawRectangles();
	var
		i : Integer;
	begin
		ClearScreen();
		for i := 0 to 99 do
		begin
			DrawRectangle(GetRandomColor(), Random(800), Random(800), Random(800), Random(800));
			FillRectangle(GetRandomColor(), Random(800), Random(800), Random(800), Random(800));
			DrawOverlay('Drawing Rectangles Example');
			ProcessEvents();
			RefreshScreen();
			if WindowCloseRequested() then break;
			Sleep(100);
		end;
	end;
	
	procedure DrawCircles();
	var
		i : Integer;
	begin
		ClearScreen();
		for i := 0 to 99 do
		begin
			DrawCircle(GetRandomColor(), Random(800), Random(800), Random(800));
			FillCircle(GetRandomColor(), Random(800), Random(800), Random(800));
			DrawOverlay('Drawing Circles Example');
			Sleep(100);
			ProcessEvents();
			RefreshScreen();
			if WindowCloseRequested() then break;
		end;
	end;
	
	procedure DrawEllipses();
	var
		i : Integer;
	begin
		ClearScreen();
		for i := 0 to 99 do
		begin
			DrawEllipse(GetRandomColor(), Random(800), Random(800), Random(400), Random(400));
			FillEllipse(GetRandomColor(), Random(800), Random(800), Random(400), Random(400));
			DrawOverlay('Drawing Ellipses Example');
			Sleep(100);
			ProcessEvents();
			RefreshScreen();
			if WindowCloseRequested() then break;
		end;
	end;
	
	procedure DrawBitmaps();
	var
		i : Integer;
		tempBitmap, tempBitmap2 : Bitmap;
	begin
		ClearScreen();
		tempBitmap := GameImage(BallImage1);
		tempBitmap2 := GameImage(BallImage2);
		for i := 0 to 900 do
		begin
			ClearScreen();
			DrawBitmap(tempBitmap, round(SGSDK_Core.sin(i) * 100) + 250, round(SGSDK_Core.cos(i) * 100) + 200);
			DrawBitmap(tempBitmap2, round(SGSDK_Core.cos(i) * 100) + 250, round(SGSDK_Core.sin(i) * 100) + 200);
			DrawOverlay('Drawing Bitmap Example');
			ProcessEvents();
			RefreshScreen();
			if WindowCloseRequested() then break;
		end;
	end;
	
	procedure DrawSpriteCaption(sprite : Sprite; caption : String);
	begin
		DrawText(caption, ColorWhite, GameFont(Courier), 
			Round((sprite.xPos + CurrentWidth(sprite) / 2) - ((Length(caption) / 2) * 10)), 
			Round(sprite.yPos + CurrentHeight(sprite)));
	end;
	
	procedure DrawSprites();
	var
		i : Integer;
		loopSprite, reverseSprite, stopSprite, reverseOnceSprite : Sprite;
		tempBitmaps : Array of Bitmap;
		tempIntegers : Array of Integer;
		tempString : String;
	begin
		SetLength(tempBitmaps, 15);
		for i := 0 to 14 do
		begin
			Str(i, tempString);
			tempBitmaps[i] := LoadBitmap(GetPathToResource('run' + tempString + '.png', ImageResource));
		end;
		SetLength(tempIntegers, 15);
		for i := 0 to 14 do
		begin
			tempIntegers[i] := 2;
		end;
		loopSprite := CreateSprite(tempBitmaps, tempIntegers, Loop);
		reverseSprite := CreateSprite(tempBitmaps, tempIntegers, ReverseLoop);
		stopSprite := CreateSprite(tempBitmaps, tempIntegers, Stop);
		reverseOnceSprite := CreateSprite(tempBitmaps, tempIntegers, ReverseOnce);
		loopSprite.xPos := 50;
		loopSprite.yPos := 200;
		reverseSprite.xPos := 150;
		reverseSprite.yPos := 200;
		stopSprite.xPos := 350;
		stopSprite.yPos := 200;
		reverseOnceSprite.xPos := 450;
		reverseOnceSprite.yPos := 200;
		for i := 0 to 150 do
		begin
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
			Sleep(50);
		end;
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
		i : Integer;
		ball1, ball2 : Sprite;
		xSpeed1, xSpeed2, ySpeed1, ySpeed2 : Integer;
	begin
		ClearScreen();
		xSpeed1 := 3;
		xSpeed2 := 3;
		ySpeed1 := 3;
		ySpeed2 := 3;
		ball1 := CreateSprite(GameImage(BallImage1));
		ball2 := CreateSprite(GameImage(BallImage2));
		ball1.xPos := 0;
		ball1.yPos := 0;
		ball2.xPos := ScreenWidth() - CurrentWidth(ball2);
		ball2.yPos := ScreenHeight() - CurrentHeight(ball2);
		ball1.usePixelCollision := true;
		ball2.usePixelCollision := true;
		for i := 0 to 1200 do
		begin
			ClearScreen();
			DrawSprite(ball1);
			DrawSprite(ball2);
			if HaveSpritesCollided(ball1, ball2) then
				DrawText('Collided!', ColorWhite, GameFont(Courier), ScreenWidth() - 90, ScreenHeight() - 20);
			MoveBall(ball1, xSpeed1, ySpeed1);
			MoveBall(ball2, xSpeed2, ySpeed2);
			DrawOverlay('Collision Detection Example');
			ProcessEvents();
			RefreshScreen();
			if WindowCloseRequested() then break;
		end;
	end;
	
	procedure PlayMusicExample();
	var
		i : Integer;
		musicSource : Music;
	begin
		ClearScreen();
		musicSource := LoadMusic(GetPathToResource('Fast.mp3', SoundResource));
		PlayMusic(musicSource);
		for i := 0 to 700 do
		begin
			DrawOverlay('Music Playback Example');
			Sleep(10);
			RefreshScreen();
			ProcessEvents();
			if WindowCloseRequested() then break;
		end;
		StopMusic();
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
	var
		i : Integer;
	begin
		ClearScreen();
		for i := 0 to 500 do
		begin
			SetFontStyle(GameFont(Courier), GetRandomFontStyle());
			DrawText('SwinGameSDK!', GetRandomColor(), GameFont(Courier), Random(ScreenWidth()), Random(ScreenHeight()));
			SetFontStyle(GameFont(Courier), GetRandomFontStyle());
			DrawText('SwinGameSDK!', GetRandomColor(), GameFont(Courier), Random(ScreenWidth()), Random(ScreenHeight()));
			SetFontStyle(GameFont(Courier), NormalFont);
			DrawOverlay('Drawing Random Texts');
			Sleep(10);
			RefreshScreen();
			ProcessEvents();
			if WindowCloseRequested() then break;
		end;
	end;
	
	procedure MoveBallUsingVector(var ball : PhysicsData);
	begin
		MoveSprite(ball.sprite, ball.movement);
		if ball.sprite.xPos > ScreenWidth() - CurrentWidth(ball.sprite) then
		begin
			ball.movement.x := ball.movement.x * -1;
			ball.sprite.xPos := ScreenWidth() - CurrentWidth(ball.sprite);
		end;
		if ball.sprite.yPos > ScreenHeight() - CurrentHeight(ball.sprite) then
		begin
			ball.movement.y := ball.movement.y * -1;
			ball.sprite.yPos := ScreenHeight() - CurrentHeight(ball.sprite);
		end;
		if ball.sprite.xPos < 0 then
		begin
			ball.movement.x := ball.movement.x * -1;
			ball.sprite.xPos := 0;
		end;
		if ball.sprite.yPos < 0 then
		begin
			ball.movement.y := ball.movement.y * -1;
			ball.sprite.yPos := 0;
		end;
	end;
	
	procedure DrawVectorCollision();
	var
		i : Integer;
		ball1, ball2 : PhysicsData;
	begin
		ClearScreen();
		ball1.movement := CreateVector(3, 3);
		ball2.movement := CreateVector(3, 3);
		ball1.mass := 1;
		ball2.mass := 1;
		ball1.sprite := CreateSprite(GameImage(BallImage1));
		ball2.sprite := CreateSprite(GameImage(BallImage2));
		ball1.sprite.xPos := 0;
		ball1.sprite.yPos := 0;
		ball2.sprite.xPos := ScreenWidth() - CurrentWidth(ball2.sprite);
		ball2.sprite.yPos := ScreenHeight() - CurrentHeight(ball2.sprite);
		ball1.sprite.usePixelCollision := true;
		ball2.sprite.usePixelCollision := true;
		for i := 0 to 4200 do
		begin
			ClearScreen();
			DrawSprite(ball1.sprite);
			DrawSprite(ball2.sprite);
			if HaveSpritesCollided(ball1.sprite, ball2.sprite) then
				VectorCollision(ball1, ball2);
			MoveBallUsingVector(ball1);
			MoveBallUsingVector(ball2);
			DrawOverlay('Vector Collision Example');
			ProcessEvents();
			RefreshScreen();
			if WindowCloseRequested() then break;
		end;
	end;
	
	procedure MoveSpriteWithInput();
	var
		ball : Sprite;
		xSpeed, ySpeed : Integer;
		i : Integer;
	begin
		ball := CreateSprite(LoadBitmap(GetPathToResource('ball.png', ImageResource)));
	    ball.xPos := 400;
	    ball.yPos := 300;

	    for i := 0 to 1499 do
	    begin
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
	        RefreshScreen();
	        ClearScreen();
			if WindowCloseRequested() then exit;
	    end;
	end;
	
	procedure MouseCursor();
	var
		ball : Sprite;
		position : Vector;
		i : Integer;
	begin
		ball := CreateSprite(LoadBitmap(GetPathToResource('ball.png', ImageResource)));
        for i := 0 to 999 do
        begin
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
            RefreshScreen();
            ClearScreen();
			if WindowCloseRequested() then exit;
        end;
	end;
	
	//The main procedure that controlls the game logic.
	//
	// SIDE EFFECTS:
	// - Creates the screen, and displays a message
	procedure Main();
	begin
		OpenGraphicsWindow('My Game', 640, 480);

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
		until WindowCloseRequested();
		
		FreeResources();
	end;
end.