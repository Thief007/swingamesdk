unit AlienFlightView;

interface
	uses 
		SysUtils, 
		SGSDK_Core, SGSDK_KeyCodes, SGSDK_Graphics, SGSDK_Font, SGSDK_Physics, SGSDK_Input,
		AlienFlightModel, GameResources;
	
	procedure DrawScreen(const data: GameDataType); overload;
	procedure DrawScreen(const data: GameDataType; showIDs: Boolean); overload;
	procedure DrawScreen(const data: GameDataType; showIDs, refresh: Boolean); overload;

	procedure DrawBackground(const data: GameDataType);
	procedure DrawUfo(const data: GameDataType);
	procedure DrawHud(const data: GameDataType);

	procedure DrawWelcome();
	procedure DrawHUDHelp();
	procedure DrawKeyHelp();
	procedure DrawThingsHelp();
	procedure DrawWatchOutForHelp();
	procedure DrawThatsYou();
	procedure DrawReadyIn(count: String);

		
implementation

	procedure DrawWelcome();
	const
		Message = 'Welcome to AlienFlight' + EOL + ' ' + EOL +
					' ----- ' + EOL + ' ' + EOL +
				  'A SwinGame Demo' + EOL + 
				  ' ' + EOL + ' ----- ' + EOL + ' ' + EOL +
				  'Developed by Andrew Cain' + EOL +
				  ' ' + EOL +
				 'Music Arranged by Pip Cain';
	begin
		DrawTextLines(Message, ColorWhite, ColorBlack, GameFont(WelcomeFont), AlignCenter, 100, 100, 600, 400);
	end;

	procedure DrawHUDHelp();
	const
		SS = 'Shield Strength';
		FL = 'Fuel Level';
	begin
		DrawText(SS, ColorWhite, GameFont(WelcomeFont), 24, 100);
		DrawText(FL, ColorWhite, GameFont(WelcomeFont), 595, 100);
	end;
	
	procedure DrawKeyHelp();
	const
		STEER = 'Move around with the ARROW keys';
		BOOST = 'Boost with the SPACEBAR';
		PAUSE = 'Pause with the P key';
		QUIT =  'Quit by closing the Window';
	
		Message = 
			'Controls' + EOL + 
			' ----- ' + EOL + ' ' + EOL +
			STEER + EOL +
			BOOST + EOL + 
			PAUSE + EOL +
			QUIT;
	begin
		DrawTextLines(Message, ColorWhite, ColorBlack, GameFont(WelcomeFont), AlignCenter, 100, 200, 600, 400);
	end;

	procedure DrawThingsHelp();
	const
		FUEL = 'Fuel barrels add to Fuel Level';
		SHIELD = 'Batteries add to Shield Strength';
		STARS = 'Stars add to your Score';
		WARP =  'Warp Holes move you to new levels';
	
		Message = 
			'Things to Collect' + EOL + 
			' ----- ' + EOL + ' ' + EOL +
			STARS + EOL +
			SHIELD + EOL + 
			FUEL + EOL +
			WARP;
	begin
		DrawTextLines(Message, ColorWhite, ColorBlack, GameFont(WelcomeFont), AlignCenter, 100, 200, 600, 400);
	end;

	procedure DrawWatchOutForHelp();
	const
		ASTERIODS = 'Large and small asteroids...';
		PLANETS = 'Planets that buzz by...';
	
		Message = 
			'Watch out for' + EOL + 
			' ----- ' + EOL + ' ' + EOL +
			ASTERIODS + EOL +
			PLANETS;
	begin
		DrawTextLines(Message, ColorWhite, ColorBlack, GameFont(WelcomeFont), AlignCenter, 100, 200, 600, 400);
	end;
	
	procedure DrawThatsYou();
	const
		SS = 'Thats you... what a nice ship!';
	begin
		DrawText(SS, ColorWhite, GameFont(WelcomeFont), 120, 400);
	end;

	procedure DrawReadyIn(count: String);
	begin
		DrawText(count, ColorWhite, GameFont(CourierHuge), 100, 190);
	end;

	procedure DrawScreen(const data: GameDataType); overload;
	begin
		DrawScreen(data, false, true);
	end;
	
	procedure DrawScreen(const data: GameDataType; showIDs: Boolean); overload;
	begin
		DrawScreen(data, showIDs, true);
	end;
	
	function Sign(data: Single): Single;
	begin
		if data < 0 then result := -1
		else if data > 0 then result := 1
		else result := 0;
	end;
	
	procedure DrawObstacleData(sprite: Sprite; name: String; move: Vector; windowX, windowY: Integer);
	var
		temp: Vector;
		sx, sy, cx, cy: Integer;
		mag: Single;
		change: Matrix2D;
	begin
		sx := Round(sprite.xPos - windowX);
		sy := Round(sprite.yPos - windowY);
		cx := sx + CurrentWidth(sprite) div 2;
		cy := sy + CurrentHeight(sprite) div 2;
		
		DrawText(name, ColorGreen, GameFont(Courier), sx, sy);

		mag := GetVectorMagnitude(move);
		
		if mag > 0 then
		begin
			change := ScaleMatrix(50);
			temp := Multiply(change, move);
		
			DrawLine(ColorRed, cx, cy, Round(cx + temp.x), Round(cy + temp.y));
		end;
	end;

	procedure DrawBackground(const data: GameDataType);
	var
		bgx, i: Integer;
		wy: Integer;
	begin
		wy := Round(data.windowY);
		
		bgx := Round(CalculateBackgroundX(data.windowX));

		DrawBitmap(GameImage(Background), 0 - bgx, 0);
		if IsKeyPressed(VK_RSHIFT) then DrawFramerate(2, SCREEN_HEIGHT - 20, GameFont(Courier));		

		for i := Low(data.MidBacks) to High(data.MidBacks) do
		begin
			DrawSprite(data.MidBacks[i], bgx, wy, SCREEN_WIDTH, SCREEN_HEIGHT);
		end;
	end;

	procedure DrawWarpHoles(const data: GameDataType; showIds: Boolean);
	var
		i: Integer;
	begin
		for i := Low(data.WarpHoles) to High(data.WarpHoles) do
		begin
			DrawSprite(data.WarpHoles[i].Sprite, Round(data.windowX), Round(data.windowY), SCREEN_WIDTH, SCREEN_HEIGHT);
			if showIDs then
			begin
				DrawObstacleData(data.WarpHoles[i].Sprite, IntToStr(data.WarpHoles[i].Level), 
					CreateVector(0, 0), Round(data.windowX), Round(data.windowY));
			end;
		end;
	end;

	procedure DrawUfo(const data: GameDataType);
	var
		x, y: Integer;
	begin
		x := Round(data.UfoSprite.xPos);
		y := Round(data.UfoSprite.yPos);
		DrawBitmap(data.ShieldBmp, x - Round(data.windowX) - SHIELD_OFFSET, y - Round(data.windowY) - SHIELD_OFFSET);
		DrawSprite(data.UfoSprite, Round(data.windowX), Round(data.windowY), SCREEN_WIDTH, SCREEN_HEIGHT);
	end;
	
	procedure DrawAnimation(const anim: Animation; wx, wy: Integer);
	var
		sx, sy: Integer;
	begin
		with anim do
		begin
			if used then
			begin
				sx := (currentCell mod cols) * partWidth;
				sy := (currentCell - (currentCell mod cols)) div cols * partHeight;
				DrawBitmapPart(image, sx, sy, partWidth, partHeight, 
					Round(x) - wx, Round(y) - wy);
			end;
		end;		
	end;
	
	procedure DrawAnimations(const data: GameDataType);
	var
		i, wx, wy: Integer;
	begin
		wx := Round(data.windowX);
		wy := Round(data.windowY);
		
		for i := 0 to High(data.temporaryAnimations) do
		begin
			DrawAnimation(data.temporaryAnimations[i], wx, wy);
		end;

		for i := 0 to High(data.levelAnimations) do
		begin
			DrawAnimation(data.levelAnimations[i], wx, wy);
		end;
	end;

	procedure DrawLevel(level: Integer);
	const
		LEVEL_X = 490;
		LEVEL_Y = 60;
	begin
		DrawText( IntToStr(level), ColorWhite, GameFont(Courier), LEVEL_X, LEVEL_Y);
	end;

	procedure DrawScore(score: Integer);
	const
		SCORE_X = 350;
		SCORE_Y = 60;
	begin
		DrawText( IntToStr(score), ColorWhite, GameFont(Courier), SCORE_X, SCORE_Y);
	end;
	
	procedure DrawHud(const data: GameDataType);
	const
		SHIELD_X = 24; SHIELD_Y = 15;
		BAR_W = 244; BAR_H = 42;
		FUEL_X = 532; FUEL_Y = 15;
	var
		width, srcX, dstX: Integer;
	begin 
		DrawBitmap(GameImage(HUDImg), 0, 0);
		
		width := Round(BAR_W * data.shieldStrength);
		DrawBitmapPart(GameImage(ShieldStrengthImg), 0, 0, width, BAR_H, SHIELD_X, SHIELD_Y);
		
		width := Round(BAR_W * data.fuelLevel / MAX_FUEL);
		srcX := BAR_W - width;
		dstX := FUEL_X + srcX;
		DrawBitmapPart(GameImage(FuelLevelImg), srcX, 0, width, BAR_H, dstX, SHIELD_Y);
		
		DrawScore(data.score);
		DrawLevel(data.currentLevel);
	end;

	procedure DrawBackScene(const data: GameDataType; showIDs: Boolean); overload;
	var
		i: Integer;
		wx, wy: Integer;
	begin
		wx := Round(data.windowX);
		wy := Round(data.windowY);
		
		DrawBackground(data);
		DrawWarpHoles(data, showIDs);
		
		//Draw Planets
		for i := Low(data.Planets) to High(data.Planets) do
		begin
			if data.Planets[i].Alive then
			begin
				DrawSprite(data.Planets[i].Sprite, wx, wy, SCREEN_WIDTH, SCREEN_HEIGHT);
				if showIDs then
				begin
					DrawObstacleData(data.Planets[i].Sprite, data.Planets[i].Name, 
						data.Planets[i].Movement, wx, wy);
				end;
			end;
		end;		
	end;
	
	procedure DrawOverlay(const data: GameDataType);
	begin
		DrawAnimations(data);
		
		DrawHud(data);
	end;

	procedure DrawEditorGuides(const data: GameDataType); overload;
	var
		i: Integer;
		wx, wy, maxX: Integer;
	begin
		wx := Round(data.windowX);
		wy := Round(data.windowY);
		maxX := Round(MaxForegroundX());
		
		DrawText( IntToStr(wx) + ',' + IntToStr(wy), ColorWhite, GameFont(Courier), 100, 60);
		
		if wx < 0 then
		begin
			DrawVerticalLine(ColorWhite, 0 - wx, 0, SCREEN_HEIGHT);
		end
		else if wx > maxX - SCREEN_WIDTH then
		begin
			DrawVerticalLine(ColorWhite, maxX - wx, 0, SCREEN_HEIGHT);
		end;

		if wy <> 0 then
		begin
			if wy < 0 then
			begin
				i := -wy;
			end
			else
			begin
				i := SCREEN_HEIGHT - wy;
			end;
			DrawHorizontalLine(ColorWhite, i, 0, SCREEN_WIDTH);
		end;
	end;
	
	procedure DrawScreen(const data: GameDataType; showIDs, refresh: Boolean); overload;
	begin
		DrawBackScene(data, showIDs);
		
		//Draw the Player
		DrawUfo(data);

		if data.state = EditorState then
		begin
				DrawEditorGuides(data);
		end;
		
		DrawOverlay(data);
		
		if refresh then RefreshScreen();
	end;
	
end.