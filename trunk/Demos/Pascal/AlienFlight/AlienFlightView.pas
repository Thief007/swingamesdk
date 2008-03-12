unit AlienFlightView;

interface
	uses 
		SysUtils, 
		SGSDK_Core, SGSDK_KeyCodes, SGSDK_Graphics, SGSDK_Font, SGSDK_Physics, SGSDK_Input, SGSDK_Camera,
		AlienFlightModel, AlienFlightLevelEditor, GameResources;	
	
	procedure DrawScreen(const data: GameDataType); overload;
	procedure DrawScreen(const data: GameDataType; showIDs: Boolean); overload;
	procedure DrawScreen(const data: GameDataType; showIDs, refresh: Boolean); overload;

	procedure DrawBackground(const data: GameDataType);
	procedure DrawBackScene(const data: GameDataType; showIDs: Boolean);
	procedure DrawUfo(const data: GameDataType);
	procedure DrawHud(const data: GameDataType);

	procedure DrawWelcome();
	procedure DrawHUDHelp();
	procedure DrawKeyHelp();
	procedure DrawThingsHelp();
	procedure DrawWatchOutForHelp();
	procedure DrawThatsYou();
	procedure DrawReadyIn(count: String);
	procedure DrawMessage(message: String);
	//procedure DrawOutline(const data: GameDataType; const sprt: AlienFlightSprite);
	procedure DrawTextIn(toDraw: String; color: Color; txtFont: Font; x, y, w, h: Integer);
		
	procedure DrawEditorStatus(const status: EditorStatusType);
		
	procedure OutlineSprite(const sprt: AlienFlightSprite; c: Color);
		
implementation

	procedure OutlineSprite(const sprt: AlienFlightSprite; c: Color);
	var
		x : Single;
	begin
		if _Templates[sprt.kind].foreground then
			DrawRectangle(c, sprt.sprite.x, sprt.sprite.y, CurrentWidth(sprt.sprite), CurrentHeight(sprt.sprite))
		else
		begin
			x := Round(sprt.sprite.x - CalculateBackgroundX(XOffset()) + XOffset());
			DrawRectangle(c, x, sprt.sprite.y, CurrentWidth(sprt.sprite), CurrentHeight(sprt.sprite));
		end;
	end;

	procedure DrawOutline(const status: EditorStatusType; const sprt: AlienFlightSprite);
	var
		c: Color;
	begin
		c := ColorWhite;
		
		if (status.selectedIdx < Length(status.selection)) and (@sprt = status.selection[status.selectedIdx]) then c := ColorYellow;
		OutlineSprite(sprt, c);
	end;

	procedure DrawMessage(message: String);
	begin		
		DrawTextLinesOnScreen(Message, ColorWhite, ColorBlack, GameFont(WelcomeFont), AlignCenter, 50, 100, 700, 400);
	end;


	procedure DrawWelcome();
	const
		MESSAGE = 'Welcome to AlienFlight' + EOL + ' ' + EOL +
					' ----- ' + EOL + ' ' + EOL +
				  'A SwinGame Demo' + EOL + 
				  ' ' + EOL + ' ----- ' + EOL + ' ' + EOL +
				  'Developed by Andrew Cain' + EOL +
				 'Music Arranged by Pip Cain' + EOL +
				 'Levels by Aki, Grinneh, and Example...';
	begin
		DrawMessage(MESSAGE);
		//DrawTextLines(Message, ColorWhite, ColorBlack, GameFont(WelcomeFont), AlignCenter, 100, 100, 600, 400);
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
	
	procedure DrawObstacleData(sprite: Sprite; idx: Integer; move: Vector);
	var
		temp: Vector;
		sx, sy, cx, cy: Single;
		mag: Single;
		change: Matrix2D;
	begin
		sx := sprite.x;
		sy := sprite.y;
		cx := sx + CurrentWidth(sprite) div 2;
		cy := sy + CurrentHeight(sprite) div 2;
		
		DrawText(IntToStr(idx), ColorGreen, GameFont(Courier), sx, sy);

		mag := Magnitude(move);
		
		if mag > 0 then
		begin
			change := ScaleMatrix(50);
			temp := Multiply(change, move);
		
			DrawLine(ColorRed, cx, cy, cx + temp.x, cy + temp.y);
		end;
	end;

	procedure DrawBackground(const data: GameDataType);
	var
		bgx, i: Integer;
		scrX: Integer;
	begin
		bgx := Round(CalculateBackgroundX(XOffset()));

		DrawBitmapOnScreen(GameImage(Background), 0 - bgx, 0);
	
		if IsKeyPressed(VK_RSHIFT) then DrawFramerate(2, SCREEN_HEIGHT - 20, GameFont(Courier));		

		for i := Low(data.sprites) to High(data.sprites) do
		begin
			if _Templates[data.sprites[i].kind].foreground = false then
			begin
				scrX := Round(data.sprites[i].sprite.x - CalculateBackgroundX(XOffset()) + XOffset());
				
				DrawBitmap(data.sprites[i].sprite.bitmaps[0], scrX, Round(data.sprites[i].sprite.y));
				//DrawSprite(data.sprites[i].sprite, -bgx, -wy);
			end;
		end;
	end;

	procedure DrawUfo(const data: GameDataType);
	var
		x, y: Single;
	begin
		x := data.UfoSprite.x;
		y := data.UfoSprite.y;
		DrawBitmap(data.ShieldBmp, x - SHIELD_OFFSET, y - SHIELD_OFFSET);
		DrawSprite(data.UfoSprite);
	end;
	
	procedure DrawAlienFlightSprite(const sprt: AlienFlightSprite);
	begin
		//WriteLn('DrawAlienFlightSprite');
		if sprt.active then
		begin
			//WriteLn('- ', HexStr(sprt.sprite), ' ', sprt.sprite.x:4:2, ',', sprt.sprite.y:4:2, ' : ', Length(sprt.sprite.bitmaps), ' ', HexStr(sprt.sprite.bitmaps[0]));
			
			DrawSprite(sprt.sprite);
			//WriteLn(' - Drawn');
			{sx := (currentCell mod cols) * partWidth;
			sy := (currentCell - (currentCell mod cols)) div cols * partHeight;
			DrawBitmapPart(image, sx, sy, partWidth, partHeight, x, y);}
		end;

		//WriteLn(' - End DrawAlienFlightSprite');
	end;
	
	procedure DrawAnimations(const data: GameDataType);
	var
		i: Integer;
	begin
		for i := 0 to High(data.animations) do
		begin
			if data.animations[i] <> nil then
			begin
				DrawSprite(data.animations[i]);
			end;
		end;
	end;

	procedure DrawSprites(const data: GameDataType; showIds: Boolean);
	var
		i: Integer;
	begin
		//WriteLn('Drawing Sprites');
		for i := 0 to High(data.sprites) do
		begin
			if _Templates[data.sprites[i].kind].foreground then
			begin
				DrawAlienFlightSprite(data.sprites[i]);
			
				if showIDs then
				begin
					DrawObstacleData(data.sprites[i].sprite, i, data.sprites[i].sprite.movement);
				end;
			end;
		end;
		//WriteLn('Ended Drawing');
	end;
	
	procedure DrawLevel(level: Integer);
	const
		LEVEL_X = 490;
		LEVEL_Y = 60;
	begin
		DrawTextOnScreen( IntToStr(level), ColorWhite, GameFont(Courier), LEVEL_X, LEVEL_Y);
	end;

	procedure DrawScore(score: Integer);
	const
		SCORE_X = 350;
		SCORE_Y = 60;
	begin
		//TODO: Change this to DrawTextOnScreen
		DrawTextOnScreen( IntToStr(score), ColorWhite, GameFont(Courier), SCORE_X, SCORE_Y);
	end;
	
	procedure DrawHud(const data: GameDataType);
	const
		SHIELD_X = 24; SHIELD_Y = 15;
		BAR_W = 244; BAR_H = 42;
		FUEL_X = 532; FUEL_Y = 15;
	var
		width, srcX, dstX: Integer;
	begin 
		DrawBitmapOnScreen(GameImage(HUDImg), 0, 0);
		
		width := Round(BAR_W * data.shieldStrength);
		DrawBitmapPartOnScreen(GameImage(ShieldStrengthImg), 0, 0, width, BAR_H, SHIELD_X, SHIELD_Y);
		
		width := Round(BAR_W * data.fuelLevel / MAX_FUEL);
		srcX := BAR_W - width;
		dstX := FUEL_X + srcX;
		DrawBitmapPartOnScreen(GameImage(FuelLevelImg), srcX, 0, width, BAR_H, dstX, SHIELD_Y);
		
		DrawScore(data.score);
		DrawLevel(data.currentLevel);
	end;
	
	function CurrentEditorImage(const status: EditorStatusType): Bitmap;
	begin
		result := _Templates[status.currentSpriteKind].editorImage;
	end;
	
	procedure DrawTextIn(toDraw: String; color: Color; txtFont: Font; x, y, w, h: Integer);
	var
		txtH, txtW: Integer;
	begin
		txtH := TextHeight(toDraw, txtFont);
		txtW := TextWidth(toDraw, txtFont);
		
		DrawTextOnScreen(toDraw, color, txtFont, x + (w - txtW) div 2, y + (h - txtH) div 2);
	end;

	procedure DrawBackScene(const data: GameDataType; showIDs: Boolean);
	begin
		DrawBackground(data);
		DrawSprites(data, showIDs);
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
		wx := Round(XOffset());
		wy := Round(YOffset());
		maxX := Round(MaxForegroundX());
		
		DrawTextOnScreen( IntToStr(wx) + ',' + IntToStr(wy), ColorWhite, GameFont(Courier), 100, 60);
		
		if wx < 0 then
		begin
			DrawVerticalLineOnScreen(ColorWhite, 0 - wx, 0, SCREEN_HEIGHT);
		end
		else if wx > maxX - SCREEN_WIDTH then
		begin
			DrawVerticalLineOnScreen(ColorWhite, maxX - wx, 0, SCREEN_HEIGHT);
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
			DrawHorizontalLineOnScreen(ColorWhite, i, 0, SCREEN_WIDTH);
		end;
	end;
	
	procedure DrawScreen(const data: GameDataType; showIDs, refresh: Boolean); overload;
	begin
		DrawBackScene(data, showIDs);
		
		//Draw the Player
		DrawUfo(data);
		DrawOverlay(data);
		
		if refresh then RefreshScreen(60);
	end;
	
	procedure DrawAddingMode(const status: EditorStatusType);
	const
		COLS = EDITOR_HUD_WIDTH div EDITOR_ICON_WIDTH;
	var
		i: AlienFlightSpriteKind;
		x, y: Integer;
	begin
		for i := Low(AlienFlightSpriteKind) to High(AlienFlightSpriteKind) do
		begin
			x := (Integer(i) mod COLS) * EDITOR_ICON_WIDTH + EDITOR_DATA_BORDER;
			y := ((Integer(i) - (Integer(i) mod COLS)) div COLS) * EDITOR_ICON_HEIGHT + SELECTION_Y;
			
			DrawBitmapOnScreen(_Templates[i].editorImage, x, y);
			
			if i = status.currentSpriteKind then
				DrawRectangleOnScreen(ColorWhite, x, y, EDITOR_ICON_WIDTH, EDITOR_ICON_HEIGHT);
		end;
	end;
	
	procedure DrawValueFor(const status: EditorStatusType; sprt: AlienFlightSpritePtr);
	var
		message: String;
	begin
		message := SelectedName(status, sprt) + '  ' + FormatFloat('0.00', SelectedValue(status, sprt));
		DrawTextOnScreen(message, ColorWhite, GameFont(Courier), VALUE_X, VALUE_TOP);
	end;
	
	procedure DrawEditorSelection(const status: EditorStatusType);
	const
		COLS = EDITOR_HUD_WIDTH div EDITOR_ICON_WIDTH;
	var
		i: Integer;
		sk: AlienFlightSpriteKind;
		x, y: Integer;
		
		various, hasValue: Boolean;
		value: Single;
	 	kind: AlienFlightSpriteKind;
	begin
		various := false;
		
		//if has selected something
		if Length(status.selection) > 0 then
		begin
			//Get kind of first thing
			kind := status.selection[0].kind;

			hasValue := HasSelectedValue(status, status.selection[0]);
			value := SelectedValue(status, status.selection[0]);
			
			for i := Low(status.selection) to High(status.selection) do
			begin				
				sk := status.selection[i].kind;
				
				//Check if sprites have the same value
				if (not various) and (sk <> kind) then various := true;
				if (hasValue) and (not various) and (value <> SelectedValue(status, status.selection[i])) then various := true;
					
				x := (i mod COLS) * EDITOR_ICON_WIDTH + EDITOR_DATA_BORDER;
				y := ((i - (i mod COLS)) div COLS) * EDITOR_ICON_HEIGHT + SELECTION_Y;

				DrawBitmapOnScreen(_Templates[sk].editorImage, x, y);

				if (status.selectedIdx < Length(status.selection)) 
					and (i = status.selectedIdx) then DrawRectangleOnScreen(ColorYellow, x, y, 32, 32);


				DrawOutline(status, status.selection[i]^);
				DrawTextOnScreen(IntToStr(i), ColorRed, GameFont(Courier), x, y);
				DrawText(IntToStr(i), ColorRed, GameFont(Courier), status.selection[i]^.sprite.x + status.selection[i]^.sprite.width, status.selection[i]^.sprite.y);
			end;
			
			if various then
			begin
				DrawTextOnScreen('Various...', ColorWhite, GameFont(Courier), VALUE_X, VALUE_TOP);
			end
			else //all the same with the same value
			begin
				DrawValueFor(status, status.selection[0]);
			end;			
		end;
	end;
	
	procedure DrawEditorStatus(const status: EditorStatusType);
	var
		data: GameDataType;
		bmp: Bitmap;
	begin
		data := status.dataPtr^;
		
		DrawBackScene(data, true);
		DrawUfo(data);
		
		DrawEditorGuides(data);
				
		//DrawBitmapOnScreen(GameImage(EditorStatus), EDITOR_STATUS_LEFT, EDITOR_STATUS_TOP);
		case status.mode of
			AddingMode: bmp := GameImage(EditorHUDAdding);
			EditingMode: bmp := GameImage(EditorHUDEditing);
			DeletingMode: bmp := GameImage(EditorHUDDeleting);
			else raise Exception.Create('Unknow editor mode');
		end;
		
		DrawBitmapOnScreen(bmp, EDITOR_HUD_LEFT, EDITOR_HUD_TOP);
		
		DrawTextIn(LevelFilename(status.dataPtr^.currentLevel), ColorWhite, GameFont(Courier), EDITOR_HUD_WIDTH, LEVEL_Y, SCREEN_WIDTH - EDITOR_HUD_WIDTH, EDITOR_HUD_DATA_HEIGHT);

		case status.mode of
			AddingMode: DrawAddingMode(status);
			EditingMode: DrawEditorSelection(status);
			DeletingMode: DrawEditorSelection(status);
		end;

		DrawOverlay(data);

		RefreshScreen(60);
				
{		if status.currentObstacle <> nil then
		begin
			DrawTextIn(status.currentObstacle^.Name, 
				ColorBlack, GameFont(Courier), BOX_X, ELEM_NAME_Y, BOX_W, BOX_H);
			DrawTextIn(Format('%10.2f', [ status.currentObstacle^.Mass ]), 
				ColorBlack, GameFont(Courier), BOX_X, MASS_Y, BOX_W, BOX_H);
		end;

		DrawCurrentSprite(status.currentSprite);
}		//TODO: DrawOutline(status.dataPtr^, status.currentObstacle.sprite);

{		RefreshScreen(60);}
	end;

	
end.