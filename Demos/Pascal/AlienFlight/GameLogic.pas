unit GameLogic;

interface
	procedure Main();

implementation
uses
	SysUtils,
	SGSDK_Core, SGSDK_Graphics, SGSDK_Audio, SGSDK_KeyCodes, SGSDK_Input, SGSDK_Physics, SGSDK_Camera,
	GameResources,
	AlienFlightLevelEditor,
	AlienFlightModel,
	AlienFlightView;

procedure StartAtLevel(var data: GameDataType; level: Integer);
begin
	LoadLevel(level, data);
	ResetLevel(data);

	data.score := 0;

	if data.currentLevel = data.topLevel then
	begin
		data.shieldStrength := data.topShield;
		data.fuelLevel := data.topFuel;
	end
	else
	begin
		data.shieldStrength := 1.0;
		data.fuelLevel := MAX_FUEL;
	end;
		
	UpdateShieldBmp(data);
	
	data.state := PlayingGameState;
end;

procedure GetShipMovement(var dx, dy: Single; speed, angle : Integer);
begin
	dx := speed * SGSDK_Core.Cos(angle);
	dy := speed * SGSDK_Core.Sin(angle);
end;

procedure HandleKeyboardInput(var data: GameDataType);
var
	dx, dy: Single;
begin
	if _paused then exit;
	
	dx := 0;
	dy := 0;
		
	if IsKeyPressed(VK_LEFT) then
	begin
		if IsKeyPressed(VK_UP) then
			GetShipMovement(dx, dy, 1, 225)
		else if IsKeyPressed(VK_DOWN) then
			GetShipMovement(dx, dy, 1, 135)
		else if not IsKeyPressed(VK_RIGHT) then
			dx := -1;
	end
	else if IsKeyPressed(VK_RIGHT) then
	begin
		if IsKeyPressed(VK_UP) then
			GetShipMovement(dx, dy, 1, 315)
		else if IsKeyPressed(VK_DOWN) then
			GetShipMovement(dx, dy, 1, 45)
		else dx := 1;
	end
	else if IsKeyPressed(VK_DOWN) and (not IsKeyPressed(VK_UP)) then
		dy := 1
	else if IsKeyPressed(VK_UP) and (not IsKeyPressed(VK_DOWN)) then
		dy := -1;
		
	MovePlayer(data, dx, dy);
end;

procedure DoPlayIntro(var data: GameDataType);
var
	i, cycle: Integer;
	shieldInc: Single;
	fuelInc: Single;
	v: Vector;
begin
	PlayMusic(GameMusic(IntroMusic), 1);
	cycle := 0;
	
	v := CreateVector(2, 0);
	
	data.fuelLevel := 0;
	data.shieldStrength := 0;
	
	shieldInc := 1 / 160;
	fuelInc := MAX_FUEL / 160;
	
	data.ufoSprite.x := -CurrentWidth(data.ufoSprite);
	
	while IsMusicPlaying() do
	begin
		ProcessEvents();
				
		if cycle > 200 then DrawBackground(data);
		if cycle > 770 then DrawHud(data);
					
		if (cycle > 2300) and (cycle < 3000) then
		begin
			DrawUfo(data);
			MoveSprite(data.ufoSprite, v);
		end;
		
		case cycle of
			300 .. 700: DrawWelcome();
			840 .. 1000:
			begin
				data.fuelLevel := Round(fuelInc * (cycle - 840));
				data.shieldStrength := shieldInc * (cycle - 840);
				DrawHUDHelp();
			end;
			1001: data.fuelLevel := MAX_FUEL; 
			1002: data.shieldStrength := 1.0;
			1070 .. 1370: DrawKeyHelp();
			1500 .. 1900: DrawThingsHelp();
			2030 .. 2230: DrawWatchOutForHelp();
			2400 .. 2600: DrawThatsYou();
			2665 .. 2755: DrawReadyIn('.. 3 ..');
			2800 .. 2890: DrawReadyIn('.. 2 ..');
			2950 .. 3050: DrawReadyIn('.. 1 ..');
		end;
		
		if cycle > 200 then RefreshScreen()
		else Sleep(0);
			
		cycle += 1;
		//if (cycle mod 50) = 0 then WriteLn(cycle);
			
		if IsKeyPressed(VK_SPACE) or IsKeyPressed(VK_RETURN) or
			IsKeyPressed(VK_ESCAPE) or WindowCloseRequested() then break;
	end;
	
	if not WindowCloseRequested() then
	begin
		for i := 0 to WARP_FRAMES do
		begin	
			ClearScreen(ColorWhite);
			RefreshScreen();
			Sleep(15);
		
			DrawScreen(data, false);	
			UpdateGameState(data);		
		end;
	
		StopMusic();
		PlayMusic(GameMusic(Loop1Music));
	end;
	
	data.fuelLevel := MAX_FUEL; 
	data.shieldStrength := 1.0;
	
	ResetLevel(data);
	data.state := PlayingGameState
end;

procedure DoPlayingGame(var data: GameDataType);
begin	
	HandleKeyboardInput(data );
	
	//Draw screen
	DrawScreen(data, false);
	
	UpdateBoost(data, IsKeyPressed(VK_SPACE));
	UpdateGameState(data);
	
	if EditorWasActivated() then
	begin
		data.state := EditorState;
	end;
	
{	if WasKeyTyped(VK_L) then 
	begin
		FreeLevel(data);
		LoadLevel(1, data);
	end;}
	
{	if WasKeyTyped(VK_R) then
	begin
		ResetLevel(data);
	end;
}	
	if WasKeyTyped(VK_P) then _Paused := not _Paused;		
		
	if WasKeyTyped(VK_ESCAPE) then data.state := GameMenuState;
end;

procedure DoLevelSummary(var data: GameDataType);
const
	MENU_X = (SCREEN_WIDTH - 400) div 2;
	MENU_Y = 55; //(SCREEN_HEIGHT - 350) div 2;
	STARS_Y = MENU_Y + 175;
	SHIELD_Y = STARS_Y + 40;
	FUEL_Y = SHIELD_Y + 40;
	PICKUP_Y = FUEL_Y + 40;
	BLACKHOLE_Y = PICKUP_Y + 40;
	LEVEL_Y = BLACKHOLE_Y + 40;
	BONUS_Y = MENU_Y + 470;
	DATA_X = MENU_X + 300;
	
	MAX_BONUS = 1000;
var
	stars, pickups, fuel, shield, progress: Single;
	starCount, pickupCount, blackholes: Integer;
	i, starUseCount, pickupUseCount, cycle, bonus, totalScore, temp: Integer;
	
	procedure DrawScores();
	begin
		DrawScreen(data, false, false);
		DrawBitmapOnScreen(GameImage(LevelSummaryMenu), MENU_X, MENU_Y);
		DrawTextIn(FormatFloat('0%', stars * 100), ColorWhite, GameFont(WelcomeFont), DATA_X, STARS_Y, 100, 40);
		DrawTextIn(FormatFloat('0%', shield * 100), ColorWhite, GameFont(WelcomeFont), DATA_X, SHIELD_Y, 100, 40);
		DrawTextIn(FormatFloat('0%', fuel * 100), ColorWhite, GameFont(WelcomeFont), DATA_X, FUEL_Y, 100, 40);
		DrawTextIn(FormatFloat('0%', pickups * 100), ColorWhite, GameFont(WelcomeFont), DATA_X, PICKUP_Y, 100, 40);
		DrawTextIn(FormatFloat('0%', progress * 100), ColorWhite, GameFont(WelcomeFont), DATA_X, LEVEL_Y, 100, 40);
		DrawTextIn(FormatFloat('0 ', blackholes), ColorWhite, GameFont(WelcomeFont), DATA_X, BLACKHOLE_Y, 100, 40);
		DrawTextIn(FormatFloat('0', temp), ColorWhite, GameFont(WelcomeFont), DATA_X, BONUS_Y, 100, 40);		
		RefreshScreen();
	end;
begin
	starUseCount := 0; pickupUseCount := 0;
	starCount := 0; pickupCount := 0; blackholes := 0;
	stars := 0.0; pickups := 0.0; fuel := 0.0;
	
	for i := Low(data.sprites) to High(data.sprites) do
	begin
		case data.sprites[i].kind of
			BlackHoleKind: 
			begin
				if data.sprites[i].sprite.x < data.ufoSprite.x then
					blackholes += 1;
			end;
			StarKind:
			 	begin
					starCount += 1;
					if data.sprites[i].active = false then
						starUseCount += 1;
				end;
			BatteryKind, FuelPackKind:
				begin
					pickupCount += 1;
					if data.sprites[i].active = false then
						pickupUseCount += 1;
				end;
		end;
	end;
	
	if starCount > 0 then
		stars := starUseCount / starCount;
	if pickupCount > 0 then
		pickups := 1 - (pickupUseCount / pickupCount);
	
	fuel := data.fuelLevel / MAX_FUEL;
	shield := data.shieldStrength;
	progress := (XOffset() + SCREEN_WIDTH) / MaxForegroundX();
	
	DrawScreen(data, false, false);
	DrawBitmapOnScreen(GameImage(LevelSummaryMenu), MENU_X, MENU_Y);
	RefreshScreen();
	
	for cycle := 0 to 30 do RefreshScreen();		
	DrawTextIn(FormatFloat('0%', stars * 100), ColorWhite, GameFont(WelcomeFont), DATA_X, STARS_Y, 100, 40);
	
	for cycle := 0 to 30 do RefreshScreen();		
	DrawTextIn(FormatFloat('0%', shield * 100), ColorWhite, GameFont(WelcomeFont), DATA_X, SHIELD_Y, 100, 40);

	for cycle := 0 to 30 do RefreshScreen();		
	DrawTextIn(FormatFloat('0%', fuel * 100), ColorWhite, GameFont(WelcomeFont), DATA_X, FUEL_Y, 100, 40);

	for cycle := 0 to 30 do RefreshScreen();		
	DrawTextIn(FormatFloat('0%', pickups * 100), ColorWhite, GameFont(WelcomeFont), DATA_X, PICKUP_Y, 100, 40);

	for cycle := 0 to 30 do RefreshScreen();		
	DrawTextIn(FormatFloat('0', blackholes), ColorWhite, GameFont(WelcomeFont), DATA_X, BLACKHOLE_Y, 100, 40);

	for cycle := 0 to 30 do RefreshScreen();		
	DrawTextIn(FormatFloat('0%', progress * 100), ColorWhite, GameFont(WelcomeFont), DATA_X, LEVEL_Y, 100, 40);

	for cycle := 0 to 30 do RefreshScreen();		

	bonus := Round(MAX_BONUS * (stars + shield * progress + fuel * progress + pickups * progress + blackholes));

	for cycle := 0 to 30 do RefreshScreen();		

	totalScore := data.score + bonus;
	temp := bonus mod 99;
	data.score += bonus mod 99;
	
	for cycle := 0 to bonus div 99 - 1 do
	begin
		ProcessEvents();
		
		temp += 99;
		data.score += 99;
		
		DrawScores();
		
		if cycle mod 99 = 0 then PlaySoundEffect(GameSound(CollectStarSound));
	end;
	
	temp := bonus;
	data.score := totalScore;
	DrawScores();
			
	for cycle := 0 to 90 do RefreshScreen();
end;

procedure DoWarpLevel(var data: GameDataType);
const
	STEPS = 5 * WARP_FRAMES;
var
	i: Integer;
	v: Vector;
	wh: AlienFlightSpritePtr;
begin
	wh := GetCollidedWarphole(data.ufoSprite, data);
	//WriteLn(HexStr(wh^.sprite));
	v := CalculateVectorFromTo(data.ufoSprite, wh^.sprite);
	
	AddWarpAnimation(data, data.ufoSprite, v, STEPS);
	PlaySoundEffect(GameSound(WarpStartSound));
	
	//Adjust vector for ship movement	
	v.x *= 1 / STEPS; v.y *= 1 / STEPS;
	
	for i := 0 to WARP_FRAMES * 5 do
	begin
		UpdateGameState(data);
		MoveSprite(data.ufoSprite, v);
		DrawScreen(data, false);	
		
		ClearScreen(ColorWhite);
		RefreshScreen();
		Sleep(15);
	end;
	
	DoLevelSummary(data);
	
	ClearScreen(ColorBlack);
	RefreshScreen();
	Sleep(1500);
	PlaySoundEffect(GameSound(SirenSound));
	Sleep(2100);
	//TODO: Player dies here?
	LoadLevel(data.currentLevel, data);
	ResetLevel(data);

	for i := 0 to WARP_FRAMES do
	begin	
		ClearScreen(ColorWhite);
		RefreshScreen();
		Sleep(15);
		
		DrawScreen(data, false);	
		UpdateGameState(data);		
	end;
	
	UpdateTopLevel(data);
		
	data.state := PlayingGameState;
end;

procedure DoEndOfLevel(var data: GameDataType);
const
	STEPS = 20;
var
	i: Integer;
	v: Vector;
	wh: AlienFlightSpritePtr;
begin
	//first one added on creation
	wh := @data.sprites[0];
	v := CalculateVectorFromTo(data.ufoSprite, wh.sprite);
	
	v.x *= 1 / STEPS;
	v.y *= 1 / STEPS;
	
	for i := 0 to STEPS do
	begin
		UpdateGameState(data);
		MoveSprite(data.ufoSprite, v);
		DrawScreen(data, false);		
		Sleep(15);
		
		if HaveSpritesCollided(wh.sprite, data.ufoSprite) then
			break;
	end;
	
	DoWarpLevel(data);
end;

procedure DoDeath(var data: GameDataType);
const
	STEPS = 110;
	VX = 80 / STEPS; 
	VY = 80 / STEPS;
var
	i: Integer;
	cx, cy: Single;
begin
	cx := data.ufoSprite.x + CurrentWidth(data.ufoSprite) / 2;
	cy := data.ufoSprite.y + CurrentHeight(data.ufoSprite) / 2;
	
	cx -= 10; //move center
	cy -= 10;	

	for i := 0 to STEPS do
	begin	
		ProcessEvents();
			
		case i of
			0:  AddExplosion(data, cx, cy, CreateVector(-VX, 0));	
			5:  AddExplosion(data, cx, cy, CreateVector(VX, -VY));
			7:  AddExplosion(data, cx, cy, CreateVector(VX, 0));
			20: AddExplosion(data, cx, cy, CreateVector(0, VY));
			23: AddExplosion(data, cx, cy, CreateVector(VX, -VY));			
			25: AddExplosion(data, cx, cy, CreateVector(0, -VY));
			33: AddExplosion(data, cx, cy, CreateVector(VX, VY));
			35: AddExplosion(data, cx, cy, CreateVector(-VX, VY));
			46: AddExplosion(data, cx, cy, CreateVector(-VX, -VY));
			50: AddExplosion(data, cx, cy, CreateVector(-VX, 0));	
			55:  AddExplosion(data, cx, cy, CreateVector(VX, -VY));
			57:  AddExplosion(data, cx, cy, CreateVector(VX, 0));
			70: AddExplosion(data, cx, cy, CreateVector(0, VY));
			73: AddExplosion(data, cx, cy, CreateVector(VX, -VY));			
			75: AddExplosion(data, cx, cy, CreateVector(0, -VY));
			83: AddExplosion(data, cx, cy, CreateVector(VX, VY));
			85: AddExplosion(data, cx, cy, CreateVector(-VX, VY));
			86: AddExplosion(data, cx, cy, CreateVector(-VX, -VY));
		end;
		
		UpdateGameState(data);
		DrawScreen(data, false);
		
		if WindowCloseRequested then break;
	end;
	
	data.state := EndOfGameState;
end;

procedure DoEndOfGame(var data: GameDataType);
var
	message: String;
begin	
	message := ' ' + EOL + 'Game Over' + EOL +
				'-----' + EOL + ' ' + EOL + 
				'Your score: ' + IntToStr(data.score) + EOL + ' ' + EOL +
				'-----' + EOL + 
				'Enter to continue';
	repeat
		ProcessEvents();
		DrawBackScene(data, false);
		DrawMessage(message);
		DrawHud(data);
		RefreshScreen();
	until WasKeyTyped(VK_RETURN) or WindowCloseRequested();
	
	if IsHighScore(data.score) then
		data.state := EnterHighScoreState
	else
	begin
		data.state := GameMenuState;
		data.score := 0;
	end;
end;

procedure DoShowScoreboard(var data: GameDataType);
CONST
	MENU_X = (SCREEN_WIDTH - 400) div 2;
	MENU_Y = 55; //(SCREEN_HEIGHT - 350) div 2;
	SCORE_Y = 150;
	LINE_HEIGHT = 50;
var
	i: Integer;
	s: HighScore;
	score: String;
	name: String[6];
begin
	DrawScreen(data, false, false);
	DrawBitmapOnScreen(GameImage(TopScoresImg), MENU_X, MENU_Y);
	
	for i := 0 to 4 do
	begin
		s := GetScore(i);
		score := IntToStr(s.score);
		name := s.name;
		if Length(score) < 6 then score := StringOfChar(' ', 6 - Length(score)) + score;
		if Length(name) < 6 then name += StringOfChar(' ', 6 - Length(name));
			
		DrawTextIn('#' + IntToStr(i + 1) + ' - ' + UpperCase(name) + ' - ' + score,
			ColorWhite, GameFont(WelcomeFont), 
			MENU_X, SCORE_Y + LINE_HEIGHT * i, 400, 50);
	end;
	
	repeat
		ProcessEvents();
		RefreshScreen();

		if WasKeyTyped(VK_RETURN) or WasKeyTyped(VK_ESCAPE) then
		begin
			data.state := GameMenuState;
		end;
	until WindowCloseRequested() or (data.state <> ShowScoreboardState);
end;

procedure DoEnterName(var data: GameDataType);
const
	MENU_X = (SCREEN_WIDTH - 400) div 2;
	MENU_Y = 55; //(SCREEN_HEIGHT - 350) div 2;
var
	name: String;
begin
	StartReadingText(ColorWhite, 6, GameFont(WelcomeFont), MENU_X + 166, MENU_Y + 245);

	while IsReadingText() and not WindowCloseRequested() do
	begin
		ProcessEvents();

		DrawScreen(data, false, false);
		DrawBitmapOnScreen(GameImage(EnterNameMenu), MENU_X, MENU_Y);
						
		RefreshScreen();
	end;

	name := TextReadAsASCII();

	if Length(name) > 0 then
	begin
		AddTopScore(data.score, name);
	end;

	data.state := GameMenuState;
	data.score := 0;
end;

procedure DoEnterLevel(var data: GameDataType);
const
	MENU_X = (SCREEN_WIDTH - 400) div 2;
	MENU_Y = 55; //(SCREEN_HEIGHT - 350) div 2;
var
	level, message: String;
	lvl: Integer;
begin
	message := '';
	repeat	
		StartReadingText(ColorWhite, 2, GameFont(WelcomeFont), MENU_X + 213, MENU_Y + 240);
	
		while IsReadingText() and not WindowCloseRequested() do
		begin
			ProcessEvents();

			DrawScreen(data, false, false);
			DrawBitmapOnScreen(GameImage(EnterLevelmenu), MENU_X, MENU_Y);
			
			if length(message) > 0 then
				DrawTextIn(message, ColorWhite, GameFont(WelcomeFont), MENU_X, MENU_Y + 350, 400, 50);
				
			RefreshScreen();
		end;
	
		level := TextReadAsASCII();
	
		if Length(level) > 0 then
		begin
			if TryStrToInt(level, lvl) then
			begin
				if (lvl <= data.topLevel) and (lvl > 0) then
				begin
					StartAtLevel(data, lvl);
					exit;
				end
				else
				begin
					DrawScreen(data, false, false);
					message := 'Max is level ' + IntToStr(data.topLevel);
				end;
			end
			else
			begin
				message := 'Only enter numbers';
			end;
		end
		else
		begin
			exit;
		end;
	until false;
end;

procedure DoMenu(var data: GameDataType);
CONST
	MENU_X = (SCREEN_WIDTH - 400) div 2;
	MENU_Y = 55; //(SCREEN_HEIGHT - 350) div 2;
	BUTTON_WIDTH = 390; BUTTON_HEIGHT = 50;
	BUTTON_X = 5 + MENU_X;
	NEW_GAME_Y = 195 + MENU_Y;
	RETURN_TO_LEVEL_Y = 250 + MENU_Y;
	SCOREBOARD_Y = 305 + MENU_Y;
	QUIT_Y = 378 + MENU_Y;
	MAX_OPTIONS = 4;
var
	cycle: Integer;
	currentOption: Integer;
	bmp: Bitmap;
	down, up: Boolean;
begin
	cycle := 0;
	currentOption := 0;
	up := false; down := false;
	
	repeat
		cycle += 1;
		ProcessEvents();
	
		if IsKeyPressed(VK_UP) then up := true;
		if IsKeyPressed(VK_DOWN) then down := true; 
	
		if cycle mod 12  = 0 then
		begin
			if down then currentOption := (currentOption + 1) mod MAX_OPTIONS;
			if up then currentOption := (currentOption - 1 + MAX_OPTIONS) mod MAX_OPTIONS;
			cycle := 0;
			down := false;
			up := false;
		end;
		
		case currentOption of
			0: bmp := GameImage(NewGameMenu);
			1: bmp := GameImage(ReturnToLevelMenu);
			2: bmp := GameImage(ScoreboardMenu);
			3: bmp := GameImage(QuitMenu);
		end;
	
		DrawScreen(data, false, false);
		DrawBitmapOnScreen(bmp, MENU_X, MENU_Y);
		RefreshScreen();
		
		if WasKeyTyped(VK_RETURN) then
		begin
			case currentOption of
				0: StartAtLevel(data, data.topLevel);
				1: DoEnterLevel(data);
				2: data.state := ShowScoreboardState;
				3: data.state := ExitGameState;
			end;
		end;
		if WasKeyTyped(VK_ESCAPE) then
		begin
			if data.shieldStrength > 0 then
			begin
				data.state := PlayingGameState;
			end
			else
			begin
				data.state := ExitGameState;
			end;
		end;	
	until WindowCloseRequested() or (data.state <> GameMenuState);
end;

procedure DoEditor(var data: GameDataType);
var
	status: EditorStatusType;
begin
	InitEdit(status, data);
	
	repeat
		ProcessEvents();
		
		HandleEditorInput(status);
		
		//Ensure black behind background
		ClearScreen(ColorBlack);
		
		DrawEditorStatus(status);			
	until EditorDeactivated();
	
	EndEdit(status, data);
end;


//The main procedure that controlls the game logic.
//
// SIDE EFFECTS:
procedure Main();
var
	data: GameDataType;
begin
	_Paused := false;
	OpenGraphicsWindow('Alien Flight', 800, 600);
	OpenAudio();
	
	LoadResources();
	SetupTemplates();

	data := SetupGameData();

 	LoadLevel(data.currentLevel, data);	

	data.state := PlayingIntroState;

	repeat
		ProcessEvents();
	
		if IsSpecialKeyPressed() and WasKeyTyped(VK_F) then ToggleFullscreen();
	
		case data.state of
			PlayingIntroState: DoPlayIntro(data);
			PlayingGameState: DoPlayingGame(data);
			WarpingState: DoWarpLevel(data);
			PlayerDyingState: DoDeath(data);
			EditorState: DoEditor(data);
			EndOfLevelState: DoEndOfLevel(data);
			EndOfGameState: DoEndOfGame(data);
			GameMenuState: DoMenu(data);
			ShowScoreboardState: DoShowScoreboard(data);
			EnterHighScoreState: DoEnterName(data);
			else break;
		end;
	until WindowCloseRequested();
	
	if data.score > 0 then
		DoEndOfGame(data);
	
	SaveScoreboard(data);
	
	CleanupGameData(data);
	FreeResources();
	
	CloseAudio();
end;

end.
