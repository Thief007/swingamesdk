unit GameLogic;

interface
	procedure Main();

implementation
uses
	SysUtils,
	SGSDK_Core, SGSDK_Graphics, SGSDK_Audio, SGSDK_KeyCodes, SGSDK_Input, SGSDK_Physics,
	GameResources,
	AlienFlightLevelEditor,
	AlienFlightModel,
	AlienFlightView;

procedure HandleKeyboardInput(var data: GameDataType);
begin
	if _paused then exit;
	
	if IsKeyPressed(VK_RIGHT) then MovePlayer(data, 1, 0);
	if IsKeyPressed(VK_LEFT)  then MovePlayer(data, -1, 0);
	if IsKeyPressed(VK_UP)    then MovePlayer(data, 0, -1);;
	if IsKeyPressed(VK_DOWN)  then MovePlayer(data, 0, 1);
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
	
	data.ufoSprite.xPos := -CurrentWidth(data.ufoSprite);
	
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
	
	if WasKeyTyped(VK_L) then 
	begin
		FreeLevel(data);
		LoadLevel(1, data);
	end;
	
	if WasKeyTyped(VK_R) then
	begin
		ResetLevel(data);
	end;
	
	if WasKeyTyped(VK_P) then _Paused := not _Paused;		
end;

procedure DoWarpLevel(var data: GameDataType);
const
	STEPS = 5 * WARP_FRAMES;
var
	i: Integer;
	v: Vector;
	wh: WarpHolePtr;
begin
	wh := GetCollidedWarphole(data.ufoSprite, data);
	v := CalculateVectorFromTo(data.ufoSprite, wh.sprite);
	
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
	
	ClearScreen(ColorBlack);
	RefreshScreen();
	Sleep(700);
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
		
	data.state := PlayingGameState;
end;

procedure DoEndOfLevel(var data: GameDataType);
const
	STEPS = 20;
var
	i: Integer;
	v: Vector;
	wh: WarpHolePtr;
begin
	//first one added on creation
	wh := @data.warpHoles[0];
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
	STEPS = 100;
	VX = 75 / STEPS; 
	VY = 75 / STEPS;
var
	i: Integer;
	cx, cy: Single;
begin
	cx := data.ufoSprite.xPos + CurrentWidth(data.ufoSprite) / 2;
	cy := data.ufoSprite.yPos + CurrentHeight(data.ufoSprite) / 2;
	
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
		end;
		
		UpdateGameState(data);
		DrawScreen(data, false);		
		Sleep(15);
		
		if WindowCloseRequested then break;
	end;
	
end;

//The main procedure that controlls the game logic.
//
// SIDE EFFECTS:
procedure Main();
var
	data: GameDataType;
	count: Integer;
begin
	_Paused := false;
	count := 0;
	OpenGraphicsWindow('Alien Flight', 800, 600);
	OpenAudio();
	
	LoadResources();	
	data := SetupGameData();
	
 	LoadLevel(data.currentLevel, data);	

	data.state := PlayingIntroState;

	repeat
		ProcessEvents();
	
		case data.state of
			PlayingIntroState: DoPlayIntro(data);
			PlayingGameState: DoPlayingGame(data);
			WarpingState: DoWarpLevel(data);
			EndOfLevelState: DoEndOfLevel(data);
			PlayerDyingState: DoDeath(data);
			EditorState: ExecuteEditor(data);
			else break;
		end;
	until WindowCloseRequested();
	
	CleanupGameData(data);
	FreeResources();
end;

end.
