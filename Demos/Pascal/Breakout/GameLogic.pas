unit GameLogic;

interface
procedure Main();

implementation
uses
	GameResources,
	SysUtils,
	SGSDK_Core, SGSDK_Font, SGSDK_Audio, SGSDK_Graphics, SGSDK_Input, SGSDK_Physics,
	SGSDK_KeyCodes, SGSDK_Shapes;

//game constants
const
	//screen prop
	screenWidth = 800;
	screenHeight = 600;
	//how high up the screen the player spawns
	playerY = 550;
	//number of bricks per row
	bricksPerRow = 8;
	//number of rows
	brickRows = 5;
	brickwidth = 80;
	brickheight = 20;
	//number of lives
	livesDefault = 5;

type
//TPlayer - for player
	TPlayer = record
		lives: integer;  //lives
		level: integer;  //level
		score: integer;  //score
		playerSprite: Sprite;  //player paddle
		bricksRemaining: integer;  //bricks remain
	end;
	brick = record
		brickColor : integer;		// brickcolor: this also controls what score the player will get when this brick is hit
		brickSprite : Sprite;		// brick sprite
		hit : boolean;				// has the brick been hit?
	end;
	GameStateType = ( Menu, Playing, GameOver, About, Options, HighScoreTable, Quit );
	GameDataType = record
		State: GameStateType;
		MusicPlaying: Boolean;
		background: sprite;
	end;

var
	bricks: array [1..bricksPerRow * brickRows] of brick;

procedure InitialiseBall(var ball: Sprite; const level: integer); //ball spawn procedure
begin
	// 1/3 of the way across the screen
	ball.X := screenWidth / 3 - ball.Width / 2;
	ball.Y := screenHeight / 3 - ball.Height / 2;
	ball.movement := CreateVector(5 + level / 2, 7 + level / 2);
end;

procedure MovePlayerToDefaultPosition(var playersprite: Sprite);
begin
	playerSprite.X := (screenWidth - PlayerSprite.Width) / 2;
	playerSprite.Y := playerY;
end;

procedure InitialisePlayer(var player: TPlayer); //resets the player
begin
	player.lives := livesDefault;
	player.score := 0;
	player.level := 1;

	player.playersprite := CreateSprite(GameImage('paddle'));
	MovePlayerToDefaultPosition(player.playersprite);
	player.bricksRemaining := brickRows * bricksPerRow;
end;

function GetBrickCoordinatesFromGridPosition(i: integer): Point2D; // x and y co-ords for the bricks that are destroyable
var
	GridX, GridY: integer;
	brickPoint: Point2D;
begin
	// Get the grid coordinates of this brick (ie. row 3 brick 4)
	GridX := ((i-1) mod bricksPerRow);
	GridY := ((i-1) div bricksPerRow);

	brickPoint.X := GridX * (brickWidth + 10);
	brickPoint.Y := GridY * (brickHeight + 10);

	result := brickPoint;
end;

procedure InitialiseBricks(); // Resets the array of bricks
var
	i: integer;
	brickPoint: Point2D;
begin
	for i:= 1 to bricksPerRow * brickRows do
	begin
		bricks[i].brickColor := ((i - 1) div bricksPerRow) + 1; // Get the row number / brick color number
		bricks[i].brickSprite := CreateSprite(GameImage('brick' + IntToStr(bricks[i].brickColor))); // Load the sprite depending on the brick number
		// Create the sprite position
		brickPoint := GetBrickCoordinatesFromGridPosition(i);
		bricks[i].brickSprite.X := (screenWidth - bricksPerRow * 90) / 2 + brickPoint.X;
		bricks[i].brickSprite.Y := brickPoint.Y + 32;

		bricks[i].hit := false;
	end;
end;

procedure NextLevel(var ball: Sprite; var player: TPlayer);
// This function is called when there are no more bricks left
begin
	player.level := player.level +1;
	player.bricksRemaining := bricksPerRow*brickRows;
	// Reset the number of bricks remaining.
	
	MovePlayerToDefaultPosition(player.playersprite);
	InitialiseBricks();
	InitialiseBall(ball, player.level);
end;


procedure LoseALife(var ball: Sprite; var player: TPlayer; var game: GameDataType);
begin
// Take off one life
	player.lives -= 1;
	if player.lives < 0 then
	begin
		// The player is dead.
		// Remove the ball from the screen and prevent it from moving.
		ball.x := -ball.Width;
		ball.y := -ball.Height;
		ball.Movement := CreateVector(0,0);
		game.state := GameOver;
	end
	else InitialiseBall(ball, player.level); // The player still has lives remaining, so just reset the ball position.
end;

procedure ScreenBallRestriction(var ball: Sprite; var player: TPlayer; var game: GameDataType);
//bounce ball of edge of screen
// x values (uses width)
//y values (uses height)
begin
	if ball.X < 0 then
	begin
		ball.X:= 0;
		ball.Movement.X := -ball.Movement.X;
	end;
	if ball.X > screenWidth - ball.Width then
	begin
		ball.X := screenWidth - ball.Width;
		ball.Movement.X := -ball.Movement.X;
	end;
	if ball.Y < 0 then
	begin
		ball.Y := 0;
		ball.Movement.Y := -ball.Movement.Y;
	end;
	if ball.y > screenHeight - ball.Height then LoseALife(ball, player, game);
end;

procedure MovementControls(var playerSprite: sprite); //check if player moves
begin
	if IsKeyPressed(VK_LEFT) then playerSprite.Movement.X := -9
	else if IsKeyPressed(VK_RIGHT) then playerSprite.Movement.X := 9
	else playerSprite.Movement.X := 0;
end;

procedure PlayerRestrictions(var player: TPlayer);
begin
	with player do
	begin
		if playerSprite.X < 0 then
		begin
			playerSprite.X := 0;
			playerSprite.Movement.X :=0;
		end
		else if player.playerSprite.X > (screenWidth - playerSprite.Width) then
		begin
			playerSprite.X := screenWidth - playerSprite.Width;
			playerSprite.Movement.X :=0;
		end;
	end;
end;

procedure BatPaddleHit(var ball: Sprite; const playerSprite: sprite);
begin
	if HaveSpritesCollided(ball, playerSprite) then
	begin
		ball.Movement.Y := -ball.Movement.Y;
		ball.Movement.X := ball.Movement.X + (playerSprite.Movement.X / 2);
	end;
end;

// when you input left or right, you move the paddle X pixels that direction thats what player ^^^^^^^^^^^ is

procedure BallBrickHit(var ball: Sprite; var player: TPlayer); //check if ball hit brick
var
	i: integer;
begin
	for i := 1 to bricksPerRow * brickRows do
	begin
		if HaveSpritesCollided(ball, bricks[i].brickSprite) and (bricks[i].hit = false) then
		begin
			//  Hit a brick!
			bricks[i].hit := true;
			// Take from the number of bricks remaining
			player.bricksRemaining -= 1;
			if player.bricksremaining = 0 then NextLevel(ball, player);
			// Bounce the ball off the brick
			ball.Movement.Y := -ball.Movement.Y;
			// Increase the player’s score
			player.score += (brickRows - bricks[i].brickColor + 1) * 50;
		end;
	end;
end;

//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// HUD AND GAME DRAW PROCEDURES

procedure Hud(const player: TPlayer);
begin
	DrawText('           '+'Level: ' + IntToStr(player.level) + '      Lives: ' + IntToStr(player.lives) + '      Score: ' + IntToStr(player.score) + '      Remaining: ' + IntToStr(player.bricksRemaining), ColorWhite, GameFont('absmall'), 8, 580);
end;

procedure ShowGame(const ball: Sprite; const player: TPlayer; const game: GameDataType);
var
	i: integer;
begin				
	//Draw Background
	UpdateSprite(game.background);
	DrawSprite(game.background);
	//Draw Ball
	DrawSprite(ball);
	UpdateSprite(ball);
	// Draw the player
	DrawSprite(player.playerSprite);
	UpdateSprite(player.playerSprite);
	// Draw the bricks
	for i := 1 to bricksPerRow * brickRows do
		if bricks[i].hit = false then
		begin
			DrawSprite(bricks[i].brickSprite);
			UpdateSprite(bricks[i].brickSprite);
		end;
	Hud(player);
end;

//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//GAME LOOP

procedure UpdateGame(var ball: Sprite; var player: TPlayer; var game: GameDataType);
begin
	if game.state = Playing then
	begin
		ScreenBallRestriction(ball, player, game);
		PlayerRestrictions(player);
		BatPaddleHit(ball, player.playerSprite);
		BallBrickHit(ball, player);
	end;
end;

procedure StartPlayingGame(var ball: Sprite; var player: TPlayer; var game: GameDataType);
begin
	InitialiseBall(ball, player.level);  
	InitialisePlayer(player);
	InitialiseBricks();
	
	game.state := Playing;
end;

//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// DISPLAY AND INPUT FUNCTIONS

//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// HIGH SCORE
procedure DisplayHighScoreTable();
begin
	ClearScreen(ColorBlack);
	DrawBitmap(GameImage('menubg'), 0, 0);
	DrawText('COMING SOON', ColorWhite, GameFont('Unreal'),100,50);
end;

procedure HandleHighScoreTableInput(var game: GameDataType);
begin
	if WasKeyTyped(VK_RETURN) then game.state := options;
end;

//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// MENU
procedure DisplayMenu();
begin
	ClearScreen(ColorBlack);
	DrawBitmap(GameImage('menubg'), 0, 0);
	DrawText('BREAKOUT', ColorWhite, GameFont('abstract'), 100, 50);
	DrawText('1. Play Game', ColorWhite, GameFont('Unreal'), 130, 100);
	DrawText('2. Options', ColorWhite, GameFont('Unreal'), 130, 200);
	DrawText('3. About', ColorWhite, GameFont('Unreal'), 130, 300);
	DrawText('4. Quit', ColorWhite, GameFont('Unreal'), 130, 400);
end;

procedure HandleMenuInput(var ball: Sprite; var player: TPlayer; var game: GameDataType);
begin
	if WasKeyTyped(VK_1) then StartPlayingGame(ball, player, game);
	if WasKeyTyped(VK_2) then game.state := Options;
	if WasKeyTyped(VK_3) then game.state := About;
	if WasKeyTyped(VK_4) then game.state := Quit;
end;
//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//GAME OVER
Procedure DrawGameOverScreen(const player: TPlayer);
begin
	ClearScreen(ColorBlack);
	DrawBitmap(GameImage('menubg'),0,0);
	DrawText('You Achieved Level: ' + IntToStr(player.level) + ' ', ColorWhite, GameFont('abstract'), 100, 50);
	DrawText('Your Score Was: ' + IntToStr(player.score) + ' ', ColorWhite, GameFont('abstract'), 100, 100);
	DrawText('Game Over', ColorWhite, GameFont('UnrealL'), 140, 250);
	DrawText('Press Y                New Game', ColorWhite, GameFont('28day'), 130, 400);
	DrawText('Press Esc or N          To Quit', ColorWhite, GameFont('28day'), 130, 500);
end;	

procedure HandleGameOverInput(var ball: Sprite; var player: TPlayer; var game: GameDataType);
begin
	if WasKeyTyped(VK_Y) then StartPlayingGame(ball, player, game);
	if WasKeyTyped(VK_N) then game.state := quit; 
	if WasKeyTyped(VK_ESCAPE) then game.state := quit; 
//	if IsKeyPressed(VK_Escape) then
//	if WindowCloseRequested
end;
//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//ABOUT
procedure HandleAboutInput(var game: GameDataType);
begin
	if WasKeyTyped(VK_RETURN) then game.state := menu;
	if WasKeyTyped(VK_N) then game.state := quit;
end;

procedure DrawAboutScreen();
begin
	ClearScreen(ColorBlack);
	DrawBitmap(GameImage('menubg'), 0, 0);
	DrawText('BREAKOUT', ColorWhite, GameFont('abstract'), 100, 50);
	DrawText('a b c d', ColorWhite, GameFont('Unreal'), 130, 100);
	DrawText('e f g h', ColorWhite, GameFont('Unreal'), 130, 200);
	DrawText('i j k l', ColorWhite, GameFont('Unreal'), 130, 300);
	DrawText('m n o p', ColorWhite, GameFont('Unreal'), 130, 400);
	DrawText('ENTER - Menu', ColorWhite, GameFont('Unreal'), 130, 500);
end;
//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// OPTIONS
procedure HandleOptionsInput(var game: GameDataType);
begin
	if WasKeyTyped(VK_1) then
	begin
		if game.MusicPlaying then StopMusic()
		else PlayMusic(GameMusic('bgmusic'));
		game.MusicPlaying := not game.MusicPlaying;
	end;
	if WasKeyTyped(VK_2) then game.state := HighScoreTable;
	if WasKeyTyped(VK_3) then game.state := Menu;  
end;

procedure DrawOptionsScreen();
begin
	ClearScreen(ColorBlack);
	DrawBitmap(GameImage('menubg'), 0, 0);
	DrawText('BREAKOUT OPTIONS', ColorWhite, GameFont('abstract'), 100, 50);
	DrawText('1. Sound On/Off', ColorWhite, GameFont('Unreal'), 130, 100);
	DrawText('2. View High Score Table', ColorWhite, GameFont('Unreal'), 130, 200);
	DrawText('3. Main Menu', ColorWhite, GameFont('Unreal'), 130, 300);
end;

//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//GENERAL INPUT AND DISPLAY SETTINGS

procedure HandleInput(var ball: Sprite; var player: TPlayer; var game: GameDataType);
begin
	case game.state of
		menu: HandleMenuInput(ball, player, game);
		playing: MovementControls(player.playersprite);
		GameOver: HandleGameOverInput(ball, player, game);
		about: HandleAboutInput(game);
		options: HandleOptionsInput(game);
		HighScoreTable: HandleHighScoreTableInput(game);
	end;
end;

procedure DrawScreen(var ball: Sprite; var player: TPlayer; var game: GameDataType);
begin
	case game.state of
		menu: DisplayMenu();
		playing: ShowGame(ball, player, game);
		GameOver: DrawGameOverScreen(player);
		about: DrawAboutScreen();
		options: DrawOptionsScreen();
		HighScoreTable: DisplayHighScoreTable();
	end;
end;

//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//MAIN

procedure Main();
var
	game: GameDataType;
	ball: Sprite;
	player: TPlayer;
begin
	OpenGraphicsWindow('Breakout', screenWidth, screenHeight);
	LoadResources();
	
	game.state := Menu;
	game.MusicPlaying := True;
	PlayMusic(GameMusic('bgmusic'));
	game.background := CreateSprite(GameImage('background'), 1, 1, 800, 600);
	ball := CreateSprite(GameImage('ball'));
	//CreateSprite(image : Bitmap; framesPerCell, frames, width, height: Integer): Sprite;
	ClearScreen(ColorBlack);
	repeat
		ProcessEvents();
		
		HandleInput(ball, player, game);
		UpdateGame(ball, player, game);
		DrawScreen(ball, player, game);
		
		RefreshScreen(45);
	until WindowCloseRequested() or (game.state = quit);
	
	FreeResources();
end;
end.
