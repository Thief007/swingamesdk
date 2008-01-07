unit GameLogic;

interface
	procedure Main();

implementation
	uses
		GameResources,
		SysUtils,
		SGSDK_Core, SGSDK_Font, SGSDK_Audio, SGSDK_Graphics, SGSDK_Input, SGSDK_Physics,
		SGSDK_KeyCodes;
	
	type
		BulletKind = (
			Normal
		);
		
		BulletSide = (
			Player,
			Enemy
		);
		
		ShipData = record
			theSprite : Sprite;
			speed : Single;
			shootDelay, currentDelay : Integer;
			maxMagazine, currentMagazine, reloadDelay: Integer;
		end;
		
		BulletData = record
			theSprite : Sprite;
			damage : Integer;
			kind : BulletKind;
			alive : Boolean;
			side : BulletSide;
		end;
		
		EnemyData = record
			theSprite : Sprite;
			health : Integer;
			alive : Boolean;
			time : Integer;
			shootDelay, currentDelay : Integer;
			maxMagazine, currentMagazine, reloadDelay: Integer;
		end;
		
		GameData = record
			player : ShipData;
			images : Array of Sprite;
			bullets : Array of BulletData;
			sounds : Array of SoundEffect;
			enemies : Array of EnemyData;
			gameTimer : Integer;
			music : Music;
		end;
	
	const
		SCREENWIDTH = 480;
		SCREENHEIGHT = 600;
	
	procedure LoadEnemies(var game : GameData);
	const
		ENEMIES = 1;
	var
		i : Integer;
	begin
		SetLength(game.enemies, ENEMIES);
		for i := 0 to Length(game.enemies) do begin
			game.enemies[i].theSprite := CreateSprite(GameImage('Enemy'), 2, 2, 75, 85);
			game.enemies[i].theSprite.xPos := -1 * game.enemies[i].theSprite.width;
			game.enemies[i].theSprite.yPos := 0;
			game.enemies[i].health := 50;
			game.enemies[i].time := 0;
			game.enemies[i].alive := true;
			game.player.shootDelay := 4;
			game.player.currentDelay := game.player.shootDelay;
			game.player.maxMagazine := 7;
			game.player.currentMagazine := game.player.maxMagazine;
			game.player.reloadDelay := 15;
		end;
	end;
	
	procedure LoadGame(var game : GameData);
	const
		PLAYEROFFSET = 150;
		BULLETCOUNT = 500;
		IMAGES = 2;
		SOUNDEFFECTS = 1;
	var
		i : Integer;
	begin
		//Initialise enemies
		LoadEnemies(game);
		//Set the length of GameSpriteData to the number of sprites I have
		SetLength(game.images, IMAGES);
		game.player.theSprite := CreateSprite(GameImage('Ship'), 2, 2, 40, 43);
		game.player.theSprite.xPos := Round(ScreenWidth / 2 - game.player.theSprite.width / 2);
		game.player.theSprite.yPos := ScreenHeight - PLAYEROFFSET;
		game.player.speed := 4;
		game.player.shootDelay := 4;
		game.player.currentDelay := game.player.shootDelay;
		game.player.maxMagazine := 7;
		game.player.currentMagazine := game.player.maxMagazine;
		game.player.reloadDelay := 15;
		//Create the background sprites from the loaded image
		game.images[0] := CreateSprite(GameImage('Map'));
		game.images[1] := CreateSprite(GameImage('Map'));
		//Initialise all bullets
		SetLength(game.bullets, BULLETCOUNT);
		for i := Low(game.bullets) to High(game.bullets) do
		begin
			game.bullets[i].alive := false;
		end;
		game.music := GameMusic('P8107');
		SetLength(game.sounds, SOUNDEFFECTS);
		game.sounds[0] := GameSound('Shoot');
		game.gameTimer := 0;
	end;

	procedure UpdateEntityPosition(speed, angle : Single; var target : Sprite);
	begin
		target.xPos := target.xPos + speed * SGSDK_Core.Cos(angle);
		target.yPos := target.yPos + speed * SGSDK_Core.Sin(angle);
	end;

	procedure MovePlayerShip(var player : ShipData);
	begin
		if IsKeyPressed(VK_LEFT) then
		begin
			//if the left arrow key is being pressed
			if IsKeyPressed(VK_UP) then
				//if the up arraw key is also being pressed
				UpdateEntityPosition(player.speed, 225, player.theSprite)
			else if IsKeyPressed(VK_DOWN) then
				//if the up down key is also being pressed
				UpdateEntityPosition(player.speed, 135, player.theSprite)
			else if not IsKeyPressed(VK_RIGHT) then
				//if the left arrow key is the only key being pressed
				player.theSprite.xPos := player.theSprite.xPos - player.speed
		end
		else if IsKeyPressed(VK_RIGHT) then
		begin
			//if the right arrow key is being pressed
			if IsKeyPressed(VK_UP) then
				//if the up arrow key is also being pressed
				UpdateEntityPosition(player.speed, 315, player.theSprite)
			else if IsKeyPressed(VK_DOWN) then
				//if the down arrow key is also being pressed
				UpdateEntityPosition(player.speed, 45, player.theSprite)
			//if the right key is the only key being pressed
			else player.theSprite.xPos := player.theSprite.xPos + player.speed
		end
		else if IsKeyPressed(VK_DOWN) and (not IsKeyPressed(VK_UP)) then
			//if the only key being pressed is the down arrow key
			player.theSprite.yPos := player.theSprite.yPos + player.speed
		else if IsKeyPressed(VK_UP) and (not IsKeyPressed(VK_DOWN)) then
			//if the only key being pressed is the up arrow key
			player.theSprite.yPos := player.theSprite.yPos - player.speed;
	end;

	procedure FixPosition(var theSprite : Sprite);
	begin
		if theSprite.xPos < 0 then
		    theSprite.xPos := 0;
		if theSprite.yPos < 0 then
		    theSprite.yPos := 0;
		if theSprite.xPos > SCREENWIDTH - theSprite.Width then
		    theSprite.xPos := SCREENWIDTH - theSprite.Width;
		if theSprite.yPos > SCREENHEIGHT - theSprite.Height then
		    theSprite.yPos := SCREENHEIGHT - theSprite.Height;
	end;
	
	procedure DeployBullet(bullet : BulletData; var bullets : Array of BulletData);
	var
		i : Integer;
	begin
		for i := Low(bullets) to High(bullets) do
		begin
			if not bullets[i].alive then begin
				bullets[i] := bullet;
				exit;
			end;
		end;
		WriteLn('No bullets to deploy…')
	end;
	
	function CreateBullet(imgName : String; shotFrom : ShipData; damage : Integer; 
						  kind : BulletKind; side : BulletSide; vec : Vector) : BulletData;
	begin
		result.theSprite := CreateSprite(GameImage(imgName));
		result.theSprite.xPos := shotFrom.theSprite.xPos + shotFrom.theSprite.width div 2 - result.theSprite.width div 2;
		result.theSprite.yPos := shotFrom.theSprite.yPos + shotFrom.theSprite.height div 2 - result.theSprite.height div 2;
		result.damage := damage;
		result.alive := true;
		result.side := side;
		result.kind := kind;
		result.theSprite.movement := vec;
	end;
	
	procedure ShootPlayerBullet(var game : GameData);
	const
		BULLETSPEED = 12;
		DAMAGE = 3;
	var
		tempBullet : BulletData;
	begin
		if game.player.currentMagazine <= 0 then
			game.player.currentMagazine := game.player.currentMagazine - 1;
		if game.player.currentDelay > 0 then
			game.player.currentDelay := game.player.currentDelay - 1;
		if game.player.currentMagazine < 1 then begin
			if game.player.currentMagazine = -1 * game.player.reloadDelay then
				game.player.currentMagazine := game.player.maxMagazine;
		end	else if IsKeyPressed(VK_Z) and (game.player.currentDelay = 0) then begin
			game.player.currentMagazine := game.player.currentMagazine - 1;
			game.player.currentDelay := game.player.shootDelay;
			PlaySoundEffect(game.sounds[0]);
			tempBullet := CreateBullet('PlayerBullet', game.player, DAMAGE, Normal, 
										Player, GetVectorFromAngle(270, BULLETSPEED));
			DeployBullet(tempBullet, game.bullets);
			tempBullet := CreateBullet('PlayerBullet', game.player, DAMAGE, Normal, 
										Player, GetVectorFromAngle(255, BULLETSPEED));
			DeployBullet(tempBullet, game.bullets);
			tempBullet := CreateBullet('PlayerBullet', game.player, DAMAGE, Normal, 
										Player, GetVectorFromAngle(285, BULLETSPEED));
			DeployBullet(tempBullet, game.bullets);
			tempBullet := CreateBullet('PlayerBullet', game.player, DAMAGE, Normal, 
										Player, GetVectorFromAngle(240, BULLETSPEED));
			DeployBullet(tempBullet, game.bullets);
			tempBullet := CreateBullet('PlayerBullet', game.player, DAMAGE, Normal, 
										Player, GetVectorFromAngle(300, BULLETSPEED));
			DeployBullet(tempBullet, game.bullets);
		end;
	end;
	
	procedure UpdateBullets(var bullets : Array of BulletData);
	var
		i : Integer;
	begin
		for i := Low(bullets) to High(bullets) do begin
			if bullets[i].alive then begin
				if IsSpriteOffscreen(bullets[i].theSprite) then begin
					bullets[i].alive := false;
					continue;
				end;
				DrawSprite(bullets[i].theSprite);
				UpdateSprite(bullets[i].theSprite);
			end;
		end;
	end;
	
	procedure UpdateShip(var game : GameData);
	begin
		MovePlayerShip(game.player);
		FixPosition(game.player.theSprite);
		ShootPlayerBullet(game);
		UpdateBullets(game.bullets);
		DrawSprite(game.player.theSprite);
		UpdateSpriteAnimation(game.player.theSprite);
	end;

	procedure UpdateBackground(var images : Array of Sprite);
	const
		BACKGROUNDSPEED = 10;
	begin
		//Slide the first background sprite by the background speed
		images[0].yPos := images[0].yPos + BACKGROUNDSPEED;
		//Reset the position of the first background sprite if it has
		//reachead the bottom
		if images[0].yPos >= SCREENHEIGHT then
			images[0].yPos := 0;
		//Set the second background position so the background is seamless
		images[1].yPos := images[0].yPos - SCREENHEIGHT;
		//Draw background sprites
		DrawSprite(images[0]);
		DrawSprite(images[1]);
	end;
	
	procedure ShootEnemyBullet(var enemy : EnemyData);
	const
		BULLETSPEED = 12;
		DAMAGE = 1;
	var
		tempBullet : BulletData;
	begin
		if enemy.currentMagazine <= 0 then
			enemy.currentMagazine := enemy.currentMagazine - 1;
		if enemy.currentDelay > 0 then
			enemy.currentDelay := enemy.currentDelay - 1;
		if enemy.currentMagazine < 1 then begin
			if enemy.currentMagazine = -1 * enemy.reloadDelay then
				enemy.currentMagazine := enemy.maxMagazine;
		end	else if IsKeyPressed(VK_Z) and (enemy.currentDelay = 0) then begin
			enemy.currentMagazine := enemy.currentMagazine - 1;
			enemy.currentDelay := enemy.shootDelay;
			tempBullet := CreateBullet('EnemyBullet', enemy, DAMAGE, Normal, 
										Enemy, GetVectorFromAngle(90, BULLETSPEED));
			DeployBullet(tempBullet, game.bullets);
			tempBullet := CreateBullet('EnemyBullet', enemy, DAMAGE, Normal, 
										Enemy, GetVectorFromAngle(75, BULLETSPEED));
			DeployBullet(tempBullet, game.bullets);
			tempBullet := CreateBullet('EnemyBullet', enemy, DAMAGE, Normal, 
										Enemy, GetVectorFromAngle(105, BULLETSPEED));
			DeployBullet(tempBullet, game.bullets);
			tempBullet := CreateBullet('EnemyBullet', enemy, DAMAGE, Normal, 
										Enemy, GetVectorFromAngle(60, BULLETSPEED));
			DeployBullet(tempBullet, game.bullets);
			tempBullet := CreateBullet('EnemyBullet', enemy, DAMAGE, Normal, 
										Enemy, GetVectorFromAngle(120, BULLETSPEED));
			DeployBullet(tempBullet, game.bullets);
		end;
	end;
	
	procedure UpdateEnemies(var game : GameData);
	const
		ENEMYSPEED = 3;
	var
		i : Integer;
	begin
		for i := 0 to High(game.enemies) do begin
			if (game.enemies[i].time <= game.gameTimer) and game.enemies[i].alive then begin
				game.enemies[i].theSprite.xPos := game.enemies[i].theSprite.xPos + ENEMYSPEED;
				if game.enemies[i].theSprite.xPos >= SCREENWIDTH then
					game.enemies[i].alive := false;
				DrawSprite(game.enemies[i].theSprite);
				UpdateSpriteAnimation(game.enemies[i].theSprite);
			end;
		end;
	end;
	
	procedure MainGame();
	var
		game : GameData;
	begin
		LoadGame(game);
		PlayMusic(game.music);
		repeat
			ProcessEvents();
			UpdateBackground(game.images);
			UpdateShip(game);
			UpdateEnemies(game);
			game.gameTimer := game.gameTimer + 1;
			RefreshScreen();
		until WindowCloseRequested();
		StopMusic();
	end;

	procedure Main();
	begin
		OpenGraphicsWindow('EmptyYourMind1.1', SCREENWIDTH, SCREENHEIGHT);
		LoadResources();
		MainGame();
		FreeResources();
	end;
end.