unit GameLogic;

interface
	procedure Main();

implementation
	uses
		GameResources,
		SysUtils,
		sgCore, sgText, sgAudio, sgGraphics, sgInput, sgPhysics, sgTypes, sgMath;
	
	type
		EnemyMovement = (
			LeftToRight,
			RightToLeft
		);
		
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
			health : Integer;
			alive : Boolean;
			shootDelay, currentDelay : Integer;
			maxMagazine, currentMagazine, reloadDelay: Integer;
			//Enemy data
			time, offset : Integer;
			direction : EnemyMovement;
			//The sprite data used for the player collision
			theSpriteC : Sprite;
		end;
		
		BulletData = record
			theSprite : Sprite;
			damage : Integer;
			kind : BulletKind;
			alive : Boolean;
			side : BulletSide;
		end;
		
		GameData = record
			player : ShipData;
			images : Array of Sprite;
			bullets : Array of BulletData;
			sounds : Array of SoundEffect;
			enemies : Array of ShipData;
			gameTimer : Integer;
			music : Music;
		end;
	
	const
		SCREENWIDTH = 480;
		SCREENHEIGHT = 600;
	
	function CreateEnemy(yPos, time : Integer; direction : EnemyMovement) : ShipData;
	begin
		result.theSprite := CreateSprite(GameImage('Enemy'), 2, 2, 75, 85);
		//Initialise the enemy's position
		result.theSprite.x := -1 * result.theSprite.width;
		result.theSprite.y := 0;
		result.speed := 1;
		result.health := 50;
		result.time := time;
		result.alive := true;
		result.shootDelay := 4;
		result.currentDelay := result.shootDelay;
		result.maxMagazine := 7;
		result.currentMagazine := result.maxMagazine;
		result.reloadDelay := 15;
		result.offset := -75;
		result.direction := direction;
		result.theSprite.y := yPos;
		//Fix the enemy's position if the direction is RightToLeft
		if result.direction = RightToLeft then begin
			result.theSprite.x := SCREENWIDTH;
			result.speed := result.speed * -1;
		end;
	end;
	
	procedure LoadEnemies(var game : GameData);
	const
		ENEMIES = 3;
	begin
		SetLength(game.enemies, ENEMIES);
		game.enemies[0] := CreateEnemy(0, 15, LeftToRight);
		game.enemies[1] := CreateEnemy(200, 650, RightToLeft);
		game.enemies[2] := CreateEnemy(100, 1200, LeftToRight);
	end;
	
	procedure LoadPlayer(var game : GameData);
	const
		PLAYEROFFSET = 150;
	begin
		game.player.theSprite := CreateSprite(GameImage('Ship'), 2, 2, 40, 43);
		game.player.theSprite.x := Round(ScreenWidth / 2 - game.player.theSprite.width / 2);
		game.player.theSprite.y := ScreenHeight - PLAYEROFFSET;
		game.player.speed := 4;
		game.player.shootDelay := 4;
		game.player.currentDelay := game.player.shootDelay;
		game.player.maxMagazine := 7;
		game.player.currentMagazine := game.player.maxMagazine;
		game.player.reloadDelay := 15;
		game.player.alive := true;
		game.player.theSpriteC := CreateSprite(GameImage('ShipC'));
		game.player.health := 1;
	end;
	
	procedure LoadGame(var game : GameData);
	const
		BULLETCOUNT = 1000;
		IMAGES = 2;
		SOUNDEFFECTS = 2;
	var
		i : Integer;
	begin
		//Initialise enemies
		LoadEnemies(game);
		//Set the length of GameSpriteData to the number of sprites I have
		SetLength(game.images, IMAGES);
		LoadPlayer(game	);
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
		game.sounds[1] := GameSound('Destroy');
		game.gameTimer := 0;
	end;

	procedure UpdateEntityPosition(speed, angle : Single; var target : Sprite);
	begin
		target.x := target.x + speed * sgCore.Cos(angle);
		target.y := target.y + speed * sgCore.Sin(angle);
	end;

	procedure MovePlayerShip(var player : ShipData);
	begin
		if KeyDown(VK_LEFT) then
		begin
			//if the left arrow key is being pressed
			if KeyDown(VK_UP) then
				//if the up arraw key is also being pressed
				UpdateEntityPosition(player.speed, 225, player.theSprite)
			else if KeyDown(VK_DOWN) then
				//if the up down key is also being pressed
				UpdateEntityPosition(player.speed, 135, player.theSprite)
			else if not KeyDown(VK_RIGHT) then
				//if the left arrow key is the only key being pressed
				player.theSprite.x := player.theSprite.x - player.speed
		end
		else if KeyDown(VK_RIGHT) then
		begin
			//if the right arrow key is being pressed
			if KeyDown(VK_UP) then
				//if the up arrow key is also being pressed
				UpdateEntityPosition(player.speed, 315, player.theSprite)
			else if KeyDown(VK_DOWN) then
				//if the down arrow key is also being pressed
				UpdateEntityPosition(player.speed, 45, player.theSprite)
			//if the right key is the only key being pressed
			else player.theSprite.x := player.theSprite.x + player.speed
		end
		else if KeyDown(VK_DOWN) and (not KeyDown(VK_UP)) then
			//if the only key being pressed is the down arrow key
			player.theSprite.y := player.theSprite.y + player.speed
		else if KeyDown(VK_UP) and (not KeyDown(VK_DOWN)) then
			//if the only key being pressed is the up arrow key
			player.theSprite.y := player.theSprite.y - player.speed;
		//Move the collision sprite
		player.theSpriteC.x := player.theSprite.x;
		player.theSpriteC.y := player.theSprite.y;
	end;

	procedure FixPosition(var theSprite : Sprite);
	begin
		if theSprite.x < 0 then
		    theSprite.x := 0;
		if theSprite.y < 0 then
		    theSprite.y := 0;
		if theSprite.x > SCREENWIDTH - theSprite.Width then
		    theSprite.x := SCREENWIDTH - theSprite.Width;
		if theSprite.y > SCREENHEIGHT - theSprite.Height then
		    theSprite.y := SCREENHEIGHT - theSprite.Height;
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
		result.theSprite.x := shotFrom.theSprite.x + shotFrom.theSprite.width div 2 - result.theSprite.width div 2;
		result.theSprite.y := shotFrom.theSprite.y + shotFrom.theSprite.height div 2 - result.theSprite.height div 2;
		result.damage := damage;
		result.alive := true;
		result.side := side;
		result.kind := kind;
		result.theSprite.movement := vec;
	end;
	
	procedure ShootPlayerBullet(var game : GameData);
	const
		BULLETSPEED = 13;
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
		end	else if KeyDown(VK_Z) and (game.player.currentDelay = 0) then begin
			game.player.currentMagazine := game.player.currentMagazine - 1;
			game.player.currentDelay := game.player.shootDelay;
			PlaySoundEffect(game.sounds[0]);
			tempBullet := CreateBullet('PlayerBullet', game.player, DAMAGE, Normal, 
										Player, VectorFrom(270, BULLETSPEED));
			DeployBullet(tempBullet, game.bullets);
			tempBullet := CreateBullet('PlayerBullet', game.player, DAMAGE, Normal, 
										Player, VectorFrom(255, BULLETSPEED));
			DeployBullet(tempBullet, game.bullets);
			tempBullet := CreateBullet('PlayerBullet', game.player, DAMAGE, Normal, 
										Player, VectorFrom(285, BULLETSPEED));
			DeployBullet(tempBullet, game.bullets);
			tempBullet := CreateBullet('PlayerBullet', game.player, DAMAGE, Normal, 
										Player, VectorFrom(240, BULLETSPEED));
			DeployBullet(tempBullet, game.bullets);
			tempBullet := CreateBullet('PlayerBullet', game.player, DAMAGE, Normal, 
										Player, VectorFrom(300, BULLETSPEED));
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
	
	procedure ProcessShipCollision(var shipToProcess : ShipData; var game : GameData; shipSide : BulletSide);
	var
		i : Integer;
		tempSprite : Sprite;
	begin
		for i := 0 to High(game.bullets) do begin
			if game.bullets[i].alive  and not (game.bullets[i].side = shipSide) then begin
				//Initialise the sprite used to check the collision
				if shipSide = Enemy then
					tempSprite := shipToProcess.theSprite
				else
					tempSprite := shipToProcess.theSpriteC;
				if SpritesCollided(game.bullets[i].theSprite, tempSprite) then begin
					shipToProcess.health := shipToProcess.health - game.bullets[i].damage;
					//Kill and play a sound effect if the health is lower than 0
					if shipToProcess.health <= 0 then begin
						shipToProcess.alive := false;
						PlaySoundEffect(game.sounds[1]);
					end;
					//Kill the bullet collided
					game.bullets[i].alive := false;
				end;
			end;
		end;
	end;
	
	procedure UpdateShip(var game : GameData);
	begin
		MovePlayerShip(game.player);
		FixPosition(game.player.theSprite);
		ShootPlayerBullet(game);
		ProcessShipCollision(game.player, game, Player);
		DrawSprite(game.player.theSprite);
		UpdateSpriteAnimation(game.player.theSprite);
	end;

	procedure UpdateBackground(var images : Array of Sprite);
	const
		BACKGROUNDSPEED = 10;
	begin
		//Slide the first background sprite by the background speed
		images[0].y := images[0].y + BACKGROUNDSPEED;
		//Reset the position of the first background sprite if it has
		//reachead the bottom
		if images[0].y >= SCREENHEIGHT then
			images[0].y := 0;
		//Set the second background position so the background is seamless
		images[1].y := images[0].y - SCREENHEIGHT;
		//Draw background sprites
		DrawSprite(images[0]);
		DrawSprite(images[1]);
	end;
	
	procedure ShootEnemyBullet(var enemyToProcess : ShipData; var game : GameData);
	const
		BULLETSPEED = 3;
		DAMAGE = 1;
	var
		tempBullet : BulletData;
		i : Integer;
	begin
		if enemyToProcess.currentMagazine <= 0 then
			enemyToProcess.currentMagazine := enemyToProcess.currentMagazine - 1;
		if enemyToProcess.currentDelay > 0 then
			enemyToProcess.currentDelay := enemyToProcess.currentDelay - 1;
		if enemyToProcess.currentMagazine < 1 then begin
			if enemyToProcess.currentMagazine = -1 * enemyToProcess.reloadDelay then
				enemyToProcess.currentMagazine := enemyToProcess.maxMagazine;
		end else if enemyToProcess.currentDelay = 0 then begin
			enemyToProcess.currentMagazine := enemyToProcess.currentMagazine - 1;
			enemyToProcess.currentDelay := enemyToProcess.shootDelay;
			//Enemy bullet pattern
			for i := 0 to 18 do begin
				tempBullet := CreateBullet('EnemyBullet', enemyToProcess, DAMAGE, Normal, 
											Enemy, VectorFromAngle(60 + 20 * i + enemyToProcess.offset, BULLETSPEED));
				DeployBullet(tempBullet, game.bullets);
				//PlaySoundEffect(game.sounds[2]);
        PlaySoundEffect(game.sounds[0]);
			end;
			enemyToProcess.offset := enemyToProcess.offset + 10;
		end;
	end;
	
	procedure UpdateEnemies(var game : GameData);
	var
		i : Integer;
	begin
		for i := 0 to High(game.enemies) do begin
			if (game.enemies[i].time <= game.gameTimer) and game.enemies[i].alive then begin
				game.enemies[i].theSprite.x := game.enemies[i].theSprite.x + game.enemies[i].speed;
				if IsSpriteOffscreen(game.enemies[i].theSprite) then begin
					game.enemies[i].alive := false;
					continue;
				end;
				ProcessShipCollision(game.enemies[i], game, Enemy);
				ShootEnemyBullet(game.enemies[i], game);
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
		//PlayMusic(game.music);
		repeat
			ProcessEvents();
			UpdateBackground(game.images);
			UpdateBullets(game.bullets);
			UpdateShip(game);
			UpdateEnemies(game);
			game.gameTimer := game.gameTimer + 1;
			RefreshScreen(60);
			if not game.player.alive then
      begin
        WriteLn('###');
        break;
      end;
      // show the current framerate... see if it's broken
      //WriteLn('FPS' + IntToStr(GetFramerate));
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