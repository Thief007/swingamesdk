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
		
		GameData = record
			player : ShipData;
			images : Array of Sprite;
			bullets : Array of BulletData;
			sounds : Array of SoundEffect;
			music : Music;
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
		if theSprite.xPos > ScreenWidth() - theSprite.Width then
		    theSprite.xPos := ScreenWidth() - theSprite.Width;
		if theSprite.yPos > ScreenHeight() - theSprite.Height then
		    theSprite.yPos := ScreenHeight() - theSprite.Height;
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
		if images[0].yPos >= ScreenHeight() then
			images[0].yPos := 0;
		//Set the second background position so the background is seamless
		images[1].yPos := images[0].yPos - ScreenHeight();
		//Draw background sprites
		DrawSprite(images[0]);
		DrawSprite(images[1]);
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
			RefreshScreen();
		until WindowCloseRequested();
		StopMusic();
	end;

	procedure Main();
	const
		SCREENWIDTH = 480;
		SCREENHEIGHT = 600;
	begin
		OpenGraphicsWindow('EmptyYourMind1.1', SCREENWIDTH, SCREENHEIGHT);
		LoadResources();
		MainGame();
		FreeResources();
	end;
end.