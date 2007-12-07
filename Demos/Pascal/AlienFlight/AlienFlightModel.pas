unit AlienFlightModel;

interface
	uses 
		SysUtils, 
		SGSDK_Core, SGSDK_Graphics, SGSDK_Audio, SGSDK_Physics,
		GameResources;
	
	type
		/// Large entities in the game that can
		/// collide with each other etc.
		Obstacle = record
			name: String;
			mass: Single;
			sprite: Sprite;
			movement: Vector;
			alive: Boolean;
		end;

		/// Special entity that is used to move
		/// between levels
		WarpHole = record
			sprite: Sprite;
			level: Integer;
		end;
		
		WarpHolePtr = ^WarpHole;
		ObstaclePtr = ^Obstacle;
	 
		/// Other things are animations
		/// colliding with an "animation"
		/// has a certain effect
		AnimationAction = (
				NoAction,
				AddFuelAction,
				AddScoreAction,
				AddShieldAction
			);
	
		Animation = record
			image: Bitmap;
			partWidth, partHeight: Integer;
			rows, cols: Integer;
			numCells: Integer;
			framesPerCell: Integer;
			frameCount: Integer;
			currentCell: Integer;
			cellDiff: Integer;
			used: Boolean;
			x, y: Single;
			movement: Vector;
			action: AnimationAction;
			value: Single;
		end;
		
		AnimationPtr = ^Animation;
	
		GameState = (
			PlayingIntroState,
			GameMenuState,
			PlayingGameState,
			WarpingState,
			PlayerDyingState,
			EndOfLevelState,
			EditorState
		);
	
		GameDataType = record
			state: GameState;
			
			currentLevel: Integer;
			
			windowX, windowY: Single;
			
			UfoSprite: Sprite;
			ShieldBmp: Bitmap;
			
			Planets: Array of Obstacle;
			MidBacks: Array of Sprite;
			WarpHoles: Array of WarpHole;
			
			TemporaryAnimations: Array of Animation;
			LevelAnimations: Array of Animation;
			//Stars: Array of Animation;
			//fuelPacks: Array of Animation;

			//gameOver: Boolean;
			score: Integer;
			boost: Single;
			fuelLevel: Integer;
			shieldStrength: Single;
		end;
				
	const
		SCREEN_WIDTH = 800; SCREEN_HEIGHT=600;
		BORDER_X = 10; BORDER_Y = 10; HEADING_HEIGHT = 50;
		GAME_AREA_HEIGHT = (SCREEN_HEIGHT - HEADING_HEIGHT - 2 * BORDER_Y);
		TOP_GAME_AREA = HEADING_HEIGHT + BORDER_Y;
		
		SCROLL_PIXELS 	= 2;
		BKG_MULT 		= 5;		//How much slower is background?
		BASE_SPEED 		= 0.5;
		SPEED_MUL		= 0.3;
		SPEED_LEVEL_MUL = 0.05;
		PLAYER_BASE_MOVE = 1.5;
		
		SHIELD_OFFSET = 15;
		
		FULL_HEALTH = 100;
		DAMAGE_FOR_MASS_1 = 90;
		
		MAX_ANIMATIONS = 255;
		
		EXPLOSION_FRAMES = 3;
		WARP_FRAMES = 3;
		
		STAR_VALUE = 20;
		FUEL_VALUE = 50;
		BATTERY_VALUE = 0.1;
		
		MAX_FUEL = 100;
		FUEL_BOOST = 1; //Cost of boost per frame
		BOOST_AMT = 0.3;
		
	var _Paused: Boolean;

	procedure ClearAnimation(var anim: Animation);
	procedure MoveGameSprite(sprt: Sprite; cx, cy: Single);
	procedure MoveGameWindow(var data: GameDataType; dx, dy: Single);	
	procedure UpdateGameState(var data: GameDataType);
	procedure CheckCollisionBetweenPlanets(var data: GameDataType);
	procedure FreeLevel(var data: GameDataType);
	procedure ResetLevel(var data: GameDataType);
	procedure CleanupGameData(var data: GameDataType);
	procedure AddExplosion(var data: GameDataType; x, y: Single; movement: Vector);
	procedure UpdateBoost(var data: GameDataType; triggerOn: Boolean);
	procedure AddWarpAnimation(var data: GameDataType; obj: Sprite; v: Vector; steps: Integer);
	procedure UpdatePlayerLocation(player: Sprite; const data: GameDataType);
	procedure MovePlayer(var data: GameDataType; mulX, mulY: Integer);	
			
	function GetCollidedWarphole(const sprte: Sprite; const data: GameDataType): WarpHolePtr;
	function CalculateBackgroundX(foregroundX: Single): Single;
	function CalculateForegroundX(backgroundX: Single): Single;
	function MaxForegroundX(): Single;
	function SetupGameData(): GameDataType;	
	function ObjectHasHitAPlanet(obj: Sprite; const data: GameDataType): Boolean;
	function PlayerHasHitPlanet(const data: GameDataType): Boolean;
	function PlanetAtPoint(v: Vector; const data: GameDataType; out obst: ObstaclePtr): Boolean;
	function WarpHoleAtPoint(v: Vector; const data: GameDataType; out warp: WarpHolePtr): Boolean;
	function MidBackAtPoint(v: Vector; const data: GameDataType; out sprt: Sprite): Boolean;
	function ObjectHasHitMidBack(obj: Sprite; const data: GameDataType): Boolean;
	function CalculateVectorFromTo(obj, dest: Sprite): Vector;

implementation
	
	procedure ClearAnimation(var anim: Animation);
	begin
		anim.image := nil;
		anim.numCells := 0;
		anim.used := false;
		anim.currentCell := 0;
		anim.framesPerCell := 1;
		anim.frameCount := 0;
		anim.partWidth := 0;
		anim.partHeight := 0;
		anim.movement.x := 0;
		anim.movement.y := 0;
		anim.x := 0;
		anim.y := 0;
		anim.cellDiff := 1;
		anim.action := NoAction;
		anim.value := 0;
	end;
	
	function GetFreeAnimationIdx(var data: GameDataType): Integer;
	var
		i, use, frameMax: Integer;
	begin
		frameMax := 0;
		
		for i := 0 to MAX_ANIMATIONS do
		begin
			if false = data.temporaryAnimations[i].used then
			begin
				use := i;
				break;
			end
			else if data.temporaryAnimations[i].currentCell > frameMax then
			begin
				//Use oldest animation if none are free
				frameMax := data.temporaryAnimations[i].currentCell;
				use := i;
			end;
		end;
		
		if data.temporaryAnimations[use].used then
		begin
			ClearAnimation(data.temporaryAnimations[use]);
		end;
		
		result := use;
	end;
	
	procedure AddTemporaryAnimation(var data: GameDataType; bmp: Bitmap; 
		xPos, yPos: Single; movementVec: Vector;
		srcRows, srcCols, srcWidth, srcHeight, cells, numFramesPerCell: Integer;
		doAction: AnimationAction; withValue: Integer);
	var
		use: Integer;
	begin
		use := GetFreeAnimationIdx(data);
		
		with data.TemporaryAnimations[use] do
		begin
			image := bmp;
			x := xPos;
			y := yPos;
			movement := movementVec;
			rows := srcRows;
			cols := srcCols;
			partWidth := srcWidth;
			partHeight := srcHeight;
			numCells := cells;
			used := true;
			framesPerCell := numFramesPerCell;
			action := doAction;
			value := withValue;
		end;
	end;
	
	procedure AddExplosion(var data: GameDataType; x, y: Single; movement: Vector);
	begin
		AddTemporaryAnimation(data, GameImage(ExplosionImg), x, y, movement, 
			2, 8, 38, 38, 16, EXPLOSION_FRAMES, NoAction, 0);
		
		PlaySoundEffect(GameSound(DestroySound));
	end;

	function GetCollidedWarphole(const sprte: Sprite; const data: GameDataType): WarpHolePtr;
	var
		i: Integer;
	begin
		result := nil;
		for i := Low(data.warpHoles) to High(data.warpHoles) do
		begin
			if HaveSpritesCollided(data.ufoSprite, data.warpHoles[i].Sprite) then
			begin
				result := @data.warpHoles[i];
			end;
		end;
	end;

	function CalculateVectorFromTo(obj, dest: Sprite): Vector;
	var
		destWdiv2, destHdiv2: Integer;
		objWdiv2, objHdiv2: Integer;
		v, pc, wc: Vector;
	begin
		objWdiv2 := CurrentWidth(obj) div 2;
		objHdiv2 := CurrentHeight(obj) div 2;
											
		destWdiv2 := CurrentWidth(dest) div 2;
		destHdiv2 := CurrentHeight(dest) div 2;
		
		pc := CreateVector(obj.xPos + objWdiv2, obj.yPos + objHdiv2);
		wc := CreateVector(dest.xPos + destWdiv2, dest.yPos + destHdiv2);
		v := SubtractVectors(wc, pc);
		
		{WriteLn('xy: ', x:4:2, ',', y:4:2);
		WriteLn('pc: ', pc.x:4:2, ',', pc.y:4:2, ' - ', objWdiv2, ',', objHdiv2);
		WriteLn('wc: ', wc.x:4:2, ',', wc.y:4:2, ' - ', destWdiv2, ',', destHdiv2);	
		WriteLn('v : ', v.x:4:2, ',', v.y:4:2);}
		
		result := v;		
	end;
	
	procedure AddWarpAnimation(var data: GameDataType; obj: Sprite; v: Vector; steps: Integer);
	const
		WARP_ANIMATION_PART_WIDTH = 90;
	var
		x, y: Single;
		objWdiv2, objHdiv2: Integer;
	begin
		objWdiv2 := CurrentWidth(obj) div 2;
		objHdiv2 := CurrentHeight(obj) div 2;
			
		v.x *= 1 / steps;
		v.y *= 1 / steps;
		
		x := obj.xPos + objWdiv2 - WARP_ANIMATION_PART_WIDTH div 2;
		y := obj.yPos + objHdiv2 - WARP_ANIMATION_PART_WIDTH div 2;
					
		AddTemporaryAnimation(data, GameImage(WarpImg), x, y, v, 
			1, 5, 90, 90, 5, WARP_FRAMES, NoAction, 0);
	end;
	
	procedure UpdateAnimation(var anim: Animation; clear: Boolean);
	begin
		if anim.used then
		begin
			with anim do
			begin
				frameCount += 1;
				
				if (frameCount mod framesPerCell) = 0 then
				begin
					currentCell += cellDiff;
										
					if (currentCell >= (numCells - 1)) or (currentCell <= 0) then
					begin
						if clear then
							ClearAnimation(anim)
						else
							cellDiff := -cellDiff;
					end;
				end;
				x += movement.X;
				y += movement.Y;
			end;
		end;
	end;
	
	procedure UpdateAnimationedElements(var data: GameDataType);
	var
		i: Integer;
	begin
		for i := 0 to MAX_ANIMATIONS - 1 do
		begin
			UpdateAnimation(data.temporaryAnimations[i], true);
		end;

		for i := 0 to High(data.levelAnimations) do
		begin
			UpdateAnimation(data.levelAnimations[i], false);
		end;
	end;

	function MaxForegroundX(): Single;
	begin
		result := CalculateForegroundX(GameImage(Background).width - SCREEN_WIDTH) + ((SCREEN_WIDTH * 3) div 4);
	end;

	function CalculateForegroundX(backgroundX: Single): Single;		
	begin
		result := (backgroundX * (BKG_MULT * SCROLL_PIXELS));
	end;

	function CalculateBackgroundX(foregroundX: Single): Single;
	begin
		result := (foregroundX / (BKG_MULT * SCROLL_PIXELS));
	end;
	
	procedure MoveGameSprite(sprt: Sprite; cx, cy: Single);
	begin
		sprt.xPos := cx - CurrentWidth(sprt) div 2;
		sprt.yPos := cy - CurrentHeight(sprt) div 2;
	end;

	/// Called by the Level Editor to position game window.
	procedure MoveGameWindow(var data: GameDataType; dx, dy: Single);
	begin
		data.windowX := data.windowX + (dx * SCROLL_PIXELS);
		data.windowY := data.windowY + (dy * SCROLL_PIXELS);
	end;
	
	procedure UpdateShieldBmp(var data: GameDataType);
	var
		w, h: Integer;
		c: Color;
	begin
		ClearSurface(data.ShieldBmp, ColorTransparent);
		w := Round(data.ShieldBmp.Surface.w);
		h := Round(data.ShieldBmp.Surface.h);
		
		c := GetHSBColor(120 / 360 * data.ShieldStrength, 0.7, 0.5);
		
		if data.ShieldStrength > 0.7 then
		begin
			DrawEllipse(data.ShieldBmp, c, 0, 0, w - 1, h - 1);
		end;
			
		if data.ShieldStrength > 0.4 then
		begin
			DrawEllipse(data.ShieldBmp, c, 1, 0, w - 2, h - 1);
			DrawEllipse(data.ShieldBmp, c, 0, 1, w - 1, h - 2);
			DrawEllipse(data.ShieldBmp, c, 1, 1, w - 2, h - 2);
		end;
		
		if data.ShieldStrength > 0.2 then
		begin		
			DrawEllipse(data.ShieldBmp, c, 1, 2, w - 2, h - 3);
			DrawEllipse(data.ShieldBmp, c, 2, 1, w - 3, h - 2);
		end;
		
		if data.ShieldStrength > 0 then
		begin				
			DrawEllipse(data.ShieldBmp, c, 2, 2, w - 3, h - 3);
		end;
	end;

	function MidBackAtPoint(v: Vector; const data: GameDataType; out sprt: Sprite): Boolean;
	var
		i: Integer;
		wx, wy: Integer;
	begin
		wx := Round(CalculateBackgroundX(data.windowX));
		wy := Round(data.windowY);
		
		for i := Low(data.MidBacks) to High(data.MidBacks) do
		begin
			if HasSpriteCollidedWithRect(data.MidBacks[i], v.x, v.y, 1, 1, wx, wy) then
			begin
				result := true;
				sprt := data.MidBacks[i]; 
				exit;
			end;
		end;	
		result := false;
	end;
	
	function PlanetAtPoint(v: Vector; const data: GameDataType; out obst: ObstaclePtr): Boolean;
	var
		i, wx, wy: Integer;
	begin
		wx := Round(data.windowX);
		wy := Round(data.windowY);
		
		for i := Low(data.Planets) to High(data.Planets) do
		begin
			if HasSpriteCollidedWithRect(data.Planets[i].Sprite, v.x, v.y, 1, 1, wx, wy) then
			begin
				result := true;
				obst := @data.Planets[i];
				exit;
			end;
		end;	
		result := false;
	end;

	function WarpHoleAtPoint(v: Vector; const data: GameDataType; out warp: WarpHolePtr): Boolean;
	var
		i, wx, wy: Integer;
	begin
		wx := Round(data.windowX);
		wy := Round(data.windowY);
		
		for i := Low(data.WarpHoles) to High(data.WarpHoles) do
		begin
			if HasSpriteCollidedWithRect(data.WarpHoles[i].Sprite, v.x, v.y, 1, 1, wx, wy) then
			begin
				result := true;
				warp := @data.WarpHoles[i];
				exit;
			end;
		end;	
		result := false;
	end;

	function ObjectHasHitMidBack(obj: Sprite; const data: GameDataType): Boolean;
	var
		i: Integer;
	begin
		for i := Low(data.MidBacks) to High(data.MidBacks) do
		begin
			if (obj <> data.MidBacks[i]) and 
				 HaveSpritesCollided(obj, data.MidBacks[i]) then
			begin
				result := true;
				exit;
			end;
		end;	
		result := false;
	end;
	
	function ObjectHasHitAPlanet(obj: Sprite; const data: GameDataType): Boolean;
	var
		i: Integer;
	begin
		for i := Low(data.Planets) to High(data.Planets) do
		begin
			if data.Planets[i].alive and (obj <> data.Planets[i].Sprite) and 
				 HaveSpritesCollided(obj, data.Planets[i].Sprite) then
			begin
				result := true;
				exit;
			end;
		end;	
		result := false;
	end;

	function ObjectHasHitAWarpHole(obj: Sprite; const data: GameDataType; out level: Integer): Boolean;
	var
		i: Integer;
	begin
		for i := Low(data.warpHoles) to High(data.warpHoles) do
		begin
			if HaveSpritesCollided(obj, data.warpHoles[i].Sprite) then
			begin
				result := true;
				level := data.warpHoles[i].level;
				exit;
			end;
		end;
		level := data.currentLevel;	
		result := false;
	end;

	function PlayerHasHitPlanet(const data: GameDataType): Boolean;
	begin
		result := ObjectHasHitAPlanet(data.UfoSprite, data);
	end;
	
	procedure DoPlanetCollision(var p1, p2: Obstacle);
	var
		colNormalAngle, a1, a2, optP: Single;
		n: Vector;
	begin
		colNormalAngle := CalculateAngle(p1.Sprite, p2.Sprite);
		
		//COLLISION RESPONSE
		// n = vector connecting the centers of the balls.
		// we are finding the components of the normalised vector n
		n := CreateVector(Cos(colNormalAngle), Sin(colNormalAngle));
		
		// now find the length of the components of each movement vectors
		// along n, by using dot product.
		a1 := DotProduct(p1.Movement, n);
		//Local a1# = c.dx*nX  +  c.dy*nY
		a2 := DotProduct(p2.Movement, n);
		//Local a2# = c2.dx*nX +  c2.dy*nY
		
		// optimisedP = 2(a1 - a2)
		//             ----------
		//              m1 + m2
		optP := (2.0 * (a1-a2)) / (p1.mass + p2.mass);
		
		// now find out the resultant vectors
		// Local r1% = c1.v - optimisedP * mass2 * n
		p1.movement.x := p1.movement.x - (optP * p2.mass * n.x);
		p1.movement.y := p1.movement.y - (optP * p2.mass * n.y);
		
		// Local r2% = c2.v - optimisedP * mass1 * n
		p2.movement.x := p2.movement.x + (optP * p1.mass * n.x);
		p2.movement.y := p2.movement.y + (optP * p1.mass * n.y);
	end;
	
	procedure DoPlayerCollision(var data: GameDataType; var obj: Obstacle);
	var
		damage: Single;
	begin
		if data.ShieldStrength <= 0 then
		begin
			data.state := PlayerDyingState;
			exit;
		end;
		
		damage := (DAMAGE_FOR_MASS_1 * obj.mass) / FULL_HEALTH;
		data.ShieldStrength := data.ShieldStrength - damage;
		//WriteLn(damage:5:2, ' = ', data.ShieldStrength:5:2 , ' - ', obj.mass);
		obj.alive := false;
		UpdateShieldBmp(data);
		
		AddExplosion(data, obj.sprite.xPos, obj.sprite.yPos, obj.movement);
	end;

	procedure CollectBattery(var data: GameDataType; var battery: Animation);
	begin
		if battery.used then
		begin
			battery.used := false;
			PlaySoundEffect(GameSound(CollectFuelSound));
			data.shieldStrength += battery.value;
			if data.shieldStrength > 1.0 then data.shieldStrength := 1.0;
		end;
	end;

	procedure CollectFuel(var data: GameDataType; var fuelPack: Animation);
	begin
		if fuelPack.used then
		begin
			fuelPack.used := false;
			PlaySoundEffect(GameSound(CollectFuelSound));
			data.fuelLevel += Round(fuelPack.value);
			if data.fuelLevel > MAX_FUEL then data.fuelLevel := Round(MAX_FUEL);
		end;
	end;
	
	procedure CollectStar(var data: GameDataType; var star: Animation);
	begin
		if star.used then
		begin
			star.used := false;
			PlaySoundEffect(GameSound(CollectStarSound));
			data.score += Round(star.value);
		end;
	end;
	
	procedure TestCollisionWithAnimation(var data: GameDataType; var anim: Animation);
	begin
		if HasSpriteCollidedWithRect(data.ufoSprite, anim.x, anim.y, anim.partWidth, anim.partHeight) then
		begin
			case anim.action of
				NoAction: 		exit;
				AddScoreAction: CollectStar(data, anim);
				AddFuelAction:	CollectFuel(data, anim);
				AddShieldAction: CollectBattery(data, anim);
			end;
		end;
	end;
	
	procedure TestPlayerCollision(var data: GameDataType);
	var
		i, level: Integer;
	begin
		for i := Low(data.Planets) to High(data.Planets) do
		begin
			if data.Planets[i].Alive and 
				HaveSpritesCollided(data.UfoSprite, data.Planets[i].Sprite) then
			begin
				DoPlayerCollision(data, data.Planets[i]);
			end;
		end;
		
		for i := Low(data.levelAnimations) to High(data.levelAnimations) do
		begin
			if data.levelAnimations[i].used then
				TestCollisionWithAnimation(data, data.levelAnimations[i]);
		end;
		
		if ObjectHasHitAWarpHole(data.ufoSprite, data, level) then
		begin
			data.currentLevel := level;
			data.state := WarpingState;
			//WriteLn('Warping to level ', level);
		end;	
	end;
	
	procedure CheckCollisionBetweenPlanets(var data: GameDataType);
	var
		i, j, temp: Integer;
	begin
		//For all planets
		for i := Low(data.Planets) to High(data.Planets) - 1 do
		begin
			if data.Planets[i].Alive then
			begin
				//For all other planets
				for j := i + 1 to High(data.Planets) do
				begin
					//Check collisions between the i'th and j'th planet
					if data.Planets[j].Alive and HaveSpritesCollided(data.Planets[i].Sprite, data.Planets[j].Sprite) then
					begin
						DoPlanetCollision(data.Planets[i], data.Planets[j]);
						
						//while HaveSpritesCollided(data.Planets[i].Sprite, data.Planets[j].Sprite) do
						begin
							MoveSprite(data.Planets[i].Sprite, data.Planets[i].Movement);
							MoveSprite(data.Planets[j].Sprite, data.Planets[j].Movement);
						end;
					end;
				end;

				//For all warp holes
				if ObjectHasHitAWarpHole(data.Planets[i].Sprite, data, temp) then
				begin
					data.Planets[i].Alive := false;
					break;
				end;
			end;
		end;
	end;
	
	procedure UpdateBoost(var data: GameDataType; triggerOn: Boolean);
	begin
		if triggerOn and (data.fuelLevel > 0 )then
		begin
			data.boost += BOOST_AMT;
			data.fuelLevel -= FUEL_BOOST;
			if data.fuelLevel < 0 then data.fuelLevel := 0;
		end
		else if data.boost > 0 then
		begin
			data.boost -= 4 * BOOST_AMT;
			if data.boost < 0 then data.boost := 0;
		end;
	end;
	
	function CalculateMovement(const data: GameDataType; out atEnd: Boolean): Single;
	var
		bgw, bgx: Single;
		speed: Single;
	begin
		bgw := GameImage(Background).width;
		bgx := CalculateBackgroundX(data.windowX) + SCREEN_WIDTH;

		atEnd := bgx >= bgw;

		speed := BASE_SPEED + SPEED_MUL * (bgx / bgw) + SPEED_LEVEL_MUL * data.currentLevel;
		result := SCROLL_PIXELS * speed; // + data.boost * SCROLL_PIXELS);
	end;
	
	procedure DoMovement(var data: GameDataType);
	var
		mv: Single;
		atEnd: Boolean;
	begin
		if data.state = PlayingGameState then
		begin
			mv := CalculateMovement(data, atEnd);
			
			if false = atEnd then
			begin
				data.windowX := data.windowX + mv;
				data.UfoSprite.xPos := data.UfoSprite.xPos + mv;
			end
			else
			begin
				data.state := EndOfLevelState;
			end;
		end;
	end;
	
	procedure UpdateGameState(var data: GameDataType);
	var
		i: Integer;
	begin
		if _Paused then exit;
		
		//Ensure that old animations are updated
		UpdateAnimationedElements(data);
		
		//move the window
		DoMovement(data);
		
		for i := Low(data.Planets) to High(data.Planets) do
		begin
			if data.Planets[i].alive and 
			   HasSpriteCollidedX(data.Planets[i].Sprite, Round(data.windowX) + SCREEN_WIDTH, CollisionRangeLessThan) then
			begin
				MoveSprite(data.Planets[i].Sprite, data.Planets[i].Movement);
			end;
		end;
		
		CheckCollisionBetweenPlanets(data);
		TestPlayerCollision(data);
	end;
	
	procedure UpdatePlayerLocation(player: Sprite; const data: GameDataType);
	var
		minX, minY: Single;
		maxX, maxY: Single;
	begin
		minY := TOP_GAME_AREA;
		minX := data.windowX;
		
		maxY := SCREEN_HEIGHT - CurrentHeight(player);
		maxX := minX + SCREEN_WIDTH - CurrentWidth(player);
				
		//WriteLn(minX:4:2, ' > ', player.xPos:4:2, ' < ', maxX:4:2);
				
		if player.xPos < minX then player.xPos := minX
		else if player.xPos > maxX then player.xPos := maxX;

		if player.yPos < minY then player.yPos := minY
		else if player.yPos > maxY then player.yPos := maxY;
	end;
	
	procedure MovePlayer(var data: GameDataType; mulX, mulY: Integer);
	var
		dx, dy: Single;
	begin			
		dx := PLAYER_BASE_MOVE * 2 * mulX; 		
		dx += PLAYER_BASE_MOVE * data.boost * mulX;
		
		if mulX >= 0 then
		begin
			//boost applied to windowX
			data.windowX += Round(SCROLL_PIXELS * data.boost) * mulX;
		end;
			
		dy := (PLAYER_BASE_MOVE * 2 + PLAYER_BASE_MOVE * 2 * data.boost) * mulY;
		
		data.ufoSprite.xPos += dx;
		data.ufoSprite.yPos += dy;
				
		UpdatePlayerLocation(data.ufoSprite, data);
	end;

	procedure FreeLevel(var data: GameDataType);
	var
		i: Integer;
	begin
		for i := Low(data.MidBacks) to High(data.MidBacks) do
		begin
			FreeSprite(data.MidBacks[i]);
		end;
		SetLength(data.MidBacks, 0);
		
		for i := Low(data.Planets) to High(data.Planets) do
		begin
			FreeSprite(data.Planets[i].Sprite);
		end;
		SetLength(data.Planets, 0);
		
		for i := Low(data.WarpHoles) to High(data.WarpHoles) do
		begin
			FreeSprite(data.WarpHoles[i].Sprite);
		end;
		SetLength(data.WarpHoles, 0);
		
		SetLength(data.levelAnimations, 0);
	end;
	
	procedure ResetLevel(var data: GameDataType);
	var
		i: Integer;
	begin
		for i := 0 to MAX_ANIMATIONS - 1 do
		begin
			ClearAnimation(data.temporaryAnimations[i]);
		end;
		
		data.windowX := 0;
		data.windowY := 0;
		
		data.UfoSprite.xPos := (SCREEN_WIDTH - CurrentWidth(data.UfoSprite)) div 2;
		data.UfoSprite.yPos := (SCREEN_HEIGHT - CurrentHeight(data.UfoSprite)) div 2;
	end;
	
	procedure CleanupGameData(var data: GameDataType);
	begin
		FreeLevel(data);
		FreeSprite(data.UfoSprite);
		FreeBitmap(data.ShieldBmp);
	end;

	function SetupGameData(): GameDataType;
	var
		i: Integer;
	begin		
		result.state := PlayingGameState;
		result.UfoSprite := CreateSprite(GameImage(Ufo));
		result.currentLevel := 1;
		result.boost := 0.0;
		result.ShieldStrength := 1.0;
		result.score := 0;
		result.fuelLevel := MAX_FUEL;
		result.state := PlayingGameState;
		result.ShieldBmp := CreateBitmap(
			CurrentWidth(result.UfoSprite) + 2 * SHIELD_OFFSET, 
			CurrentHeight(result.UfoSprite) + 2 * SHIELD_OFFSET);
				
		SetLength(result.temporaryAnimations, MAX_ANIMATIONS);
		for i := 0 to MAX_ANIMATIONS - 1 do
		begin
			ClearAnimation(result.temporaryAnimations[i]);
		end;
		
		SetLength(result.levelAnimations, 0);
		
		UpdateShieldBmp(result);
		
		ResetLevel(result);
	end;
end.