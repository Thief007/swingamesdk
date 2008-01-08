unit AlienFlightModel;

interface
	uses 
		SysUtils, 
		SGSDK_Core, SGSDK_Graphics, SGSDK_Audio, SGSDK_Physics, SGSDK_Camera,
		GameResources;
	
	type
		
		GameState = (
			PlayingIntroState,
			GameMenuState,
			PlayingGameState,
			WarpingState,
			PlayerDyingState,
			EndOfLevelState,
			EndOfGameState,
			EditorState,
			ExitGameState,
			ShowScoreboardState,
			EnterHighScoreState
		);

		/// What occurs when the player collides
		/// with a collectable object
		CollectAction = (
				NoAction,
				AddFuelAction,
				AddScoreAction,
				AddShieldAction,
				WarpAction,
				DamageAction,
				SuckAndCrushAction
			);
		
		/// What kind is the sprite.
		AlienFlightSpriteKind = (
				SmallAsteroidKind,
				AsteroidKind,
				EarthKind,
				SaturnKind,
				UranusKind,
				PlaceHolderKind_1,
				PlaceHolderKind_2,
				PlaceHolderKind_3,
				PlaceHolderKind_4,
				PlaceHolderKind_5,
				PlaceHolderKind_6,
				PlaceHolderKind_7,
				PlaceHolderKind_8,
				PlaceHolderKind_9,
				PlaceHolderKind_10,
				PlaceHolderKind_11,
				PlaceHolderKind_12,
				PlaceHolderKind_13,
				PlaceHolderKind_14,
				PlaceHolderKind_15,
				PlaceHolderKind_16,
				PlaceHolderKind_17,
				PlaceHolderKind_18,
				PlaceHolderKind_19,
				WarpHoleKind,
				PlaceHolderKind_20,
				PlaceHolderKind_21,
				PlaceHolderKind_22,
				PlaceHolderKind_23,
				PlaceHolderKind_24,	
				StarKind,
				BatteryKind,
				FuelPackKind,
				PlaceHolderKind_25,
				PlaceHolderKind_26,
				PlaceHolderKind_27,
				PlaceHolderKind_28,
				PlaceHolderKind_29,
				PlaceHolderKind_30,
				PlaceHolderKind_31,
				PlaceHolderKind_32,
				PlaceHolderKind_33,
				BlackHoleKind,
				PlaceHolderKind_34,
				PlaceHolderKind_35,
				PlaceHolderKind_36,
				PlaceHolderKind_37,
				PlaceHolderKind_38,
				GalaxyKind1,
				GalaxyKind2,
				GalaxyKind3,
				GalaxyKind4
			);
		
		TemplateValue = record
			name: String;
			value: Single;
			increments: Single;
			min, max: Single;
		end;
		
		/// Template used to store the creation data for
		/// Generating new AlienFlightSprites.
		AlienFlightSpriteTemplate = record
			image: Bitmap;
			isMulti: Boolean;
			numOfCells: Integer; 
			framesPerCell: Integer; 
			endingAction: SpriteEndingAction; 
			width, height: Integer;
			
			mass: Single;
			
			kind: 	AlienFlightSpriteKind;
			
			action:	CollectAction;
			
			values: Array of TemplateValue;
						
			editorImage: Bitmap;
			foreground: Boolean;
			destroyedOnCollision: Boolean;
			used: Boolean;
		end;
		
		AlienFlightSprite = record
			{name: 			String;}
			active:			Boolean;
			kind: 			AlienFlightSpriteKind;

			sprite:	 		Sprite;
			
			{action:			CollectAction;}
			
			/// The data associated with the Sprite.
			/// These values depend on the kind for meaning
			values: 			Array of Single;
		end;
		
		AlienFlightSpritePtr = ^AlienFlightSprite;
		
		/// Large entities in the game that can
		/// collide with each other etc.
		{Obstacle = record
			name: String;
			mass: Single;
			sprite: Sprite;
			movement: Vector;
			alive: Boolean;
		end;}

		/// Special entity that is used to move
		/// between levels
		{WarpHole = record
			sprite: Sprite;
			level: Integer;
		end;
		
		WarpHolePtr = ^WarpHole;
		ObstaclePtr = ^Obstacle;}
	 
{		Animation = record
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
			sprite: Sprite;
			movement: Vector;
			action: AnimationAction;
			value: Single;
		end;}
		
{		AnimationPtr = ^Animation;}	

		HighScore = record
			name: String;
			score: Integer;
		end;

		GameDataType = record
			state: GameState;
			
			currentLevel: Integer;
			
			ufoSprite: Sprite;
			shieldStrength: Single;
			shieldBmp: Bitmap;

			{midBacks: Array of Sprite;}
			
			sprites: Array of AlienFlightSprite;
			
			{Planets: Array of Obstacle;
			WarpHoles: Array of WarpHole;}
			
			animations: Array of Sprite;

			score: Integer;
			boost: Single;
			fuelLevel: Integer;
			
			topLevel: Integer;
			topShield: Single;
			topFuel: Integer;
		end;
				
	const
		SCREEN_WIDTH = 800; SCREEN_HEIGHT = 600;
		BORDER_X = 10; BORDER_Y = 10; HEADING_HEIGHT = 50;
		GAME_AREA_HEIGHT = (SCREEN_HEIGHT - HEADING_HEIGHT - 2 * BORDER_Y);
		TOP_GAME_AREA = HEADING_HEIGHT + BORDER_Y;
		
		SCROLL_PIXELS 	= 2;
		BKG_MULT 		= 5;		//How much slower is background?
		BASE_SPEED 		= 0.5;
		SPEED_MUL		= 0.3;
		SPEED_LEVEL_MUL = 0.05;
		PLAYER_BASE_MOVE = 3;
		
		MAX_FUEL = 150;
		FUEL_BOOST = 2; //Cost of boost per frame
		BOOST_AMT = 0.15;
		
		SHIELD_OFFSET = 15;
		
		FULL_HEALTH = 200;
		DAMAGE_FOR_MASS_1 = 200;
		
		MAX_ANIMATIONS = 255;
		
		EXPLOSION_FRAMES = 3;
		WARP_FRAMES = 3;
		
		STAR_VALUE = 20;
		FUEL_VALUE = 0.5;
		BATTERY_VALUE = 0.5;
		
		BLACK_SUCK_TIME = 90;
		
		//Consts related to location of value in AlienFlightSprite's Value collection
		BATTERY_VALUE_IDX = 0;
		FUELPACK_VALUE_IDX = 0;
		STAR_VALUE_IDX = 0;
		WARP_LEVEL_IDX = 0;
		BLACK_HOLE_SUCK_IDX = 0;
		BLACK_HOLE_PULL_FACTOR_IDX = 1;
				
	var 
		_Paused: Boolean;
		_Scoreboard: Array [0..4] of HighScore;

	procedure MoveGameWindow(var data: GameDataType; dx, dy: Single);	
	procedure UpdateGameState(var data: GameDataType);
	procedure FreeLevel(var data: GameDataType);
	procedure ResetLevel(var data: GameDataType);
	procedure CleanupGameData(var data: GameDataType);
	procedure AddExplosion(var data: GameDataType; x, y: Single; movement: Vector);
	procedure UpdateBoost(var data: GameDataType; triggerOn: Boolean);
	procedure AddWarpAnimation(var data: GameDataType; obj: Sprite; v: Vector; steps: Integer);
	procedure UpdatePlayerLocation(player: Sprite; const data: GameDataType);
	procedure MovePlayer(var data: GameDataType; mulX, mulY: Single);	
			
	function GetScore(idx: Integer): HighScore;
	function IsHighScore(score: Integer): Boolean;
	procedure AddTopScore(score: Integer; name: String);
	procedure SaveScoreboard(const data: GameDataType);
	procedure UpdateTopLevel(var data: GameDataType);	

	function CalculateBackgroundX(foregroundX: Single): Single;
	function CalculateForegroundX(backgroundX: Single): Single;
	function MaxForegroundX(): Single;
	function SetupGameData(): GameDataType;	
	
	function CreateSpriteKind(idx: AlienFlightSpriteKind; worldOffset: Vector): AlienFlightSprite;	
	function SpriteAtPoint(v: Vector; const data: GameDataType; out sprt: AlienFlightSpritePtr): Boolean;
	function CanPlaceAt(const data: GameDataType; kind: AlienFlightSpriteKind; sprite: AlienFlightSpritePtr; x, y: Single): Boolean;
	function GetCollidedWarphole(const sprte: Sprite; const data: GameDataType): AlienFlightSpritePtr;
		
	procedure SetupTemplates();
	procedure AddSpriteKind(var data: GameDataType; idx: AlienFlightSpriteKind; worldOffset: Vector);
	procedure DeleteSprite(var data: GameDataType; sprt: AlienFlightSpritePtr);
		
	procedure UpdateShieldBmp(var data: GameDataType);
		
	var
		_Templates: Array [AlienFlightSpriteKind] of AlienFlightSpriteTemplate;
		
implementation
	uses AlienFlightView;
	
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

	procedure AddObstacleTemplate(kind: AlienFlightSpriteKind;image, editorImg: Bitmap; mass: Single);
	var
		template: AlienFlightSpriteTemplate;
	begin
		template.used := true;
		template.isMulti := false; 
		template.mass := mass;
		template.endingAction := Stop;
		template.framesPerCell := 1;
		template.foreground := true;
		
		template.kind := kind;
		template.image := image;
		template.editorImage := editorImg;
		template.numOfCells := 1;
		template.width := image.width;
		template.height := image.height;
		template.action := DamageAction;
		template.destroyedOnCollision := false;

		SetLength(template.values, 0);

		_Templates[kind] := template;
	end;
	
	procedure AddAllObstacleTemplates();
	begin
		AddObstacleTemplate(SmallAsteroidKind, GameImage(SmallAsteroidImg), GameImage(SmallAsteroidEditImg), 0.1);
		AddObstacleTemplate(AsteroidKind, GameImage(AsteroidImg), GameImage(AsteroidEditImg), 0.4);
		AddObstacleTemplate(EarthKind, GameImage(EarthImg), GameImage(EarthEditImg), 6.0);
		AddObstacleTemplate(SaturnKind, GameImage(SaturnImg), GameImage(SaturnEditImg), 4.5);
		AddObstacleTemplate(UranusKind, GameImage(UranusImg), GameImage(UranusEditImg), 3.0);
	end;

	procedure AddMidbackTemplate(kind: AlienFlightSpriteKind;image, editorImg: Bitmap; mass: Single);
	var
		template: AlienFlightSpriteTemplate;
	begin
		template.used := true;
		template.isMulti := false; 
		template.mass := mass;
		template.endingAction := Stop;
		template.framesPerCell := 1;
		template.foreground := false;
		
		template.kind := kind;
		template.image := image;
		template.editorImage := editorImg;
		template.numOfCells := 1;
		template.width := image.width;
		template.height := image.height;
		template.action := NoAction;
		template.destroyedOnCollision := false;

		SetLength(template.values, 0);

		_Templates[kind] := template;
	end;

	procedure AddAllGalaxyTemplates();
	begin
		AddMidbackTemplate(GalaxyKind1, GameImage(MidBack_Galaxy1), GameImage(MidBack_Galaxy1_Edit), 0.1);
		AddMidbackTemplate(GalaxyKind2, GameImage(MidBack_Galaxy2), GameImage(MidBack_Galaxy2_Edit), 0.1);
		AddMidbackTemplate(GalaxyKind3, GameImage(MidBack_Galaxy3), GameImage(MidBack_Galaxy3_Edit), 0.1);
		AddMidbackTemplate(GalaxyKind4, GameImage(MidBack_Galaxy4), GameImage(MidBack_Galaxy4_Edit), 0.1);
	end;
	
	procedure AddWarpHoleTemplate();
	var
		template: AlienFlightSpriteTemplate;
	begin
		//Base
		template.used := true;
		template.isMulti := false; 
		template.mass := 50.0;
		template.endingAction := Stop;
		template.framesPerCell := 1;
		template.foreground := true;
		template.destroyedOnCollision := false;
		
		template.kind := WarpHoleKind;
		template.image := GameImage(WarpHoleImg);
		template.editorImage := GameImage(WarpHoleEditImg);
		template.numOfCells := 1;
		template.width := 52;
		template.height := 82;
		template.action := WarpAction;
		
		SetLength(template.values, 1);
		template.values[WARP_LEVEL_IDX].value := 1.0;
		template.values[WARP_LEVEL_IDX].increments := 1.0 ;
		template.values[WARP_LEVEL_IDX].name := 'Warp to level:';
		template.values[WARP_LEVEL_IDX].min := 1.0;
		template.values[WARP_LEVEL_IDX].max := 100.0;

		_Templates[WarpHoleKind] := template;
	end;
				
	procedure AddCollectableTemplates();
	var
		template: AlienFlightSpriteTemplate;
	begin
		//Base
		template.used := true;
		template.isMulti := true; 
		template.mass := 0.05;
		template.endingAction := ReverseLoop;
		template.framesPerCell := 10;
		template.foreground := true;
		template.destroyedOnCollision := true;
				
		//Battery
		template.kind := BatteryKind;
		template.image := GameImage(BatteryImg);
		template.editorImage := GameImage(BatteryEditImg);
		template.numOfCells := 8;
		template.width := 15;
		template.height := 23;
		template.action := AddShieldAction;
		
		SetLength(template.values, 1);
		template.values[0].value := BATTERY_VALUE;
		template.values[0].increments := 0.1 ;
		template.values[0].name := 'Recharge %:';
		template.values[0].min := 0.0;
		template.values[0].max := 1.0;

		_Templates[BatteryKind] := template;

		//Fuel
		template.kind := FuelPackKind;
		template.image := GameImage(FuelPackImg);
		template.editorImage := GameImage(FuelPackEditImg);
		template.numOfCells := 9;
		template.width := 19;
		template.height := 29;
		template.action := AddFuelAction;

		SetLength(template.values, 1);
		template.values[0].value := FUEL_VALUE;
		template.values[0].increments := 0.1 ;
		template.values[0].name := 'Refill %:';
		template.values[0].min := 0.0;
		template.values[0].max := 1.0;
						
		_Templates[FuelPackKind] := template;
				
		//Stars
		template.kind := StarKind;
		template.image := GameImage(StarImg);
		template.editorImage := GameImage(StarEditImg);
		template.numOfCells := 10;
		template.width := 16;
		template.height := 16;
		template.action := AddScoreAction;

		SetLength(template.values, 1);
		template.values[0].value := STAR_VALUE;
		template.values[0].increments := 10.0 ;
		template.values[0].name := 'Score +:';
		template.values[0].min := 0.0;
		template.values[0].max := 1000.0;
		
		//override default for star, they cant be destroyed
		template.destroyedOnCollision := false;

		_Templates[StarKind] := template;
	end;

	procedure AddBlackHoleTemplate();
	var
		template: AlienFlightSpriteTemplate;
	begin
		//Base
		template.used := true;
		template.isMulti := true; 
		template.mass := 1000.0;
		template.endingAction := Loop;
		template.framesPerCell := 10;
		template.foreground := true;
		template.destroyedOnCollision := false;
		
		template.kind := BlackHoleKind;
		template.image := GameImage(BlackHoleImg);
		template.editorImage := GameImage(BlackHoleEditImg);
		template.numOfCells := 3;
		template.width := 270;
		template.height := 270;
		template.action := SuckAndCrushAction;
		
		SetLength(template.values, 2);
		template.values[BLACK_HOLE_SUCK_IDX].value := 300.0;
		template.values[BLACK_HOLE_SUCK_IDX].increments := 10.0 ;
		template.values[BLACK_HOLE_SUCK_IDX].name := 'Attract radius:';
		template.values[BLACK_HOLE_SUCK_IDX].min := 200.0;
		template.values[BLACK_HOLE_SUCK_IDX].max := 1000.0;

		template.values[BLACK_HOLE_PULL_FACTOR_IDX].value := 0.50;
		template.values[BLACK_HOLE_PULL_FACTOR_IDX].increments := 0.1;
		template.values[BLACK_HOLE_PULL_FACTOR_IDX].name := 'Pull power:';
		template.values[BLACK_HOLE_PULL_FACTOR_IDX].min := 0.1;
		template.values[BLACK_HOLE_PULL_FACTOR_IDX].max := 1.0;

		_Templates[BlackHoleKind] := template;
	end;

	procedure FillWithPlaceholders();
	var
		kind: AlienFlightSpriteKind;
		template: AlienFlightSpriteTemplate;
	begin
		template.used := false;
		template.isMulti := false; 
		template.mass := 0.0;
		template.endingAction := Stop;
		template.framesPerCell := 1;
		template.foreground := true;
		template.image := GameImage(PlaceHolderImg);
		template.editorImage := GameImage(PlaceHolderImg);
		template.numOfCells := 1;
		template.width := 32;
		template.height := 32;
		template.action := NoAction;
		template.destroyedOnCollision := true;	
		
		for kind := Low(AlienFlightSpriteKind) to High(AlienFlightSpriteKind) do
		begin
			template.kind := kind;
			_Templates[template.kind] := template;
		end;
	end;

	procedure SetupTemplates();
	begin
		//Fill all slots with empty placeholders
		FillWithPlaceholders();
		
		//Override with real value
		AddAllObstacleTemplates();
		AddCollectableTemplates();
		AddWarpHoleTemplate();
		AddBlackHoleTemplate();
		AddAllGalaxyTemplates();
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

	function ToBackground(v: Vector): Vector;
	begin
		result := v;
		
		result.x :=	CalculateBackgroundX(v.x);
	end;

	function CreateSpriteKind(idx: AlienFlightSpriteKind; worldOffset: Vector): AlienFlightSprite;
	var
		i: Integer;
		sprt: AlienFlightSprite;
		template: AlienFlightSpriteTemplate;
		fpCell: Array of Integer; 
	begin
		if _Templates[idx].used = false then raise Exception.Create('Attempting to created unused template');
		if (idx < Low(AlienFlightSpriteKind)) or (idx > High(AlienFlightSpriteKind)) then
		begin
			raise Exception.Create('Error Creating sprite. idx is out of range');
		end;
		
		//WriteLn('CreateSpriteKind - ', Integer(idx));	
		template := _Templates[idx];

		//WriteLn(' - Setting Length');
		
		//Get frames per cell as an array
		SetLength(fpCell, template.numOfCells);
		for i := 0 to High(fpCell) do
		begin
			fpCell[i] := template.framesPerCell;
		end;
		
		//WriteLn(' - Length Set and Initialised');	
		
		
		//Create the sprite
		sprt.sprite := CreateSprite( 
			template.image, template.isMulti, fpCell, template.endingAction, template.width, template.height
			);
		sprt.sprite.mass := template.mass;
		sprt.sprite.movement := CreateVector(0, 0);
		
		//WriteLn(' - Sprite setup');	
		
		//Create the AlienFlight sprite properties
		sprt.active := true;
		sprt.kind   := template.kind;
		{sprt.action := template.action;}
		
		//WriteLn(' - Attributes set');	
		
		
		SetLength(sprt.values, Length(template.values));
		for i := Low(template.values) to High(template.values) do
		begin
			sprt.values[i] := template.values[i].value;
		end;
		
		//WriteLn(' - Values copied');	
		
		//Move into place
		//if template.foreground then
			MoveSprite(sprt.sprite, worldOffset);
		//else
		//	MoveSprite(sprt.sprite, ToBackground(worldOffset));
			
		result := sprt;
		
		//WriteLn(' - Moved to position');	
	end;
	
	procedure AddSpriteKind(var data: GameDataType; idx: AlienFlightSpriteKind; worldOffset: Vector);
	begin
		//Adjust to center point
		worldOffset.x -= _Templates[idx].width / 2;
		worldOffset.y -= _Templates[idx].height / 2;

		//WriteLn('Add Sprite Kind');
		SetLength(data.sprites, Length(data.sprites) + 1);
		//WriteLn(' - length set');
		
		data.sprites[High(data.sprites)] := CreateSpriteKind(idx, worldOffset);
		
		if idx = WarpHoleKind then //Default warp holes to next level
			data.sprites[High(data.sprites)].values[0] := data.currentLevel + 1;
		
	end;
	
	function KindsMayCollide(kind1, kind2: AlienFlightSpriteKind): Boolean;
	begin
		if (kind1 = StarKind) or (kind2 = StarKind) then result := true
		else if (kind1 >= GalaxyKind1) and (kind1 <= GalaxyKind4) then result := (kind2 < GalaxyKind1) or (kind2 > GalaxyKind4)
		else if (kind2 >= GalaxyKind1) and (kind2 <= GalaxyKind4) then result := (kind1 < GalaxyKind1) or (kind1 > GalaxyKind4)
		else if (kind1 = BlackHoleKind) or (kind2 = BlackHoleKind) then result := true
		else result := false;
	end;
	
	function CanPlaceAt(const data: GameDataType; kind: AlienFlightSpriteKind; sprite: AlienFlightSpritePtr; x, y: Single): Boolean;
	var
		i: Integer;
		template: AlienFlightSpriteTemplate;
	begin
		template := _Templates[kind];
		
		for i := Low(data.sprites) to High(data.sprites) do
		begin
			//WriteLn(data.sprites[i].sprite.xPos, ',', data.sprites[i].sprite.yPos, '  ', data.sprites[i].sprite.width, 'x', data.sprites[i].sprite.height);
			//WriteLn(x, ',', y, '  ', template.width, 'x', template.height);
			
			if (not KindsMayCollide(data.sprites[i].kind, kind)) and 
				(@data.sprites[i] <> sprite) and
				HasSpriteCollidedWithRect(data.sprites[i].sprite, x, y, template.width, template.height) then
			begin
				result := false;
				exit;
			end;
		end;	
		result := true;	
	end;
	
	procedure DeleteSprite(var data: GameDataType; sprt: AlienFlightSpritePtr);
	var
		i: Integer;
		found: boolean;
		other: AlienFlightSpritePtr;
	begin
		found := false;
		
		for i := Low(data.sprites) to High(data.sprites) do
		begin
			
			if found then 
				data.sprites[i - 1] := data.sprites[i]
			else
			begin
				other := @data.sprites[i];
				//WriteLn(i, '  ', HexStr(other), '=', HexStr(sprt), '   ', other = sprt);
				if other = sprt then
				begin
					//WriteLn('found');
					found := true;
					FreeSprite(data.sprites[i].sprite);
				end;
			end;
		end;
		
		if found then 
			SetLength(data.sprites, Length(data.sprites) - 1)
		else
			raise Exception.Create('No match found from selected sprite');
	end;

	function GetFreeAnimationIdx(var data: GameDataType): Integer;
	var
		i, use, frameMax: Integer;
	begin
		frameMax := 0;
		
		for i := 0 to MAX_ANIMATIONS do
		begin
			if nil = data.animations[i] then
			begin
				use := i;
				break;
			end
			else if data.animations[i].currentFrame > frameMax then
			begin
				//Use oldest animation if none are free
				frameMax := data.animations[i].currentFrame;
				use := i;
			end;
		end;
		
		if nil <> data.animations[use] then
		begin
			FreeSprite(data.animations[i]);
		end;
		
		result := use;
	end;
	
	procedure AddAnimation(var data: GameDataType; bmp: Bitmap; 
		xPos, yPos: Single; movementVec: Vector;
		srcWidth, srcHeight, cells, numFramesPerCell: Integer);
	var
		use, i: Integer;
		fpCell: Array of Integer; 
	begin
		use := GetFreeAnimationIdx(data);
		
		SetLength(fpCell, cells);
		for i := 0 to cells - 1 do
		begin
			fpCell[i] := numFramesPerCell;
		end;
		
		data.animations[use] := CreateSprite(bmp, true, fpCell, Stop, srcWidth, srcHeight);

		data.animations[use]^.xPos := xPos; 
		data.animations[use]^.yPos := yPos;
		
		data.animations[use]^.movement := movementVec;
		data.animations[use]^.mass := 1.0;
	end;
	
	procedure AddExplosion(var data: GameDataType; x, y: Single; movement: Vector);
	begin
		AddAnimation(data, GameImage(ExplosionImg), x, y, movement, 
			38, 38, 16, EXPLOSION_FRAMES);
		
		PlaySoundEffect(GameSound(DestroySound));
	end;

	function GetCollidedWarphole(const sprte: Sprite; const data: GameDataType): AlienFlightSpritePtr;
	var
		i: Integer;
	begin
		result := nil;
		for i := Low(data.sprites) to High(data.sprites) do
		begin
			if (data.sprites[i].kind = WarpHoleKind) and HaveSpritesCollided(data.ufoSprite, data.sprites[i].sprite) then
			begin
				result := @data.sprites[i];
				//WriteLn('Found wh: ', HexStr(data.sprites[i].sprite));
				exit;
			end;
		end;
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
					
		AddAnimation(data, GameImage(WarpImg), x, y, v, 90, 90, 5, WARP_FRAMES);
	end;
	
	procedure UpdateAnimation(var anim: Sprite; clear: Boolean);
	begin
		if anim <> nil then
		begin
			//Move and animate
			UpdateSprite(anim);

			if anim.hasEnded and clear then
			begin
				FreeSprite(anim);
			end;
		end;
	end;
	
	procedure UpdateAnimations(var data: GameDataType);
	var
		i: Integer;
	begin
		for i := 0 to MAX_ANIMATIONS - 1 do
		begin
			UpdateAnimation(data.animations[i], true);
		end;
	end;

	procedure DestroySprite(var data: GameDataType; var obj: AlienFlightSprite; withMovement: Boolean); overload;
	begin
		if obj.active then
		begin
			obj.active := false;		

			if withMovement then
				AddExplosion(data, obj.sprite.xPos, obj.sprite.yPos, obj.sprite.movement)
			else
				AddExplosion(data, obj.sprite.xPos, obj.sprite.yPos, CreateVector(0,0));
		end;
	end;
	
	procedure DestroySprite(var data: GameDataType; var obj: AlienFlightSprite); overload;
	begin
		DestroySprite(data, obj, true);
	end;

	function ApplyGravity(var data: GameDataType; sprt: Sprite; blkHole: AlienFlightSpritePtr; applyToMovement:Boolean): Boolean;
	var
		mt: Single;
		temp, move : Vector;
	begin
		temp := CalculateVectorFromTo(sprt, blkHole.sprite);
		mt := Magnitude(temp);
		result := false;
		
		if (mt > 0) and (mt <= 25.0) then
		begin
			result := true;
		end
		else if (mt > 25.0) and (mt < blkHole^.values[BLACK_HOLE_SUCK_IDX]) then
		begin
			if applyToMovement then
			begin
				//WriteLn((blkHole.sprite.mass) / (mt * mt));
				move := MultiplyVector(GetUnitVector(temp), ((blkHole.sprite.mass) / (mt * mt)) * 5);
{					(blkHole^.values[BLACK_HOLE_SUCK_IDX] - (blkHole^.values[BLACK_HOLE_SUCK_IDX] - mt)) 
					* blkHole^.values[BLACK_HOLE_PULL_FACTOR_IDX] * 1/(BLACK_SUCK_TIME * 10));
}				//WriteLn(' -> ', Magnitude(move));
				sprt.movement := AddVectors(sprt.movement, move);	
			end
			else
			begin
				move := MultiplyVector(GetUnitVector(temp), blkHole^.values[BLACK_HOLE_SUCK_IDX] * blkHole^.values[BLACK_HOLE_PULL_FACTOR_IDX] * 1/BLACK_SUCK_TIME);
		
				if (sprt = data.ufoSprite) and (move.x <= 0) then move.x -= SCROLL_PIXELS; //reverse automatic movement
		
				//WriteLn('moving - ', Magnitude(move):4:2);
				MoveSprite(sprt, move);
			end;
		end;
	end;

	procedure ApplyGravityToSprite(var data: GameDataType; blkHole, sprt: AlienFlightSpritePtr);
	begin
		if ApplyGravity(data, sprt^.sprite, blkHole, true) then
		begin
			DestroySprite(data, sprt^, false);
		end;
	end;
	
	procedure ApplyGravityToPlayer(var data: GameDataType; blkHole: AlienFlightSpritePtr);
	begin
		if ApplyGravity(data, data.ufoSprite, blkHole, false) then
		begin
			data.shieldStrength := 0;
			data.state := PlayerDyingState;
			UpdateShieldBmp(data);
		end;
	end;

	procedure UpdateBlackHole(var data: GameDataType; blkHole: AlienFlightSpritePtr);
	var
		i : Integer;
	begin
		for i := Low(data.sprites) to High(data.sprites) do
		begin
			if data.sprites[i].active and (@data.sprites[i] <> blkHole) and (data.sprites[i].kind <> BlackHoleKind) then
			begin
				ApplyGravitytoSprite(data, blkHole, @data.sprites[i]);
			end;
		end;
		
		for i := 0 to MAX_ANIMATIONS do
		begin
			if data.animations[i] <> nil then
			begin
				ApplyGravity(data, data.animations[i], blkHole, true);
			end;
		end;
		
		ApplyGravityToPlayer(data, blkHole);
	end;

	procedure UpdateSprites(var data: GameDataType);
	var
		i: Integer;
	begin
		for i := 0 to High(data.sprites) do
		begin
			if data.sprites[i].active and HasSpriteCollidedX(data.sprites[i].sprite, GameX(SCREEN_WIDTH), CollisionRangeLessThan) then
			begin
				UpdateSprite(data.sprites[i].sprite);
				
				if data.sprites[i].kind = BlackHoleKind then
				begin
					UpdateBlackHole(data, @data.sprites[i]);
				end;
			end
			else if data.sprites[i].active then
			begin
				UpdateSpriteAnimation(data.sprites[i].sprite);
			end;
		end;
	end;
	
	/// Called by the Level Editor to position game window.
	procedure MoveGameWindow(var data: GameDataType; dx, dy: Single);
	begin
		MoveVisualArea(dx, dy);
	end;
	
	function SpriteAtPoint(v: Vector; const data: GameDataType; out sprt: AlienFlightSpritePtr): Boolean;
	var
		i: Integer;
		fgX, fgY: Single;
	begin		
		for i := High(data.sprites) downto Low(data.sprites) do
		begin
			if _Templates[data.sprites[i].kind].foreground then
			begin
				if IsSpriteOnScreenAt(data.sprites[i].sprite, v) then
				begin
					result := true;
					sprt := @data.sprites[i];
					exit;
				end;
			end
			else
			begin			
				fgX := data.sprites[i].sprite.xPos - CalculateBackgroundX(XOffset());
				fgY := data.sprites[i].sprite.yPos - YOffset();
				
				//WriteLn(fgX:4:2, ',', fgY:4:2, ' - ', v.x:4:2, ',', v.y:4:2);
				if VectorWithinRect(v, fgX, fgY, data.sprites[i].sprite.width, data.sprites[i].sprite.height) then
				begin
					result := true;
					sprt := @data.sprites[i];
					exit;
				end;
			end;
		end;	
		result := false;
	end;
		
	procedure DoPlanetCollision(var p1, p2: AlienFlightSprite);
	//var
		//colNormalAngle, a1, a2, optP: Single;
		//n: Vector;
	begin
		VectorCollision(p1.sprite, p2.sprite);
{		colNormalAngle := CalculateAngle(p1.Sprite, p2.Sprite);
		
		//COLLISION RESPONSE
		// n = vector connecting the centers of the balls.
		// we are finding the components of the normalised vector n
		n := CreateVector(Cos(colNormalAngle), Sin(colNormalAngle));
		
		// now find the length of the components of each movement vectors
		// along n, by using dot product.
		a1 := DotProduct(p1.sprite.Movement, n);
		//Local a1# = c.dx*nX  +  c.dy*nY
		a2 := DotProduct(p2.sprite.Movement, n);
		//Local a2# = c2.dx*nX +  c2.dy*nY
		
		// optimisedP = 2(a1 - a2)
		//             ----------
		//              m1 + m2
		optP := (2.0 * (a1-a2)) / (p1.sprite.mass + p2.sprite.mass);
		
		// now find out the resultant vectors
		// Local r1% = c1.v - optimisedP * mass2 * n
		p1.sprite.movement.x := p1.sprite.movement.x - (optP * p2.sprite.mass * n.x);
		p1.sprite.movement.y := p1.sprite.movement.y - (optP * p2.sprite.mass * n.y);
		
		// Local r2% = c2.v - optimisedP * mass1 * n
		p2.sprite.movement.x := p2.sprite.movement.x + (optP * p1.sprite.mass * n.x);
		p2.sprite.movement.y := p2.sprite.movement.y + (optP * p1.sprite.mass * n.y);}
	end;
	
	procedure DoPlayerCollision(var data: GameDataType; var obj: AlienFlightSprite);
	var
		damage: Single;
	begin		
		damage := (DAMAGE_FOR_MASS_1 * obj.sprite.mass) / FULL_HEALTH;
		data.shieldStrength := data.shieldStrength - damage;
			
		DestroySprite(data, obj);
		
		if data.shieldStrength < 0 then
		begin
			data.shieldStrength := 0;
			data.state := PlayerDyingState;
		end;

		UpdateShieldBmp(data);
	end;

	procedure CollectBattery(var data: GameDataType; var battery: AlienFlightSprite);
	begin
		if battery.active then
		begin
			battery.active := false;
			PlaySoundEffect(GameSound(CollectFuelSound));
			
			data.shieldStrength += battery.values[BATTERY_VALUE_IDX];
			if data.shieldStrength > 1.0 then data.shieldStrength := 1.0;
			
			UpdateShieldBmp(data);
		end;
	end;

	procedure CollectFuel(var data: GameDataType; var fuelPack: AlienFlightSprite);
	begin
		if fuelPack.active then
		begin
			fuelPack.active := false;
			PlaySoundEffect(GameSound(CollectFuelSound));
			
			data.fuelLevel += Round(fuelPack.values[FUELPACK_VALUE_IDX] * MAX_FUEL);			
			if data.fuelLevel > MAX_FUEL then data.fuelLevel := Round(MAX_FUEL);
		end;
	end;
	
	procedure CollectStar(var data: GameDataType; var star: AlienFlightSprite);
	begin
		//It is active...
		star.active := false;
		PlaySoundEffect(GameSound(CollectStarSound));
		data.score += Round(star.values[STAR_VALUE_IDX]);
	end;
		
	procedure SetupWarp(var data: GameDataType; const warp: AlienFlightSprite);
	begin
		data.currentLevel := Round(warp.values[WARP_LEVEL_IDX]);
		data.state := WarpingState;		
	end;
		
	procedure TestCollisionWithSprite(var data: GameDataType; var sprt: AlienFlightSprite);
	begin
		if HaveSpritesCollided(data.ufoSprite, sprt.sprite) then
		begin
			case _Templates[sprt.kind].action of
				AddScoreAction:	CollectStar(data, sprt);
				AddShieldAction:	CollectBattery(data, sprt);
				AddFuelAction:		CollectFuel(data, sprt);
				WarpAction:			SetupWarp(data, sprt);
				DamageAction:		DoPlayerCollision(data, sprt);
			end;
		end;
	end;
	
	procedure TestPlayerCollision(var data: GameDataType);
	var
		i: Integer;
	begin
		{for i := Low(data.Planets) to High(data.Planets) do
		begin
			if data.Planets[i].Alive and 
				HaveSpritesCollided(data.UfoSprite, data.Planets[i].Sprite) then
			begin
				DoPlayerCollision(data, data.Planets[i]);
			end;
		end;}
		
		for i := Low(data.sprites) to High(data.sprites) do
		begin
			if data.sprites[i].active then TestCollisionWithSprite(data, data.sprites[i]);
		end;
		
{		if ObjectHasHitAWarpHole(data.ufoSprite, data, level) then
		begin
			data.currentLevel := level;
			data.state := WarpingState;
			//WriteLn('Warping to level ', level);
		end;	}
	end;
	
	procedure CheckCollisionBetweenSprites(var data: GameDataType);
	var
		i, j, k: Integer;
	begin
		//For all sprites
		for i := Low(data.sprites) to High(data.sprites) - 1 do
		begin
			if data.sprites[i].active and HasSpriteCollidedX(data.sprites[i].sprite, GameX(SCREEN_WIDTH), CollisionRangeLessThan) and
				HasSpriteCollidedX(data.sprites[i].sprite, GameX(0), CollisionRangeGreaterThan) then
			begin
				//For all other sprites
				for j := i + 1 to High(data.sprites) do
				begin
					//Check collisions between the i'th and j'th planet
					if data.sprites[j].active and
						(not KindsMayCollide(data.sprites[i].kind, data.sprites[j].kind)) and
						HaveSpritesCollided(data.sprites[i].sprite, data.sprites[j].sprite) then
					begin
						DoPlanetCollision(data.sprites[i], data.sprites[j]);
						
						if _Templates[data.sprites[j].kind].destroyedOnCollision then
							DestroySprite(data, data.sprites[j]);
						if _Templates[data.sprites[i].kind].destroyedOnCollision then
						begin
							DestroySprite(data, data.sprites[i]);
							break; //cant collide any more...
						end;
						
						if not (data.sprites[i].active and data.sprites[j].active) then exit;
						
						//if HaveSpritesCollided(data.sprites[i].Sprite, data.sprites[j].Sprite) then
						begin
							MoveSprite(data.sprites[i].Sprite);
							MoveSprite(data.sprites[j].Sprite);
						end;
						
						{if HaveSpritesCollided(data.sprites[i].Sprite, data.sprites[j].Sprite) then
						begin
							
							//WriteLn('Collision...');
							if Magnitude(data.sprites[i].sprite.movement) > Magnitude(data.sprites[j].sprite.movement) then
								MoveSprite(data.sprites[i].Sprite)
							else
								MoveSprite(data.sprites[j].Sprite);
							
							for k := 0 to 15 do
								begin
									DrawScreen(data, true, false);
									OutlineSprite(data.sprites[i], ColorGreen);
									OutlineSprite(data.sprites[j], ColorBlue);
									RefreshScreen();
								end;
						end;}
					end;
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
			data.boost -= 10 * BOOST_AMT;
			if data.boost < 0 then data.boost := 0;
		end;
	end;
	
	function CalculateMovement(const data: GameDataType; out atEnd: Boolean): Single;
	var
		bgw, bgx: Single;
		speed: Single;
	begin
		bgw := GameImage(Background).width;
		bgx := CalculateBackgroundX(XOffset) + SCREEN_WIDTH;

		atEnd := bgx >= bgw;

		speed := BASE_SPEED + SPEED_MUL * (bgx / bgw) + SPEED_LEVEL_MUL * data.currentLevel;
		result := SCROLL_PIXELS * speed;
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
				MoveVisualArea(mv, 0);
				data.UfoSprite.xPos := data.UfoSprite.xPos + mv;
			end
			else
			begin
				data.state := EndOfLevelState;
			end;
		end;
	end;
	
	procedure UpdateGameState(var data: GameDataType);
	begin
		if _Paused then exit;
		
		//Ensure that old animations are updated
		UpdateAnimations(data);
		UpdateSprites(data);
		
		//move the window
		DoMovement(data);
		
		CheckCollisionBetweenSprites(data);
		TestPlayerCollision(data);
	end;
	
	procedure UpdatePlayerLocation(player: Sprite; const data: GameDataType);
	var
		minX, minY: Single;
		maxX, maxY: Single;
	begin
		minY := TOP_GAME_AREA;
		minX := XOffset();
		
		maxY := SCREEN_HEIGHT - CurrentHeight(player);
		maxX := minX + SCREEN_WIDTH - CurrentWidth(player);
				
		//WriteLn(minX:4:2, ' > ', player.xPos:4:2, ' < ', maxX:4:2);
				
		if player.xPos < minX then player.xPos := minX
		else if player.xPos > maxX then player.xPos := maxX;

		if player.yPos < minY then player.yPos := minY
		else if player.yPos > maxY then player.yPos := maxY;
	end;
	
	procedure MovePlayer(var data: GameDataType; mulX, mulY: Single);
	var
		dx, dy, boost: Single;
	begin			
		boost := PLAYER_BASE_MOVE * data.boost;
		
		if (mulX = 0) and (mulY = 0) and (boost > 0) then
		begin
			mulX := 1;
		end;
		
		if mulX > 0 then
		begin
			//boost applied to windowX
			MoveVisualArea(boost * mulX, 0);
		end;

		dx := (PLAYER_BASE_MOVE + boost) * mulX;			
		dy := (PLAYER_BASE_MOVE + boost) * mulY;
		
		data.ufoSprite.xPos += dx;
		data.ufoSprite.yPos += dy;
				
		UpdatePlayerLocation(data.ufoSprite, data);
	end;

	procedure FreeLevel(var data: GameDataType);
	var
		i: Integer;
	begin
{		for i := Low(data.MidBacks) to High(data.MidBacks) do
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
}		
		for i := Low(data.sprites) to High(data.sprites) do
		begin
			FreeSprite(data.sprites[i].sprite);
		end;
		SetLength(data.sprites, 0);
	end;
	
	procedure ResetLevel(var data: GameDataType);
	var
		i: Integer;
	begin
		for i := 0 to MAX_ANIMATIONS - 1 do
		begin
			FreeSprite(data.animations[i]);
		end;
		
		SetScreenOffset(0, 0);

		data.UfoSprite.xPos := (SCREEN_WIDTH - CurrentWidth(data.UfoSprite)) div 2;
		data.UfoSprite.yPos := (SCREEN_HEIGHT - CurrentHeight(data.UfoSprite)) div 2;
	end;
	
	procedure CleanupGameData(var data: GameDataType);
	begin
		FreeLevel(data);
		FreeSprite(data.UfoSprite);
		FreeBitmap(data.ShieldBmp);
	end;

	function GetScore(idx: Integer): HighScore;
	begin
		result := _Scoreboard[idx];
	end;

	function IsHighScore(score: Integer): Boolean;
	begin
		result := _Scoreboard[4].score < score;
	end;

	procedure AddTopScore(score: Integer; name: String);
	var
		i: Integer;
	begin
		if false = IsHighScore(score) then exit;

		for i := High(_Scoreboard) - 1 downto Low(_Scoreboard)do
		begin
			if _Scoreboard[i].score >= score then
			begin
				_Scoreboard[i + 1].score := score;				
				_Scoreboard[i + 1].name := name;
				exit;
			end
			else
			begin
				_Scoreboard[i + 1] := _Scoreboard[i];
			end;
		end;
	end;

	procedure SaveScoreboard(const data: GameDataType);
	var
		scoreFile: Text;
		i : Integer;
	begin
		Assign(scoreFile, GetPathToResource('Scoreboard.dat'));
		Rewrite(scoreFile);
		
		WriteLn(scoreFile, data.topLevel, ' ', data.topShield:4:5, ' ', data.topFuel);
		
		for i := 0 to 4 do
		begin
			WriteLn(scoreFile, _Scoreboard[i].score, ' ', _Scoreboard[i].name);
		end;
		Close(scoreFile);
	end;

	procedure LoadScoreboard(var data: GameDataType);
	var
		scoreFile: Text;
		filename: String;
		i: Integer;
	begin
		filename := GetPathToResource('Scoreboard.dat');
		
		if false = FileExists(filename) then
		begin
			data.topLevel := 1;
			data.topFuel := MAX_FUEL;
			data.topShield := 1.0;

			_Scoreboard[0].name := 'Ace   '; _Scoreboard[0].score := 100000;
			_Scoreboard[1].name := 'Bossy '; _Scoreboard[1].score := 75000;
			_Scoreboard[2].name := 'Bexley'; _Scoreboard[2].score := 50000;
			_Scoreboard[3].name := 'Holly '; _Scoreboard[3].score := 20000;
			_Scoreboard[4].name := 'Fun   '; _Scoreboard[4].score := 10000;
			
			exit;
		end;
		
		Assign(scoreFile, filename);
		Reset(scoreFile);
		
		ReadLn(scoreFile, data.topLevel, data.topShield, data.topFuel);
		
		for i := 0 to 4 do
		begin
			ReadLn(scoreFile, _Scoreboard[i].score, _Scoreboard[i].name);
			_Scoreboard[i].name := Trim(_Scoreboard[i].name);
		end;
		Close(scoreFile);
	end;
	
	procedure UpdateTopLevel(var data: GameDataType);
	begin
		if (data.currentLevel < 80) and (data.currentLevel > data.topLevel) then
		begin
			data.topLevel := data.currentLevel;
			data.topShield := data.shieldStrength;
			data.topFuel := data.fuelLevel;
		end
		else if data.currentLevel = data.topLevel then
		begin
			if data.shieldStrength > data.topshield then
			begin
				data.topShield := data.shieldStrength;
				data.topFuel := data.fuelLevel;
			end;
		end;
	end;

	function SetupGameData(): GameDataType;
	var
		i: Integer;
	begin		
		result.state := PlayingGameState;
		result.ufoSprite := CreateSprite(GameImage(Ufo));
		result.currentLevel := 1;
		result.boost := 0.0;
		result.shieldStrength := 1.0;
		result.score := 0;
		result.fuelLevel := MAX_FUEL;
		result.state := PlayingGameState;
		result.shieldBmp := CreateBitmap(
			CurrentWidth(result.ufoSprite) + 2 * SHIELD_OFFSET, 
			CurrentHeight(result.ufoSprite) + 2 * SHIELD_OFFSET);
		
		SetLength(result.animations, MAX_ANIMATIONS);
		for i := 0 to MAX_ANIMATIONS - 1 do
		begin
			result.animations[i] := nil;
		end;
		
		LoadScoreboard(result);
		
		SetLength(result.sprites, 0);
		
		UpdateShieldBmp(result);
		
		ResetLevel(result);
	end;
end.