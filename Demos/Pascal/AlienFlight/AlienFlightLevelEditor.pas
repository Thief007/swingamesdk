unit AlienFlightLevelEditor;

interface
	uses 
		SysUtils,
		SGSDK_Core, SGSDK_Input, SGSDK_KeyCodes, SGSDK_Graphics, SGSDK_Font, SGSDK_Physics,
		GameResources, 
		AlienFlightModel;

	procedure LoadLevel(level: Integer; var data: GameDataType);	
	procedure ExecuteEditor(var data: GameDataType);	
	function EditorWasActivated(): Boolean;

implementation
	uses AlienFlightView;
	
	const
		EDITOR_STATUS_TOP = 230; EDITOR_STATUS_LEFT = 5;
		ELEM_PICT_X = 15; ELEM_PICT_Y = 183;
		ELEM_PICT_W = 142; ELEM_PICT_H = 100;
		
		BOX_X = 15;
		BOX_W = 142; BOX_H = 24;
	
		LEVEL_Y = 51;
		ELEM_NAME_Y = 147;
		MASS_Y = 331;
		
		MASS_ADJ = 0.05;

	type GameDataTypePtr = ^GameDataType;
		
	type EditorMode = (
			AddingNewObstacles,
			EditingObstacle,
			AddingMidBack,
			MovingMidBack,
			AddingWarpHoles,
			EditingWarpHole,
			AddingStar,
			AddingFuelPack,
			AddingBattery
		);
	
	type EditorStatusType = record
			currentObstacle: ObstaclePtr;
			currentWarpHole: WarpHolePtr;
			currentSprite: Sprite;
			mode: EditorMode;
			dataPtr: GameDataTypePtr;
		end;
	
	procedure DrawTextIn(toDraw: String; color: Color; txtFont: Font; x, y, w, h: Integer);
	var
		txtH, txtW: Integer;
	begin
		txtH := TextHeight(toDraw, txtFont);
		txtW := TextWidth(toDraw, txtFont);
		
		DrawText(toDraw, color, txtFont, 
			EDITOR_STATUS_LEFT + x + (w - txtW) div 2, 
			EDITOR_STATUS_TOP + y + (h - txtH) div 2);
	end;
	
	procedure DrawCurrentSprite(sprt: Sprite);
	var
		x, y: Integer;
	begin
		x := Round(EDITOR_STATUS_LEFT + 
			ELEM_PICT_X + ((ELEM_PICT_W - CurrentWidth(sprt)) / 2));
		y := Round(EDITOR_STATUS_TOP +
			ELEM_PICT_Y + ((ELEM_PICT_H - CurrentHeight(sprt)) / 2));

		DrawBitmap(sprt.Bitmaps[sprt.currentFrame], x, y);
	end;
	
	procedure DrawEditorStatus(var status: EditorStatusType);
	begin
		DrawBitmap(GameImage(EditorStatus), EDITOR_STATUS_LEFT, EDITOR_STATUS_TOP);
		DrawTextIn('level' + IntToStr(status.dataPtr^.currentLevel) + '.dat', 
			ColorBlack, GameFont(Courier), BOX_X, LEVEL_Y, BOX_W, BOX_H);
		
		if status.currentObstacle <> nil then
		begin
			DrawTextIn(status.currentObstacle^.Name, 
				ColorBlack, GameFont(Courier), BOX_X, ELEM_NAME_Y, BOX_W, BOX_H);
			DrawTextIn(Format('%10.2f', [ status.currentObstacle^.Mass ]), 
				ColorBlack, GameFont(Courier), BOX_X, MASS_Y, BOX_W, BOX_H);
		end;
		
		DrawCurrentSprite(status.currentSprite);
		RefreshScreen();
	end;
	
	function MouseToWorld(v: Vector; const status: EditorStatusType): Vector;
	begin
		result.x := v.x + status.dataPtr^.windowX;
		result.y := v.y + status.dataPtr^.windowY;
		result.w := 1;
	end;
	
	function LevelFilename(level: Integer): String;
	begin
		result := GetPathToResource('level' + IntToStr(level) + '.dat')
	end;
	
	function IsSpecialKeyPressed(): Boolean;
	begin
		{$IFDEF DARWIN}
			result := IsKeyPressed(VK_CONTROL) or IsKeyPressed(VK_LMENU) or IsKeyPressed(VK_RMENU);
		{$ELSE}
			result := IsKeyPressed(VK_CONTROL);		
		{$ENDIF}
	end;
			
	///
	/// String Format: img x y vx vy name
	procedure LoadObstacle(var obst: Obstacle; var data: Text);
	var
		img, x, y: Integer;
		vx, vy, mass: Single;
		space: Char;
		name: String;
	begin
		ReadLn(data, img, x, y, vx, vy, mass, space, name);
			
		obst.Sprite := CreateSprite(GameImage(GameImages(img)));
		obst.Sprite.xPos := x;
		obst.Sprite.yPos := y;
		obst.Sprite.usePixelCollision := true;
		obst.Movement := CreateVector(vx, vy);
		obst.Mass := mass;
		obst.Name := name;
		obst.Alive := true;			
	end;

	procedure LoadWarpHole(var warp: WarpHole; var data: Text);
	var
		x, y: Single;
		level: Integer;
	begin
		ReadLn(data, x, y, level);
			
		warp.Sprite := CreateSprite(GameImage(GameImages(WarpHoleImg)));
		warp.Sprite.xPos := x;
		warp.Sprite.yPos := y;
		warp.Sprite.usePixelCollision := true;
		warp.Level := level;
	end;
		
	procedure LoadMidBacks(var input: Text; var data: GameDataType);
	var
		i, img, count, x, y: Integer;
	begin
		ReadLn(input, count);
		SetLength(data.MidBacks, count);		
		
		for i := Low(data.MidBacks) to High(data.MidBacks) do
		begin
			ReadLn(input, img, x, y);
			data.MidBacks[i] := CreateSprite(GameImage(GameImages(img)));
			data.MidBacks[i].xPos := x;
			data.MidBacks[i].yPos := y;
		end;
	end;
	
	procedure LoadAnimation(var anim: Animation; var input: Text);
	var
		img, action: Integer;	
	begin
		ClearAnimation(anim);
		ReadLn(input, img, anim.partWidth, anim.partHeight,
			anim.rows, anim.cols, anim.numCells, anim.framesPerCell,
			anim.x, anim.y, action, anim.value);
		
		anim.action := AnimationAction(action);
		anim.Movement.x := 0;
		anim.Movement.y := 0;
		anim.currentCell := 0;
		anim.used := true;
		
		anim.image := GameImage(GameImages(img));
	end;

	procedure SaveMidBacks(var output: Text; const data: GameDataType);
	var
		i: Integer;
	begin
		WriteLn(output, Length(data.MidBacks));
		
		for i := Low(data.MidBacks) to High(data.MidBacks) do
		begin
			WriteLn(output, Integer(ImageID(data.MidBacks[i].bitmaps[0])), ' ', 
				data.MidBacks[i].xPos:0:0, ' ', 
				data.MidBacks[i].yPos:0:0);
		end;
	end;

	procedure SaveWarpHoles(var output: Text; const data: GameDataType);
	var
		i: Integer;
	begin
		WriteLn(output, Length(data.WarpHoles));
		
		for i := Low(data.WarpHoles) to High(data.WarpHoles) do
		begin
			WriteLn(output, data.WarpHoles[i].Sprite.xPos:4:2, ' ',
				data.WarpHoles[i].Sprite.yPos:4:2, ' ', data.WarpHoles[i].Level);
		end;
	end;

	procedure SaveAnimation(var output: Text; const anim: Animation);
	begin
		WriteLn(output, Integer(ImageID(anim.image)), ' ', 
			anim.partWidth, ' ', anim.partHeight, ' ',
			anim.rows, ' ', anim.cols, ' ',
			anim.numCells, ' ',
			anim.framesPerCell, ' ',
			anim.x:4:2, ' ', anim.y:4:2, ' ',
			Integer(anim.action), ' ', anim.value
			);
	end;

	procedure SaveAnimationArray(var output: Text; const arr: Array of Animation);
	var
		i: Integer;
	begin
		WriteLn(output, Length(arr));
		
		for i := Low(arr) to High(arr) do
		begin
			SaveAnimation(output, arr[i]);
		end;
	end;

	procedure SaveLevel(const data: GameDataType);
	var
		i, j, k: Integer;
		levelFile: Text;
	begin
		Assign(levelFile, LevelFilename(data.currentLevel));
		Rewrite(levelFile);
		
		SaveMidBacks(levelFile, data);
		WriteLn(levelFile, Length(data.Planets));
		
		j := 0;
		
		for i := Low(data.Planets) to High(data.Planets) do
		begin
			Write(levelFile, Integer(ImageID(data.Planets[i].Sprite.bitmaps[0])), ' ',
				data.Planets[i].Sprite.xPos:0:0, ' ', data.Planets[i].Sprite.yPos:0:0, ' ',
				data.Planets[i].Movement.x:0:2, ' ', data.Planets[i].Movement.y:0:2, ' ',
				data.Planets[i].Mass:0:2, ' ');
			
			if TryStrToInt(data.Planets[i].Name, k) then
			begin
				j := j + 1;
				WriteLn(levelFile, j);
			end
			else
			begin
				WriteLn(levelFile, data.Planets[i].Name);
			end;
		end;
		
		SaveWarpHoles(levelFile, data);
		SaveAnimationArray(levelFile, data.levelAnimations);
				
		Close(levelFile);
	end;

	procedure DoChangeMass(var status: EditorStatusType; amt: Single);
	begin
		if status.currentObstacle <> nil then
		begin
			if amt + status.currentObstacle.mass > 0 then
			begin
				status.currentObstacle.mass := status.currentObstacle.mass + amt;
			end;
		end;
	end;

	procedure AddMidBack(sprt: Sprite; x, y: Single; var data: GameDataType);
	var
		temp: Sprite;
	begin
		temp := CreateSprite(sprt.Bitmaps[0]);
		temp.xPos := x - data.windowX + (data.windowX  / (3 * SCROLL_PIXELS));
		temp.yPos := y;
		
		if not ObjectHasHitMidBack(temp, data) then
		begin
			SetLength(data.MidBacks, Length(data.MidBacks) + 1);
			data.MidBacks[High(data.MidBacks)] := temp;
		end
		else
		begin
			FreeSprite(temp);
		end;
	end;
	
	function NewLevelAnimation(var data: GameDataType) : AnimationPtr;
	begin
		SetLength(data.levelAnimations, Length(data.levelAnimations) + 1);
		ClearAnimation(data.levelAnimations[High(data.levelAnimations)]);
		result := @data.levelAnimations[High(data.levelAnimations)];
	end;
	
	procedure AddStar(x, y: Single; var data: GameDataType);
	var
		star : AnimationPtr;
	begin
		star := NewLevelAnimation(data);
		
		star.image := GameImage(StarImg);
		star.used := true;
		star.partWidth := 16;
		star.partHeight := 16;
		star.rows := 2;
		star.cols := 5;
		star.numCells := 10;
		star.framesPerCell := 10;
		star.x := x;
		star.y := y;
		star.action := AddScoreAction;
		star.value := STAR_VALUE;
	end;
	
	procedure AddFuelPack(x, y: Single; var data: GameDataType);
	var
		fuelPack : AnimationPtr;
	begin
		fuelPack := NewLevelAnimation(data);
		
		fuelPack.image := GameImage(FuelPackImg);
		fuelPack.used := true;
		fuelPack.partWidth := 19;
		fuelPack.partHeight := 29;
		fuelPack.rows := 2;
		fuelPack.cols := 5;
		fuelPack.numCells := 9;
		fuelPack.framesPerCell := 10;
		fuelPack.x := x;
		fuelPack.y := y;
		fuelPack.action := AddFuelAction;
		fuelPack.value := FUEL_VALUE;
	end;

	procedure AddBattery(x, y: Single; var data: GameDataType);
	var
		battery : AnimationPtr;
	begin
		battery := NewLevelAnimation(data);
		
		battery.image := GameImage(BatteryImg);
		battery.used := true;
		battery.partWidth := 14;
		battery.partHeight := 23;
		battery.rows := 2;
		battery.cols := 4;
		battery.numCells := 8;
		battery.framesPerCell := 10;
		battery.x := x;
		battery.y := y;
		battery.action := AddShieldAction;
		battery.value := BATTERY_VALUE;
	end;
	
	procedure AddWarpHole(sprt: Sprite; x, y: Single; var data: GameDataType);
	var
		temp: Sprite;
	begin
		temp := CreateSprite(sprt.Bitmaps[0]);
		temp.xPos := x;
		temp.yPos := y;
		
		if not ObjectHasHitAPlanet(temp, data) then
		begin
			SetLength(data.WarpHoles, Length(data.WarpHoles) + 1);
			data.WarpHoles[High(data.WarpHoles)].Sprite := temp;
			data.WarpHoles[High(data.WarpHoles)].Level := data.CurrentLevel + 1;
		end
		else
		begin
			FreeSprite(temp);
		end;
	end;
	
	procedure AddNewObstacle(newObst: ObstaclePtr; x, y: Single; var data: GameDataType);
	var
		temp: Obstacle;
	begin
		temp := newObst^;
		//Copy sprite... by creating new sprite
		temp.Sprite := CreateSprite(newObst^.Sprite.bitmaps[0]);
		temp.Sprite.xPos := x;
		temp.Sprite.yPos := y;
		
		if not ObjectHasHitAPlanet(temp.Sprite, data) then
		begin
			SetLength(data.Planets, Length(data.Planets) + 1);
			temp.Name := IntToStr(Length(data.Planets));
			data.Planets[High(data.Planets)] := temp;
		end
		else
		begin
			FreeSprite(temp.Sprite);
		end;
	end;
	
	procedure DeleteLastObstacle(var data: GameDataType);
	begin
		FreeSprite(data.Planets[High(data.Planets)].Sprite);
		SetLength(data.Planets, Length(data.Planets) - 1);
	end;

	function EditorWasActivated(): Boolean;
	begin
		result := IsSpecialKeyPressed() and IsKeyPressed(VK_E);
	end;
	
	function Deactivated(): Boolean;
	begin
		result := WasKeyTyped(VK_ESCAPE) or WindowCloseRequested();
	end;

	function NewObstacle(bmp: GameImages; mass: Single; name: String): ObstaclePtr;
	begin
		New(result);
		result.Sprite := CreateSprite(GameImage(bmp));
		result.Sprite.xPos := 0;
		result.Sprite.yPos := 1;
		result.Movement := CreateVector(0,0);
		result.Mass := mass;
		result.Name := name;
		result.Alive := True;
	end;
	
	function StandardAsteroid(): ObstaclePtr;
	begin
		result := NewObstacle(Asteroid, 0.1, 'New Asteroid');
	end;

	function NewSmallAsteroid(): ObstaclePtr;
	begin
		result := NewObstacle(SmallAsteroid, 0.05, 'Small Asteroid');
	end;
	
	function NewSaturn(): ObstaclePtr;
	begin
		result := NewObstacle(Saturn, 0.85, 'Saturn');
	end;
	
	function NewUranus(): ObstaclePtr;
	begin
		result := NewObstacle(Uranus, 1.00, 'Uranus');
	end;
	
	function NewEarth(): ObstaclePtr;
	begin
		result := NewObstacle(Earth, 1.50, 'Earth');
	end;
	
	procedure DoAddClick(var status: EditorStatusType; clickedAt: Vector);
	var
		cX, cY: Single;
	begin
		//Center X
		cX := status.dataPtr^.windowX + clickedAt.x - CurrentWidth(status.currentSprite) / 2;
		cY := status.dataPtr^.windowY + clickedAt.y - CurrentHeight(status.currentSprite) / 2;
		
		case status.mode of
			AddingNewObstacles: 
				begin
					if status.currentObstacle <> nil then
					begin
						AddNewObstacle(status.currentObstacle, cX, cY, status.dataPtr^);
					end;					
				end;
			AddingWarpHoles:
				AddWarpHole(status.currentSprite, cX, cY, status.dataPtr^);
			AddingStar:
				AddStar(cX, cY, status.dataPtr^);
			AddingMidBack:
				AddMidBack(status.currentSprite, cX, cY, status.dataPtr^);
			AddingFuelPack:
				AddFuelPack(cX, cY, status.dataPtr^);
			AddingBattery:
				AddBattery(cX, cY, status.dataPtr^)
		end;
	end;
	
	procedure PrepareStatusChange(var status: EditorStatusType);
	begin
		if status.mode = AddingNewObstacles then
		begin
			FreeSprite(status.currentSprite);
			Dispose(status.currentObstacle);
		end 
		else if status.mode = AddingMidBack then
		begin
			FreeSprite(status.currentSprite);
		end;
		status.currentObstacle := nil;
		status.currentSprite := nil;
		status.currentWarpHole := nil;
	end;
	
	procedure DoChangeToEditClick(var status: EditorStatusType; clickedAt: Vector);
	var
		warp: WarpHolePtr;
		obst: ObstaclePtr;
		sprt: Sprite;
	begin
		if PlanetAtPoint(clickedAt, status.dataPtr^, obst) then
		begin
			WriteLn('Editing Obstancle...');
			
			PrepareStatusChange(status);
			status.mode := EditingObstacle;
			status.currentObstacle := obst;
			status.currentSprite := obst.Sprite;
		end
		else if WarpHoleAtPoint(clickedAt, status.dataPtr^, warp) then
		begin
			WriteLn('Editing WarpHole...');
			
			PrepareStatusChange(status);
			status.mode := EditingWarpHole;
			status.currentWarpHole := warp;
			status.currentSprite := warp.sprite;			
		end
		else if MidBackAtPoint(clickedAt, status.dataPtr^, sprt) then
		begin
			WriteLn('Editing MidBack');
			
			PrepareStatusChange(status);
			status.mode := MovingMidBack;
			status.currentSprite := sprt;
		end;
	end;
	
	procedure DoChangeToAdding(var status: EditorStatusType; obst: ObstaclePtr);
	begin
		PrepareStatusChange(status);
		status.mode := AddingNewObstacles;
		status.currentObstacle := obst;
		status.currentSprite := obst^.Sprite;
	end;
	
	procedure DoChangeToAddingStar(var status: EditorStatusType);
	begin
		PrepareStatusChange(status);
		status.mode := AddingStar;
		status.currentSprite := CreateSprite(GameImage(StarImgEdit));
	end;

	procedure DoChangeToFuelPack(var status: EditorStatusType);
	begin
		PrepareStatusChange(status);
		status.mode := AddingFuelPack;
		status.currentSprite := CreateSprite(GameImage(FuelPackImgEdit));
	end;
	
	procedure DoChangeToAddingBattery(var status: EditorStatusType);
	begin
		PrepareStatusChange(status);
		status.mode := AddingBattery;
		status.currentSprite := CreateSprite(GameImage(BatteryEditImg));
	end;
	
	procedure DoChangeAddMidBack(var status: EditorStatusType);
	begin
		PrepareStatusChange(status);
		status.mode := AddingMidBack;
		status.currentSprite := CreateSprite(GameImage(MidBack_Galaxy1));
	end;

	procedure DoChangeToWarpHoles(var status: EditorStatusType);
	begin
		PrepareStatusChange(status);
			
		status.mode := AddingWarpHoles;
		status.currentSprite := CreateSprite(GameImage(WarpHoleImg));
	end;
	
	procedure DoMoveSprite(var status: EditorStatusType; clickedAt: Vector);
	begin
		MoveGameSprite(status.currentSprite,
			clickedAt.x + status.dataPtr^.windowX,
			clickedAt.y + status.dataPtr^.windowY);
	end;
	
	procedure DoSetVector(var status: EditorStatusType; clickedAt: Vector);
	var
		cx, cy: Single;
		pos: Vector;
	begin
		cx := status.currentObstacle^.sprite.xPos + 
			CurrentWidth(status.currentObstacle^.sprite) div 2;
		cy := status.currentObstacle^.sprite.yPos + 
			CurrentHeight(status.currentObstacle^.sprite) div 2;
			
		pos := CreateVector(cx, cy);
		status.currentObstacle.Movement := 
			Multiply(ScaleMatrix(1/50), 
				SubtractVectors(MouseToWorld(clickedAt, status), pos));
	end;
	
	procedure DoChangeWarpHoleLevel(warpHole: WarpHolePtr; inc: Integer);
	begin
		warpHole.level += inc;
		if warpHole.level < 1 then
		begin
			warpHole.level := 1;
		end;
	end;
	
	procedure HandleUserInputWarpHoles(var status: EditorStatusType);
	begin
		if MouseWasClicked(LeftButton) then 
			DoAddClick(status, GetMousePosition());
	end;
	
	procedure HandleUserInputAdding(var status: EditorStatusType);
	var
		obst: ObstaclePtr;
	begin
		if MouseWasClicked(LeftButton) then 
			DoAddClick(status, GetMousePosition());
		
		if status.mode = AddingNewObstacles then
		begin
			obst := nil;
				
			//Else required to avoid memory leak	
			if WasKeyTyped(VK_1) then obst := StandardAsteroid()
			else if WasKeyTyped(VK_2) then obst := NewSmallAsteroid()
			else if WasKeyTyped(VK_3) then obst := NewSaturn()
			else if WasKeyTyped(VK_4) then obst := NewUranus()
			else if WasKeyTyped(VK_5) then obst := NewEarth();
				
			if obst <> nil then
			begin
				DoChangeToAdding(status, obst);
			end;
		end;
	end;

	procedure HandleUserInputMidBack(var status: EditorStatusType);
	var
		sprt: Sprite;
	begin
		sprt := nil;
		
		if MouseWasClicked(LeftButton) then 
			DoAddClick(status, GetMousePosition());
		
		//Else required to avoid memory leak	
		if WasKeyTyped(VK_1) then sprt := CreateSprite(GameImage(MidBack_Galaxy1))
		else if WasKeyTyped(VK_2) then sprt := CreateSprite(GameImage(MidBack_Galaxy2))
		else if WasKeyTyped(VK_3) then sprt := CreateSprite(GameImage(MidBack_Galaxy3))
		else if WasKeyTyped(VK_4) then sprt := CreateSprite(GameImage(MidBack_Galaxy4));
		
		if sprt <> nil then
		begin
			FreeSprite(status.currentSprite);
			status.currentSprite := sprt;
		end;
	end;

	procedure HandleUserInputMoveSprite(var status: EditorStatusType);
	begin
		if MouseWasClicked(LeftButton) then 
		begin
			if not IsSpecialKeyPressed() then
				DoMoveSprite(status, GetMousePosition());
		end;	
	end;

	procedure HandleUserInputEditingObstacle(var status: EditorStatusType);
	begin
		if status.currentObstacle <> nil then
		begin
			if IsSpecialKeyPressed() and IsMouseDown(LeftButton) then
				DoSetVector(status, GetMousePosition());
			if WasKeyTyped(VK_ADD) or WasKeyTyped(VK_EQUALS) then
				DoChangeMass(status, MASS_ADJ);
			if WasKeyTyped(VK_SUBTRACT) then
				DoChangeMass(status, -MASS_ADJ);			
		end;
		
		HandleUserInputMoveSprite(status);				
	end;
	
	procedure HandleUserInputEditingWarpHole(var status: EditorStatusType);
	begin
		if status.currentWarpHole <> nil then
		begin
			if WasKeyTyped(VK_ADD) or WasKeyTyped(VK_EQUALS) then
				DoChangeWarpHoleLevel(status.currentWarpHole, 1);
			if WasKeyTyped(VK_SUBTRACT) then
				DoChangeWarpHoleLevel(status.currentWarpHole, -1);			
		end;
		
		HandleUserInputMoveSprite(status);				
	end;
	
	procedure DoLoadLevel(var status: EditorStatusType);
	begin
		DoChangeToAdding(status, StandardAsteroid());
		LoadLevel(status.dataPtr^.currentLevel, status.dataPtr^);
	end;
	
	procedure HandleUserInput(var status: EditorStatusType);
	begin
		if IsKeyPressed(VK_RIGHT) then MoveGameWindow(status.dataPtr^, SCROLL_PIXELS * 2, 0);
		if IsKeyPressed(VK_LEFT) then MoveGameWindow(status.dataPtr^, -SCROLL_PIXELS * 2, 0);;
		if IsKeyPressed(VK_Up) then MoveGameWindow(status.dataPtr^, 0, -SCROLL_PIXELS * 2);
		if IsKeyPressed(VK_Down) then MoveGameWindow(status.dataPtr^, 0, SCROLL_PIXELS * 2);
		
		if MouseWasClicked(RightButton) then 
			DoChangeToEditClick(status, GetMousePosition());
		if WasKeyTyped(VK_M) and (status.Mode <> AddingMidBack) then
			DoChangeAddMidBack(status);
		if WasKeyTyped(VK_A) and (status.Mode <> AddingNewObstacles) then
			DoChangeToAdding(status, StandardAsteroid());
		if WasKeyTyped(VK_W) and (status.Mode <> AddingWarpHoles) then
			DoChangeToWarpHoles(status);
		if WasKeyTyped(VK_Z) and (status.Mode <> AddingStar) then
			DoChangeToAddingStar(status);
		if WasKeyTyped(VK_F) and (status.Mode <> AddingFuelPack) then
			DoChangeToFuelPack(status);
		if WasKeyTyped(VK_B) and (status.Mode <> AddingBattery) then
			DochangeToAddingBattery(status);
				
		if IsSpecialKeyPressed() and WasKeyTyped(VK_S) then
			SaveLevel(status.dataPtr^);
		if IsSpecialKeyPressed() and WasKeyTyped(VK_L) then
			DoLoadLevel(status);
		
		case status.mode of
			AddingNewObstacles: HandleUserInputAdding(status);
			AddingStar: 		HandleUserInputAdding(status);
			AddingFuelPack: 	HandleUserInputAdding(status);
			AddingBattery:		HandleUserInputAdding(status);
			EditingObstacle:	HandleUserInputEditingObstacle(status);
			AddingMidBack: 		HandleUserInputMidBack(status);
			MovingMidBack: 		HandleUserInputMoveSprite(status);
			AddingWarpHoles: 	HandleUserInputWarpHoles(status);
			EditingWarpHole: 	HandleUserInputEditingWarpHole(status);
		end;
	end;
	
	procedure InitEdit(out status: EditorStatusType; var data: GameDataType);
	var
		x, y: Single;
	begin
		data.state := EditorState;
		
		status.currentObstacle := StandardAsteroid();
		status.currentSprite := status.currentObstacle.Sprite;
		status.dataPtr := @data;
		status.mode := AddingNewObstacles;
		
		x := data.windowX;
		y := data.windowY;
		
		LoadLevel(data.currentLevel, data);
		ResetLevel(data);
		
		data.windowX := x;
		data.windowY := y;
	end;
	
	procedure EndEdit(var status: EditorStatusType; var data: GameDataType);
	begin
		if status.mode = AddingNewObstacles then
		begin
			FreeSprite(status.currentSprite);
			Dispose(status.currentObstacle);
			status.currentObstacle := nil;
			status.currentSprite := nil;
		end;
		
		if status.mode = AddingMidBack then
		begin
			FreeSprite(status.currentSprite);
		end;
		
		ResetLevel(data);
		data.state := PlayingGameState;
	end;
	
	procedure ExecuteEditor(var data: GameDataType);
	var
		status: EditorStatusType;
	begin
		InitEdit(status, data);
		
		repeat
			ProcessEvents();
			HandleUserInput(status);
			ClearScreen(ColorBlack);
			DrawScreen(data, true, false);
			DrawEditorStatus(status);
		until Deactivated();
		
		EndEdit(status, data);
	end;
	
	procedure CreateLevel(var data: GameDataType; level: Integer);
	var
		sprt : Sprite;
		x, y: Single;
	begin
		sprt := CreateSprite(GameImage(WarpHoleImg));
		
		x := MaxForegroundX(); //CalculateForegroundX(GameImage(Background).width - SCREEN_WIDTH) + ((SCREEN_WIDTH * 3) div 4);
		y := (SCREEN_HEIGHT - TOP_GAME_AREA) div 2 + TOP_GAME_AREA - CurrentWidth(sprt) div 2;
		
		data.currentLevel := level;
		AddWarpHole(sprt, x, y, data);
		
		FreeSprite(sprt);
		SaveLevel(data);
		FreeLevel(data);
	end;
	
	procedure LoadLevel(level: Integer; var data: GameDataType);
	var
		count, i: Integer;
		levelFile: Text;
	begin
		FreeLevel(data);
		
		if false = FileExists(LevelFilename(level)) then
		begin
			CreateLevel(data, level);
		end;
		
		Assign(levelFile, LevelFilename(level));
		Reset(levelFile);
		
		LoadMidBacks(levelFile, data);
				
		ReadLn(levelFile, count);
		SetLength(data.Planets, count);	
		for i := Low(data.Planets) to High(data.Planets) do
		begin
			LoadObstacle(data.Planets[i], levelFile);
		end;
		
		ReadLn(levelFile, count);
		SetLength(data.WarpHoles, count);
		for i := Low(data.WarpHoles) to High(data.WarpHoles) do
		begin
			LoadWarpHole(data.WarpHoles[i], levelFile);
		end;
		
		ReadLn(levelFile, count);
		SetLength(data.levelAnimations, count);
		for i := Low(data.levelAnimations) to High(data.levelAnimations) do
		begin
			LoadAnimation(data.levelAnimations[i], levelFile);
		end;

		Close(levelFile);
	end;

end.