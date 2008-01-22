unit AlienFlightLevelEditor;

interface
	uses 
		SysUtils,
		SGSDK_Core, SGSDK_Input, SGSDK_KeyCodes, SGSDK_Graphics, SGSDK_Font, SGSDK_Physics, SGSDK_Camera,
		GameResources, 
		AlienFlightModel;

	const
		//EDITOR_STATUS_TOP = 230; EDITOR_STATUS_LEFT = 5;
		EDITOR_HUD_TOP = 55; EDITOR_HUD_LEFT = 0; EDITOR_HUD_WIDTH = 200;
		EDITOR_HUD_DATA_HEIGHT = 15;
		EDITOR_DATA_BORDER = 4;

		LEVEL_Y = SCREEN_HEIGHT - EDITOR_HUD_DATA_HEIGHT;

		EDITOR_ICON_WIDTH = 32; EDITOR_ICON_HEIGHT = 32;

		SELECTION_Y = SCREEN_HEIGHT - 10 * EDITOR_ICON_HEIGHT;
		
		BUTTONS_TOP = EDITOR_HUD_TOP + 60;
		BUTTON_WIDTH = 59; BUTTON_HEIGHT = 25;
		ADD_BUTTON_X = 5; EDIT_BUTTON_X = 70; DELETE_BUTTON_X = 136;
		CLEAR_BUTTON_X = 65; CLEAR_BUTTON_Y = 515 + EDITOR_HUD_TOP;
		
		VALUE_TOP = BUTTONS_TOP + BUTTON_HEIGHT + 25 + EDITOR_HUD_DATA_HEIGHT;
		VALUE_X = EDITOR_HUD_LEFT + 5;
				
	type GameDataTypePtr = ^GameDataType;

	type EditorMode = (
		AddingMode,
		EditingMode,
		DeletingMode
	);

	type EditorStatusType = record
		mode: EditorMode;
		dataPtr: GameDataTypePtr;
		currentSpriteKind: AlienFlightSpriteKind;
		
		selection: Array of AlienFlightSpritePtr;	
		selectedIdx: Integer;
		selectedValueIdx: Integer;

		//currentObstacle: ObstaclePtr;
		//currentWarpHole: WarpHolePtr;
		//currentSprite: Sprite;
	end;

	function EditorWasActivated(): Boolean;
	function EditorDeactivated(): Boolean;
	function LevelFilename(level: Integer): String;
	function SelectedValue(const status: EditorStatusType; sprt: AlienFlightSpritePtr): Single;
	function SelectedName(const status: EditorStatusType; sprt: AlienFlightSpritePtr): String;
	function HasSelectedValue(const status: EditorStatusType; sprt: AlienFlightSpritePtr): boolean;
	function IsSpecialKeyPressed(): Boolean;
	
	procedure InitEdit(out status: EditorStatusType; var data: GameDataType);
	procedure EndEdit(var status: EditorStatusType; var data: GameDataType);
	procedure LoadLevel(level: Integer; var data: GameDataType);		
	procedure HandleEditorInput(var status: EditorStatusType);

implementation
	
	function SelectedName(const status: EditorStatusType; sprt: AlienFlightSpritePtr): String;
	begin
		if HasSelectedValue(status, sprt) then
		begin
			if status.selectedValueIdx >= 0 then
				result := _Templates[sprt^.kind].values[status.selectedValueIdx].name
			else
				result := 'Mass: ';
		end
		else
		begin
			result := 'No value';
		end;
	end;
	
	function SelectedValue(const status: EditorStatusType; sprt: AlienFlightSpritePtr): Single;
	begin
		result := 0.0;
		
		if status.selectedValueIdx = -1 then
			result := sprt.sprite.mass
		else
		begin
			if Length(sprt.values) > status.selectedValueIdx then
			begin
				result := sprt^.values[status.selectedValueIdx];
			end;
		end;
	end;
	
	function HasSelectedValue(const status: EditorStatusType; sprt: AlienFlightSpritePtr): boolean;
	var
		temp: AlienFlightSpriteTemplate;
	begin
		temp := _Templates[sprt^.kind];
		result := (status.selectedValueIdx = -1) or (Length(temp.values) > status.selectedValueIdx);
	end;
	
	procedure SetSelectedValue(var status: EditorStatusType; sprt: AlienFlightSpritePtr; newValue: Single);
	var
		temp: AlienFlightSpriteTemplate;
		tempValue: TemplateValue;
	begin
		if status.selectedValueIdx >= 0 then
		begin
			temp := _Templates[sprt^.kind];
			tempValue := temp.values[status.selectedValueIdx];
	
			if (newValue >= tempValue.min) and (newValue <= tempValue.max) then
			begin
				sprt^.values[status.selectedValueIdx] := newValue;
			end;
		end
		else
		begin
			if (newValue > 0.05) then sprt^.sprite.mass := newValue;
		end;
	end;
	
	function SelectedIncrement(const status: EditorStatusType; kind: AlienFlightSpriteKind): Single;
	begin
		if status.selectedValueIdx = -1 then
		begin
			result := 0.05;
		end
		else
		begin
			if Length(_Templates[kind].values) > status.selectedValueIdx then
			begin
				result := _Templates[kind].values[status.selectedValueIdx].increments;
			end;
		end;
	end;
		
	function IsSpecialKeyPressed(): Boolean;
	begin
		{$IFDEF DARWIN}
			result := IsKeyPressed(VK_CONTROL) or IsKeyPressed(VK_LMENU) or IsKeyPressed(VK_RMENU);
		{$ELSE}
			result := IsKeyPressed(VK_CONTROL);		
		{$ENDIF}
	end;

	function LevelFilename(level: Integer): String;
	begin
		result := GetPathToResource('level' + IntToStr(level) + '.dat')
	end;

	function EditorWasActivated(): Boolean;
	begin
		result := IsSpecialKeyPressed() and IsKeyPressed(VK_E);
	end;
	
	function EditorDeactivated(): Boolean;
	begin
		result := WasKeyTyped(VK_ESCAPE) or WindowCloseRequested();
	end;
			
	///
	/// String Format: img x y vx vy name
	procedure LoadObstacleSprite(var sprt: AlienFlightSprite; var data: Text);
	var
		img, x, y: Integer;
		vx, vy, mass: Single;
		space: Char;
		name: String;
		
		kind : AlienFlightSpriteKind;
		worldOffset: Vector;
	begin
		ReadLn(data, img, x, y, vx, vy, mass, space, name);

		case img of
			1: kind := EarthKind;
			2: kind := SaturnKind;
			3: kind := UranusKind;
			4: kind := AsteroidKind;
			5: kind := SmallAsteroidKind;
		else
			raise Exception.Create('Unknown img'); 
		end;

		worldOffset := CreateVector(x, y);
		
		sprt := CreateSpriteKind(kind, worldOffset);
		sprt.sprite.movement := CreateVector(vx, vy);
		sprt.sprite.mass := mass;
	end;

	procedure LoadSprite(var sprt: AlienFlightSprite; var output: Text);
	var
		i, count, kindIdx: Integer;
		x, y, mass, mX, mY: Single;
		worldOffset: Vector;
	begin
		//Reads:
		//kind x y mass mX mY #values val1...
		Read(output, kindIdx, x, y, mass, mX, mY, count);
		
		worldOffset := CreateVector(x, y);
				
		sprt := CreateSpriteKind(AlienFlightSpriteKind(kindIdx), worldOffset);
		sprt.sprite.movement := CreateVector(mX, mY);
		sprt.sprite.mass := mass;
		
		SetLength(sprt.values, count);
		for i := 0 to High(sprt.values) do
		begin
			Read(output, sprt.values[i]);
		end;
		
		ReadLn(output);
	end;
		
	procedure SaveSprite(var output: Text; const sprt: AlienFlightSprite);
	var
		i: Integer;
	begin
		//Writes:
		//kind x y mass mX mY #values val1...
		Write(output, Integer(sprt.kind), ' ', 
			sprt.sprite.x:4:5, ' ', sprt.sprite.y:4:5, ' ',
			sprt.sprite.mass:4:2, ' ',
			sprt.sprite.movement.x:4:5, ' ', sprt.sprite.movement.y:4:5, ' ', 
			Length(sprt.values)
			);
			
		for i := 0 to High(sprt.values) do
		begin
			Write(output, ' ', sprt.values[i]:4:5);
		end;
		
		WriteLn(output);
	end;

	procedure SaveSpriteArray(var output: Text; const arr: Array of AlienFlightSprite);
	var
		i: Integer;
	begin
		WriteLn(output, Length(arr), ' # sprites');
		
		for i := Low(arr) to High(arr) do
		begin
			SaveSprite(output, arr[i]);
		end;
	end;

	procedure SaveLevel(const data: GameDataType);
	var
//		i, j, k: Integer;
		levelFile: Text;
	begin
		Assign(levelFile, LevelFilename(data.currentLevel));
		Rewrite(levelFile);
		
		SaveSpriteArray(levelFile, data.sprites);
				
		Close(levelFile);
	end;

	procedure DoChangeValue(var status: EditorStatusType; sign: Integer);
	var
		i: Integer;
		newValue: Single;
	begin
		if status.mode <> EditingMode then exit;
		
		for i := Low(status.selection) to High(status.selection) do
		begin
			if HasSelectedValue(status, status.selection[i]) then
			begin
				newValue := SelectedValue(status, status.selection[i]) + sign * SelectedIncrement(status, status.selection[i].kind);
				SetSelectedValue(status, status.selection[i], newValue);
			end;
		end;
	end;
	
	procedure AddSprite(var status: EditorStatusType; screenOffset: Vector);
	var
		v: Vector;
	begin
		v := ToGameCoordinates(screenOffset);
		
		if _Templates[status.currentSpriteKind].foreground = false then
		begin
			v.x := v.x - XOffset() + CalculateBackgroundX(XOffset());
		end;
		
		if CanPlaceAt(status.dataPtr^, status.currentSpriteKind, nil, v.x, v.y) then
			AddSpriteKind(status.dataPtr^, status.currentSpriteKind, v);
	end;	
	
	procedure SwitchToMode(var status: EditorStatusType; mode :EditorMode);
	begin
		if mode = AddingMode then
		begin
			SetLength(status.selection, 0);
		end;
		status.selectedValueIdx := 0;
		status.currentSpriteKind := Low(AlienFlightSpriteKind);
				
		status.mode := mode;
	end;
	
	procedure DoSetMovement(var status: EditorStatusType; clickedAt: Vector);
	var
		sprt: Sprite;
		cx, cy: Single;
		pos, movement: Vector;
		i: Integer;
	begin
		if Length(status.selection) > 0 then
		begin
			if status.selectedIdx >= Length(status.selection) then status.selectedIdx := High(status.selection);
				
			sprt := status.selection[status.selectedIdx]^.sprite;
			
			cx := sprt.x + sprt.width div 2;
			cy := sprt.y + sprt.height div 2;
			
			pos := CreateVector(cx, cy);
			movement := Multiply(ScaleMatrix(1/50), SubtractVectors(ToGameCoordinates(clickedAt), pos));
			
			for i := Low(status.selection) to High(status.selection) do
			begin	
				if _Templates[status.selection[i].kind].foreground then
					status.selection[i].sprite.movement := movement;
			end;
		end;
	end;
	
	procedure DoMove(var status: EditorStatusType; clickedAt: Vector);
	var
		sprt: Sprite;
		cx, cy: Single;
		pos, offset: Vector;
		i: Integer;
	begin
		if Length(status.selection) > 0 then
		begin
			if status.selectedIdx >= Length(status.selection) then status.selectedIdx := High(status.selection);
			sprt := status.selection[status.selectedIdx]^.sprite;
			
			cx := sprt.x + sprt.width div 2;
			cy := sprt.y + sprt.height div 2;
			
			pos := CreateVector(cx, cy);
			offset := SubtractVectors(ToGameCoordinates(clickedAt), pos);
			
			for i := Low(status.selection) to High(status.selection) do
			begin
				if CanPlaceAt(status.dataPtr^, status.selection[i].kind, status.selection[i], sprt.x + offset.x, sprt.y + offset.y) then	
					MoveSprite(status.selection[i].sprite, offset);
			end;
		end;
	end;
	
	procedure RemoveFromSelection(var status: EditorStatusType; idx: Integer);
	var
		i : Integer;
	begin
		for i := idx to High(status.selection) - 1 do
		begin
			status.selection[i] := status.selection[i + 1];
		end;
		SetLength(status.selection, Length(status.selection) - 1);
	end;
	
	procedure ClearSelection(var status: EditorStatusType);
	begin
		SetLength(status.selection, 0);
		status.selectedIdx := 0;
	end;
	
	procedure SelectSprite(var status: EditorStatusType; v: Vector);
	var
		temp: AlienFlightSpritePtr;
		i: Integer;
	begin
		if not IsKeyPressed(VK_SHIFT) then ClearSelection(status);
		
		if SpriteAtPoint(v, status.dataPtr^, temp) then
		begin
			for i := Low(status.selection) to High(status.selection) do
			begin
				if status.selection[i] = temp then
				begin
					RemoveFromSelection(status, i);
					if status.selectedIdx > i then status.selectedIdx -= 1;
					exit;
				end;
			end;
			
			SetLength(status.selection, Length(status.selection) + 1);
			status.selection[High(status.selection)] := temp;
			status.selectedIdx := High(status.selection);
		end;
	end;
	
	procedure DeleteSelectedSprites(var status: EditorStatusType);
	var
		i: Integer;
	begin
		for i := Low(status.selection) to High(status.selection) do
		begin
			DeleteSprite(status.dataPtr^, status.selection[i])
		end;
		
		ClearSelection(status);
	end;
	
	/// Adds the current sprite kind at the position clicked.
	procedure HandleUserInputAdding(var status: EditorStatusType);
	begin
		if MouseWasClicked(LeftButton) then 
			AddSprite(status, GetMousePositionAsVector());
			
		//Check for right click to edit...
		if MouseWasClicked(RightButton) then
		begin
			SwitchToMode(status, EditingMode);
			SelectSprite(status, GetMousePositionAsVector());
		end;
	end;

	procedure HandleUserInputEditing(var status: EditorStatusType);
	begin
		//Right click selects
		if MouseWasClicked(RightButton) then
			SelectSprite(status, GetMousePositionAsVector());
			
		//Increase or Decrease Value
		if WasKeyTyped(VK_ADD) or WasKeyTyped(VK_EQUALS) then
			DoChangeValue(status, +1);
		if WasKeyTyped(VK_SUBTRACT) then
			DoChangeValue(status, -1);

		//Update movement
		if IsSpecialKeyPressed() and IsMouseDown(LeftButton) then
			DoSetMovement(status, GetMousePositionAsVector());
			
		if (false = IsSpecialKeyPressed()) and MouseWasClicked(LeftButton) then //Move sprites
			DoMove(status, GetMousePositionAsVector());	
			
		//Switch to delete if delete pressed
		if WasKeyTyped(VK_DELETE) or WasKeyTyped(VK_D) or WasKeyTyped(VK_BACK) then
				SwitchToMode(status, DeletingMode);
		if WasKeyTyped(VK_A) then 
				SwitchToMode(status, AddingMode);
		if WasKeyTyped(VK_C) then 
			ClearSelection(status);
			
		//Select other values
		if WasKeyTyped(VK_1) then status.selectedValueIdx := 0;
		if WasKeyTyped(VK_2) then status.selectedValueIdx := 1;
		if WasKeyTyped(VK_3) then status.selectedValueIdx := 2;
		if WasKeyTyped(VK_4) then status.selectedValueIdx := 3;
		if WasKeyTyped(VK_5) then status.selectedValueIdx := 4;
		if WasKeyTyped(VK_6) then status.selectedValueIdx := 5;
		if WasKeyTyped(VK_7) then status.selectedValueIdx := 6;
		if WasKeyTyped(VK_8) then status.selectedValueIdx := 7;
		if WasKeyTyped(VK_9) then status.selectedValueIdx := 8;
		if WasKeyTyped(VK_0) then status.selectedValueIdx := -1;
	end;
	
	procedure HandleUserInputDeleting(var status: EditorStatusType);
	begin
		//Right click selects
		if MouseWasClicked(RightButton) then
			SelectSprite(status, GetMousePositionAsVector());
			
		//Delete selection
		if WasKeyTyped(VK_DELETE) then
			DeleteSelectedSprites(status);
			
		if WasKeyTyped(VK_A) then 
			SwitchToMode(status, AddingMode);
		if WasKeyTyped(VK_E) then
			SwitchToMode(status, EditingMode);
		if WasKeyTyped(VK_C) then 
			ClearSelection(status);
	end;
	
	procedure DoLoadLevel(var status: EditorStatusType; lvl: Integer);
	begin
		SwitchToMode(status, AddingMode);
		if lvl < 1 then lvl := 1;
		if lvl > 99 then lvl := 99;
		LoadLevel(lvl, status.dataPtr^);
	end;
		
	procedure CheckChangeStateClick(var status: EditorStatusType; clickAt: Vector);
	begin
		if VectorWithinRect(clickAt, ADD_BUTTON_X, BUTTONS_TOP, BUTTON_WIDTH, BUTTON_HEIGHT) then
			SwitchToMode(status, AddingMode)
		else if VectorWithinRect(clickAt, EDIT_BUTTON_X, BUTTONS_TOP, BUTTON_WIDTH, BUTTON_HEIGHT) then
			SwitchToMode(status, EditingMode)
		else if VectorWithinRect(clickAt, DELETE_BUTTON_X, BUTTONS_TOP, BUTTON_WIDTH, BUTTON_HEIGHT) then
			SwitchToMode(status, DeletingMode);
	end;
	
	procedure CheckAddingEditorClick(var status: EditorStatusType; clickAt: Vector);
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
			
			if VectorWithinRect(clickAt, x, y, EDITOR_ICON_WIDTH, EDITOR_ICON_HEIGHT) then
			begin
				if _Templates[i].used then status.currentSpriteKind := i;
				
				exit;
			end;
		end;	
	end;
	
	procedure CheckSelectionClick(var status: EditorStatusType; clickAt: Vector);
	const
		COLS = EDITOR_HUD_WIDTH div EDITOR_ICON_WIDTH;
	var
		i: integer;
		x, y: Integer;
	begin
		for i := Low(status.selection) to High(status.selection) do
		begin
			x := (i mod COLS) * EDITOR_ICON_WIDTH + EDITOR_DATA_BORDER;
			y := ((i - (i mod COLS)) div COLS) * EDITOR_ICON_HEIGHT + SELECTION_Y;

			if VectorWithinRect(clickAt, x, y, EDITOR_ICON_WIDTH, EDITOR_ICON_HEIGHT) then
			begin
				status.selectedIdx := i;
				exit;
			end;
		end;
		//Clear Selection
		if VectorWithinRect(clickAt, CLEAR_BUTTON_X, CLEAR_BUTTON_Y, BUTTON_WIDTH, BUTTON_HEIGHT) then
			ClearSelection(status);
	end;
	
	procedure CheckEditorClick(var status: EditorStatusType; clickAt: Vector; out handled: Boolean);
	begin
		handled := false;
		
		if clickAt.x <= EDITOR_HUD_LEFT + EDITOR_HUD_WIDTH then
		begin
			handled := true;
			
			CheckChangeStateClick(status, clickAt);
			
			case status.mode of
				AddingMode: CheckAddingEditorClick(status, clickAt);
				EditingMode: CheckSelectionClick(status, clickAt);
				DeletingMode: CheckSelectionClick(status, clickAt);
			end;
		end;
	end;
	
	procedure HandleEditorInput(var status: EditorStatusType);
	var
		handled: Boolean;
		mul: Single;
	begin
		handled := false;
		
		//Check editor panel click
		if MouseWasClicked(LeftButton) then CheckEditorClick(status, GetMousePositionAsVector(), handled);
		
		if handled then exit;
		
		mul := 1;
		if IsSpecialKeyPressed() then mul := 10;
		if IsKeyPressed(VK_SHIFT) then mul := 0.125;
		
		//Move the camera
		if IsKeyPressed(VK_RIGHT) then MoveGameWindow(status.dataPtr^, mul * SCROLL_PIXELS * 4, 0);
		if IsKeyPressed(VK_LEFT) then MoveGameWindow(status.dataPtr^, mul * -SCROLL_PIXELS * 4, 0);;
		if IsKeyPressed(VK_Up) then MoveGameWindow(status.dataPtr^, 0, mul * -SCROLL_PIXELS * 4);
		if IsKeyPressed(VK_Down) then MoveGameWindow(status.dataPtr^, 0, mul * SCROLL_PIXELS * 4);

		if WasKeyTyped(VK_QUESTION) then MoveGameWindow(status.dataPtr^, -XOffset(), -YOffset());
		if WasKeyTyped(VK_COMMA) then MoveGameWindow(status.dataPtr^, -XOffset(), 0);
		if WasKeyTyped(VK_PERIOD) then MoveGameWindow(status.dataPtr^, MaxForegroundX() - XOffset() - SCREEN_WIDTH, 0);

		//Load and save
		if IsSpecialKeyPressed() and WasKeyTyped(VK_S) then
			SaveLevel(status.dataPtr^);
		if IsSpecialKeyPressed() and WasKeyTyped(VK_L) then
			DoLoadLevel(status, status.dataPtr^.currentLevel);
		if IsSpecialKeyPressed() and WasKeyTyped(VK_N) then
			DoLoadLevel(status, status.dataPtr^.currentLevel + 1);			
		if IsSpecialKeyPressed() and WasKeyTyped(VK_B) then
			DoLoadLevel(status, status.dataPtr^.currentLevel - 1);
		
		//Handle change of state
{		if MouseWasClicked(RightButton) then 
			DoChangeToEditClick(status, GetMousePositionAsVector());
		if WasKeyTyped(VK_M) and (status.Mode <> AddingMidBack) then
			DoChangeAddMidBack(status);
		if WasKeyTyped(VK_A) and (status.Mode <> AddingNewObstacles) then
			DoChangeToAdding(status, StandardAsteroid());
		if WasKeyTyped(VK_W) and (status.Mode <> AddingWarpHoles) then
			DoChangeToWarpHoles(status);
		if WasKeyTyped(VK_Z) and (status.Mode <> AddingMode) then
			DoChangeToAddingSprite(status);
}				
		//Perform action for editor mode
		case status.mode of
			AddingMode: 	HandleUserInputAdding(status);
			EditingMode:	HandleUserInputEditing(status);
			DeletingMode:	HandleUserInputDeleting(status);
{			AddingNewObstacles: HandleUserInputAdding(status);
			EditingObstacle:	HandleUserInputEditingObstacle(status);
			AddingMidBack: 		HandleUserInputMidBack(status);
			MovingMidBack: 		HandleUserInputMoveSprite(status);
			AddingWarpHoles: 	HandleUserInputWarpHoles(status);
			EditingWarpHole: 	HandleUserInputEditingWarpHole(status);
}		end;
	end;
	
	procedure InitEdit(out status: EditorStatusType; var data: GameDataType);
	var
		x, y: Single;
	begin
		data.state := EditorState;
		
{		status.currentObstacle := nil; //StandardAsteroid();
		status.currentSprite := nil; //status.currentObstacle.Sprite;}

		status.dataPtr := @data;
		status.mode := AddingMode;
		
		x := XOffset();
		y := YOffset();
		
		LoadLevel(data.currentLevel, data);
		ResetLevel(data);
		
		SetScreenOffset(x, y);
		
		SwitchToMode(status, AddingMode);
	end;
	
	procedure EndEdit(var status: EditorStatusType; var data: GameDataType);
	begin
{		if status.mode = AddingNewObstacles then
		begin
			FreeSprite(status.currentSprite);
			Dispose(status.currentObstacle);
			status.currentObstacle := nil;
			status.currentSprite := nil;
		end;
		
		if status.mode = AddingMidBack then
		begin
			FreeSprite(status.currentSprite);
		end;}
				
		ResetLevel(data);
		data.state := PlayingGameState;
	end;
		
	procedure CreateLevel(var data: GameDataType; level: Integer);
	var
		x, y: Single;
		worldOffset: Vector;
	begin
		x := MaxForegroundX(); //CalculateForegroundX(GameImage(Background).width - SCREEN_WIDTH) + ((SCREEN_WIDTH * 3) div 4);
		y := (SCREEN_HEIGHT - TOP_GAME_AREA) div 2 + TOP_GAME_AREA - _Templates[WarpHoleKind].Width div 2;
		
		data.currentLevel := level;
		worldOffset := CreateVector(x, y);
		
		AddSpriteKind(data, WarpHoleKind, worldOffset);		
		
		SaveLevel(data);
		FreeLevel(data);
	end;
	
	procedure LoadLevel(level: Integer; var data: GameDataType);
	var
		count, i: Integer;
		other: string;
		levelFile: Text;
	begin
		FreeLevel(data);
				
		if false = FileExists(LevelFilename(level)) then
		begin
			CreateLevel(data, level);
		end;
		
		data.currentLevel := level;
		
		Assign(levelFile, LevelFilename(level));
		Reset(levelFile);
		
		ReadLn(levelFile, count, other);
		SetLength(data.sprites, count);
		for i := Low(data.sprites) to High(data.sprites) do
		begin
			LoadSprite(data.sprites[i], levelFile);
		end;
		
{		LoadMidBacks(levelFile, data);
		
		temp := count;
		
		ReadLn(levelFile, count);
		SetLength(data.sprites, temp + count);	
		for i := temp to High(data.sprites) do
		begin
			LoadObstacleSprite(data.sprites[i], levelFile);
		end;}
		
{		ReadLn(levelFile, count);
		SetLength(data.sprites, count);
		for i := Low(data.sprites) to High(data.sprites) do
		begin
			LoadWarpHoleSprite(data.sprites[i], levelFile);
		end;

		temp := count;

		ReadLn(levelFile, count);
		SetLength(data.sprites, temp + count);
		for i := temp to High(data.sprites) do
		begin
			LoadSpriteOld(data.sprites[i], levelFile);
		end;
}
		Close(levelFile);
	end;

end.