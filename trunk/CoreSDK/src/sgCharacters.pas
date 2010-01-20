//=============================================================================
// sgAnimation.pas
//=============================================================================
//
// Version 0.1 -  
// 
//
// Change History:
//
// Version 0.1:
// - 2010-01-18: Created sgCharacters
//
//=============================================================================

unit sgCharacters;

//=============================================================================
interface
  uses sgTypes;
//=============================================================================

type
	DirectionAngles = record
		min : LongInt;
		max : LongInt;
	end;
	
	DirStateData = record
		Anim : LongInt;
		LayerOrder: Array of LongInt;
	end;
	
	Character = ^CharacterData;
	LayerCache = Array of Array of Array of LongInt;

	CharacterData = record
		CharSprite		 				: Sprite;
		CharName  						: String;
		CharType  						: String;
		States	 							: NamedIndexCollection;
		Directions						: NamedIndexCollection;
		CharValueNames 				: NamedIndexCollection;
		LastAngle 						: LongInt;
		CurrentState	 				: LongInt;
		CurrentDirection	 		: LongInt;
		DirAngles							: Array of DirectionAngles;
		CharValues						: Array of LongInt;
		DirectionParameters 	: Array of LongInt;
		ShownLayers						: Array of Boolean;
		ShownLayersByDirState	: Array of Array of DirStateData;
		ShownLayerCache				: LayerCache;
	end;
		
	
	function LoadCharacter(filename: String) : Character;
	procedure DrawCharacterWithStationary(var c: Character; state: Integer);
	function UpdateDirectionAnimation(var c: Character) : Boolean;
	function UpdateDirectionAnimationWithStationary(var c: Character; state, newState: integer) : Boolean;
	procedure HideShowLayer(c: Character; index: integer);

//=============================================================================
implementation
	uses
		crt, sgCore, sgAudio, sgAnimations, sgGeometry, sgResources,
		sgImages, sgInput, sgPhysics, sgNamedIndexCollection, sgShared,
		sgSprites, SysUtils, MyStrUtils, stringhash, StrUtils;
//=============================================================================	
	
	procedure SetActiveLayer(var c: Character);
	begin
		c^.CharSprite^.visibleLayers := c^.ShownLayerCache[c^.CurrentState, c^.CurrentDirection];
	end;

	function UpdateDirectionAnimationWithStationary(var c: Character; state, newState: integer) : Boolean;
	begin
		result := false;
		if (c^.CharSprite^.velocity.x = 0) AND (c^.CharSprite^.velocity.y = 0) then 
		begin
			if c^.CurrentState = state then exit;
			c^.CurrentState := state;
			SpriteStartAnimation(c^.CharSprite, c^.ShownLayersByDirState[c^.CurrentState, c^.CurrentDirection].Anim);
			result:= true;
		end
		else begin
			if c^.CurrentState <> newState then
			begin
				c^.CurrentState := newState;
				SetActiveLayer(c);
				SpriteStartAnimation(c^.CharSprite, c^.ShownLayersByDirState[c^.CurrentState, c^.CurrentDirection].Anim);
			end;
			result := UpdateDirectionAnimation(c);
		end;
	end;
	
	function UpdateShownLayerCache(c: Character): LayerCache;
	var
		states, directions, layers, count : Integer;
	begin
		SetLength(result, NameCount(c^.States), NameCount(c^.Directions));
		
		for states := Low(result) to High(result) do
		begin
			for directions := Low(result[states]) to High(result[states]) do
			begin
				count := 0;
				for layers := Low(c^.ShownLayersByDirState[states, directions].LayerOrder) to High(c^.ShownLayersByDirState[states, directions].LayerOrder) do
				begin
					if c^.ShownLayers[c^.ShownLayersByDirState[states, directions].LayerOrder[layers]] then
					begin
						SetLength(result[states,directions], Length(result[states,directions]) + 1);
						result[states,directions,count] := c^.ShownLayersByDirState[states, directions].LayerOrder[layers];
						count += 1;
					end;
				end;
			end;
		end;		
	end;

	function UpdateDirectionAnimation(var c: Character) : Boolean;
	var
		i : Integer;
		angle : single;
	begin
		angle := VectorAngle(c^.CharSprite^.velocity);
		result := false;
		for i := 0 to NameCount(c^.Directions) -1 do
		begin
			if (i = c^.CurrentDirection) then continue;
			
			if ((c^.DirAngles[i].min < c^.DirAngles[i].max) AND (angle >= c^.DirAngles[i].min) AND (angle <= c^.DirAngles[i].max)) OR 
			(((c^.DirAngles[i].min > 0) AND (c^.DirAngles[i].max < 0)) AND (((angle >= c^.DirAngles[i].min) AND (angle <= 180)) OR ((angle <= c^.DirAngles[i].max) AND (angle >= -180)))) then
			begin
				c^.CurrentDirection := i;
				SetActiveLayer(c);
				SpriteStartAnimation(c^.CharSprite, c^.ShownLayersByDirState[c^.CurrentState, c^.CurrentDirection].Anim);
				result := true;
			end
		end;
	end; 
	
	procedure HideShowLayer(c: Character; index: integer);
	begin
		c^.ShownLayers[index] := not c^.ShownLayers[index];
		c^.ShownLayerCache := UpdateShownLayerCache(c);
		SetActiveLayer(c);
	end;
	
	procedure SetCharacterName(var c: Character; name : String);
	begin
		c^.CharName := name;
	end;
	
	procedure SetCharacterType(var c: Character; name : String);
	begin
		c^.CharType := name;
	end;
	
	procedure SetCharacterCurrentState(var c: Character; state: Integer);
	begin
		c^.CurrentState := state;
	end;
	
	procedure SetCharacterCurrentDirection(var c: Character; direction: Integer);
	begin
		c^.CurrentDirection := direction;
	end;

	procedure FreeCharacter(c : character);
	begin
		Dispose(c);
	end;
	
	procedure DrawCharacterWithStationary(var c: character; state: integer);
	begin
		UpdateDirectionAnimationWithStationary(c,0, state);
		DrawSprite(c^.CharSprite);
	end;
	
	procedure DrawCharacter(c: character);
	begin
		DrawSprite(c^.CharSprite);
	end;

	function LoadCharacter(filename: String) : Character;
	var
		bmpArray: Array of Bitmap;
		data, line, id, path, name: string;
		lineno, maxid, w, h, cols, rows, count, i: integer;
		ShownLayers : Array of Boolean;
		
		function MyStrToInt(str: String; allowEmpty: Boolean) : LongInt;
		begin
			if allowEmpty and (Length(str) = 0) then
			begin
				result := -1;
			end
			else if not TryStrToInt(str, result) then
			begin
				result := 0;
				RaiseException('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. Value is not an integer : ' + str);
			end
			else if result < 0 then
			begin
				result := 0;
				RaiseException('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. Values should be positive : ' + str);
			end;
		end;
		
		procedure SetCellDetails();
		begin
			w 	 	:= StrToInt(ExtractDelimited(1, data, [',']));
			h 	 	:= StrToInt(ExtractDelimited(2, data, [',']));
			cols 	:= StrToInt(ExtractDelimited(3, data, [',']));
			rows 	:= StrToInt(ExtractDelimited(4, data, [',']));
			count := StrToInt(ExtractDelimited(5, data, [',']));
		end;
		
		procedure SetName();
		begin
			if Length(ExtractDelimited(2, line, [':'])) > 0 then SetCharacterName(result, ExtractDelimited(1, data, [',']));
		end;
		
		procedure SetType();
		begin
			if Length(ExtractDelimited(2, line, [':'])) > 0 then SetCharacterType(result, ExtractDelimited(1, data, [',']));
		end;
		
		procedure AddBitmapToCharacterSprite();
		var
			i: integer;
		begin
			SetLength(bmpArray, CountDelimiter(data, ',') + 1);
			
			for i := Low(bmpArray) to High(bmpArray) do
			begin
				bmpArray[i] := LoadBitmap(ExtractDelimited(i+1, data, [',']));
				SetTransparentColor(bmpArray[i], RGBColor(255,255,255));
				SetBitmapCellDetails(bmpArray[i], w, h, cols, rows, count);
			end;
			result^.CharSprite := CreateSprite(bmpArray);
		end;
		
		procedure AddValuesToCharacter();
		begin
			SetLength(result^.CharValues, Length(result^.CharValues) + 1);
			AddName(result^.CharValueNames, ExtractDelimited(1, data, [',']));
			result^.CharValues[High(result^.CharValues)] := StrToInt(ExtractDelimited(2, data, [',']));
		end;
		
		procedure SetDirections();
		var
			i: integer;
		begin
			result^.CurrentDirection := StrToInt(ExtractDelimited(1, data, [',']));
			InitNamedIndexCollection(result^.Directions);
			
			for i := 0 to StrToInt(ExtractDelimited(2, data, [','])) -1 do
			begin
				AddName(result^.Directions, ExtractDelimited(i + 3, data, [',']));
			end;
		end;
		
		procedure SetStates();
		var
			i: integer;
		begin
			result^.CurrentState := StrToInt(ExtractDelimited(1, data, [',']));
			InitNamedIndexCollection(result^.States);
			
			for i := 0 to StrToInt(ExtractDelimited(2, data, [','])) -1 do
			begin
				AddName(result^.States, ExtractDelimited(i + 3, data, [',']));
			end;
		end;
		
		procedure SetAngles();
		var
			i: integer;
		begin
			SetLength(result^.DirAngles, Length(result^.DirAngles) + 1);
			i := IndexOf(result^.Directions, ExtractDelimited(1, data, [',']));
			result^.DirAngles[i].min := StrToInt(ExtractDelimited(2, data, [',']));
			result^.DirAngles[i].max := StrToInt(ExtractDelimited(3, data, [',']));
		end;
		
		procedure SetDirectionStateDetails();
		var
			dirIndex, stateIndex, aniIndex: integer;
		begin
			stateIndex := IndexOf(result^.States, ExtractDelimited(1, data, [',']));
			dirIndex	 := IndexOf(result^.Directions, ExtractDelimited(2, data, [',']));
			
			if High(result^.ShownLayersByDirState) < stateIndex then SetLength(result^.ShownLayersByDirState, stateIndex + 1);
			if High(result^.ShownLayersByDirState[stateIndex]) < dirIndex then SetLength(result^.ShownLayersByDirState[stateIndex], dirIndex + 1);
						
			result^.ShownLayersByDirState[stateIndex,dirIndex].Anim := IndexOf(result^.CharSprite^.animationTemplate^.animationIDs, ExtractDelimited(3, data, [',']));
			result^.ShownLayersByDirState[stateIndex,dirIndex].LayerOrder := ProcessRange(ExtractDelimitedWithRanges(4, data));
		end;
		
		procedure SetShownLayersBooleans();
		var
			i, id: integer;
			draw: string;
		begin			
			SetLength(result^.ShownLayers, Length(bmpArray));
			for i := Low(bmpArray) to High(bmpArray) do
			begin
				draw := ExtractDelimited(i + 1, data, [',']);
				if draw = 't' then result^.ShownLayers[i] := true
				else if draw = 'f' then result^.ShownLayers[i] := false
				else begin
					RaiseException('Error at line ' + IntToStr(lineNo) + ' in character ' + filename + '. Error with ShownLayers: ' + ExtractDelimited(3 + i, data, [',']) + '. This should be one of f (false) or t (true)');
					exit;
				end; 
			end;
		end;
		
		procedure AddAniTemplateToChar();
		begin
			result^.CharSprite^.animationTemplate := LoadAnimationTemplate(ExtractDelimited(1, data, [',']));
		end;
		
		procedure InitializeAnimation();
		begin
			result^.CurrentDirection := IndexOf(result^.Directions, data);
			SpriteStartAnimation(result^.CharSprite, result^.ShownLayersByDirState[result^.CurrentState, result^.CurrentDirection].Anim);
		end;
			
		procedure ProcessLine();
		begin
			// Split line into id and data
			id := ExtractDelimited(1, line, [':']);
			data := ExtractDelimited(2, line, [':']);
			
			WriteLn(data);
			// Process based on id
			
			if Length(id) = 2 then SetDirectionStateDetails()
			
			else
			
				case LowerCase(id)[1] of // in all cases the data variable is read
					'n': SetName();
					't': SetType();
					'b': AddBitmapToCharacterSprite();
					'a': AddAniTemplateToChar();
					'd': SetDirections();
					's': SetStates();
					'v': AddValuesToCharacter();	
					'l': SetShownLayersBooleans();		
					'c': SetCellDetails();		
					'p': SetAngles();		
					'i': InitializeAnimation();									
	{				else
					begin
						RaiseException('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. Error with id: ' + id + '. This should be one of f,m,i or s.');
						exit;
					end; }
				end;
		end;
			
		procedure VerifyVersion();
		begin
			if EOF(input) then exit;
			line := '';
			
			while (Length(line) = 0) or (MidStr(line,1,2) = '//') do
			begin
				ReadLn(input, line);
				line := Trim(line);
			end;
			
			//Verify that the line has the right version
			if line <> 'SwinGame Character #v1' then 
				RaiseException('Error in character ' + filename + '. Character files must start with "SwinGame Character #v1"');
			
		end;
	begin
		{$IFDEF TRACE}
			TraceEnter('sgCharacters', 'LoadCharacter');
		{$ENDIF}
		
		path := PathToResource('characters\' + filename);
		lineNo := 0;
		
		Assign(input, path);
		Reset(input);
		
		VerifyVersion();
		
		New(result);
		
		InitNamedIndexCollection(result^.CharValueNames);
				
		try
			while not EOF(input) do
			begin	

				lineNo := lineNo + 1;
				ReadLn(input, line);
				line := Trim(line);
				if Length(line) = 0 then continue;  //skip empty lines
				if MidStr(line,1,2) = '//' then continue; //skip lines starting with //

				ProcessLine();
			end;
		
		finally
			Close(input);
		end;
		
		result^.ShownLayerCache := UpdateShownLayerCache(result);
		SetActiveLayer(result);
		
		{$IFDEF TRACE}
			TraceExit('sgCharacters', 'LoadCharacter');
		{$ENDIF}  
	end;

end.
	