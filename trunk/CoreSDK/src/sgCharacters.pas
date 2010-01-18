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
	CharacterData = record
		CharSprite: Sprite;
		States: NamedIndexCollection;
		Directions: NamedIndexCollection;
		CurrentState: LongInt;
		CurrentDirection: LongInt;
		DrawOrder: Array of Array of LongInt;
		DrawLayer: Array of Array of Boolean;
		CharValueNames: NamedIndexCollection;
		CharValues: Array of LongInt;
		DirectionParameters: Array of LongInt;
		CharName : String;
		CharType: String;
	end;
	
	Character = ^CharacterData;
	
	function LoadCharacter(filename: String) : Character;
	procedure DrawCharacter(c: Character);

//=============================================================================
implementation
	  uses
    SysUtils, StrUtils, Classes, 
    stringhash, MyStrUtils, sgNamedIndexCollection,   // libsrc
    sgShared, sgResources, sgTrace, sgAudio, sgImages, sgSprites, sgAnimations;
//=============================================================================	
{
	function SetupDirectionParameters() : Array of LongInt;
	begin
	end; }
	

	
	procedure HideShowLayer(c: Character; layerID: string; show: boolean);
	begin
		c^.DrawLayer[(c^.CurrentDirection * NameCount(c^.States) + c^.CurrentState) ,IndexOf(c^.CharSprite^.layerIDs, layerID)] := show;
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
	
	procedure SetActiveLayer(c : Character);
	var
		drawIndex, i, count: integer;
		newDrawOrder : Array of LongInt;
	begin
		drawIndex := c^.CurrentDirection * NameCount(c^.States) + c^.CurrentState;
		count := 0;
		
		for i := Low(c^.DrawOrder[drawIndex]) to High(c^.DrawOrder[drawIndex]) do
		begin
			if c^.DrawLayer[drawIndex,i] then
			begin
				SetLength(newDrawOrder, Length(newDrawOrder) + 1);
				newDrawOrder[count] := c^.DrawOrder[drawIndex,i];
				count += 1;
			end;
		end;		
		c^.CharSprite^.visibleLayers := newDrawOrder;
	end;

	procedure FreeCharacter(c : character);
	begin
		Dispose(c);
	end;
	
	procedure DrawCharacter(c: character);
	begin
		DrawSprite(c^.CharSprite);
	end;

	function LoadCharacter(filename: String) : Character;
	var
		bmpArray: Array of Bitmap;
		data, line, id, path, name: string;
		lineno, maxid: integer;
		
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
			SetLength(bmpArray, StrToInt(ExtractDelimited(1, data, [','])));
			for i := 0 to StrToInt(ExtractDelimited(1, data, [','])) - 1 do
			begin
				WriteLn(ExtractDelimited(i + 2, data, [',']));
				bmpArray[i] := LoadBitmap(ExtractDelimited(i + 2, data, [',']));
				WriteLn(ExtractDelimited(i + 2, data, [',']));
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
				SetLength(result^.DrawOrder, Length(result^.DrawOrder) + 1);
				SetLength(result^.DrawLayer, Length(result^.DrawLayer) + 1);
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
				SetLength(result^.DrawOrder, Length(result^.DrawOrder) + 1);
				SetLength(result^.DrawLayer, Length(result^.DrawLayer) + 1);
			end;
		end;
		
		procedure SetDrawOrder();
		var
			drawRange: string;
			id : integer;
		begin
			drawRange := ExtractDelimited(2, data, [',']);
			id := IndexOf(result^.Directions, ExtractDelimited(2, data, [',']));
			
			if id <> -1 then result^.DrawOrder[id] := ProcessRange(ExtractDelimitedWithRanges(1, drawRange));

		end;
		
		procedure SetDrawLayerBooleans();
		var
			i, id: integer;
			draw: string;
		begin			
			id := IndexOf(result^.Directions, ExtractDelimited(2, data, [',']));
			if id <> -1 then
			begin				
				for i := 0 to Length(bmpArray) -1 do
				begin
					draw := ExtractDelimited(3 + i, data, [',']);
					SetLength(id, Length(result^.DrawLayer[id]) + 1);
					if draw = 't' then result^.DrawLayer[id,i] := true
					else if draw = 'f' then result^.DrawLayer[id,i] := false
					else begin
						RaiseException('Error at line ' + IntToStr(lineNo) + ' in character ' + filename + '. Error with DrawLayer: ' + ExtractDelimited(3 + i, data, [',']) + '. This should be one of f (false) or t (true)');
						exit;
					end; 
				end;
			end;
		end;
		
		procedure AddAniTemplateToChar();
		begin
			result^.CharSprite^.animationTemplate := LoadAnimationTemplate(ExtractDelimited(1, data, [',']));
		end;
			
		procedure ProcessLine();
		begin
			// Split line into id and data
			id := ExtractDelimited(1, line, [':']);
			data := ExtractDelimited(2, line, [':']);
			
			// Verify that id is a single char
			if Length(id) <> 1 then
			begin
				RaiseException('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. Error with id: ' + id + '. This should be a single character.');
				exit;
			end;
			WriteLn(data);
			// Process based on id
			case LowerCase(id)[1] of // in all cases the data variable is read
				'n': SetName();
				't': SetType();
				'b': AddBitmapToCharacterSprite();
				'a': AddAniTemplateToChar();
				'd': SetDirections();
				's': SetStates();
				'v': AddValuesToCharacter();				
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
		maxId := -1;
//		result := nil;
		
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
		{$IFDEF TRACE}
			TraceExit('sgCharacters', 'LoadCharacter');
		{$ENDIF}  
	end;

end.
	