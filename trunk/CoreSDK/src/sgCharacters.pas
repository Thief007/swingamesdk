//=============================================================================
// sgCharacters.pas
//=============================================================================
//
// Version 0.2 -  
// 
//
// Change History:
// Version 0.2:
// - 2010-01-28:	David : Added functions and procedures to the interface section
//												of the code.
//
// Version 0.1:
// - 2010-01-20:  David : Added functions and procedures to access record data
// - 2010-01-20:  David : Created Angle functions, show/hide layer functions
// - 2010-01-19:  David : Created LoadCharacter function
// - 2010-01-18:  David : Created sgCharacters
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
    Anim      : LongInt;
    LayerOrder: Array of LongInt;
  end;
  
  Character  = ^CharacterData;
  LayerCache = Array of Array of Array of LongInt;

  CharacterData = record
    CharSprite            : Sprite;
    CharName              : String;
    CharType              : String;
    States                : NamedIndexCollection;
    Directions            : NamedIndexCollection;
    CharValueNames        : NamedIndexCollection;
    LastAngle             : LongInt;
    CurrentState          : LongInt;
    CurrentDirection      : LongInt;
    DirectionParameters   : Array of DirectionAngles;
    CharValues            : Array of LongInt;
    ShownLayers           : Array of Boolean;
    ShownLayersByDirState : Array of Array of DirStateData;
    ShownLayerCache       : LayerCache;
  end;
	
  //---------------------------------------------------------------------------
  // Character Name and Type
  //---------------------------------------------------------------------------   
  procedure CharacterSetName(c: Character; name : String);
	function CharacterName(c: Character) : String;
	function CharacterType(c: Character): String;
	
	//---------------------------------------------------------------------------
  // Character Directions and States
  //--------------------------------------------------------------------------- 
  procedure CharacterSetCurrentState(c: Character; state: Integer); 
	procedure CharacterSetDirectionState(c: Character; direction: Integer);
	function CharacterCurrentState(c: Character): LongInt;
	function CharacterCurrentDirection(c: Character): LongInt;
	function CharacterDirections(c: Character) : NamedIndexCollection;
	function CharacterStates(c: Character) : NamedIndexCollection;
	
	//---------------------------------------------------------------------------
  // Character Angles
  //--------------------------------------------------------------------------- 
	function CharacterLastAngle(c: Character): LongInt;  
  function CharacterAngleAt(c: Character; index : integer): DirectionAngles;  
  function CharacterAnglesLength(c: Character): LongInt;
  function CharacterAngleMinAt(c: Character; index : integer): LongInt;
  function CharacterAngleMaxAt(c: Character; index : integer): LongInt;
  function CharacterShownLayersAt(c: Character; index: integer) : Boolean;
  procedure CharacterSetCurrentDirection(c: Character; direction: Integer);
	
	  //---------------------------------------------------------------------------
  // Character Values
  //---------------------------------------------------------------------------   
  function CharacterValueNames(c: Character) : NamedIndexCollection; 
  function CharacterValueAt(c: Character; index: Integer): LongInt;  
  function CharacterValueCount(c: Character): LongInt;
  
  //---------------------------------------------------------------------------
  // Character Sprite
  //---------------------------------------------------------------------------    
  function CharacterSprite(c: Character) : Sprite;  
	
 //---------------------------------------------------------------------------
  // Handle Character Layers
  //--------------------------------------------------------------------------- 
  
  procedure SetActiveLayer(var c: Character);
  function UpdateShownLayerCache(c: Character): LayerCache;  	
	
  //Update the animation of the character depending on its direction
  function UpdateDirectionAnimation(c: Character) : Boolean;
  
  //Update the animation of the character depending on its direction, including updating
  //When the character's state goes stationary
  function UpdateDirectionAnimationWithStationary(c: Character; state, newState: integer) : Boolean;
  
  //Inverts the boolean of Showlayer for the layer at the index
  procedure ToggleLayerVisibility(c: Character; index: integer);

  //---------------------------------------------------------------------------
  // Handle Character Drawing
  //---------------------------------------------------------------------------   
  
  //Draw Character that changes state when its velocity is 0
  procedure DrawCharacterWithStationary(c: Character; stationaryState, state: Integer);
  
  //Draw Character without a stationary state with default facing down when not moving
  procedure DrawCharacter(c: Character);
	
  //---------------------------------------------------------------------------
  // Load and Free Character
  //--------------------------------------------------------------------------- 
	
  //Loads Character from TextFile
  function LoadCharacter(filename: String) : Character;
    
  //Frees the pointer to the character and it's sprite
  procedure FreeCharacter(var c: Character);

//=============================================================================
implementation
  uses
    sgCore, sgAnimations, sgGeometry, sgResources,
    sgImages, sgNamedIndexCollection, sgShared,
    sgSprites, SysUtils, MyStrUtils, StrUtils;
//============================================================================= 

  //---------------------------------------------------------------------------
  // Character Name and Type
  //--------------------------------------------------------------------------- 

  procedure CharacterSetName(c: Character; name : String);
  begin
    c^.CharName := name;
  end;
  
  function CharacterName(c: Character) : String;
  begin
    result := c^.CharName;
  end;
  
  procedure CharacterSetType(c: Character; name : String);
  begin
    c^.CharType := name;
  end;
  
  function CharacterType(c: Character): String;
  begin
    result := c^.CharType;
  end;

  //---------------------------------------------------------------------------
  // Character Directions and States
  //--------------------------------------------------------------------------- 
  
  procedure CharacterSetCurrentState(c: Character; state: Integer);
  begin
    c^.CurrentState := state;
  end;
  
  procedure CharacterSetDirectionState(c: Character; direction: Integer);
  begin
    c^.CurrentState := direction;
  end;
  
  function CharacterCurrentState(c: Character): LongInt;
  begin
    result := c^.CurrentState;
  end;
  
  function CharacterCurrentDirection(c: Character): LongInt;
  begin
    result := c^.CurrentDirection;
  end;
  
  function CharacterDirections(c: Character) : NamedIndexCollection;
  begin
    result := c^.Directions;
  end;
  
  function CharacterStates(c: Character) : NamedIndexCollection;
  begin
    result := c^.States;
  end;
  
  //---------------------------------------------------------------------------
  // Character Angles
  //--------------------------------------------------------------------------- 
    
  function CharacterLastAngle(c: Character): LongInt;
  begin
    result := c^.LastAngle;
  end;
  
  function CharacterAngleAt(c: Character; index : integer): DirectionAngles;
  begin
    result := c^.DirectionParameters[index];
  end;
  
  function CharacterAnglesLength(c: Character): LongInt;
  begin
    result := Length(c^.DirectionParameters);
  end;
  
  function CharacterAngleMinAt(c: Character; index : integer): LongInt;
  begin
    result := c^.DirectionParameters[index].min;
  end;
  
  function CharacterAngleMaxAt(c: Character; index : integer): LongInt;
  begin
    result := c^.DirectionParameters[index].max;
  end;
  
  function CharacterShownLayersAt(c: Character; index: integer) : Boolean;
  begin
    result := c^.ShownLayers[index];
  end;
  
  procedure CharacterSetCurrentDirection(c: Character; direction: Integer);
  begin
    c^.CurrentDirection := direction;
  end;

  //---------------------------------------------------------------------------
  // Character Values
  //---------------------------------------------------------------------------   

  function CharacterValueNames(c: Character) : NamedIndexCollection;
  begin
    result := c^.CharValueNames;
  end;
  
  function CharacterValueAt(c: Character; index: Integer): LongInt;
  begin
    result := c^.CharValues[index];
  end;
  
  function CharacterValueCount(c: Character): LongInt;
  begin
    result := Length(c^.CharValues);
  end;
  
  //---------------------------------------------------------------------------
  // Character Sprite
  //---------------------------------------------------------------------------   
  
  function CharacterSprite(c: Character) : Sprite;
  begin
    result := c^.CharSprite;
  end;
  
    procedure FreeCharacter(var c : character);
  begin
    if not assigned(c) then exit;
    
    FreeSprite(c^.charSprite);
    Dispose(c);
    c := nil;
  end;
  
  //---------------------------------------------------------------------------
  // Handle Character Layers
  //--------------------------------------------------------------------------- 
  
  procedure SetActiveLayer(var c: Character);
  begin
    c^.CharSprite^.visibleLayers := c^.ShownLayerCache[c^.CurrentState, c^.CurrentDirection];
  end;

  function UpdateDirectionAnimationWithStationary(c: Character; state, newState: integer) : Boolean;
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
  
  function UpdateDirectionAnimation(c: Character) : Boolean;
  var
    i : Integer;
    angle : single;
  begin
    angle := VectorAngle(c^.CharSprite^.velocity);
    result := false;
    for i := 0 to NameCount(c^.Directions) -1 do
    begin
      if (i = c^.CurrentDirection) then continue;
      
      if ((c^.DirectionParameters[i].min < c^.DirectionParameters[i].max) AND (angle >= c^.DirectionParameters[i].min) AND (angle <= c^.DirectionParameters[i].max)) OR 
      (((c^.DirectionParameters[i].min > 0) AND (c^.DirectionParameters[i].max < 0)) AND (((angle >= c^.DirectionParameters[i].min) AND (angle <= 180)) OR ((angle <= c^.DirectionParameters[i].max) AND (angle >= -180)))) then
      begin
        c^.CurrentDirection := i;
        SetActiveLayer(c);
        SpriteStartAnimation(c^.CharSprite, c^.ShownLayersByDirState[c^.CurrentState, c^.CurrentDirection].Anim);
        result := true;
      end
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
  
  procedure ToggleLayerVisibility(c: Character; index: integer);
  begin
    c^.ShownLayers[index] := not c^.ShownLayers[index];
    c^.ShownLayerCache := UpdateShownLayerCache(c);
    SetActiveLayer(c);
  end;
  

  //---------------------------------------------------------------------------
  // Handle Character Drawing
  //---------------------------------------------------------------------------   
  
  procedure DrawCharacterWithStationary(c: character; stationaryState, state: integer);
  begin
    UpdateDirectionAnimationWithStationary(c, stationaryState, state);
    DrawSprite(c^.CharSprite);
  end;
  
  procedure DrawCharacter(c: character);
  begin
    UpdateDirectionAnimation(c);
    DrawSprite(c^.CharSprite);
  end;
  
  //---------------------------------------------------------------------------
  // Load Character
  //--------------------------------------------------------------------------- 

  function LoadCharacter(filename: String) : Character;
  var
    bmpArray: Array of Bitmap;
    data, line, id, path: string;
    lineno, w, h, cols, rows, count, colliIndex: integer;
    aniTemp: AnimationTemplate;
    bmpIDs: NamedIndexCollection;
    
    procedure SetCellDetails();
    begin
      w     := StrToInt(ExtractDelimited(1, data, [',']));
      h     := StrToInt(ExtractDelimited(2, data, [',']));
      cols  := StrToInt(ExtractDelimited(3, data, [',']));
      rows  := StrToInt(ExtractDelimited(4, data, [',']));
      count := StrToInt(ExtractDelimited(5, data, [',']));
    end;
    
    procedure SetName();
    begin
      if Length(ExtractDelimited(2, line, [':'])) > 0 then CharacterSetName(result, ExtractDelimited(1, data, [',']));
    end;
    
    procedure SetType();
    begin
      if Length(ExtractDelimited(2, line, [':'])) > 0 then CharacterSetType(result, ExtractDelimited(1, data, [',']));
    end;
    
    procedure AddBitmapToCharacterSprite();
    var
      i: integer;
    begin
      SetLength(bmpArray, Length(bmpArray) + 1);
      
      AddName(bmpIDs,ExtractDelimited(1, data, [',']));
      bmpArray[High(bmpArray)] := LoadBitmap(ExtractDelimited(2, data, [',']));
      SetTransparentColor(bmpArray[High(bmpArray)], RGBColor(255,255,255));
      SetBitmapCellDetails(bmpArray[High(bmpArray)], w, h, cols, rows, count);
      
      if ((CountDelimiter(data,',') = 3) AND (ExtractDelimited(1, data, [',']) = 't')) then colliIndex := High(bmpArray);
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
      SetLength(result^.DirectionParameters, Length(result^.DirectionParameters) + 1);
      i := IndexOf(result^.Directions, ExtractDelimited(1, data, [',']));
      result^.DirectionParameters[i].min := StrToInt(ExtractDelimited(2, data, [',']));
      result^.DirectionParameters[i].max := StrToInt(ExtractDelimited(3, data, [',']));
    end;
    
    procedure SetDirectionStateDetails();
    var
      dirIndex, stateIndex: integer;
    begin
      stateIndex := IndexOf(result^.States, ExtractDelimited(1, data, [',']));
      dirIndex   := IndexOf(result^.Directions, ExtractDelimited(2, data, [',']));
      
      if High(result^.ShownLayersByDirState) < stateIndex then SetLength(result^.ShownLayersByDirState, stateIndex + 1);
      if High(result^.ShownLayersByDirState[stateIndex]) < dirIndex then SetLength(result^.ShownLayersByDirState[stateIndex], dirIndex + 1);
            
      result^.ShownLayersByDirState[stateIndex,dirIndex].Anim := IndexOf(aniTemp^.animationIDs, ExtractDelimited(3, data, [',']));
      result^.ShownLayersByDirState[stateIndex,dirIndex].LayerOrder := ProcessRange(ExtractDelimitedWithRanges(4, data));
    end;
    
    procedure SetShownLayersBooleans();
    var
      i: integer;
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
      aniTemp := LoadAnimationTemplate(ExtractDelimited(1, data, [',']));
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
          else
          begin
            RaiseException('Error at line ' + IntToStr(lineNo) + ' in animation ' + filename + '. Error with id: ' + id + '. This should be one of n, t, b, a, d, s, v, l, c, p, i, sd.');
            exit;
          end; 
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
    
    SetLength(bmpArray, 0);
    
    {$IFDEF UNIX}
      path := PathToResource('characters/' + filename);
    {$ELSE}
      path := PathToResource('characters\' + filename);
    {$ENDIF}
    lineNo := 0;
    
    Assign(input, path);
    Reset(input);
    
    VerifyVersion();
    
    New(result);
    
    InitNamedIndexCollection(result^.CharValueNames);
    InitNamedIndexCollection(bmpIDs);
    colliIndex := -1;
        
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
    
    WriteLn('Creating Sprite');
    result^.CharSprite := CreateSprite(bmpArray, NamesOf(bmpIDs));
    result^.CharSprite^.animationTemplate := aniTemp;
    if colliIndex <> -1 then SpriteSetCollisionBitmap(result^.CharSprite,bmpArray[colliIndex]);
    result^.ShownLayerCache := UpdateShownLayerCache(result);
    SetActiveLayer(result);
    
    {$IFDEF TRACE}
      TraceExit('sgCharacters', 'LoadCharacter');
    {$ENDIF}  
  end;

end.
  