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
    Name                  : String;
    FileName              : String;
    CharSprite            : Sprite;                               // The Character's Sprite
    CharName              : String;                               // The Character's Name
    CharType              : String;                               // The Character's Type
    States                : NamedIndexCollection;                 // The names and indexs of the Character's States
    Directions            : NamedIndexCollection;                 // The names and indexs of the Character's Direction
    CurrentState          : LongInt;                              // The Character's Current State
    CurrentDirection      : LongInt;                              // The Character's Current Direction
    DirectionParameters   : Array of DirectionAngles;             // The different angle parameters the character checks to change the animation based on the direction
    ShownLayers           : Array of Boolean;                     // Boolean stating whether a layer is to be drawn
    ShownLayersByDirState : Array of Array of DirStateData;       // 
    ShownLayerCache       : LayerCache;                           // The Character's Sprite
  end;
	
  ///---------------------------------------------------------------------------
  /// Character Name and Type
  ///--------------------------------------------------------------------------- 
  
  /// Sets the Character's name
  ///
  /// @lib
  /// @sn character:%s setName:%s
  ///
  /// @class Character
  /// @setter Name
  procedure CharacterSetName(c: Character; name : String);  
  
  /// Returns the Character's name
  ///
  /// @lib
  ///
  /// @class Character
  /// @getter Name
	function CharacterCharacterName(c: Character) : String;
  
  /// Sets the the name of the Type of Character(eg boss, grunt etc)
  ///
  /// @lib
  /// @sn character:%s setType:%s
  ///
  /// @class Character
  /// @setter CharacterType
  procedure CharacterSetType(c: Character; name : String);
  
  /// Returns the string value of the character's type
  ///
  /// @lib
  ///
  /// @class Character
  /// @getter CharacterType
	function CharacterType(c: Character): String;
	
	//---------------------------------------------------------------------------
  // Character Directions and States
  //--------------------------------------------------------------------------- 
    
  /// Sets the current state of the character
  ///
  /// @lib
  /// @sn character:%s setState:%s
  ///
  /// @class Character
  /// @setter CurrentState
  procedure CharacterSetCurrentState(c: Character; state: Integer); 
    
  /// Sets the current direction of the character
  ///
  /// @lib
  /// @sn character:%s setDirection:%s
  ///
  /// @class Character
  /// @method CurrentDirection  
  procedure CharacterSetCurrentDirection(c: Character; direction: Integer);
    
  /// Returns the index of the current state of the character
  ///
  /// @lib
  ///
  /// @class Character
  /// @getter CurrentState
	function CharacterCurrentState(c: Character): LongInt;
    
  /// Returns the index of the current direction of the character
  ///
  /// @lib
  ///
  /// @class Character
  /// @getter CurrentDirection
	function CharacterCurrentDirection(c: Character): LongInt;
  
  /// Returns the count of the amount of directions that the character has
  ///
  /// @lib
  ///
  /// @class Character
  /// @getter DirectionCount
  function CharacterDirectionCount(c: Character): LongInt;
    
  /// Returns all of the possible directions of the character
  ///
  /// @lib
  ///
  /// @class Character
  /// @getter Directions
  /// @length CharacterDirectionsCount
	function CharacterDirections(c: Character) : StringArray;
       
  /// Returns all of the possible states of the character
  ///
  /// @lib
  ///
  /// @class Character
  /// @getter StateCount
  function CharacterStateCount(c: Character): LongInt;
    
  /// Returns all of the possible directions of the character
  ///
  /// @lib
  ///
  /// @class Character
  /// @getter States
  /// @length CharacterStateCount
	function CharacterStates(c: Character) : StringArray;
	
	//---------------------------------------------------------------------------
  // Character Angles
  //---------------------------------------------------------------------------    
      
  /// Returns the DirectionAngles data at the selected index. The min and max
  /// of this record can be accessed by .min and .max
  ///
  /// @lib
  /// @sn character:%s idx:%s
  ///
  /// @class Character
  /// @method AngleAt
  function CharacterAngleAt(c: Character; index : integer): DirectionAngles;  
        
  /// Returns the count of the Angles of the character
  ///
  /// @lib
  ///
  /// @class Character
  /// @getter AngleLength
  function CharacterAnglesLength(c: Character): LongInt;    
  
  /// Returns the minimum angle in the DirectionAngles record at the index
  /// specified
  ///
  /// @lib
  /// @sn character:%s idx:%s
  ///
  /// @class Character
  /// @method AngleMinAt
  function CharacterAngleMinAt(c: Character; index : integer): LongInt;
    
  /// Returns the maximum angle in the DirectionAngles record at the index
  /// specified
  ///
  /// @lib
  /// @sn character:%s idx:%s
  ///
  /// @class Character
  /// @method AngleMaxAt
  function CharacterAngleMaxAt(c: Character; index : integer): LongInt;
	
	//---------------------------------------------------------------------------
  // Character Values
  //---------------------------------------------------------------------------     
    
  /// Returns the count of character values
  ///
  /// @lib
  ///
  /// @class Character
  /// @getter ValueCount 
  function CharacterValueCount(c: Character) : LongInt;
  
  /// Returns the names of all of the values of the character
  ///
  /// @lib
  ///
  /// @class Character
  /// @getter ValueNames
  /// @length CharacterValueCount
  function CharacterValueNames(c: Character) : StringArray; 
    
  /// Returns the character's value at the index specified
  ///
  /// @lib
  /// @sn character:%s valueIndex:$s
  ///
  /// @class Character
  /// @method Value
  function CharacterValueAt(c: Character; index: Integer): Single;
  
  //---------------------------------------------------------------------------
  // Character Sprite
  //---------------------------------------------------------------------------   
    
  /// Returns the character's sprite
  ///
  /// @lib
  ///
  /// @class Character
  /// @getter CharSprite  
  function CharacterSprite(c: Character) : Sprite;  
	
 //---------------------------------------------------------------------------
  // Handle Character Layers
  //--------------------------------------------------------------------------- 
      
  /// Sets the active layers from the shown layers cache, using the current
  /// states and directions for the indexes of the array
  ///
  /// @lib
  ///
  /// @class Character
  /// @method ActiveLayer  
  procedure SetActiveLayer(var c: Character);
	
  /// Returns a new LayerCache. This is usually used when the values of the layers change
  /// such as the order or visibility
  ///
  /// @lib
  ///
  /// @class Character
  /// @method UpdateDirectionAnimation 
  function UpdateShownLayerCache(c: Character): LayerCache;  	
	
  /// Update the animation of the character depending on its direction. Returns true
  /// if the direction was changed and false if it was no changed
  ///
  /// @lib
  ///
  /// @class Character
  /// @method UpdateDirectionAnimation 
  function UpdateDirectionAnimation(c: Character) : Boolean;
  
  ///Update the animation of the character depending on its direction, including updating
  ///When the character's state goes stationary
  ///
  /// @lib
  /// @sn character:%s stationaryState:%s newState:%s
  ///
  /// @class Character
  /// @method UpdateDirectionAnimationWithStationary
  /// @csn stationaryState:%s newState:%s
  function UpdateDirectionAnimationWithStationary(c: Character; state, newState: integer) : Boolean;
  
  /// Toggles whether or not the layer at the specified index is drawn or not
  ///
  /// @lib
  /// @sn character:%s idx:%s
  ///
  /// @class Character
  /// @method ToggleVisibility 
  procedure ToggleLayerVisibility(c: Character; index: integer);
      
  /// Returns whether or not the layer at the selected index is drawn
  ///
  /// @lib
  ///
  /// @class Character
  /// @method LayerShownAt 
  function CharacterShownLayersAt(c: Character; index: integer) : Boolean;

  //---------------------------------------------------------------------------
  // Handle Character Drawing
  //---------------------------------------------------------------------------   
  
  /// Draw Character that changes state when its velocity is 0 to be the stationary
  /// state which is specified.
  ///
  /// @lib
  /// @sn character:%s stationaryState:%s state:%s
  ///
  /// @class Character
  /// @method DrawCharacterWithStationary
  /// @csn stationaryState:%s state:%s
  procedure DrawCharacterWithStationary(c: Character; stationaryState, state: Integer);
  
  /// Draw Character without a stationary state with default facing down when not moving   
  ///
  /// @lib
  ///
  /// @class Character
  /// @method DrawCharacter
  procedure DrawCharacter(c: Character);
   
  /// Draws the character's sprite with no additional functionality
  ///
  /// @lib
  ///
  /// @class Character
  /// @method DrawCharacterSprite
  procedure DrawCharacterSprite(c: character);
	
  //---------------------------------------------------------------------------
  // Load and Free Character
  //--------------------------------------------------------------------------- 
	    
  /// Loads the character from a text file
  ///
  /// @lib
  ///
  /// @class Character
  /// @method LoadCharacter
  function MapCharacter(name, filename: String): Character;
  function LoadCharacter(filename: String): Character;
  function DoLoadCharacter(filename, name: String): Character;
  procedure DoFreeCharacter(var c: Character);
  procedure FreeCharacter(var c: Character);
  procedure ReleaseCharacter(name: String);
  procedure ReleaseAllCharacters();
  function CharacterFilename(c: Character): String;  
  function CharacterName(c: Character): String;
  function CharacterNamed(name: String): Character;
  function HasCharacter(name: String): Boolean;
    
  /// Frees the Characrer, as well as calling FreeSprite on the Character's Sprite
  ///
  /// @lib
  ///
  /// @class Character
  /// @method FreeCharacter
 // procedure FreeCharacter(var c: Character);

//=============================================================================
implementation
  uses
    sgCore, sgAnimations, sgGeometry, sgResources, stringHash,
    sgImages, sgNamedIndexCollection, sgShared,
    sgSprites, SysUtils, sgUtils, StrUtils;
//============================================================================= 

  var
    _Characters : TStringHash;
  //---------------------------------------------------------------------------
  // Character Name and Type
  //--------------------------------------------------------------------------- 

  procedure CharacterSetName(c: Character; name : String);
  begin
		if not Assigned(c) then exit;
    c^.CharName := name;
  end;
  
  function CharacterCharacterName(c: Character) : String;
  begin
		if not Assigned(c) then exit;
    result := c^.CharName;
  end;
  
  procedure CharacterSetType(c: Character; name : String);
  begin
		if not Assigned(c) then exit;
    c^.CharType := name;
  end;
  
  function CharacterType(c: Character): String;
  begin
		if not Assigned(c) then exit;
    result := c^.CharType;
  end;

  //---------------------------------------------------------------------------
  // Character Directions and States
  //--------------------------------------------------------------------------- 
  
  procedure CharacterSetCurrentState(c: Character; state: Integer);
  begin
		if not Assigned(c) then exit;
    c^.CurrentState := state;
  end;
  
  function CharacterCurrentState(c: Character): LongInt;
  begin
		if not Assigned(c) then exit;
    result := c^.CurrentState;
  end;
  
  function CharacterCurrentDirection(c: Character): LongInt;
  begin
		if not Assigned(c) then exit;
    result := c^.CurrentDirection;
  end;
  
  procedure CharacterSetCurrentDirection(c: Character; direction: Integer);
  begin
		if not Assigned(c) then exit;
    c^.CurrentDirection := direction;
  end;
  
  function CharacterDirectionCount(c: Character) : LongInt;
  begin
		if not Assigned(c) then exit;
    result := NameCount(c^.Directions);
  end;
  
  function CharacterDirections(c: Character) : StringArray;
  begin
		if not Assigned(c) then exit;
    result := c^.Directions.names;
  end;
  
  function CharacterStateCount(c: Character) : LongInt;
  begin
		if not Assigned(c) then exit;
    result := NameCount(c^.States);
  end;
  
  function CharacterStates(c: Character) : StringArray;
  begin
		if not Assigned(c) then exit;
    result := c^.States.names;
  end;
  
  //---------------------------------------------------------------------------
  // Character Angles
  //--------------------------------------------------------------------------- 
      
  function CharacterAngleAt(c: Character; index : integer): DirectionAngles;
  begin
		if not Assigned(c) then exit;
    result := c^.DirectionParameters[index];
  end;
  
  function CharacterAnglesLength(c: Character): LongInt;
  begin
		if not Assigned(c) then exit;
    result := Length(c^.DirectionParameters);
  end;
  
  function CharacterAngleMinAt(c: Character; index : integer): LongInt;
  begin
		if not Assigned(c) then exit;
    result := c^.DirectionParameters[index].min;
  end;
  
  function CharacterAngleMaxAt(c: Character; index : integer): LongInt;
  begin
		if not Assigned(c) then exit;
    result := c^.DirectionParameters[index].max;
  end;

  //---------------------------------------------------------------------------
  // Character Values
  //---------------------------------------------------------------------------   
  
  function CharacterValueNames(c: Character) : StringArray;
  begin
		if not Assigned(c) then exit;
    result := c^.CharSprite^.valueIds.names;
  end;
  
  function CharacterValueAt(c: Character; index: Integer): Single;
  begin
		if not Assigned(c) then exit;
    result := c^.CharSprite^.values[index];
  end;
  
  function CharacterValueCount(c: Character): LongInt;
  begin
		if not Assigned(c) then exit;
    result := NameCount(c^.CharSprite^.valueIds);
  end;
  
  //---------------------------------------------------------------------------
  // Character Sprite
  //---------------------------------------------------------------------------   
  
  function CharacterSprite(c: Character) : Sprite;
  begin
		if not Assigned(c) then exit;
    result := c^.CharSprite;
  end;
 { 
    procedure FreeCharacter(var c : character);
  begin
    if not assigned(c) then exit;
    
    FreeSprite(c^.charSprite);
    Dispose(c);
    c := nil;
  end;
  }
  //---------------------------------------------------------------------------
  // Handle Character Layers
  //--------------------------------------------------------------------------- 
  
  // Setting the visibile layers array in the sprite to match the array of the shown layer cache
  procedure SetActiveLayer(var c: Character);
  begin
		if not Assigned(c) then exit;
    c^.CharSprite^.visibleLayers := c^.ShownLayerCache[c^.CurrentState, c^.CurrentDirection];
  end;
  
  function CharacterShownLayersAt(c: Character; index: integer) : Boolean;
  begin
		if not Assigned(c) then exit;
    result := c^.ShownLayers[index];
  end;
   
  function UpdateDirectionAnimationWithStationary(c: Character; state, newState: integer) : Boolean;
  begin
		if not Assigned(c) then exit;
    result := false;
    if (c^.CharSprite^.velocity.x = 0) AND (c^.CharSprite^.velocity.y = 0) then  // Check if Character is moving
    begin
      if c^.CurrentState = state then exit;                                      // if current state is the stationary state then exit
      c^.CurrentState := state;                                                  // Set the current state to the stationary state
      SpriteStartAnimation(c^.CharSprite, c^.ShownLayersByDirState[c^.CurrentState, c^.CurrentDirection].Anim); // Restart the animation for the animation at the index
      result:= true;                                                              // Return true that the state was changed
    end
    else begin
      if c^.CurrentState <> newState then                                         // if the current state isnt the new state then the current state needs to be changed
      begin
        c^.CurrentState := newState;  
        SetActiveLayer(c);                                                        // Change the active layer in the sprite to draw for this state
        SpriteStartAnimation(c^.CharSprite, c^.ShownLayersByDirState[c^.CurrentState, c^.CurrentDirection].Anim); // Restart the animation for the animation at the index
      end;  
      result := UpdateDirectionAnimation(c);                                      // Call Update direction Animation
    end;
  end;
  
  function UpdateDirectionAnimation(c: Character) : Boolean;
  var
    i : Integer;
    angle : single;
  begin
		if not Assigned(c) then exit;
    angle := VectorAngle(c^.CharSprite^.velocity);                                    // Get the angle of the character from it's velocity
    result := false;
    for i := 0 to NameCount(c^.Directions) -1 do                                      // Low to high directions
    begin
      if (i = c^.CurrentDirection) then continue;                                     // if the index is the current direction then nothing needs to be checked so continue
      
      if ((c^.DirectionParameters[i].min < c^.DirectionParameters[i].max) AND 
          (angle >= c^.DirectionParameters[i].min) AND (angle <= c^.DirectionParameters[i].max)) OR 
          (((c^.DirectionParameters[i].min > 0) AND (c^.DirectionParameters[i].max < 0)) AND 
          (((angle >= c^.DirectionParameters[i].min) AND (angle <= 180)) OR 
          ((angle <= c^.DirectionParameters[i].max) AND (angle >= -180)))) then
      begin
        c^.CurrentDirection := i;
        SetActiveLayer(c);
        SpriteStartAnimation(c^.CharSprite, c^.ShownLayersByDirState[c^.CurrentState, c^.CurrentDirection].Anim); // Restart the animation for the animation at the index
        result := true;
      end
    end;
  end; 
  
  function UpdateShownLayerCache(c: Character): LayerCache; // Layer cache is a 3d array for states, directions and layers
  var
    states, directions, layers, count : Integer;
  begin
		if not Assigned(c) then exit;
    SetLength(result, NameCount(c^.States), NameCount(c^.Directions));
    
    for states := Low(result) to High(result) do                                      // Loop through state indexes
    begin
      for directions := Low(result[states]) to High(result[states]) do
      begin
        count := 0;
        for layers := Low(c^.ShownLayersByDirState[states, directions].LayerOrder) to High(c^.ShownLayersByDirState[states, directions].LayerOrder) do
        begin
          if c^.ShownLayers[c^.ShownLayersByDirState[states, directions].LayerOrder[layers]] then
          begin
            SetLength(result[states,directions], Length(result[states,directions]) + 1);
            result[states,directions,count] := c^.ShownLayersByDirState[states, directions].LayerOrder[layers]; //Update the shown layer cache using the updated values
            count += 1;
          end;
        end;
      end;
    end;    
  end;
  
  procedure ToggleLayerVisibility(c: Character; index: integer);
  begin
		if not Assigned(c) then exit;
    c^.ShownLayers[index] := not c^.ShownLayers[index];     // Invert the boolean of the shown layer  for the specified index
    c^.ShownLayerCache := UpdateShownLayerCache(c);         // Since a shown layer has been changed,  the cache needs to be updated
    SetActiveLayer(c);                                      // Set the active layer in the sprites visibile layer array
  end;
  

  //---------------------------------------------------------------------------
  // Handle Character Drawing
  //---------------------------------------------------------------------------   
  
  procedure DrawCharacterWithStationary(c: character; stationaryState, state: integer);
  begin
		if not Assigned(c) then exit;
    UpdateDirectionAnimationWithStationary(c, stationaryState, state);
    DrawSprite(c^.CharSprite);
  end;
  
  procedure DrawCharacter(c: character);
  begin
		if not Assigned(c) then exit;
    UpdateDirectionAnimation(c);
    DrawSprite(c^.CharSprite);
  end;
  
  procedure DrawCharacterSprite(c: character);
  begin
		if not Assigned(c) then exit;
    DrawSprite(c^.CharSprite);
  end;

  //---------------------------------------------------------------------------
  // Loading And Freeing Character
  //---------------------------------------------------------------------------  
  
  function DoLoadCharacter(filename, name: String): Character;
  var
    bmpArray: Array of Bitmap;
    data, line, id, path: string;
    lineno, w, h, cols, rows, count, colliIndex: integer;
    aniTemp: AnimationTemplate;
    bmpIDs, tempValueIDs: NamedIndexCollection;
    singleValues: Array of Single;
    
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
    {  SetLength(result^.CharSprite^.values, Length(result^.CharSprite^.values) + 1);
      AddName(result^.CharSprite^.valueIds, ExtractDelimited(1, data, [',']));
      result^.CharSprite^.values[High(result^.CharSprite^.values)] := StrToInt(ExtractDelimited(2, data, [',']));}
      SetLength(singleValues, Length(singleValues) + 1);
      AddName(tempValueIDs, ExtractDelimited(1, data, [',']));
      singleValues[High(singleValues)] := StrToInt(ExtractDelimited(2, data, [',']));
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
      WriteLn('Starting to Verify');
      if EOF(input) then exit;
      line := '';
      WriteLn('Verify');
      while (Length(line) = 0) or (MidStr(line,1,2) = '//') do
      begin
        ReadLn(input, line);
        line := Trim(line);
        WriteLn(line);
      end;
       WriteLn('EndVerify');
      //Verify that the line has the right version
      if line <> 'SwinGame Character #v1' then 
        RaiseException('Error in character ' + filename + '. Character files must start with "SwinGame Character #v1"');     
    end;
    
  begin
    WriteLn('Start Load');
    {$IFDEF TRACE}
      TraceEnter('sgCharacters', 'DoLoadCharacter', name + ' = ' + filename);
    {$ENDIF}
    result := nil;
    
    if not FileExists(filename) then
    begin
      filename := PathToResource(filename, CharacterResource);
      if not FileExists(filename) then
      begin
        RaiseException('Unable to locate Character ' + filename);
        exit;
      end;
    end;
    WriteLn('Checked if Exists');
    New(result);    
    result^.filename := filename;
    result^.name := name;
    
    SetLength(bmpArray, 0);
    
    lineNo := 0;
    WriteLn('Assign');
    Assign(input, filename);
    Reset(input);
    WriteLn(filename);
    VerifyVersion();
    WriteLn('EndVerify2');
   // InitNamedIndexCollection(result^.CharSprite^.valueIds);
    InitNamedIndexCollection(tempValueIDs);
    InitNamedIndexCollection(bmpIDs);
    colliIndex := -1;
        
    try
      while not EOF(input) do
      begin 
        WriteLn('Start Loop');
        lineNo := lineNo + 1;
        ReadLn(input, line);
        line := Trim(line);
        WriteLn(line);
        if Length(line) = 0 then continue;  //skip empty lines
        if MidStr(line,1,2) = '//' then continue; //skip lines starting with //

        ProcessLine();
      end;
    
    finally
      Close(input);
    end;
    
    result^.CharSprite := CreateSprite(bmpArray, NamesOf(bmpIDs));
    result^.CharSprite^.valueIDs := tempValueIDs;
    result^.CharSprite^.values := singleValues;
    result^.CharSprite^.animationTemplate := aniTemp;
    if colliIndex <> -1 then SpriteSetCollisionBitmap(result^.CharSprite,bmpArray[colliIndex]);
    result^.ShownLayerCache := UpdateShownLayerCache(result);
    SetActiveLayer(result);
    WriteLn('End Load CHar');
    {$IFDEF TRACE}
      TraceExit('sgCharacters', 'DoLoadCharacter', HexStr(result));
    {$ENDIF}  
  end;

  function LoadCharacter(filename: String): Character;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCharacters', 'LoadCharacter', filename);
    {$ENDIF}
    
    result := MapCharacter(filename, filename);
    
    {$IFDEF TRACE}
      TraceExit('sgCharacters', 'LoadCharacter');
    {$ENDIF}
  end;
  
  function MapCharacter(name, filename: String): Character;
  var
    obj: tResourceContainer;
    chr: Character;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCharacters', 'MapCharacter', name + ' = ' + filename);
    {$ENDIF}
        
    if _Characters.containsKey(name) then
    begin
      result := CharacterNamed(name);
      exit;
    end;
    
    chr := DoLoadCharacter(filename, name);
    obj := tResourceContainer.Create(chr);
    
    result := chr;
    
    {$IFDEF TRACE}
      TraceExit('sgCharacters', 'MapCharacter');
    {$ENDIF}
  end;
  
  // private:
  // Called to actually free the resource
  procedure DoFreeCharacter(var c: Character);
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCharacters', 'DoFreeCharacter', 'c = ' + HexStr(c));
    {$ENDIF}
    
    if assigned(c) then
    begin
      CallFreeNotifier(c);     
      Dispose(c);
    end;
    c := nil;
    
    {$IFDEF TRACE}
      TraceExit('sgCharacters', 'DoFreeCharacter');
    {$ENDIF}
  end;
  
  procedure FreeCharacter(var c: Character);
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCharacters', 'FreeCharacter', 'c = ' + HexStr(c));
    {$ENDIF}
    
    if(assigned(c)) then
    begin
      ReleaseCharacter(c^.name);
    end;
    c := nil;
    
    {$IFDEF TRACE}
      TraceExit('sgCharacters', 'FreeCharacter');
    {$ENDIF}
  end;
  
  procedure ReleaseCharacter(name: String);
  var
    chr: Character;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCharacters', 'ReleaseCharacter', 'c = ' + name);
    {$ENDIF}
    
    chr := CharacterNamed(name);
    if (assigned(chr)) then
    begin
      _Characters.remove(name).Free();
      DoFreeCharacter(chr);
    end;
    
    {$IFDEF TRACE}
      TraceExit('sgCharacters', 'ReleaseCharacter');
    {$ENDIF}
  end;
  
  procedure ReleaseAllCharacters();
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCharacters', 'ReleaseAllCharacters', '');
    {$ENDIF}
    
    ReleaseAll(_Characters, @ReleaseCharacter);
    
    {$IFDEF TRACE}
      TraceExit('sgCharacters', 'ReleaseAllCharacters');
    {$ENDIF}
  end;
 
//-----------------------------------------------------------------------

  function HasCharacter(name: String): Boolean;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCharacter', 'HasCharacter', name);
    {$ENDIF}
    
    result := _Characters.containsKey(name);
    
    {$IFDEF TRACE}
      TraceExit('sgCharacter', 'HasCharacter', BoolToStr(result, true));
    {$ENDIF}
  end;
  
  function CharacterNamed(name: String): Character;
  var
    tmp : TObject;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCharacter', 'CharacterNamed', name);
    {$ENDIF}
    
    tmp := _Characters.values[name];
    if assigned(tmp) then result := Character(tResourceContainer(tmp).Resource)
    else result := nil;
    
    {$IFDEF TRACE}
      TraceExit('sgCharacter', 'CharacterNamed', HexStr(result));
    {$ENDIF}
  end;

  function CharacterName(c: Character): String;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCharacter', 'CharacterName', HexStr(c));
    {$ENDIF}
    
    if assigned(c) then result := c^.name
    else result := '';
    
    {$IFDEF TRACE}
      TraceExit('sgCharacter', 'CharacterName', result);
    {$ENDIF}
  end;
  
  function CharacterFilename(c: Character): String;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCharacter', 'CharacterFilename', HexStr(c));
    {$ENDIF}
    
    if assigned(c) then result := c^.filename
    else result := '';
    
    {$IFDEF TRACE}
      TraceExit('sgCharacter', 'CharacterFilename', result);
    {$ENDIF}
  end;
  
  
  
  //---------------------------------------------------------------------------
  // Load Character
  //--------------------------------------------------------------------------- 
 
  initialization 
  begin
    {$IFDEF TRACE}
      TraceEnter('sgCharacters', 'Initialise', '');
    {$ENDIF}
    
    InitialiseSwinGame();
    _Characters := TStringHash.Create(False, 1024);
    
    {$IFDEF TRACE}
      TraceExit('sgCharacters', 'Initialise');
    {$ENDIF}
  end;

end.
  