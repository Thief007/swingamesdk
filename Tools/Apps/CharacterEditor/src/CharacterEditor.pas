unit CharacterEditor;

//=============================================================================
interface
uses sgTypes, EditorShared;

const
  CharDetails     = 0;
  CharDirections  = 1;
  CharStates      = 2;
  CharValues      = 3;
  CharAngles      = 4;
  LayerList       = 5;
  DirAngle        = 6;
  LayerOrder      = 7;
  Preview         = 8;
  CharMenu        = 9;
   
  procedure InitializeCharEditor(var CharMode : CharEditorValues);
  procedure UpdateCharEditor(var CharMode : CharEditorValues;var sharedVals: EditorValues);

implementation
	uses sgImages, sgInput, sgText, sgGeometry, sgGraphics, SysUtils, sgResources, sgShared, StrUtils, sgUtils, sgSprites,
  sgUserInterface, sgCharacters, sgNamedIndexCollection, sgAnimations;

  //---------------------------------------------------------------------------
  // Initialize
  //--------------------------------------------------------------------------- 
  
  procedure InitializeCharPanels(var p: PanelArray);
  var
    i: Integer;
  begin
    SetLength(p, 10);
    
    p[CharDetails]      := LoadPanel('CharDetails.txt');
    p[CharDirections]   := LoadPanel('CharDirections.txt');
    p[CharStates]       := LoadPanel('CharStates.txt');
    p[CharValues]       := LoadPanel('CharValues.txt');
    p[CharAngles]       := LoadPanel('CharAngles.txt');
    p[LayerList]        := LoadPanel('CharLayerList.txt');
    p[DirAngle]         := LoadPanel('DirAngles.txt');
    p[LayerOrder]       := LoadPanel('CharLayerOrder.txt');
    p[Preview]          := LoadPanel('CharPreview.txt');
    p[CharMenu]         := LoadPanel('CharMenuPanel.txt');
  end;
  
  procedure InitializeCharEditor(var CharMode : CharEditorValues);
  var
    b : Bitmap;
  begin
    with CharMode do
    begin
      b := nil;
      New(MainChar);
      MainChar^.CharSprite := CreateSprite();
      bg := LoadBitmap('CHAREDITOR.png');
      InitializeCharPanels(panels);
      InitNamedIndexCollection(MainChar^.Directions);
      InitNamedIndexCollection(MainChar^.States);
      InitNamedIndexCollection(MainChar^.CharSprite^.valueIds);
      MainChar^.CharSprite^.position.x := 498;
      MainChar^.CharSprite^.position.y := 517;
      MainChar^.CurrentState := -1;
      MainChar^.CurrentDirection := -1;
      SetLength(MainChar^.ShownLayers, 0);
    end;
  end;
  
  //---------------------------------------------------------------------------
  // Misc
  //--------------------------------------------------------------------------- 
    
  function CurrentCheck(c: Character): Boolean;
  begin
    result := true;
    
    if (CharacterCurrentState(c) = -1) OR (CharacterCurrentDirection(c) = -1) then result := false;
  end;
  
  //---------------------------------------------------------------------------
  // Manage Layers
  //--------------------------------------------------------------------------- 
 
  procedure ShowAniDetails(CharMode : CharEditorValues; state, dir: Integer);
  begin
    with CharMode.MainChar^ do
    begin
      if (dir < 0) OR (state < 0) then exit;
      if ShownLayersByDirState[state][dir].Anim = -1 then
      begin
        LabelSetText(RegionWithID('AniLbl'), 'None');
        ListSetActiveItemIndex(ListFromRegion(RegionWithID('AniLayerList')), -1);
      end else begin
        LabelSetText(RegionWithID('AniLbl'), NameAt(CharSprite^.animationTemplate^.animationIds, ShownLayersByDirState[state][dir].Anim));
        ListSetActiveItemIndex(ListFromRegion(RegionWithID('AniLayerList')), ShownLayersByDirState[state][dir].Anim);
      end;
    end;
  end;
  
  procedure ShowLayerOrder(var CharMode : CharEditorValues);
  var
    i,j,k, state, dir, index: Integer;
    mainList, orderList : GUIList;
  begin
    state := ListActiveItemIndex(RegionWithID('StateLayerList'));
    dir   := ListActiveItemIndex(RegionWithID('DirLayerList'));
    mainList   :=  ListFromRegion(RegionWithID('LayerList'));
    orderList  :=  ListFromRegion(RegionWithID('LayerOrder'));
    
    if (dir = -1) OR (state = -1) then exit;
    
    ListClearItems(orderList);
    
    with CharMode.MainChar^ do
    begin      
      for i := Low(ShownLayersByDirState[state][dir].LayerOrder) to High(ShownLayersByDirState[state][dir].LayerOrder) do
      begin
        index := ShownLayersByDirState[state][dir].LayerOrder[i];
        ListAddItem(orderList, mainList^.items[index].image, mainList^.items[index].image.bmp^.name)
      end;
    end;
    
    ShowAniDetails(CharMode, state,dir);
  end;
  
  procedure RefreshSpriteLayers(var CharMode: CharEditorValues; sharedVals: EditorValues);
  var
    i: Integer;
    lst : GUIList;
    imageName : string;
  begin
    lst :=  ListFromRegion(RegionWithID('LayerList'));
    with CharMode.MainChar^.CharSprite^ do
    begin
      SetLength(layerOffsets, 0);
      SetLength(layers, 0);
      FreeNamedIndexCollection(layerIds);
      InitNamedIndexCollection(layerIds);
      
      for i := 0 to ListItemCount(lst) -1 do
      begin
        imageName := lst^.items[i].image.bmp^.name;
        SpriteAddLayer(CharMode.MainChar^.CharSprite, BitmapAt(sharedVals, imageName).scaled[PreviewGroup], imageName)
      end;
    end;
  end;
  
  procedure RemoveLayerFromOrder(var order : DirStateData; idx: Integer);
  var
    i: Integer;
  begin
    if idx = -1 then exit;
    
    for i := idx to High(order.LayerOrder)-1 do
    begin
      order.LayerOrder[i] := order.LayerOrder[i+1];
    end;     
    SetLength(order.LayerOrder, Length(order.LayerOrder) -1);
  end;
  
  procedure RemoveLayerFromOrder(var CharMode: CharEditorValues);
  var
    state, dir, idx: Integer;
  begin
    state := ListActiveItemIndex(RegionWithID('StateLayerList'));
    dir   := ListActiveItemIndex(RegionWithID('DirLayerList'));
    idx   := ListActiveItemIndex(RegionWithID('LayerOrder'));
    
    if (dir < 0) OR (state < 0) then exit;
    
    RemoveLayerFromOrder(CharMode.MainChar^.ShownLayersByDirState[state][dir], idx);
    ShowLayerOrder(CharMode);
    CharMode.MainChar^.ShownLayerCache := UpdateShownLayerCache(CharMode.MainChar);
    SetActiveLayer(CharMode.MainChar);    
  end;
  
  procedure AdjustLayerOrderIds(var order : DirStateData; idx: Integer);
  var
    i, layerIdx : Integer;
  begin
    layerIdx := -1;
    for i := Low(order.LayerOrder) to High(order.LayerOrder) do
    begin
      if order.LayerOrder[i] > idx then order.LayerOrder[i] := order.LayerOrder[i] -1;
      if order.LayerOrder[i] = idx then layerIdx := i;
    end;   
    
    RemoveLayerFromOrder(order, layerIdx);
  end;
  
  procedure RemoveLayer(var CharMode : CharEditorValues; sharedVals: EditorValues);
  var
    idx, state, dir, vis : Integer;
  begin
    idx := ListActiveItemIndex(RegionWithID('LayerList'));
    ListRemoveItem(ListFromRegion(RegionWithID('LayerList')), idx);
    
    with CharMode.MainChar^ do
    begin
      for state := Low(ShownLayersByDirState) to High(ShownLayersByDirState) do
      begin
        for dir := Low(ShownLayersByDirState[state]) to High(ShownLayersByDirState[state]) do
        begin
          AdjustLayerOrderIds(ShownLayersByDirState[state][dir], idx);
          WriteLn('length: ',Length(ShownLayersByDirState[state][dir].LayerOrder));
        end;
      end;
            
      for vis := idx to High(ShownLayers)-1 do
      begin
        ShownLayers[vis] := ShownLayers[vis + 1];
      end;
      
      if Length(ShownLayers) <> 0 then SetLength(ShownLayers , Length(ShownLayers) - 1);
      
      RefreshSpriteLayers(CharMode, sharedVals);
          
      if not CurrentCheck(CharMode.MainChar) then exit;
      
      CharMode.MainChar^.ShownLayerCache := UpdateShownLayerCache(CharMode.MainChar);
      SetActiveLayer(CharMode.MainChar);
    end;
  end;
  
  procedure AddLayer(var CharMode : CharEditorValues; bmp : LoadedBitmapPtr; sharedVals: EditorValues);
  var
    cell: BitmapCell;
    i, j: Integer;
  begin
    cell := BitmapCellOf(bmp^.scaled[Original], 0 );
    
    with CharMode.MainChar^ do
    begin
      for i := Low(ShownLayersByDirState) to High(ShownLayersByDirState) do
      begin
        for j := Low(ShownLayersByDirState[i]) to High(ShownLayersByDirState[i]) do
        begin
          SetLength(ShownLayersByDirState[i][j].LayerOrder, Length(ShownLayersByDirState[i][j].LayerOrder) + 1);
          ShownLayersByDirState[i][j].LayerOrder[High(ShownLayersByDirState[i][j].LayerOrder)] := ListItemCount(ListFromRegion(RegionWithID('LayerList')));
        end;
      end;
      
      SetLength(ShownLayers , Length(ShownLayers) + 1);
      ShownLayers[High(ShownLayers)] := true;
    end;
    ListAddItem(ListFromRegion(RegionWithID('LayerList')), cell, cell.bmp^.name);    
    RefreshSpriteLayers(CharMode, sharedVals);
  end;
    
  procedure SwapParts(var part1, part2: Integer);
  var
    tmp: Integer;
  begin
    tmp := part1;
    part1 := part2;
    part2 := tmp;
  end;
  
  procedure MoveLayer(var CharMode : CharEditorValues; dest: Integer);    
  var
   state, dir, active: Integer;
  begin
    with CharMode do
    begin
      state   := ListActiveItemIndex(RegionWithID('StateLayerList'));
      dir     := ListActiveItemIndex(RegionWithID('DirLayerList'));     
      active  := ListActiveItemIndex(RegionWithID('LayerOrder'));
      
      if ((dest < 0) AND (active <= 0)) OR
         ((dest > 0) AND (active >= High(MainChar^.ShownLayersByDirState[state][dir].LayerOrder))) OR
         (dir < 0) OR (state < 0) then exit;
      
      SwapParts(MainChar^.ShownLayersByDirState[state][dir].LayerOrder[active], MainChar^.ShownLayersByDirState[state][dir].LayerOrder[active + dest]);
    
      ShowLayerOrder(CharMode);
      ListSetActiveItemIndex(ListFromRegion(RegionWithID('LayerOrder')), active + dest);
    end;
  end;
  
  //---------------------------------------------------------------------------
  // Animation
  //---------------------------------------------------------------------------  
   
  procedure RefreshAniComboList(var CharMode: CharEditorValues);
  var
    i, j: Integer;
  begin
    ListClearItems(RegionWithID('CharAniComboList'));
    
    with CharMode.MainChar^ do 
    begin
      for i := Low(ShownLayersByDirState) to High(ShownLayersByDirState) do
      begin
        for j := Low(ShownLayersByDirState[i]) to High(ShownLayersByDirState[i]) do
        begin
          if ShownLayersByDirState[i][j].Anim <> -1 then
          begin
            ListAddItem(RegionWithID('CharAniComboList'), NameAt(States, i) + ',' + NameAt(Directions, j));
          end;
        end;
      end;
    end;
  end;
  
  procedure ResetAnimationDetails(var CharMode: CharEditorValues);
  var
    i, j: Integer;
  begin
    with CharMode.MainChar^ do 
    begin
      for i := Low(ShownLayersByDirState) to High(ShownLayersByDirState) do
      begin
        for j := Low(ShownLayersByDirState[i]) to High(ShownLayersByDirState[i]) do
        begin
          if ShownLayersByDirState[i][j].Anim <> -1 then ShownLayersByDirState[i][j].Anim := -1;
        end;
      end;
    end;
    RefreshAniComboList(CharMode);
    ShowAniDetails(CharMode, ListActiveItemIndex(RegionWithID('StateLayerList')), ListActiveItemIndex(RegionWithID('DirLayerList')));
  end;
  
  procedure LoadAniToPreview(var CharMode: CharEditorValues);
  var
    i: Integer;
  begin
    with CharMode.MainChar^ do
    begin
      if CharSprite^.animationTemplate <> nil then FreeAnimationTemplate(CharSprite^.animationTemplate);
      CharSprite^.animationTemplate := LoadAnimationTemplate(dialogPath);
      ListClearItems(RegionWithID('AniLayerList'));
      ResetAnimationDetails(CharMode);
      for i := 0 to NameCount(CharSprite^.animationTemplate^.animationIDs)-1 do
      begin 
        ListAddItem(RegionWithID('AniLayerList'), NameAt(CharSprite^.animationTemplate^.animationIDs, i));
      end;
    end;
  end;
  
  procedure AcceptAnimation(var CharMode: CharEditorValues);
  var
    state, dir: Integer;
  begin
    if ListActiveItemIndex(RegionWithID('AniLayerList')) = -1 then exit;
    state   := ListActiveItemIndex(RegionWithID('StateLayerList'));
    dir     := ListActiveItemIndex(RegionWithID('DirLayerList'));     
    
    if (dir < 0) OR (state < 0) then exit;
    
    CharMode.MainChar^.ShownLayersByDirState[state][dir].anim := ListActiveItemIndex(RegionWithID('AniLayerList'));
    LabelSetText(RegionWithID('AniLbl'), NameAt(CharMode.MainChar^.CharSprite^.animationTemplate^.animationIds, CharMode.MainChar^.ShownLayersByDirState[state][dir].anim));
    RefreshAniComboList(CharMode);
  end;
  
  procedure PreviewCharacter(var CharMode: CharEditorValues);
  var
    stateIdx, dirIdx : Integer;
    input : String;
  begin
    if ListActiveItemIndex(RegionWithID('CharAniComboList')) = -1 then exit;
    
    input := ListActiveItemText(RegionWithID('CharAniComboList'));
    
    stateIdx := IndexOf(CharMode.MainChar^.States, ExtractDelimited(1, input, [',']));
    dirIdx   := IndexOf(CharMode.MainChar^.Directions, ExtractDelimited(2, input, [',']));
    
    CharacterSetCurrentDirection(CharMode.MainChar, dirIdx);
    CharMode.MainChar^.CurrentState := stateIdx;
    CharMode.MainChar^.ShownLayerCache := UpdateShownLayerCache(CharMode.MainChar);
    SetActiveLayer(CharMode.MainChar);
    SpriteReplayAnimation(CharMode.MainChar^.CharSprite);
    SpriteStartAnimation(CharMode.MainChar^.CharSprite, CharMode.MainChar^.ShownLayersByDirState[CharMode.MainChar^.CurrentState, CharMode.MainChar^.CurrentDirection].Anim);
  end;
  
  //---------------------------------------------------------------------------
  // Add Data to List
  //--------------------------------------------------------------------------- 
  
  procedure AddDirection(var CharMode: CharEditorValues; value: string);
  var
    i, j: Integer;
  begin   
    with CharMode.MainChar^ do
    begin
      if value = '' then exit;
      value := Trim(value);
      AddName(Directions, value);
      SetLength(DirectionParameters, NameCount(Directions));
      ListAddItem(ListFromRegion(RegionWithID('DirList')), value);
      ListAddItem(ListFromRegion(RegionWithID('DirAngleList')), value);
      ListAddItem(ListFromRegion(RegionWithID('DirLayerList')), value);
      TextBoxSetText(TextBoxFromRegion(RegionWithID('DirIn')), '');
            
      for i := 0 to NameCount(States) -1 do
      begin
        SetLength(ShownLayersByDirState[i], Length(ShownLayersByDirState[i]) + 1);
        SetLength(ShownLayersByDirState[i][High(ShownLayersByDirState[i])].LayerOrder, ListItemCount(ListFromRegion(RegionWithID('LayerList'))));
        ShownLayersByDirState[i][High(ShownLayersByDirState[i])].anim := -1;
        for j := 0 to ListItemCount(ListFromRegion(RegionWithID('LayerList'))) -1 do
        begin
          ShownLayersByDirState[i][High(ShownLayersByDirState[i])].LayerOrder[j] := j;
        end;
      end;
    end;
  end;
  
  procedure AddState(var CharMode: CharEditorValues; value: string);
  var
    i, j: Integer;
  begin
    with CharMode.MainChar^ do
    begin
      if value = '' then exit;
      value := Trim(value);
      AddName(states, value);
      ListAddItem(ListFromRegion(RegionWithID('StateList')),value );
      ListAddItem(ListFromRegion(RegionWithID('StateLayerList')),value );
      TextBoxSetText(TextBoxFromRegion(RegionWithID('StateIn')), '');
      
      SetLength(ShownLayersByDirState, Length(ShownLayersByDirState) +1);
      SetLength(ShownLayersByDirState[High(ShownLayersByDirState)], NameCount(Directions));
      
      for i := 0 to NameCount(Directions) -1 do
      begin
        SetLength(ShownLayersByDirState[High(ShownLayersByDirState)][i].LayerOrder, ListItemCount(ListFromRegion(RegionWithID('LayerList'))));
        ShownLayersByDirState[High(ShownLayersByDirState)][i].anim := -1;
        for j := 0 to ListItemCount(ListFromRegion(RegionWithID('LayerList'))) -1 do
        begin
          ShownLayersByDirState[High(ShownLayersByDirState)][i].LayerOrder[j] := j;
        end;
      end;
    end;
  end;
  
  procedure AddValue(var CharMode: CharEditorValues; name, value: string);
  var
    newVal : extended;
  begin
    if (name = '') OR not TryStrToFloat(value, newVal) then exit;
    with CharMode do
    begin
      AddName(MainChar^.CharSprite^.ValueIds, value);
      SetLength(MainChar^.CharSprite^.values, Length(MainChar^.CharSprite^.values) + 1);
      MainChar^.CharSprite^.values[High(MainChar^.CharSprite^.values)] := newVal;
      ListAddItem(ListFromRegion(RegionWithID('ValueList')), name + ',' +  value);
      TextBoxSetText(TextBoxFromRegion(RegionWithID('NameValueIn')), '');
      TextBoxSetText(TextBoxFromRegion(RegionWithID('ValueValueIn')), '');
    end;
  end;
  
  procedure AddAngle(var CharMode: CharEditorValues; min, max: string);
  var
    newVal, newVal2, index : Integer;
  begin
    if not TryStrToInt(min, newVal2) OR not TryStrToInt(max, newVal) OR (ListActiveItemIndex(RegionWIthID('DirAngleList')) = -1) then exit;
    with CharMode.MainChar^ do
    begin
      index := IndexOf(Directions, ListActiveItemText(RegionWIthID('DirAngleList')));
      directionParameters[index].min := newval2;
      directionParameters[index].max := newval;
      
      ListAddItem(ListFromRegion(RegionWithID('AngleList')), ListActiveItemText(RegionWIthID('DirAngleList')) + ',' + min + ',' +  max);
      TextBoxSetText(TextBoxFromRegion(RegionWithID('MinIn')), '');
      TextBoxSetText(TextBoxFromRegion(RegionWithID('MaxIn')), '');
      ListSetActiveItemIndex(ListFromRegion(RegionWIthID('DirAngleList')), -1);
    end;
  end;

  //---------------------------------------------------------------------------
  // Remove Data From List
  //--------------------------------------------------------------------------- 
  
  procedure RemoveDirectionLayer(var CharMode : CharEditorValues; index: Integer);
  var
    i, j: Integer;
  begin
    with CharMode.MainChar^ do
    begin
      for i := Low(ShownLayersByDirState) to High(ShownLayersByDirState) do
      begin
        for j := index to High(ShownLayersByDirState[i])-1 do
        begin
          ShownLayersByDirState[i][j] := ShownLayersByDirState[i][j+1]
        end;
        SetLength(ShownLayersByDirState[i], Length(ShownLayersByDirState[i]) -1);
      end;
    end;
  end;
  
  procedure RemoveStateLayer(var CharMode : CharEditorValues; index: Integer);
  var
    i: Integer;
  begin
    with CharMode.MainChar^ do
    begin
      for i := index to High(ShownLayersByDirState)-1 do
      begin
        ShownLayersByDirState[i] := ShownLayersByDirState[i+1];
      end;
      SetLength(ShownLayersByDirState, Length(ShownLayersByDirState) -1);
    end;
  end;
  
  procedure RemoveDataFromList(listID, activeID:string);
  begin
    ListRemoveItem(ListFromRegion(RegionWithID(listID)), ListActiveItemIndex(RegionWithID(activeId)));
    ListSetActiveItemIndex(ListFromRegion(RegionWithID(listID)), -1);
  end;
  
  procedure RemoveAngle(var CharMode : CharEditorValues; activeID: string);
  var
    i, listLength, index : Integer;
    id : string;
    activeList : GUIList;
  begin
    with CharMode.MainChar^ do
    begin
      activeList := ListFromRegion(RegionWithID(activeId));
      Index := ListActiveItemIndex(activeList);
      id := ListActiveItemText(RegionWithID(activeId));
      listLength := ListItemCount(activeList);
      
      for i := index to High(DirectionParameters) do
      begin
        if i <> High(DirectionParameters) then
        begin
          DirectionParameters[i] := DirectionParameters[i+1];
        end;
      end;
      SetLength(DirectionParameters, Length(DirectionParameters) -1);
      for i := 0 to listLength do
      begin
        if Trim(ExtractDelimited(1, ListItemText(RegionWithID('AngleList'), i), [',']))  = id then
        begin
          ListRemoveItem(ListFromRegion(RegionWithID('AngleList')), i);
          exit;
        end;
      end;
    end;
  end;
  
  procedure RemoveDirection(var CharMode : CharEditorValues);
  const
    mainList = 'DirList';
  begin
    RemoveAngle(CharMode, mainList);
    RemoveDirectionLayer(CharMode, ListActiveItemIndex(RegionWithID(mainList)));
    RemoveName(CharMode.MainChar^.Directions, ListActiveItemIndex(RegionWithID(mainList)));
    RemoveDataFromList('DirAngleList', mainList);
    RemoveDataFromList('DirLayerList', mainList);
    RemoveDataFromList(mainList, mainList);
    CharMode.MainChar^.CurrentDirection := -1;
    RefreshAniComboList(CharMode);
  end;
  
  procedure RemoveState(var CharMode : CharEditorValues);
  const
    mainList = 'StateList';
  begin
    RemoveStateLayer(CharMode, ListActiveItemIndex(RegionWithID(mainList)));
    RemoveName(CharMode.MainChar^.States, ListActiveItemIndex(RegionWithID(mainList)));
    RemoveDataFromList('StateLayerList', mainList);
    RemoveDataFromList(mainList, mainList);
    CharMode.MainChar^.CurrentState := -1;
    RefreshAniComboList(CharMode);
  end;
  
  procedure RemoveValue(var CharMode : CharEditorValues);
  var
    i, index: Integer;
    activeText : String;
  begin
    with CharMode.MainChar^ do
    begin
      activeText := ListActiveItemText(RegionWithID('ValueList'));
      index := IndexOf(CharSprite^.valueIds, activeText);
      RemoveName(CharSprite^.valueIds, activeText);
      for i := index to High(CharSprite^.values) do
      begin
        if i <> High(CharSprite^.values) then
        begin
          CharSprite^.values[i] := CharSprite^.values[i+1];
        end;
      end;
      SetLength(CharSprite^.values, Length(CharSprite^.values) -1);
      RemoveDataFromList('ValueList', 'ValueList');
    end;
  end; 
      
  //---------------------------------------------------------------------------
  // Update
  //---------------------------------------------------------------------------  
  
  procedure UpdateDropDown(listID, listLblID : string; p: Panel);
  begin
    if (RegionClickedID() = listLblID) then ToggleShowPanel(p);
    if RegionClickedID <> listID then exit;
    
    LabelSetText(RegionWithID(listLblID), ListActiveItemText(RegionWIthID(listID)));
    HidePanel(p);
  end;
  
  procedure UpdateFromTextBox(var CharMode : CharEditorValues;var sharedVals: EditorValues);
  begin
    with CharMode do
    begin
      if GUITextEntryComplete AND (RegionPanel(RegionOfLastUpdatedTextBox) = panels[CharDetails]) then
      begin
        case IndexOfLastUpdatedTextBox of 
          0: MainChar^.CharName 			:= TextBoxText(GUITextBoxOfTextEntered);
          1: MainChar^.CharType 			:= TextBoxText(GUITextBoxOfTextEntered);
        end;
      end;
      
      if sharedVals.BitmapPtr <> nil then AddLayer(CharMode, sharedVals.BitmapPtr, sharedVals);
      
      // Adds
      if (RegionClickedID() = 'DirAdd')     then AddDirection(CharMode,  TextBoxText(TextBoxFromRegion(RegionWithID('DirIn'))));
      if (RegionClickedID() = 'StateAdd')   then AddState(CharMode,      TextBoxText(TextBoxFromRegion(RegionWithID('StateIn'))));
      if (RegionClickedID() = 'ValueAdd')   then AddValue(CharMode,      TextBoxText(TextBoxFromRegion(RegionWithID('NameValueIn'))),
                                                                         TextBoxText(TextBoxFromRegion(RegionWithID('ValueValueIn'))));      
      if (RegionClickedID() = 'AngleAdd')   then AddAngle(CharMode,      TextBoxText(TextBoxFromRegion(RegionWithID('MinIn'))),
                                                                         TextBoxText(TextBoxFromRegion(RegionWithID('MaxIn'))));
      // Remove
      if (RegionClickedID() = 'DirRemove')   then RemoveDirection(CharMode);
      if (RegionClickedID() = 'StateRemove') then RemoveState(CharMode);
      if (RegionClickedID() = 'ValueRemove') then RemoveValue(CharMode);
      if (RegionClickedID() = 'AngleRemove') then RemoveDataFromList('AngleList', 'AngleList');
      
      if (RegionClickedID() = 'ExportCharacter') then DoSaveDialog(sharedVals, SaveChar);
      if (RegionClickedID() = 'LoadCharIntoEditor') then DoSaveDialog(sharedVals, LoadChar);
      if (RegionClickedID() = 'BrowseAni')   then DoOpenDialog(sharedVals, LoadAni);
      
      if (RegionClickedID() = 'AddLayer')    then ShowPanel(sharedVals.panels[BrowserPanel]);
      if (RegionClickedID() = 'AcceptAni') then AcceptAnimation(CharMode);
      if (RegionClickedID() = 'CharAniComboList') then PreviewCharacter(CharMode);
      if (RegionClickedID() = 'LORemove') then RemoveLayerFromOrder(CharMode);
      
      if (RegionClickedID() = 'RemoveLayer') then RemoveLayer(CharMode, sharedVals);
      if (RegionClickedID() = 'MoveUP') then MoveLayer(CharMode, -1);
      if (RegionClickedID() = 'MoveDown') then MoveLayer(CharMode, 1);
      if (RegionClickedID() = 'DirLayerList') OR (RegionClickedID() = 'StateLayerList') then ShowLayerOrder(CharMode);

      if DialogComplete AND (sharedVals.OpenSave = LoadAni) then LoadAniToPreview(CharMode);
      if DialogComplete AND (sharedVals.OpenSave = SaveChar) then ExportCharacter(CharMode, sharedVals, dialogPath);
      if DialogComplete AND (sharedVals.OpenSave = LoadChar) then 
      begin
        LoadCharacterToEditor(CharMode, dialogPath);
        RefreshAniComboList(CharMode);
      end;
      //Drop Down
      UpdateDropDown('DirAngleList'   , 'DirAngleLbl'   , panels[DirAngle]);
    end;
  end;
  
  procedure UpdateCharEditor(var CharMode : CharEditorValues;var sharedVals: EditorValues);
  var
    i : Integer;
  begin
    UpdateFromTextBox(CharMode, sharedVals);
    if (ListActiveItemIndex(RegionWithID('CharAniComboList')) <> -1) AND CurrentCheck(CharMode.MainChar)then
    begin      
      DrawCharacterSprite(CharMode.MainChar);
      UpdateSpriteAnimation(CharMode.MainChar^.CharSprite);
    end;
  end;
end.