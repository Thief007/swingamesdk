unit CharacterEditor;

//=============================================================================
interface
uses sgTypes, sgUserInterface, EditorShared, sgCharacters, sgNamedIndexCollection;

const
  CharDetails   = 0;
  LayerList    = 1;
  DirAngle      = 2;
  DirLayer      = 3;
  StateLayer    = 4;
  Browser       = 5;
  LayerOrder    = 6;
  
  
  procedure InitializeCharEditor(var CharMode : CharEditorValues);
  procedure UpdateCharEditor(var CharMode : CharEditorValues;var sharedVals: EditorValues);

implementation
	uses sgImages, sgCore, sgInput, sgText, sgGeometry, sgGraphics, SysUtils, sgResources, sgShared, StrUtils, sgUtils, sgSprites;

  //---------------------------------------------------------------------------
  // Initialize
  //--------------------------------------------------------------------------- 
  
  procedure InitializeCharPanels(var p: PanelArray);
  var
    i: Integer;
  begin
    SetLength(p, 7);
    
    p[CharDetails]  := LoadPanel('CharDetails.txt');
    p[LayerList]   := LoadPanel('LayerOrder.txt');
    p[DirAngle]     := LoadPanel('DirAngles.txt');
    p[DirLayer]     := LoadPanel('DirLayer.txt');
    p[StateLayer]   := LoadPanel('StateLayer.txt');
    p[Browser]      := LoadPanel('CharOption.txt');
    p[LayerOrder]      := LoadPanel('CharLayerOrder.txt');
  end;
  
  procedure InitializeCharEditor(var CharMode : CharEditorValues);
  var
    b : Bitmap;
  begin
    with CharMode do
    begin
      b := nil;
      New(MainChar);
      //MainChar^.CharSprite := CreateSprite(b);
      MainChar^.CharSprite := CreateSprite();
      bg := LoadBitmap('CHAREDITOR.png');
      InitializeCharPanels(panels);
      InitNamedIndexCollection(MainChar^.Directions);
      InitNamedIndexCollection(MainChar^.States);
      InitNamedIndexCollection(MainChar^.CharSprite^.valueIds);
    end;
  end;
  
  //---------------------------------------------------------------------------
  // Add Data to List
  //--------------------------------------------------------------------------- 
  
  procedure AddDirection(var CharMode: CharEditorValues; value: string);
  var
    i: Integer;
  begin   
    if value = '' then exit;
    value := Trim(value);
    AddName(CharMode.MainChar^.Directions, value);
    SetLength(CharMode.MainChar^.DirectionParameters, NameCount(CharMode.MainChar^.Directions));
    ListAddItem(ListFromRegion(RegionWithID('DirList')), value);
    ListAddItem(ListFromRegion(RegionWithID('DirAngleList')), value);
    ListAddItem(ListFromRegion(RegionWithID('DirLayerList')), value);
    TextBoxSetText(TextBoxFromRegion(RegionWithID('DirIn')), '');
        
    for i := 0 to NameCount(CharMode.MainChar^.States) -1 do
    begin
      SetLength(CharMode.Cache[i], Length(CharMode.Cache[i]) + 1);
      CharMode.Cache[i][High(CharMode.Cache[i])] := Copy(CharMode.BaseLayer, 0 , Length(CharMode.BaseLayer));
    end;
  end;
  
  procedure AddState(var CharMode: CharEditorValues; value: string);
  var
    i: Integer;
  begin
    if value = '' then exit;
    value := Trim(value);
    AddName(CharMode.MainChar^.States, value);
    ListAddItem(ListFromRegion(RegionWithID('StateList')),value );
    ListAddItem(ListFromRegion(RegionWithID('StateLayerList')),value );
    TextBoxSetText(TextBoxFromRegion(RegionWithID('StateIn')), '');
    
    SetLength(CharMode.Cache, Length(CharMode.Cache) + 1);
    SetLength(CharMode.Cache[High(CharMode.Cache)], NameCount(CharMode.MainChar^.Directions));
    
    for i := 0 to NameCount(CharMode.MainChar^.Directions) -1 do
    begin      
      CharMode.Cache[High(CharMode.Cache),i] := Copy(CharMode.BaseLayer, 0 , Length(CharMode.BaseLayer));
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
      ListAddItem(ListFromRegion(RegionWithID('ValueList')), name + ' , ' +  value);
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
      
      ListAddItem(ListFromRegion(RegionWithID('AngleList')), ListActiveItemText(RegionWIthID('DirAngleList')) + ' , ' + min + ' , ' +  max);
      TextBoxSetText(TextBoxFromRegion(RegionWithID('MinIn')), '');
      TextBoxSetText(TextBoxFromRegion(RegionWithID('MaxIn')), '');
      ListSetActiveItemIndex(ListFromRegion(RegionWIthID('DirAngleList')), -1);
    end;
  end;

  //---------------------------------------------------------------------------
  // Remove Data From List
  //--------------------------------------------------------------------------- 

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
    RemoveName(CharMode.MainChar^.Directions, ListActiveItemIndex(RegionWithID(mainList)));
    RemoveDataFromList('DirAngleList', mainList);
    RemoveDataFromList('DirLayerList', mainList);
    RemoveDataFromList(mainList, mainList);
  end;
  
  procedure RemoveState(var CharMode : CharEditorValues);
  const
    mainList = 'StateList';
  begin
    RemoveName(CharMode.MainChar^.Directions, ListActiveItemIndex(RegionWithID(mainList)));
    RemoveDataFromList('StateLayerList', mainList);
    RemoveDataFromList(mainList, mainList);
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
  // Edit Data From List
  //--------------------------------------------------------------------------- 
  
  procedure EditDirection(var CharMode : CharEditorValues);
  begin
    TextBoxSetText(TextBoxFromRegion(RegionWithID('DirIn')), ListActiveItemText(RegionWithID('DirList')));
    RemoveDirection(CharMode);
  end;
  
  procedure EditState(var CharMode : CharEditorValues);
  begin
    TextBoxSetText(TextBoxFromRegion(RegionWithID('StateIn')), ListActiveItemText(RegionWithID('StateList')));
    RemoveState(CharMode);
  end;
  
  procedure EditValue(var CharMode : CharEditorValues);
  var
    values : string;
  begin
    values := ListActiveItemText(RegionWithID('ValueList'));
    TextBoxSetText(TextBoxFromRegion(RegionWithID('NameValueIn')), ExtractDelimited(1, values, [',']));
    TextBoxSetText(TextBoxFromRegion(RegionWithID('ValueValueIn')), ExtractDelimited(2, values, [',']));
    RemoveValue(CharMode);
  end;
  
  procedure EditAngle(var CharMode : CharEditorValues);
  var
    values : string;
  begin
    values := ListActiveItemText(RegionWithID('AngleList'));
    TextBoxSetText(TextBoxFromRegion(RegionWithID('MinIn')), ExtractDelimited(2, values, [',']));
    TextBoxSetText(TextBoxFromRegion(RegionWithID('MaxIn')), ExtractDelimited(3, values, [',']));
    RemoveDataFromList('AngleList', 'AngleList');
  end;
  
  //---------------------------------------------------------------------------
  // Manage Item Lists
  //--------------------------------------------------------------------------- 
  
  
  function GetItemIndex(id: string; out index: Integer): Boolean;
  begin
    result := true;
    index := ListActiveItemIndex(RegionWithID(id));
    if index <> -1  then result := false;
  end;
  
  function NewItem(id, body, part, bmp : integer) : ItemCache;
  begin
    result.listId := id;
    result.body := body;
    result.part := part;
    result.bmp := bmp;
  end;
    
  procedure AddLayer(var CharMode : CharEditorValues; browser: CharBodyTypes);
  var
    cell: BitmapCell;
    bodyID, partID, imageID, i, j: Integer;
  begin
    if GetItemIndex('BodyList', bodyID) OR GetItemIndex('PartsList', partID) OR GetItemIndex('ImageList', imageID) then exit;
    
    cell := BitmapCellOf(browser.BodyType[bodyID].parts[partID].bmps[imageID].original, 0 );
    SetLength(CharMode.BaseLayer, Length(CharMode.BaseLayer) + 1);
    CharMode.BaseLayer[High(CharMode.BaseLayer)] := NewItem(High(CharMode.BaseLayer), bodyID, partID, imageID);
    
    with CharMode do
    begin
      for i := Low(Cache) to High(Cache) do
      begin
        for j := Low(Cache[i]) to High(Cache[i]) do
        begin
          SetLength(Cache[i,j], Length(Cache[i,j]) + 1);
          Cache[i,j, High(Cache[i,j])] := NewItem(High(CharMode.BaseLayer), bodyID, partID, imageID);
        end;
      end;
    end;   
    ListAddItem(ListFromRegion(RegionWithID('LayerList')), cell, cell.bmp^.name);
  end;
  
  procedure ShowLayerOrder(var CharMode : CharEditorValues; browser: CharBodyTypes);
  var
    i,j,k, state, dir : Integer;
  begin
    state := ListActiveItemIndex(RegionWithID('StateLayerList'));
    dir   := ListActiveItemIndex(RegionWithID('DirLayerList'));
    
    if (dir = -1) OR (state = -1) then exit;
    
    ListClearItems(ListFromRegion(RegionWithID('LayerOrder')));
    
    with CharMode do
    begin      
      for k := Low(Cache[state,dir]) to High(Cache[state,dir]) do
        ListAddItem(ListFromRegion(RegionWithID('LayerOrder')), BitmapCellOf(browser.BodyType[Cache[state,dir,k].body].parts[Cache[state,dir,k].part].bmps[Cache[state,dir,k].bmp].original, 0),
                                                                browser.BodyType[Cache[state,dir,k].body].parts[Cache[state,dir,k].part].bmps[Cache[state,dir,k].bmp].original^.name);

    end; 
  end;
  
  procedure SwapParts(var part1, part2: ItemCache);
  var
    tmp: ItemCache;
  begin
    tmp := part1;
    part1 := part2;
    part2 := tmp;
  end;
  
  procedure MoveLayerUp(var CharMode : CharEditorValues;var browser: CharBodyTypes);    
  var
   state, dir, active, i, j , k : Integer;
  begin
    with CharMode do
    begin
      state   := ListActiveItemIndex(RegionWithID('StateLayerList'));
      dir     := ListActiveItemIndex(RegionWithID('DirLayerList'));     
      active  := ListActiveItemIndex(RegionWithID('LayerOrder'));
      
      if (dir < 0) OR (state < 0) OR (active <= 0) then exit;
      
      SwapParts(Cache[state,dir,active], Cache[state,dir,active - 1]);
    
      ShowLayerOrder(CharMode, browser);
      ListSetActiveItemIndex(ListFromRegion(RegionWithID('LayerOrder')), active -1);
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
  
  procedure UpdateFromTextBox(var CharMode : CharEditorValues; sharedVals: EditorValues);
  begin
    with CharMode do
    begin
      case IndexOfLastUpdatedTextBox of 
        0: MainChar^.CharName 			:= TextBoxText(GUITextBoxOfTextEntered);
        1: MainChar^.CharType 			:= TextBoxText(GUITextBoxOfTextEntered);
      end;
      
      // Adds
      if (RegionClickedID() = 'DirAdd')     then AddDirection(CharMode,  TextBoxText(TextBoxFromRegion(RegionWithID('DirIn'))));
      if (RegionClickedID() = 'StateAdd')   then AddState(CharMode,      TextBoxText(TextBoxFromRegion(RegionWithID('StateIn'))));
      if (RegionClickedID() = 'ValueAdd')   then AddValue(CharMode,               TextBoxText(TextBoxFromRegion(RegionWithID('NameValueIn'))),
                                                                                  TextBoxText(TextBoxFromRegion(RegionWithID('ValueValueIn'))));      
      if (RegionClickedID() = 'AngleAdd')   then AddAngle(CharMode,               TextBoxText(TextBoxFromRegion(RegionWithID('MinIn'))),
                                                                                  TextBoxText(TextBoxFromRegion(RegionWithID('MaxIn'))));
      // Remove
      if (RegionClickedID() = 'DirRemove')   then RemoveDirection(CharMode);
      if (RegionClickedID() = 'StateRemove') then RemoveState(CharMode);
      if (RegionClickedID() = 'ValueRemove') then RemoveValue(CharMode);
      if (RegionClickedID() = 'AngleRemove') then RemoveDataFromList('AngleList', 'AngleList');
      
      //Edit
      if (RegionClickedID() = 'DirEdit')   then EditDirection(CharMode);
      if (RegionClickedID() = 'StateEdit') then EditState(CharMode);
      if (RegionClickedID() = 'ValueEdit') then EditValue(CharMode);
      if (RegionClickedID() = 'AngleEdit') then EditAngle(CharMode);
      

   //   if (RegionClickedID() = 'AddItemButton') then AddLayer(CharMode, sharedVals.Browser);
      if (RegionClickedID() = 'MoveUP') then MoveLayerUp(CharMode, sharedVals.Browser);
      if (RegionClickedID() = 'DirLayerList') OR (RegionClickedID() = 'StateLayerList') then ShowLayerOrder(CharMode, sharedVals.Browser);

      //Drop Down
      UpdateDropDown('DirAngleList'   , 'DirAngleLbl'   , panels[DirAngle]);
      UpdateDropDown('DirLayerList'   , 'DirLayerLbl'   , panels[DirLayer]);
      UpdateDropDown('StateLayerList' , 'StateLayerLbl' , panels[StateLayer]);
    end;
  end;
  
  procedure UpdateCharEditor(var CharMode : CharEditorValues;var sharedVals: EditorValues);
  var
    i : Integer;
  begin
    UpdateFromTextBox(CharMode, sharedVals);
    if KeyTyped(vk_4) then 
    begin
      for i := Low(CharMode.BaseLayer) to High(CharMode.BaseLayer) do
      begin
        if IndexOf(CharMode.MainChar^.CharSprite^.valueids, sharedVals.Browser.BodyType[CharMode.BaseLayer[i].body].parts[CharMode.BaseLayer[i].part].bmps[CharMode.BaseLayer[i].bmp].original^.name) <> -1 then continue;
        SpriteAddLayer(CharacterSprite(CharMode.MainChar), sharedVals.Browser.BodyType[CharMode.BaseLayer[i].body].parts[CharMode.BaseLayer[i].part].bmps[CharMode.BaseLayer[i].bmp].original,
                                                            sharedVals.Browser.BodyType[CharMode.BaseLayer[i].body].parts[CharMode.BaseLayer[i].part].bmps[CharMode.BaseLayer[i].bmp].original^.name);
        SpriteShowLayer(CharacterSprite(CharMode.MainChar), i);
      end;
      CharMode.MainChar^.CharSprite^.position.x := 350;
      CharMode.MainChar^.CharSprite^.position.y :=440;
    end;
    if Length(CharMode.MainChar^.CharSprite^.layers) <> 0 then
    begin      
      DrawCharacter(CharMode.MainChar);
    end;
  end;
end.