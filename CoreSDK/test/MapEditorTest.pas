program GUITests;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}

uses
  sgCore, sgUserInterface, sgAudio, sgGraphics, sgResources, sgText,
  sgGeometry, sgTypes, sgInput, SysUtils, sgImages, sgMaps,
  sgCamera;

type
  PanelName = (Selector, SelectorDropDown, MapProperties, Kind, DefaultValues, Bottom, Grid, Bitmap, Bitmap_Type, Palette, BmpKind, DisplayList, LayerList);
  PanelArray = Array[PanelName] of Panel;


function InitInterface():PanelArray;

begin

  GUISetForegroundColor(RGBAColor(255,255,255,255));
  GUISetBackgroundColor(ColorTransparent);
  result[Selector] := LoadPanel('mapeditortest_EditorSelect.txt');
  result[SelectorDropDown] := LoadPanel('mapeditortest_EditorSelectList.txt');
  result[MapProperties] := LoadPanel('mapeditortest_MapProperties.txt');
  result[Kind] := LoadPanel('mapeditortest_Kind.txt');
  result[DefaultValues] := LoadPanel('mapeditortest_DefaultValues.txt');
  result[Bottom] := LoadPanel('mapeditortest_BottomPanel.txt');
  result[Grid] := LoadPanel('mapeditortest_Grid.txt');
  result[Bitmap]  := LoadPanel('mapeditortest_Bitmap.txt');
  result[Bitmap_Type]  := LoadPanel('mapeditortest_Bitmap_Type.txt');  
  result[Palette] := LoadPanel('mapeditortest_Palette.txt');
  result[BmpKind] := LoadPanel('mapeditortest_BmpKind.txt');
  result[DisplayList] := LoadPanel('mapeditortest_DisplayList.txt');
  result[LayerList] := LoadPanel('mapeditortest_LayerList.txt');

  

  ListAddItem(RegionWithID('SelectorList'), 'Map');
  ListAddItem(RegionWithID('SelectorList'), 'Tile');
  
  ListAddItem(RegionWithID('dD.BitmapType'), 'Cell');
  ListAddItem(RegionWithID('dD.BitmapType'), 'Bitmap');

  
  ListAddItem(RegionWithID('dD.displayList'), 'Grid');
  ListAddItem(RegionWithID('dD.displayList'), 'Bitmap');
  ListAddItem(RegionWithID('dD.displayList'), 'Bitmap+Grid');


end;

procedure HideAllPanel(pnls: PanelArray);
var
pN : PanelName;
begin
  for pN := low(pnls) to high(pnls) do
  begin
    HidePanel(pnls[pN]);
  end;
end;

procedure ShowMapEditor(pnls: PanelArray);
var
  pN: PanelName;
begin
  HideAllPanel(pnls);
  for pN := Low(pnls) to Grid do // 6 is the last panel for the map editor everything after that is the tile editor
  begin
    ShowPanel(pnls[pN]);
  end;

  HidePanel(pnls[SelectorDropDown]);
  
  DrawGUIAsVectors(true);
    ListSetActiveItemIndex(ListFromRegion(RegionWithID('SelectorList')),-1);

end;

procedure ShowTileEditor(pnls: PanelArray);
var
  pN: PanelName;
begin
  HideAllPanel(pnls);
  for pN := Grid to high(pnls) do // 7 is the first panel for the tile editor everything before that is the map editor
  begin
    ShowPanel(pnls[pN]);
  end;
  ShowPanel(pnls[Selector]);
  ShowPanel(pnls[Bottom]);
  
  HidePanel(pnls[Bitmap_Type]);
  HidePanel(pnls[DisplayList]);
  HidePanel(pnls[LayerList]);
  
  DrawGUIAsVectors(true);

  ListSetActiveItemIndex(ListFromRegion(RegionWithID('SelectorList')),-1);
end;


procedure CurrentMapProperties(m:Map; out mWidth, mHeight, mLayers, tWidth, tHeight : LongInt; out iso : Boolean);
begin
  mWidth   := MapWidth(m);
  mHeight  := MapHeight(m);
  mLayers  := LayerCount(m);
  tWidth  := TileWidth(m);
  tHeight := TileHeight(m);
  iso  := Isometric(m);
end;

procedure ShowMapProperties(m:Map);
var
  mWidth,
  mHeight,
  mLayers,
  tWidth,
  tHeight : LongInt;
  iso     : Boolean;
begin
  CurrentMapProperties(m, mWidth, mHeight, mLayers, tWidth, tHeight, iso);
  TextboxSetText(RegionWithId('tB.Map.Width'), mWidth);
  TextboxSetText(RegionWithId('tB.Map.Height'),mHeight);
  TextboxSetText(RegionWithId('tB.Map.Layers'),mLayers);
  TextboxSetText(RegionWithId('tB.Map.TileWidth'),tWidth);
  TextboxSetText(RegionWithId('tB.Map.TileHeight'),tHeight);
  CheckboxSetState(RegionWithId('cB.Map.Isometric'),iso);
end;

procedure ApplyMapProperties(m:Map);
var
  mWidth,
  mHeight,
  mLayers,
  tWidth,
  tHeight : LongInt;
  iso     : Boolean;
begin
  CurrentMapProperties(m, mWidth, mHeight, mLayers, tWidth, tHeight, iso);
  
  TryStrToInt(TextBoxText(RegionWithId('tB.Map.Width')), mWidth);
  TryStrToInt(TextBoxText(RegionWithId('tB.Map.Height')), mHeight);
  TryStrToInt(TextBoxText(RegionWithId('tB.Map.Layers')), mLayers);
  TryStrToInt(TextBoxText(RegionWithId('tB.Map.TileWidth')), tWidth);
  TryStrToInt(TextBoxText(RegionWithId('tB.Map.TileHeight')), tHeight);
  
  if CheckboxState(RegionWithId('cB.Map.Isometric')) then iso := true
  else iso := false;
  
  MapSetDimension(m, mWidth, mHeight, mLayers, tWidth, tHeight, iso);
end;

procedure ResetMapProperties(m:Map);
begin

  TextboxSetText(RegionWithId('tB.Map.Width'), 0);
  TextboxSetText(RegionWithId('tB.Map.Height'),0);
  TextboxSetText(RegionWithId('tB.Map.Layers'),0);
  TextboxSetText(RegionWithId('tB.Map.TileWidth'),0);
  TextboxSetText(RegionWithId('tB.Map.TileHeight'),0);
  CheckboxSetState(RegionWithId('cB.Map.Isometric'),false);
    
end;

procedure UpdateKindList(m : map);
var
  i     : LongInt;
  kinds : stringArray;
begin
  kinds := MapKinds(m);
  ListClearItems(RegionWithID('lst.Kind'));
  for i := Low(kinds) to High(kinds)do
  begin
    ListAddItem(RegionWithID('lst.Kind'), kinds[i]);
  end;
end;

procedure UpdateValuesList(m : map);
var
  i     : LongInt;
  values : stringArray;
begin
  values := MapValues(m);
  ListClearItems(RegionWithID('lst.Values'));
  for i := Low(Values) to High(Values)do
  begin
    ListAddItem(RegionWithID('lst.Values'), values[i]);
  end;
end;

procedure UpdateValueTextBox(m: map; out kIdx,vIdx:LongInt);
var
    selectedKind : string;
  selectedValues : string;
begin
  
  selectedKind := ListActiveItemText(RegionWithID('lst.Kind'));
  selectedValues := ListActiveItemText(RegionWithID('lst.Values'));
  kIdx := IndexOfKind(m, selectedKind);
  vIdx := IndexOfValues(m, selectedValues);
  if (vIdx = -1) or (kIdx = -1) or (RegionClickedID() = 'b.Values.Assign')  then exit;
  TextboxSetText(RegionWithID('Tb.Map.Value'), FloatToStr(MapDefaultValues(m, kIdx, VIdx)));
  
end;

procedure AssignValues(m : Map);
var
  kIdx,vIdx : LongInt;
  val : single;
  
begin
  UpdateValueTextBox(m, kIdx, vIdx);
  if (length(TextBoxText(RegionWithId('Tb.Map.Value'))) = 0) then exit;
  if not TryStrToFloat(TextBoxText(RegionWithId('Tb.Map.Value')), val) then exit;
  AddMapValues(m, kIdx, vIdx, val);
end;

procedure UpdateLayerList(m : map);
var
i : LongInt;
begin
  ListClearItems(RegionWithID('dD.layerList'));
  for i:= 0 to LayerCount(m)-1 do
  begin
    ListAddItem(RegionWithID('dD.layerList'),IntToStr(i));
  end;
end;
  
  
procedure UpdateGUI(pnls : PanelArray; var m:map; var openingFile, savingFile, openingBmp : Boolean);
begin
  
  LabelSetText(RegionWithID('b.Value.KindSelected'), ListActiveItemText(RegionWithID('lst.Kind')));


  if (RegionClickedID() = 'DropDown1') or (RegionClickedID() = 'SelectorList') then
  begin
  	ToggleShowPanel(pnls[SelectorDropDown]);
    LabelSetText(RegionWithID('DropDown1'), ListActiveItemText(RegionWithID('SelectorList')));
  end;

  if (RegionClickedID() = 'b.Map.New') then
  begin
    dispose(m);
    m := NewMap();
   //writeln('new');
    ShowMapProperties(m);
  end;
  
  if (RegionClickedID() = 'b.Map.Apply') then
  begin
    ApplyMapProperties(m);
    UpdateLayerList(m);
  end;

  if (RegionClickedID() = 'b.Map.Reset') then
  begin
    ResetMapProperties(m);
  end;
  
  if (RegionClickedID() = 'b.Kind.Add') then
  begin
    AddKind(m, TextBoxText(RegionWithId('tB.Kind.Name')));
    UpdateKindList(m);
  end;
  
  if (RegionClickedID() = 'b.Kind.Remove') then
  begin
    RemoveKind(m,ListActiveItemText(RegionWithID('lst.Kind')));
    UpdateKindList(m);
  end;
  
  if (RegionClickedID() = 'b.Values.Add') then
  begin
    AddValue(m, TextBoxText(RegionWithId('tB.Values.Name')));
    
    UpdateValuesList(m);
  end;
  
  if (RegionClickedID() = 'b.Values.Remove') then
  begin
    RemoveValue(m,ListActiveItemText(RegionWithID('lst.Values')));
    writeln(length(m^.MapDefaultValues[0]));
    UpdateValuesList(m);
  end;

  
  if (RegionClickedID() = 'b.Map.Load') then
  begin
   writeln( 'opening file');
    ShowOpenDialog();
    openingFile:= true;
  end;

  
  if (RegionClickedID() = 'b.Map.Save') then
  begin
  writeln( 'saving file');
    ShowSaveDialog();
    savingFile:= true;
  end;

  if DialogComplete() then
  begin
    if openingFile then
    begin
      //writeln('loading file');
      m:=LoadMap(DialogPath());
      ShowMapProperties(m);
      openingFile := false;
    end;

    if savingFile then
    begin
     // writeln('saving file');
      SaveMap(m,DialogPath());
      savingFile  := false;
    end;

    if openingBmp then
    begin
    TextboxSetText(RegionWithID('tB.Bitmap.Path'), DialogPath());
    openingBmp := false;
    end;
  end;

  if (RegionClickedID() = 'b.Values.Assign') then
  begin
    AssignValues(m);
  end;

  if MouseClicked(LeftButton) and not GUIClicked() then
  UpdateSelect(m);

  if (RegionClickedID() = 'lst.Values') or (RegionClickedID() = 'lst.Kind') then
  begin
    AssignValues(m);
  end;

  if ListActiveItemText(RegionWithID('SelectorList')) = 'Tile' then ShowTileEditor(pnls)
  else if ListActiveItemText(RegionWithID('SelectorList')) = 'Map' then ShowMapEditor(pnls);

  /////// from here on tile editor specific events.

  if (RegionClickedID() = 'dD.BitmapTypeIndicator') or (RegionClickedID() = 'dD.BitmapType') then
  begin
  	ToggleShowPanel(pnls[Bitmap_Type]);
    LabelSetText(RegionWithID('dD.BitmapTypeIndicator'), ListActiveItemText(RegionWithID('dD.BitmapType')));
  end;


  if (RegionClickedID() = 'Lbl.Map.DisplayIndicator') or (RegionClickedID() = 'dD.displayList') then
  begin
  	ToggleShowPanel(pnls[DisplayList]);
    LabelSetText(RegionWithID('Lbl.Map.DisplayIndicator'), ListActiveItemText(RegionWithID('dD.displayList')));
  end;
  
  if (RegionClickedID() = 'Lbl.Map.layer') or (RegionClickedID() = 'dD.layerList') then
  begin
  	ToggleShowPanel(pnls[LayerList]);
    writeln(ListActiveItemText(RegionWithID('dD.layerList')));
      if (ListActiveItemText(RegionWithID('dD.layerList')) <> '') then
    LabelSetText(RegionWithID('Lbl.Map.layer'), ListActiveItemText(RegionWithID('dD.layerList')));
  end;

   if (RegionClickedID() = 'b.Bitmap.Browse') then
  begin
    ShowOpenDialog();
    openingBmp:= true;
  end;

  

end;
  

procedure DrawUpdate(m : Map; offset:vector);
begin
  if ListActiveItemText(RegionWithID('dD.displayList')) = 'Grid' then DrawMapGrid(m, offset);
  if ListActiveItemText(RegionWithID('dD.displayList')) =  'Bitmap' then
  begin
    if CheckboxState('cB.Map.ShowAll') then
    DrawMap(m,offset)
    else
    begin
      DrawMapLayer(m, offset,StrToInt(LabelText(RegionWithID('Lbl.Map.layer'))));
    end;
  end;
   
  if ListActiveItemText(RegionWithID('dD.displayList')) = 'Bitmap+Grid' then
  begin
    DrawMap(m, offset);
    DrawMapGrid(m, offset);
   end;
   if ListActiveItemText(RegionWithID('dD.displayList')) = '' then DrawMapGrid(m, offset);
end;
  
procedure UpdateCamera();
begin

  if KeyDown(VK_LEFT) then
    MoveCameraBy(-5,0)
  else if KeyDown(VK_RIGHT) then
    MoveCameraBy(5,0)
  else if KeyDown(VK_UP) then
    MoveCameraBy(0,-5)
  else if KeyDown(VK_DOWN) then
    MoveCameraBy(0,5)
end;

  
procedure Main();
var
  pnls: PanelArray;
  myMap: Map;
  openingFile,savingFile,openingBmp : boolean;
  offset : Vector;
begin
  offset := VectorTo(150,30);
  openingFile := false;
  savingFile  := false;
  openingBmp  := false;
  OpenAudio();
  OpenGraphicsWindow('Map Editor v1.0', 800, 600);
  myMap := LoadMap('test1.txt');
  //ToggleFullScreen();
  LoadResourceBundle('MapEditorBundleResource.txt');
  pnls:=InitInterface();
  ShowMapEditor(pnls);
  UpdateKindList(myMap);
  UpdateValuesList(myMap);
  UpdateLayerList(myMap);
  ShowMapProperties(myMap);

  //SetCameraPos(VectorTo(-150,-30));

  repeat
    ProcessEvents();
    ClearScreen(ColorBlack);
    UpdateCamera();

    UpdateInterface();
    UpdateGUI(pnls, myMap, openingFile, savingFile, openingBmp);
    DrawUpdate(myMap,offset);
    DrawPanels();
    DrawFramerate(0,0);
    RefreshScreen();
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  CloseAudio();
end;


begin
  Main();
end.

