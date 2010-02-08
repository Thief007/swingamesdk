program GUITests;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}

uses
  sgCore, sgUserInterface, sgAudio, sgGraphics, sgResources, sgText,
  sgGeometry, sgTypes, sgInput, SysUtils, sgImages, sgMaps,
  sgCamera;

type
  PanelName = (Selector, SelectorDropDown, MapProperties, Kind, DefaultValues, Bottom,Grid);
  PanelArray = Array[PanelName] of Panel;


function InitInterface():PanelArray;
var
  pN: PanelName;
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

    ListAddItem(RegionWithID('SelectorList'), 'Map');
    ListAddItem(RegionWithID('SelectorList'), 'Tile');

  for pN := Low(result) to high(result) do
  begin
    ShowPanel(result[pN]);
  end;

  HidePanel(result[SelectorDropDown]);
  
  DrawGUIAsVectors(false);
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

procedure AssignValues(m : Map);
var
  kIdx,vIdx : LongInt;
  val : single;
  selectedKind : string;
  selectedValues : string;
  
begin
  if (length(TextBoxText(RegionWithId('Tb.Map.Values'))) =0) then exit;
  selectedKind := ListActiveItemText(RegionWithID('lst.Kind'));
  selectedValues := ListActiveItemText(RegionWithID('lst.Values'));
  kIdx := IndexOfKind(m, selectedKind);
  vIdx := IndexOfValues(m, selectedValues);
  writeln(vidx);
  TryStrToFloat(TextBoxText(RegionWithId('Tb.Map.Values')), val);
  AddMapValues(m, kIdx, vIdx, val);
end;  
  
  
procedure UpdateGUI(pnls : PanelArray; var m:map; var openingFile, savingFile : Boolean);
begin
  
  LabelSetText(RegionWithID('b.Value.KindSelected'), ListActiveItemText(RegionWithID('lst.Kind')));


  if (RegionClickedID() = 'DropDown1') then
  begin
  	ToggleShowPanel(pnls[SelectorDropDown]);
  end;

  if (RegionClickedID() = 'b.Map.New') then
  begin
    m := NewMap();
  end;
  
  if (RegionClickedID() = 'b.Map.Apply') then
  begin
    ApplyMapProperties(m);
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
      writeln('loading file');
      m:=LoadMap(DialogPath());
      ShowMapProperties(m);
      openingFile := false;
    end;

    if savingFile then
    begin
      writeln('saving file');
      SaveMap(m,DialogPath());
      savingFile  := false;
    end;
  end;

  if (RegionClickedID() = 'b.Values.Assign') then
  begin
    AssignValues(m);
  end;
    


    if MouseClicked(LeftButton) then
    UpdateSelect(m);
  

end;
  

  
procedure UpdateCamera();
begin

  if KeyDown(VK_LEFT) then
    MoveCameraBy(-2,0)
  else if KeyDown(VK_RIGHT) then
    MoveCameraBy(2,0)
  else if KeyDown(VK_UP) then
    MoveCameraBy(0,-2)
  else if KeyDown(VK_DOWN) then
    MoveCameraBy(0,2)
end;

  
procedure Main();
var
  pnls: PanelArray;
  myMap: Map;
  openingFile,savingFile : boolean;
begin
  openingFile := false;
  savingFile  := false;
  OpenAudio();
  OpenGraphicsWindow('Map Editor v1.0', 800, 600);
  myMap := LoadMap('test1.txt');
  //ToggleFullScreen();
  LoadResourceBundle('MapEditorBundleResource.txt');
  pnls:=InitInterface();
  UpdateKindList(myMap);
  UpdateValuesList(myMap);
  ShowMapProperties(myMap);

  SetCameraPos(VectorTo(-150,-30));

  repeat
    ProcessEvents();
    ClearScreen(ColorBlack);
    PushClip(RectangleFrom(150, 30, 650 ,527));
    DrawMapGrid(myMap);//, VectorTo(150,30));
    DrawMapDebug(myMap);
    //DrawMap(myMap, VectorTo(150,30));
    PopClip();
    DrawPanels();
    UpdateGUI(pnls, myMap, openingFile, savingFile);
    UpdateInterface();

    
    DrawFramerate(0,0);
    RefreshScreen(60);
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  CloseAudio();
end;


begin
  Main();
end.

