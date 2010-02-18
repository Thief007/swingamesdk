program BitmapAnimationEditor;

uses
  crt, sgTypes, sgCore, sgAudio, sgText, sgGraphics, sgResources,
  sgGeometry, sgImages, sgInput, 
  SysUtils, sgUserInterface, sgAnimations, sgNamedIndexCollection,
	EditorShared, BitmapEditor, AnimationEditor, CharacterEditor;
  
  // Initialize the Toolbar panel for the editor
  // the one with file, bmp, ani, char editor modes
procedure InitializeFilePanel(out p: PanelArray);
begin
  SetLength(p, 3);
	p[ToolBarMenu] := LoadPanel('ToolBarPanel.txt');
	p[FileMenu] := LoadPanel('File.txt');
  p[BrowserPanel]      := LoadPanel('Browser.txt');
	ShowPanel(p[ToolBarMenu]);
	
	GUISetBackGroundColor(RGBAColor(0,0,0,0));
	GUISetForeGRoundColor(ColorWhite);
end;

procedure PopulateImageList(browser: CharBodyTypes);
var
  parts : GUIList;
  bodyIndex, partIndex, i : Integer;
begin
  parts := ListFromRegion(RegionWithID('PartsList'));
  
  bodyIndex  := ListActiveItemIndex(ListFromRegion(RegionWithID('BodyList')));
  partIndex := ListActiveItemIndex(parts);
  

  
  ListClearItems(RegionWithID('ImageList'));
      
  for i := 0 to NameCount(browser.bodyType[bodyIndex].parts[partIndex].ids) -1 do
  begin
    ListAddItem(ListFromRegion(RegionWithID('ImageList')), BitmapCellOf(browser.bodyType[bodyIndex].parts[partIndex].bmps[i].original, 0));
  end;   
end;

procedure PopulatePartsList(browser: CharBodyTypes);
var
  parts : GUIList;
  bodyIndex, i : Integer;
begin
  parts := ListFromRegion(RegionWithID('PartsList'));
  bodyIndex := ListActiveItemIndex(RegionWithID('BodyList'));
  
  if (bodyIndex = -1) then exit;
  ListClearItems(parts);
     
  for i := 0 to NameCount(browser.bodyType[bodyIndex].ids) -1 do
  begin
    ListAddItem(parts, NameAt(browser.bodyType[bodyIndex].ids, i));
  end;   
end;

function GetSelectedBitmap(browser: CharBodyTypes) : LoadedBitmapPtr;
var
  bodyIndex, partIndex, activeIndex : Integer;
begin  
  bodyIndex   := ListActiveItemIndex(ListFromRegion(RegionWithID('BodyList')));
  partIndex   := ListActiveItemIndex(ListFromRegion(RegionWithID('PartsList')));
  activeIndex := ListActiveItemIndex(ListFromRegion(RegionWithID('ImageList')));
  
  if (bodyIndex = -1) or (partIndex = -1) or (activeIndex = -1) then
  begin
    result := nil;
    exit;
  end;
  
  result := @browser.bodyType[bodyIndex].parts[partIndex].bmps[activeIndex];
end;


// Displays the file menu drop down on button click
procedure ShowFileMenu(p: Panel);
begin
  if RegionClicked = nil then exit;
  if RegionClickedID = 'FileButton' then
  begin
    ToggleShowPanel(p);
  end;
end;

// Shows the bitmap panels when the mode is selected
procedure ShowBitmapPanels(panels: PanelArray);
var
	i: integer;
begin
	for i := Low(panels) to High(panels) do
	begin
		ShowPanel(panels[i]);
	end;
end;

// Shows the Animation panels when the mode is selected
procedure ShowAnimationPanels(panels: PanelArray);
var
	i: integer;
begin	
	for i := Low(panels) to High(panels) do
	begin
		ShowPanel(panels[i]);
	end;
end;

procedure ShowCharPanels(panels: PanelArray);
var
	i: integer;
begin	
	for i := Low(panels) to High(panels) do
	begin
		ShowPanel(panels[i]);
	end;
end;

// Hides the previously active panels of the previous mode
procedure HidePanels(panels: PanelArray);
var
	i: integer;
begin
	for i := Low(panels) to High(panels) do
	begin
		HidePanel(panels[i]);
	end;
end;

  // Handles Hiding/Showing of panels based on the mode
procedure ChangePanelMode(BitmapPanels, AnimationPanels, CharacterPanels: PanelArray; var prevMode :integer; currentMode: Integer);
var
	i: integer;
begin
	case currentMode of
		BitmapEditorID 		: ShowBitmapPanels(BitmapPanels);
		AnimationEditorID : ShowAnimationPanels(AnimationPanels);
		CharacterEditorID : ShowCharPanels(CharacterPanels);
	end;
	
	case prevMode of
		BitmapEditorID 		:  HidePanels(BitmapPanels);
		AnimationEditorID :  HidePanels(AnimationPanels);
		CharacterEditorID :  HidePanels(CharacterPanels);
	end;
	prevMode := currentMode;
end;

  //Draws the background depending on the mode
procedure DrawBackGround(BitmapMode: BitmapEditorValues; AniMode: AnimationEditorValues; CharMode : CharEditorValues);
begin
	case ActiveRadioButtonIndex(RadioGroupFromRegion(RegionWithID('Bitmap'))) of 
		0: DrawBitmap(BitmapMode.bg, 0, 0);
		1: DrawBitmap(AniMode.bg, 0, 0);
		2: DrawBitmap(CharMode.bg, 0, 0);
	end;
end;

procedure HandleBrowser(var sharedVals: EditorValues);
begin
  if (RegionClickedID() = 'BodyList')      then PopulatePartsList(sharedVals.Browser);
  if (RegionClickedID() = 'PartsList')     then PopulateImageList(sharedVals.Browser);
  if (RegionClickedID() = 'AddItemButton') then
  begin
    sharedVals.BitmapPtr := GetSelectedBitmap(sharedVals.Browser);
    HidePanel(sharedVals.panels[BrowserPanel]);
  end;
end;

procedure Main();
var
	prevMode: LongInt; // Stores previous mode value so the program knows when to Update panels
	p : PanelArray; //Panel Array for the editor (toolbar, file)
	sharedVals : EditorValues;  //Editor record
	BitmapMode : BitmapEditorValues;  //Bitmap Editor Record
	AniMode		 : AnimationEditorValues; // Animation Editor Record
	CharMode	 : CharEditorValues; //Character Editor Record
  drawvect : boolean;
  i, j : integer;
begin
  OpenAudio();
  OpenGraphicsWindow('Bitmap | Animation | Character Editor', ScreenWidth, ScreenHeight);
  LoadResourceBundle('CharacterEditor.txt');			
  
	InitializeFilePanel(sharedVals.panels);
	InitializeBitmapEditor(BitmapMode);
	InitializeAnimationEditor(AniMode);
  InitializeCharEditor(CharMode);

      
  LoadBitmapsFromTextFile(sharedVals.Browser, PathToResource('\images\test.txt'), ListFromRegion(RegionWithID('BMPList')), ListFromRegion(RegionWithID('AniBMPList')));
  
  sharedVals.dragCell := nil;
  sharedVals.dragGroup := false;
  sharedVals.OpenSave := None;
	prevMode := 0;
  drawVect := false;

  repeat // The game loop...
    ProcessEvents();
    sharedVals.BitmapPtr := nil;
		DrawBackGround(BitmapMode, AniMode, CharMode);
    HandleBrowser(sharedVals);
    
		case ActiveRadioButtonIndex(RadioGroupFromRegion(RegionWithID('Bitmap'))) of 
			0: UpdateBitmapEditor(BitmapMode, sharedVals);
			1: UpdateAnimationEditor(AniMode, sharedVals);
			2: UpdateCharEditor(CharMode, sharedVals);
		end;
	
		if (prevMode <> ActiveRadioButtonIndex(RadioGroupFromRegion(RegionWithID('Bitmap')))) then
		begin
			ChangePanelMode(BitmapMode.panels, AniMode.panels, CharMode.panels, prevMode, ActiveRadioButtonIndex(RadioGroupFromRegion(RegionWithID('Bitmap'))));
		end;
    
    DrawPanels();
    if sharedVals.dragCell <> nil then DrawOnMouse(sharedVals);
    if KeyTyped(vk_ESCAPE) then sharedVals.dragCell := nil;
    if KeyTyped(vk_TAB) then
    begin
      drawVect := not drawvect;
     DrawGUIAsVectors(drawvect);
    end;
    
    if DialogComplete then sharedVals.OpenSave := None;
    
   // if KeyTyped(Vk_8) then ToggleSHowPanel(p[Browser]);
            
		UpdateInterface();
		RefreshScreen(60);   
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  CloseAudio(); 
end;

begin
	Main();
end.