program BitmapAnimationEditor;

uses
  crt, sgTypes, sgCore, sgAudio, sgText, sgGraphics, sgResources,
  sgGeometry, sgImages, sgInput, 
  SysUtils, sgUserInterface, sgAnimations, 
	EditorShared, BitmapEditor, AnimationEditor;
  
const
  ToolBarMenu = 0; // Top menu of the editor
  FileMenu     = 1; // Drop down menu from the file button

  // Initialize the Toolbar panel for the editor
  // the one with file, bmp, ani, char editor modes
procedure InitializeFilePanel(out p: PanelArray);
begin
  SetLength(p, 2);
	p[ToolBarMenu] := LoadPanel('ToolBarPanel.txt');
	p[FileMenu] := LoadPanel('File.txt');
	ShowPanel(p[ToolBarMenu]);
	
	GUISetBackGroundColor(RGBAColor(0,0,0,0));
	GUISetForeGRoundColor(ColorWhite);
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
	HidePanel(panels[CellBitmapNames]);
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
	HidePanel(panels[AniCellBMPNames]);
	HidePanel(panels[Preview1]);
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
procedure ChangePanelMode(BitmapPanels, AnimationPanels: PanelArray; var prevMode :integer; currentMode: Integer);
var
	i: integer;
begin
	case currentMode of
		BitmapEditorID 		: ShowBitmapPanels(BitmapPanels);
		AnimationEditorID : ShowAnimationPanels(AnimationPanels);
		//	CharacterEditorID : ShowBitmapPanels(panels[i]);
	end;
	
	case prevMode of
		BitmapEditorID 		:  HidePanels(BitmapPanels);
		AnimationEditorID :  HidePanels(AnimationPanels);
		//	CharacterEditorID : ShowBitmapPanels(panels[i]);
	end;
	prevMode := currentMode;
end;

  //Draws the background depending on the mode
procedure DrawBackGround(BitmapMode: BitmapEditorValues; AniMode: AnimationEditorValues);
begin
	case ActiveRadioButtonIndex(RadioGroupFromRegion(RegionWithID('Bitmap'))) of 
		0: DrawBitmap(BitmapMode.bg, 0, 0);
		1: DrawBitmap(AniMode.bg, 0, 0);
	end;
end;

procedure Main();
var
	prevMode: LongInt; // Stores previous mode value so the program knows when to Update panels
	p : PanelArray; //Panel Array for the editor (toolbar, file)
	SharedData : EditorValues;  //Editor record
	BitmapMode : BitmapEditorValues;  //Bitmap Editor Record
	AniMode		 : AnimationEditorValues; // Animation Editor Record
//	CharMode	 : CharacterEditorValues; //Character Editor Record
  drawvect : boolean;
begin
  OpenAudio();
  OpenGraphicsWindow('Bitmap | Animation | Character Editor', ScreenWidth, ScreenHeight);
  LoadResourceBundle('CharacterEditor.txt');			
	  
	AddBitmapToArray(SharedData.BMPArray, 'Corpo', 'corpolupo1.png');
	AddBitmapToArray(SharedData.BMPArray, 'Black', 'blackbody.png');
  
	InitializeFilePanel(p);
	InitializeBitmapEditor(BitmapMode, SharedData.BMPArray);
	InitializeAnimationEditor(AniMode);
	WriteLn('BItmaps');
  AddBitmapToList(SharedData.BMPArray[0].original, ListFromRegion(RegionWithID('BMPList')), ListFromRegion(RegionWithID('AniBMPList')));
  AddBitmapToList(SharedData.BMPArray[1].original, ListFromRegion(RegionWithID('BMPList')), ListFromRegion(RegionWithID('AniBMPList')));
	WriteLn('BItmaps');
  SharedData.dragCell := nil;
  SharedData.dragGroup := false;
	prevMode := 0;
  drawVect := false;

  repeat // The game loop...
    ProcessEvents();
		DrawBackGround(BitmapMode, AniMode);
    
    ShowFileMenu(p[FileMenu]);
   
		case ActiveRadioButtonIndex(RadioGroupFromRegion(RegionWithID('Bitmap'))) of 
			0: UpdateBitmapEditor(BitmapMode, SharedData);
			1: UpdateAnimationEditor(AniMode, SharedData);
		end;
	
		if (prevMode <> ActiveRadioButtonIndex(RadioGroupFromRegion(RegionWithID('Bitmap')))) then
		begin
			ChangePanelMode(BitmapMode.panels, AniMode.panels, prevMode, ActiveRadioButtonIndex(RadioGroupFromRegion(RegionWithID('Bitmap'))));
		end;
    
    DrawPanels();
    if SharedData.dragCell <> nil then DrawOnMouse(SharedData);
    if KeyTyped(vk_ESCAPE) then sharedData.dragCell := nil;
    if KeyTyped(vk_TAB) then
    begin
      drawVect := not drawvect;
     DrawGUIAsVectors(drawvect);
    end;
        
		UpdateInterface();
		RefreshScreen(60);   
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  CloseAudio(); 
end;

begin
	Main();
end.