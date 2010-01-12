program GUITests;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}

uses
  sgCore, sgUserInterface, sgAudio, sgGraphics, sgResources, sgText, sgGeometry, sgTypes;
	
  
procedure InitInterface(var pnla, pnlb: panel);
begin
  SetGUIColorForVectors(ColorWhite);
  
  pnla := LoadPanel('panelwithbutton.txt');
  pnlb := LoadPanel('panelwithlabel.txt');
  
  ShowPanel(pnla);
  HidePanel(pnlb);
  
  ActivatePanel(pnla);
  DeactivatePanel(pnlb);
  
  AddPanelToGUI(pnla);
  AddPanelToGUI(pnlb);
  
  DrawGUIAsVectors(true);
end;
  
procedure UpdateGUI(pnla, pnlb: panel);
var
  reg: Region;
  parpnl: Panel;
  radGroup: GUIRadioGroup;
begin
  if (RegionClicked() = 'Button1') And (CheckboxState('Checkbox2')) then
  begin
    ToggleShowPanel(pnlb);
    Toggleactivatepanel(pnlb);
  end;
    
  case GetRegionByID('radButton1')^.parent^.radioGroups[GetRegionByID('radButton1')^.elementIndex]^.activeButton of
    0: SetGUIColorForVectors(ColorWhite);
    1: SetGUIColorForVectors(ColorRed);
    2: SetGUIColorForVectors(ColorBlue);
  end;
end;
  
procedure Main();
var
	pnla, pnlb: Panel;
begin
  OpenAudio();
  
  OpenGraphicsWindow('Hello World', 800, 600);
  
  LoadResourceBundle('MainMenu.txt');

  
  InitInterface(pnla,pnlb);
  
  repeat
    ProcessEvents();
    ClearScreen(ColorBlack);
    
    DrawPanels();
    UpdateGUI(pnla, pnlb);
    UpdateInterface();
    
    DrawFramerate(0,0);
    RefreshScreen();
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  CloseAudio();
end;

begin
  Main();
end.
