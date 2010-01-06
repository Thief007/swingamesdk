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
begin
  if (RegionClicked() = 'Button1') And (CheckboxState('Checkbox2')) then
  begin
    ToggleShowPanel(pnlb);
    Toggleactivatepanel(pnlb);
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
