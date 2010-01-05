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
  ActivatePanel(pnlb);
  
  AddPanelToGUI(pnla);
  AddPanelToGUI(pnlb);
  
  DrawGUIAsVectors(true);
end;
  
procedure UpdateInterface(pnla, pnlb: panel);
begin
  if RegionStringID(RegionClicked(PanelClicked())) = 'Button1' then
  begin
    WriteLn('Button1 Clicked');
    ToggleShowPanel(pnlb);
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
    UpdateInterface(pnla, pnlb);
	
    DrawFramerate(0,0);
    RefreshScreen();
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  CloseAudio();
end;

begin
  Main();
end.
