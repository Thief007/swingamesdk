program GUITests;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}

uses
  sgCore, sgUserInterface, sgAudio, sgGraphics, sgResources, sgText, sgGeometry, sgTypes, SysUtils;
	
  
procedure InitInterface(var pnla, pnlb: panel);
begin
  GUISetForegroundColor(ColorRed);
  GUISetBackgroundColor(ColorYellow);
  
  pnla := LoadPanel('panelwithbutton.txt');
  pnlb := LoadPanel('panelwithlabel.txt');
  
  ShowPanel(pnla);
  HidePanel(pnlb);
  
  ActivatePanel(pnla);
  DeactivatePanel(pnlb);
  
  AddPanelToGUI(pnla);
  AddPanelToGUI(pnlb);
  
  DrawGUIAsVectors(false);
end;
  
procedure UpdateGUI(var pnla, pnlb: panel; lst: GUilist);
var
  reg: Region;
  parpnl: Panel;
  radGroup: GUIRadioGroup;
begin
  if (RegionClickedID() = 'Button1') And (CheckboxState(RegionWithID('Checkbox2'))) then
  begin
    ToggleShowPanel(pnlb);
    Toggleactivatepanel(pnlb);
  end;

  radGroup := RadioGroupFromRegion(RegionWithID('radButton1'));
end;
  
procedure Main();
var
	pnla, pnlb: Panel;
	lst: GUIList;
begin
  OpenAudio();
  OpenGraphicsWindow('Hello World', 800, 600);
  
  LoadResourceBundle('MainMenu.txt');
  InitInterface(pnla,pnlb);
  
  lst := ListFromRegion(regionWithID('List1'));

  ListAddItem(lst, 'Hat');
  ListAddItem(lst, 'Sword');
  ListAddItem(lst, 'Cape');
  ListAddItem(lst, 'Cheese');
  ListAddItem(lst, 'Cake');
  ListAddItem(lst, 'Mouse');
  ListAddItem(lst, 'Dog');
  ListAddItem(lst, 'Axe');
  ListAddItem(lst, 'Mace');
  ListAddItem(lst, 'Chainmail');
  ListAddItem(lst, 'Ninja');
  ListAddItem(lst, 'Newspaper');
  ListAddItem(lst, 'Car');  
  ListAddItem(lst, 'Hat2');
  ListAddItem(lst, 'Sword2');
  ListAddItem(lst, 'Cape2');
  ListAddItem(lst, 'Cheese2');
  ListAddItem(lst, 'Cake2');
  ListAddItem(lst, 'Mouse2');
  ListAddItem(lst, 'Dog2');
  ListAddItem(lst, 'Axe2');
  ListAddItem(lst, 'Mace2');
  ListAddItem(lst, 'Chainmail2');
  ListAddItem(lst, 'Ninja2');
  ListAddItem(lst, 'Newspaper2');
  ListAddItem(lst, 'Car2');
  
  
  repeat
    ProcessEvents();
    ClearScreen(ColorBlack);
    
    DrawPanels();
    UpdateGUI(pnla, pnlb, lst);
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
