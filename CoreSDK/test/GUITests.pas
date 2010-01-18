program GUITests;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}

uses
  sgCore, sgUserInterface, sgAudio, sgGraphics, sgResources, sgText, sgGeometry, sgTypes, SysUtils;
	
  
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
  
procedure UpdateGUI(pnla, pnlb: panel; i: Integer);
var
  reg: Region;
  parpnl: Panel;
  radGroup: GUIRadioGroup;
  Item: GUIListItem;
begin

  if (RegionClicked() = 'Button1') And (CheckboxState('Checkbox2')) then
  begin
    ToggleShowPanel(pnlb);
    Toggleactivatepanel(pnlb);
  end;
  
  SetLabelString('Label1', TextboxString('TextBox1'));
  
  case ActiveRadioButton('RadioGroup1') of
    0: SetGUIColorForVectors(ColorWhite);
    1: SetGUIColorForVectors(ColorRed);
    2: SetGUIColorForVectors(ColorBlue);
  end;
  
  if (i = 600) OR (i = 1200) OR (i = 1800) then
  	AddItemToList(GetRegionByID('List1'), NewListItem('List1', 'List1Item' + IntToStr(i), 'n', 'I''m ListItem' + IntToStr(i)));
  
end;
  
procedure Main();
var
	pnla, pnlb: Panel;
	i: Integer;
begin
  OpenAudio();
  OpenGraphicsWindow('Hello World', 800, 600);
  
  LoadResourceBundle('MainMenu.txt');
  InitInterface(pnla,pnlb);
  
  i := 5;

  repeat
    ProcessEvents();
    ClearScreen(ColorBlack);
    
    // Need to re-initialize Lists!
    
    DrawPanels();
    UpdateGUI(pnla, pnlb, i);
    UpdateInterface();
    
    i += 1;
    
    DrawFramerate(0,0);
    RefreshScreen();
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  CloseAudio();
end;

begin
  Main();
end.
