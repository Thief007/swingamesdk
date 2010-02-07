program GUITests;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}

uses
  sgCore, sgUserInterface, sgAudio, sgGraphics, sgResources, sgText, sgGeometry, sgTypes, SysUtils, sgImages;
  
type
  ArrayOfPanel = Array of Panel;
  
const
  Tabs = 0;
  EditPanel = 1;
  EditRegion = 2;
  EditButton = 3;
  EditLabel = 4;
  EditCheckBox = 5;
  EditRadioGroup = 6;
  EditTextBox = 7;
  EditList = 8;
  
  
procedure InitInterface(var p: ArrayOfPanel);
var
  i: LongInt;
begin
  SetLength(p, 9);
  p[Tabs] := LoadPanel('IETabs.txt');
  p[EditPanel] := LoadPanel('IEpanelPanel.txt');
  p[EditRegion] := LoadPanel('IEregionPanel.txt');
  p[EditCheckBox] := LoadPanel('IEcheckboxPanel.txt');
  p[EditRadioGroup] := LoadPanel('IEradiogroupPanel.txt');
  p[EditLabel] := LoadPanel('IElabelPanel.txt');
  p[EditTextBox] := LoadPanel('IEtextboxPanel.txt');
  p[EditList] := LoadPanel('IElistPanel.txt');
  p[EditButton] := LoadPanel('IEbuttonPanel.txt');
  
  for i := 0 to 2 do
  begin
    ShowPanel(p[i]);
  end;
  
  for i := 3 to High(p) do
  begin
    HidePanel(p[i]);
  end;
  
  DrawGUIasVectors(false);
  GUISetForegroundColor(ColorBlack);
  GUISetBackgroundColor(ColorWhite);
end;
  
procedure MyUpdateGUI(p: ArrayOfPanel);
var
  tab, kinds: GUIRadioGroup;
  i: LongInt;
begin

  tab := RadioGroupFromId(p[Tabs], 'UpperTabs');
  
  case ActiveRadioButtonIndex(tab) of
    0: begin ShowPanel(p[EditPanel]); HidePanel(p[EditRegion]); end;
    1: begin ShowPanel(p[EditRegion]); HidePanel(p[EditPanel]); end;
  end;
  
  kinds := RadioGroupFromId(p[EditRegion], 'KindRadio');
  
  for i := Low(kinds^.buttons) to High(kinds^.buttons) do
  begin
    //WriteLn(i, ': Testing');
    if i = ActiveRadioButtonIndex(kinds) then
    begin
      case ActiveRadioButtonIndex(kinds) of
        0:  ShowPanel(p[EditButton]);
        1:  ShowPanel(p[EditLabel]);
        2:  ShowPanel(p[EditCheckBox]);
        3:  ShowPanel(p[EditRadioGroup]);
        4:  ShowPanel(p[EditTextbox]);
        5:  ShowPanel(p[EditList]);
      end;
    end
    else
    begin
      //WriteLn('Found inactive');
      HidePanel(p[i + 3]); //Offset the hidden panel index to skip the first 3 panels in the array.
    end;
  end;
end;

procedure Main();
var
	lst: GUIList;
	PanelsArray: ArrayOfPanel;
	
begin
  OpenAudio();
  OpenGraphicsWindow('Interface Editor', 800, 600);
  
  //ToggleFullScreen();
  
  LoadResourceBundle('InterfaceEditorBundle.txt');
  
  InitInterface(PanelsArray);
  
  repeat
    ProcessEvents();
    ClearScreen(ColorBlack);
    
    DrawPanels();
    UpdateInterface();
    MyUpdateGUI(PanelsArray);
    
    DrawFramerate(0,0);
    RefreshScreen();
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  CloseAudio();
end;

begin
  Main();
end.
