program HelloWorld;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}

uses
  sgCore, sgUserInterface, sgAudio, sgGraphics, sgResources, sgText;
	
procedure Main();
var
	pnl: Panel;
	i: integer;
begin
  OpenAudio();
  
  OpenGraphicsWindow('Hello World', 800, 600);
  
  //LoadResourceBundle('MainMenu.txt');
  //New(GUIC);

  pnl := LoadPanel('main_menu.txt');
  WriteLn('Panel loaded.');
  ShowPanel(pnl);
  WriteLn('Panel shown successfully.');
  AddPanelToGUI(pnl);
  WriteLn('Panel added to GUIC successfully.');
  
  repeat
    ProcessEvents();
    ClearScreen(ColorBlack);
    
	DrawPanelsAsRectangles();
	
    DrawFramerate(0,0);
    RefreshScreen();
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  CloseAudio();
end;

begin
  Main();
end.
