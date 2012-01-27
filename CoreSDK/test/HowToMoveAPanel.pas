program HowToMoveAPanel;
uses 
 SwinGame, sgTypes, sgUserInterface;

procedure Main();
var
	p : Panel;	
begin    
	OpenGraphicsWindow('How To Move A Panel', 800, 600);
	LoadDefaultColors();  
	
	p := LoadPanel('panelwithlabel.txt');	
	ShowPanel(p);
	
	repeat // The game loop...
		ProcessEvents();    
		
		ClearScreen(ColorWhite);				
		DrawPanels();		
		
	  PanelSetDraggable(p, true);
		UpdateInterface();
		
		RefreshScreen();    		
	until WindowCloseRequested();
	
	ReleaseAllResources();
end;

begin
    Main();
end.