program HowToDrawAPanel;
uses 
sgGraphics, sgTypes, sgUtils, sgInput, sgResources, sgUserInterface;		

procedure Main();
var
	p : Panel;
	
begin    
    //OpenGraphicsWindow('How To Draw A Panel', 800, 600);    

		p := NewPanel('Test');
		
    //repeat
			//ProcessEvents();
			
			ShowPanel(p);
			
			//ClearScreen(ColorWhite);			
			
			//RefreshScreen();			
    //until WindowCloseRequested();
    
    ReleaseAllResources();
end;

begin
    Main();
end.