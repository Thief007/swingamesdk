program HowToDrawAPanel;
uses 
 SwinGame, sgTypes, sgUserInterface;

procedure Main();
var
  p : Panel;  
begin    
  OpenGraphicsWindow('How To Draw A Panel', 600, 400);
  LoadDefaultColors();  
  
  p := LoadPanel('SimplePanel.txt');  
  ShowPanel(p);
  
  ClearScreen(ColorWhite);        
  DrawPanels();
  
	RefreshScreen();
	
	Delay(20000);	
  ReleaseAllResources();
end;

begin
    Main();
end.