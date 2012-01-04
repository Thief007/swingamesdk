program HowToMovingShape;
uses
  sgInput, sgGraphics, sgResources, sgText;  

procedure Main();
var 
	right : Single;

begin
  right := 0;  
  
  OpenGraphicsWindow('Moving Shape', 800, 600);
  
  repeat // The game loop...
    ProcessEvents();
    
    ClearScreen(ColorBlack);
	
	FillRectangle(ColorGreen, right, 250, 200, 100);
	right += 1;
	if (right = 800) then right := 0;
	
    RefreshScreen();
  until WindowCloseRequested();
  
  ReleaseAllResources();
end;

begin
  Main();
end.