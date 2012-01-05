program HowToControlMovement;
uses
  sgInput, sgGraphics, sgResources, sgText, sgTypes;
  
procedure HandleInput(var x,y: Single);
begin
	if(KeyDown(vk_LEFT)) then	x -= 1;
	if(KeyDown(vk_RIGHT)) then x += 1;
	if(KeyDown(vk_UP)) then y -= 1;
	if(KeyDown(vk_DOWN)) then y += 1;
end;
  
procedure Main();
var 
	x, y : Single;
	w, h : LongInt;
	c : Color;
begin 
	OpenGraphicsWindow('Moving Shape', 800, 600);

	x := 300;
	y := 250;
	w := 200;
	h := 100;
	c := ColorGreen;
	
	repeat // The game loop...
		ProcessEvents();
    
		ClearScreen(ColorWhite);	
		FillRectangle(c, x, y, w, h);
		HandleInput(x, y);
	
		RefreshScreen();
	until WindowCloseRequested();
  
	ReleaseAllResources();
end;

begin
  Main();
end.