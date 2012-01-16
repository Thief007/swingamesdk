program HowToMoveAShape;
uses
  sgInput, sgGraphics, sgResources, sgText, sgTypes;

procedure Main();
var 
	x,y : Single;	
begin
	x := 300;
	y := 250;
  
  OpenGraphicsWindow('Moving Shape', 800, 600);
  
  repeat // The game loop...
    ProcessEvents();
    
    ClearScreen(ColorWhite);
	
	FillRectangle(ColorGreen, x, y, 200, 100);
	
	if(KeyDown(vk_UP)) then
	begin
		y -= 1;
		if (y = 0) then y := 600;
	end;
	if(KeyDown(vk_DOWN)) then
	begin
		y += 1;
		if (y = 600) then y := 0;
	end;
	if(KeyDown(vk_LEFT)) then
	begin
		x -= 1;
		if (x = 0) then x := 800;
	end;
	if(KeyDown(vk_RIGHT)) then
	begin
		x += 1;
		if (x = 800) then x := 0;
	end;
	
    RefreshScreen();
  until WindowCloseRequested();
  
  ReleaseAllResources();
end;

begin
  Main();
end.