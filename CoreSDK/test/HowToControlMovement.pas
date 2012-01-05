program HowToControlMovement;
uses
  sgInput, sgGraphics, sgResources, sgText, sgTypes;

procedure setCoordinate(out x, y : Single; out w, h : LongInt);
begin	
	x := 300;
	y := 250;
	w := 200;
	h := 100;
end;
  
procedure DrawShape(c: Color; x, y: Single; w,h : LongInt);
begin	
	FillRectangle(c, x, y, w, h);
end;
  
procedure Main();
var 
	x, y : single;
	width, height : LongInt;
	c : Color;

begin 
	c := ColorGreen;
	OpenGraphicsWindow('Moving Shape by User Input', 800, 600);
	SetCoordinate(x, y, width, height);
  
	repeat // The game loop...
		ProcessEvents();
    
		ClearScreen(ColorBlack);	
		DrawShape(c, x, y, width, height);

		if(KeyDown(vk_LEFT)) then	x -= 1;
		if(KeyDown(vk_RIGHT)) then x += 1;
		if(KeyDown(vk_UP)) then y -= 1;
		if(KeyDown(vk_DOWN)) then y += 1;		
	
		RefreshScreen();
	until WindowCloseRequested();
  
	ReleaseAllResources();
end;

begin
  Main();
end.