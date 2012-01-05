program HowToDrawMultipleShape;
uses
  sgTypes, sgInput, sgGraphics, sgResources, sgUtils;

type	
	Shape = record
		c : Color;
		x, y : Single;
		w, h : LongInt;
	end;	
		
	listShape = Array[0..9] of Shape;	
  
procedure Main();
var	
	i,ran : Integer;
	lShape : listShape;
begin 
	OpenGraphicsWindow('Modelling Multi Shape', 800, 600);	
	
	repeat // The game loop...
		ProcessEvents();
    
		ClearScreen(ColorWhite);	
		
		for i := 0 to 9 do
		begin
			lShape[i].c := RandomColor();
			lShape[i].x := Rnd(800);
			lShape[i].y := Rnd(600);
			lShape[i].w := Rnd(300);
			lShape[i].h := Rnd(300);			
			ran := Rnd(3);			
			case ran of
				0: FillRectangle(lShape[i].c, lShape[i].x, lShape[i].y, lShape[i].w, lShape[i].h);
				1: DrawLine(lShape[i].c, lShape[i].x, lShape[i].y, lShape[i].w, lShape[i].h);
				2: FillEllipse(lShape[i].c, lShape[i].x, lShape[i].y, lShape[i].w, lShape[i].h);
			end;
		end;		
		RefreshScreen();
	until WindowCloseRequested();
  
	ReleaseAllResources();
end;

begin
  Main();
end.