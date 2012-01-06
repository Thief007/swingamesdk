program HowToDrawDifferentShapeType;
uses
  sgTypes, sgInput, sgGraphics, sgResources, sgUtils;

type
	ShapeType = (Circle, Line, Rectangle, Triangle, Square);
	
procedure HandleInput();
var	
	typeOfShape : ShapeType;
begin	
	if(KeyDown(vk_c)) then typeOfShape := Circle;
	if(KeyDown(vk_l)) then typeOfShape := Line;
	if(KeyDown(vk_r)) then typeOfShape := Rectangle;
	if(KeyDown(vk_t)) then	typeOfShape := Triangle;
	if(KeyDown(vk_s)) then	typeOfShape := Square;
	
	if(MouseClicked(LeftButton)) then
	begin		
		case typeOfShape of
			Circle: 	FillCircle(RandomColor(), MousePosition.x, MousePosition.y, 40);
			Line:		DrawLine(RandomColor(), MousePosition.x-30, MousePosition.y-30, MousePosition.x+30, MousePosition.y+30);
			Rectangle: 	FillRectangle(RandomColor(), MousePosition.x-30, MousePosition.y-15, 60, 30);
			Square:		FillRectangle(RandomColor(), MousePosition.x-30, MousePosition.y-30, 60, 60);
			Triangle:	FillTriangle(RandomColor(), MousePosition.x-25, MousePosition.y+25, MousePosition.x+25, MousePosition.y+25, MousePosition.x, MousePosition.y-25);
		end;		
	end
end;
  
procedure Main();
begin 
	OpenGraphicsWindow('Drawing different type of shapes', 800, 600);	
	
	ClearScreen(ColorWhite);	
	
	repeat // The game loop...
		ProcessEvents();		
		
		HandleInput();		
		
		RefreshScreen();
	until WindowCloseRequested();
  
	ReleaseAllResources();
end;

begin
  Main();
end.