program HowToModelShapeUsingRecord;
uses
  sgTypes, sgInput, sgGraphics, sgResources, sgUtils, sgGeometry;
  
procedure Main();
var
  c : Circle;
  r : Rectangle;
  t : Triangle;
  l : LineSegment;
begin 
  OpenGraphicsWindow('Model Shape Using Record', 600, 100); 

  c := CircleAt(50, 50, 30);
  r := RectangleFrom(300, 20, 100, 60);
  t := TriangleFrom(450, 80, 550, 80, 500, 20);
  l := LineFrom(100, 40, 250, 40);
  
  ClearScreen(ColorWhite);   

  DrawCircle(ColorBlue, c);  
  DrawRectangle(ColorGreen, r);
  DrawTriangle(ColorPink, t);
  DrawLine(ColorRed, l);
  
  RefreshScreen();
  
  Delay(5000);
  ReleaseAllResources();  
end;

begin
  Main();
end.