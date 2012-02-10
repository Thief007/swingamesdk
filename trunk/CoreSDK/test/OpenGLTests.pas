program OpenGLTests;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}
uses
  sgDriverGraphics,sgGraphics, sgUtils,sgInput,sgGeometry;
  
const LONG_INT_LIMIT = 2147483647;

procedure Main();
var
  x,y : LongInt;
begin
  x:=0;
  y:=0;
  Writeln('opening window');
  OpenGraphicsWindow('Hello World', 1024, 768);
  Writeln('clearing screen to red');
  Writeln('refreshing screen');
  repeat
    begin
      ProcessEvents();
      ClearScreen(LONG_INT_LIMIT);
      GraphicsDriver.DrawRectangle(nil, RectangleFrom(0,0,x,y), rnd(LONG_INT_LIMIT));
      GraphicsDriver.FillRectangle(nil, RectangleFrom(50,50,x,y), $FF00FFFF);
      GraphicsDriver.DrawLine(nil,0,0,x+10,y+10, rnd(LONG_INT_LIMIT));
      GraphicsDriver.FillTriangle(nil,$FF0000FF,0,0,0,10+y,x,y);
      GraphicsDriver.DrawTriangle(nil,rnd(LONG_INT_LIMIT),0,0,0,10+y,x-50,y-50);
      GraphicsDriver.FillCircle(nil,$FFFF0000,10*2,10*2,600);
      GraphicsDriver.DrawCircle(nil,rnd(LONG_INT_LIMIT),x*2,y*2,y);
      // GraphicsDriver.DrawEllipse(nil,rnd(LONG_INT_LIMIT),x*2,y*2,x,y);
      // GraphicsDriver.FillEllipse(nil,$FF00FF00,x*2,y*2,x,y);
      RefreshScreen();
      x += 1;
      y += 1;
    end;
  until WindowCloseRequested();


end;

begin
  Main();
end.
