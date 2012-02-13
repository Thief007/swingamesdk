program OpenGLTests;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}
uses
  sgDriverGraphics,sgGraphics, sgUtils,sgInput,sgGeometry,sgTypes, sgTimers;
  
const LONG_INT_LIMIT = 2147483647;

procedure Main();
var
  x,y : LongInt;
  stopwatch : Timer;
begin
  x :=0;
  Writeln('opening window');
  OpenGraphicsWindow('Hello World', 1024, 768);
  Writeln('clearing screen to red');
  Writeln('refreshing screen');

  stopwatch := CreateTimer();

  StartTimer(stopwatch);
  repeat
    begin
      
      ProcessEvents();
      ClearScreen(LONG_INT_LIMIT);
      GraphicsDriver.DrawRectangle(Screen, RectangleFrom(rnd(1024),rnd(768),rnd(120),rnd(120)), rnd(LONG_INT_LIMIT));
      GraphicsDriver.FillRectangle(Screen, RectangleFrom(rnd(1024),rnd(768),rnd(120),rnd(120)), $FF00FFFF);
      GraphicsDriver.DrawLine(Screen,rnd(1024),rnd(768),rnd(120),rnd(120), rnd(LONG_INT_LIMIT));
      GraphicsDriver.FillTriangle(Screen,$FF0000FF,rnd(1024),rnd(768),rnd(120),rnd(120),rnd(120),rnd(120));
      GraphicsDriver.DrawTriangle(Screen,rnd(LONG_INT_LIMIT),rnd(1024),rnd(768),rnd(120),rnd(120),rnd(120),rnd(120));
      GraphicsDriver.FillCircle(Screen,$FFFF0000,rnd(1024),rnd(768),rnd(120));
      GraphicsDriver.DrawCircle(Screen,rnd(LONG_INT_LIMIT),rnd(1024),rnd(768),rnd(120));
      GraphicsDriver.DrawEllipse(Screen,rnd(LONG_INT_LIMIT),rnd(1024),rnd(768),rnd(120),rnd(120));
      GraphicsDriver.FillEllipse(Screen,$FF00FF00,rnd(1024),rnd(768),rnd(120),rnd(120));
      RefreshScreen();
      x += 1;
    end;
  until x= 1000;
WriteLn('Time: ', TimerTicks(stopwatch));
FreeTimer(stopwatch);

end;

begin
  Main();
end.
