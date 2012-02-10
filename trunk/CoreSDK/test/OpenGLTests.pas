program OpenGLTests;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}
uses
  sgDriverGraphics,sgGraphics, sgUtils;
  


procedure Main();
begin
  Writeln('opening window');
  GraphicsDriver.InitializeGraphicsWindow('Hello World', 640, 480);
  Writeln('clearing screen to red');
  Writeln('refreshing screen');
  ClearScreen($FFFFFFFF);
  RefreshScreen();
  Delay(5000);
end;

begin
  Main();
end.
