program OpenGLTests;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}
uses
  sgDriverGraphics,sgGraphics, sgUtils,sgInput;
  


procedure Main();
begin
  Writeln('opening window');
  GraphicsDriver.InitializeGraphicsWindow('Hello World', 640, 480);
  Writeln('clearing screen to red');
  Writeln('refreshing screen');
  repeat
    begin
      ProcessEvents();
      ClearScreen(rnd(2147483647));
      RefreshScreen();
    end;
  until WindowCloseRequested();


end;

begin
  Main();
end.
