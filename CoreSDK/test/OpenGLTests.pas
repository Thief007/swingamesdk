program HelloWorld;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}
uses
  SwinGame;


procedure Main();
var
  img: Bitmap;
  i: Integer;
begin
  OpenGLDriver.CreateWindow('Hello World', 640, 480);

end;

begin
  Main();
end.
