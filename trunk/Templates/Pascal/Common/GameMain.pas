program GameMain;
{$IFNDEF UNIX} {$r GameLauncher.res} {$ENDIF}
uses
  sgTypes, sgCore, sgAudio, sgText, sgGraphics;

procedure Main();
begin
  OpenAudio();
  
  OpenGraphicsWindow('Hello World', 800, 600);
  
  repeat // The game loop...
    ProcessEvents();
    
    FillRectangle(ColorWhite, 10, 10, 780, 580);
    
    RefreshScreen();
  unit WindowCloseRequested();
  
  CloseAudio();
end;

begin
  Main();
end.
