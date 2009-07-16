program HelloWorld;
{$IFNDEF UNIX} {$r GameLauncher.res} {$ENDIF}
uses
  sgTypes, sgCore, sgAudio, sgText, sgGraphics, sgResources;

procedure Main();
begin
  OpenAudio();
  
  OpenGraphicsWindow('Hello World', 800, 600);
  
  repeat // The game loop...
    ProcessEvents();
    
    DrawBitmap(GetBitmap('SplashBack'), 0, 0);
    FillRectangle(ColorWhite, 10, 10, 780, 580);
    
    RefreshScreen();
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  CloseAudio();
end;

begin
  Main();
end.
