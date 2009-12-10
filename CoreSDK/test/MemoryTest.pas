program HelloWorld;
{$IFNDEF UNIX} {$r GameLauncher.res} {$ENDIF}
uses
  cmem, sgTypes, sgCore, sgAudio, sgText, sgGraphics, sgResources;

procedure Main();
begin
  OpenAudio();
  
  OpenGraphicsWindow('Hello World', 800, 600);
  
  repeat // The game loop...
    ProcessEvents();
    
    ClearScreen();
    
    ReleaseAllResources();
    LoadResourceBundle('splash.txt');
    
    DrawFramerate(0,0);
    
    RefreshScreen();
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  CloseAudio();
end;

begin
  Main();
end.
