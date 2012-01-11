program HowToOpenHelloWorldWindowWithFramerate;
uses
  sgTypes, sgInput, sgAudio, sgGraphics, sgResources, sgUtils, sgText;

procedure Main();
begin
  OpenAudio();
  
  OpenGraphicsWindow('Hello World', 800, 600);
  
  repeat // The game loop...
    ProcessEvents();
    
    ClearScreen();
    DrawFramerate(0,0);
    
    RefreshScreen();
  until WindowCloseRequested();
  
  CloseAudio();
  ReleaseAllResources();
end;

begin
  Main();
end.
