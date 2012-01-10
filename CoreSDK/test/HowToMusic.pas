program HowToMusic;
uses
  sgGraphics, sgResources, sgAudio, sgUtils;

procedure Main();
begin
  OpenAudio();
  
  OpenGraphicsWindow('How To Music', 800, 600);  
  
  PlayMusic(LoadMusic('Fast.mp3')); 
  
  ClearScreen(ColorWhite);
  
  RefreshScreen();
  
  Delay(7000);
  CloseAudio();
  ReleaseAllResources();  
  
end;

begin
  Main();
end.
