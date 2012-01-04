program HowToMusic;
uses
  sgTypes, sgInput, sgAudio, sgGraphics, sgResources, sgUtils, sgText;

procedure Main();
begin
  OpenAudio();
  
  OpenGraphicsWindow('How To Music', 800, 600);  
  
  PlayMusic(LoadMusic('Fast.mp3')); 
  
  ClearScreen(ColorBlack);
  
  RefreshScreen();
  
  Delay(7000);
  CloseAudio();
  ReleaseAllResources();  
  
end;

begin
  Main();
end.
