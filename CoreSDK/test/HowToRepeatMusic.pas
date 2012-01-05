program HowToRepeatMusic;
uses
  sgInput, sgGraphics, sgResources, sgText, sgAudio;

procedure Main();
begin  
  OpenAudio();
  OpenGraphicsWindow('Repeat Music', 800, 600);
  
  PlayMusic(LoadMusic('Fast.mp3')); 
  
  repeat // The game loop...
    ProcessEvents();
	
    ClearScreen(ColorWhite);
	
    RefreshScreen();
  until WindowCloseRequested();
  
  CloseAudio();
  ReleaseAllResources();
end;

begin
  Main();
end.