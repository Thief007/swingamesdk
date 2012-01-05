program HowToRepeatSoundEffect;
uses
  sgInput, sgGraphics, sgResources, sgText, sgAudio;

procedure Main();
begin  
  OpenAudio();
  OpenGraphicsWindow('Repeat Sound Effect', 800, 600);
  
  repeat // The game loop...
    ProcessEvents();
    
	PlaySoundEffect(LoadSoundEffect('boom.wav')); // load sound effect in ogg, wave audio files
	
    ClearScreen(ColorWhite);
	
    RefreshScreen();
  until WindowCloseRequested();
  
  CloseAudio();
  ReleaseAllResources();
end;

begin
  Main();
end.