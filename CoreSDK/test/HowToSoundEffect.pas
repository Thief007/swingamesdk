program HowToSoundEffect;
uses
  sgInput, sgGraphics, sgResources, sgText, sgAudio, sgUtils;

procedure Main();
begin
  OpenAudio();
  
  OpenGraphicsWindow('How To Sound Effect', 800, 600);
  
  PlaySoundEffect(LoadSoundEffect('boom.wav')); // load sound effect in ogg, wave audio files
  
  ClearScreen(ColorWhite);
    
  RefreshScreen();
    
  Delay(5000);	
  CloseAudio();
  ReleaseAllResources();  
end;

begin
  Main();
end.
