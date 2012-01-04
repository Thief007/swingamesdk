program HowToSoundEffect;
uses
  sgTypes, sgInput, sgAudio, sgGraphics, sgResources, sgUtils, sgText;

procedure Main();
begin
  OpenAudio();
  
  OpenGraphicsWindow('How To Sound Effect', 800, 600);
  
  PlaySoundEffect(LoadSoundEffect('boom.wav')); // load sound effect in ogg, wave audio files
  
  ClearScreen(ColorBlack);
  DrawFramerate(0,0);
    
  RefreshScreen();
    
  Delay(5000);	
  CloseAudio();
  ReleaseAllResources();  
end;

begin
  Main();
end.
