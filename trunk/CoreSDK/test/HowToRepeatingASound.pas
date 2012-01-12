program HowToRepeatingASound;
uses
  sgInput, sgGraphics, sgResources, sgText, sgAudio, sgUtils, sgTypes;

procedure Main();
begin  
  OpenAudio();
  OpenGraphicsWindow('Repeating Sound Effect', 320, 240);
  
  repeat // The game loop...
    ProcessEvents();    
	
	ClearScreen(ColorWhite);
	
	PlaySoundEffect(LoadSoundEffect('Siren.ogg')); // load sound effect in ogg, wave audio files	
	DrawText('How To Play Sound Effect!!!', ColorRed, 10, 120);	
	
    RefreshScreen();
  until WindowCloseRequested();
  
  CloseAudio();
  ReleaseAllResources();
end;

begin
  Main();
end.