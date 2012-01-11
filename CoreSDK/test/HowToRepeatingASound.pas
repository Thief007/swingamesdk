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
	
	PlaySoundEffect(LoadSoundEffect('applause-3.wav')); // load sound effect in ogg, wave audio files	
	DrawText('The applause sound effect only lasted', ColorRed, 10, 90);
	DrawText('for 18 sec yet the sound keep looping!', ColorRed, 10, 110);	
	
    RefreshScreen();
  until WindowCloseRequested();
  
  CloseAudio();
  ReleaseAllResources();
end;

begin
  Main();
end.