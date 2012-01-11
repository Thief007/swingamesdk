program HowToRepeatingSoundEffect;
uses
  sgInput, sgGraphics, sgResources, sgText, sgAudio, sgUtils, sgTypes;

procedure Main();
begin  
  OpenAudio();
  OpenGraphicsWindow('Repeat Sound Effect', 320, 240);
  
  repeat // The game loop...
    ProcessEvents();    
	
	ClearScreen(ColorWhite);
	
	PlaySoundEffect(LoadSoundEffect('applause-3.wav')); // load sound effect in ogg, wave audio files	
	DrawText('How To Playing A Sound Effect!!!', ColorRed, 40, 120);
	
    RefreshScreen();
  until WindowCloseRequested();
  
  CloseAudio();
  ReleaseAllResources();
end;

begin
  Main();
end.