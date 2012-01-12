program HowToRepeatSound;
uses
  sgInput, sgGraphics, sgResources, sgText, sgAudio, sgUtils, sgTypes;

procedure Main();
begin  
  OpenAudio();
  OpenGraphicsWindow('How To Repeat Sound', 320, 240);
  
  repeat // The game loop...
    ProcessEvents();    
	
	ClearScreen(ColorWhite);
	
	PlaySoundEffect(LoadSoundEffect('chipmunk.ogg')); // load sound effect in ogg, wave audio files	
	DrawText('Repeat Sound Effect!!!', ColorRed, 55, 120);	
	
    RefreshScreen();
  until WindowCloseRequested();
  
  CloseAudio();
  ReleaseAllResources();
end;

begin
  Main();
end.