program HowToSoundEffect;
uses
  sgInput, sgGraphics, sgResources, sgText, sgAudio, sgUtils;

procedure Main();
begin
	OpenAudio();

	OpenGraphicsWindow('How To Playing A Sound Effect File', 320, 240);

	ClearScreen(ColorWhite);
	
	PlaySoundEffect(LoadSoundEffect('applause-3.wav')); // load sound effect in ogg, wave audio files	
	DrawText('How To Playing A Sound Effect File!!!', ColorRed, 40, 120);	
		
	RefreshScreen();
		
	Delay(5000);	
	CloseAudio();
	ReleaseAllResources();  
end;

begin
  Main();
end.
