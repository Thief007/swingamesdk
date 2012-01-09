program HowToPlaySound;
uses sgGraphics, sgTypes, sgImages, sgUtils, sgAudio, sgResources;

procedure Main();
begin
    OpenAudio();
    OpenGraphicsWindow('Boom Boom', 800, 600);
	
	ClearScreen();
	
	LoadSoundEffectNamed('boom', 'boom.wav');
	
	PlaySoundEffect('boom');
	Delay(500);	
	
	RefreshScreen();
    
	PlaySoundEffect('boom');
    Delay(500);
	
	CloseAudio();
	ReleaseAllResources();
end;

begin
	Main();
end.