program HowToPlaySoundEffect;
uses
    sgInput, sgGraphics, sgResources, sgText, sgAudio, sgUtils;

procedure Main();
begin
    OpenAudio();

    OpenGraphicsWindow('How To Play Sound Effect', 320, 240);

    ClearScreen(ColorWhite);

    PlaySoundEffect(LoadSoundEffect('chipmunk.ogg')); // load sound effect in ogg, wave audio files	
    DrawText('How To Play Sound Effect!!!', ColorRed, 10, 120);	
	
    RefreshScreen();
	
    Delay(5000);	
    CloseAudio();
    ReleaseAllResources();  
end;

begin
    Main();
end.
