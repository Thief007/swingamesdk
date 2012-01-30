program HowToPlaySoundEffect;
uses
    sgInput, sgGraphics, sgResources, sgText, sgAudio, sgUtils;

procedure Main();
begin
    OpenAudio();

    OpenGraphicsWindow('How To Play Sound Effect', 320, 240);

    ClearScreen(ColorWhite);
		LoadSoundEffect('chipmunk.ogg');    
		PlaySoundEffect('chipmunk.ogg');
    DrawText('How To Play Sound Effect!!!', ColorRed, 10, 120);	
	
    RefreshScreen();
	
    Delay(5000);	
    CloseAudio();
    ReleaseAllResources();  
end;

begin
    Main();
end.
