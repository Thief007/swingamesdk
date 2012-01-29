program HowToPlayMusic;
uses
    sgInput, sgGraphics, sgResources, sgText, sgAudio, sgUtils;

procedure Main();
begin
    OpenAudio();

    OpenGraphicsWindow('How To Play Music', 320, 240);

    ClearScreen(ColorWhite);

    PlayMusic(LoadMusic('diving-turtle.mp3')); 
    DrawText('How To Play Music!!!', ColorRed, 40, 120);	

    RefreshScreen();

    Delay(5000);	
    CloseAudio();
    ReleaseAllResources();  
end;

begin
    Main();
end.
