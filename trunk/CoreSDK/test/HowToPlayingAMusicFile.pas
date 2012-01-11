program HowToPlayingAMusicFile;
uses
  sgInput, sgGraphics, sgResources, sgText, sgAudio, sgUtils;

procedure Main();
begin
	OpenAudio();

	OpenGraphicsWindow('How To Playing A Music File', 320, 240);

	ClearScreen(ColorWhite);
	
	PlayMusic(LoadMusic('diving-turtle.mp3')); 
	DrawText('How To Playing A Music File!!!', ColorRed, 40, 120);	
		
	RefreshScreen();
		
	Delay(5000);	
	CloseAudio();
	ReleaseAllResources();  
end;

begin
  Main();
end.
