program HowToDrawBitmap;
uses sgGraphics, sgTypes, sgImages, sgUtils, sgAudio, sgResources, sgInput, sgShared, sgText;

procedure main();
begin    
    OpenGraphicsWindow('SDLTest', 300, 400);
    repeat // The game loop...
    ClearScreen(ColorWhite);
      ProcessEvents();
      RefreshScreen();
    until WindowCloseRequested();
    
	//ReleaseAllResources();
end;

begin
	main();
end.