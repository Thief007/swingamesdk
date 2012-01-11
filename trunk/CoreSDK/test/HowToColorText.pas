program HowToColorText;
uses sgGraphics, sgImages, sgUtils, sgText;

procedure main();
begin
    OpenGraphicsWindow('Colored text', 800, 600);

	ClearScreen(ColorWhite);
	
	DrawText('Hello World!', ColorRed, 380, 280);
	DrawText('Im Here!!!', ColorGreen, 380, 320);
	
	RefreshScreen();
    
    Delay(500);
end;

begin
	main();
end.
