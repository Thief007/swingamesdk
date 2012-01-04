program HowToDrawBitmap;
uses sgGraphics, sgTypes, sgImages, sgUtils, sgAudio;

procedure main();
begin
    OpenAudio();
    OpenGraphicsWindow('Import Bitmap', 800, 600);
    
	ClearScreen();
	
	LoadBitmapNamed('predator', 'corpolupo1.png');
	DrawBitmap ('predator', 350, 100);
	
	RefreshScreen();
    
    delay(5000);
end;

begin
	main();
end.