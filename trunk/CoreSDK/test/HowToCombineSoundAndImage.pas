program HowToCombineSoundAndImage;
uses sgAudio, sgGraphics, sgUtils, sgResources;

procedure CircleEffect();
begin
	FillCircle(ColorRed, 400, 300, 25);
	RefreshScreen();
	PlaySoundEffect('boom');
	Delay(400);
	ClearScreen(ColorWhite);
	Delay(200);
end;

procedure RectangleEffect();
begin
	FillRectangle(ColorBlue,300,275,200,50);
	RefreshScreen();
	PlaySoundEffect('shock');
	Delay(400);
	ClearScreen(ColorWhite);
	Delay(200);
end;
	
procedure main();
begin
    OpenAudio();
    OpenGraphicsWindow('Sound and image', 800, 600);
	
	ClearScreen(ColorWhite);
	
	Delay(200);
   
    LoadSoundEffectNamed('boom', 'boom.wav');
	LoadSoundEffectNamed('shock', 'shock.wav');
	
	CircleEffect();
	RectangleEffect();
	Delay(1600);
	
	CircleEffect();
	RectangleEffect();
	Delay(1600);
	
	CloseAudio();
	ReleaseAllResources();
end;

begin
	main();
end.