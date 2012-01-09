program HowToCreateASprite;
uses sgGraphics, sgSprites, sgTypes, sgImages, sgUtils, sgResources;

procedure Main();
var
	ball: Sprite;
begin
    OpenGraphicsWindow('Create a Sprite', 800, 600);
	
	ClearScreen(ColorWhite);
	
	LoadBitmapNamed('ball', 'ball_small.png');
	
	ball := CreateSprite(BitmapNamed('ball'));
	SpriteSetX(ball, 50);
	SpriteSetY(ball, 100);
	
	DrawSprite(ball);
	UpdateSprite(ball);
	RefreshScreen();
	Delay(5000);
	
	ReleaseAllResources();
end;

begin
	Main();
end.