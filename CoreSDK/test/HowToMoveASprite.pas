program HowToMoveASprite;
uses sgGraphics, sgSprites, sgTypes, sgImages, sgUtils, sgInput, sgResources;

procedure Main();
var
	ball: Sprite;
begin
    OpenGraphicsWindow('Create a Sprite', 800, 600);
	
	LoadBitmapNamed('ball', 'ball_small.png');
	
	ball := CreateSprite(BitmapNamed('ball'));
	SpriteSetX(ball, 0);
	SpriteSetY(ball, 100);
	
	SpriteSetDx(ball, 0.5);
	SpriteSetDy(ball, 0.5);
	
	repeat
		ClearScreen(ColorWhite);
		DrawSprite(ball);
		UpdateSprite(ball);
		RefreshScreen();
		ProcessEvents();
	until WindowCloseRequested();

	ReleaseAllResources();  
end;

begin
	Main();
end.