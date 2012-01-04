program HowToCreateASprite;
uses sgGraphics, sgSprites, sgTypes, sgImages, sgUtils, sgInput;

procedure CheckExplosion(ball, explosion : sprite);
begin
	
end;

procedure Main();
var
	ball, explosion: Sprite;
begin
    OpenGraphicsWindow('Exploding a Sprite', 800, 600);
	
	LoadBitmapNamed('ball', 'ball_small.png');
	bitmap explosion = load_bitmap_named("explosion", "explosion_pro.png");
    bitmap_set_cell_details(explosion, 72, 72, 6, 7, 45);
    load_animation_script_named("explosion", "explosion.txt");
	
	ball := CreateSprite(BitmapNamed('ball'));
	SpriteSetX(ball, 50);
	SpriteSetY(ball, 100);
	
	repeat
		ClearScreen(ColorWhite);
		
		CheckExplosion(ball, explosion);
		
		SpriteSetDx(ball, 0.5);
		SpriteSetDy(ball, 0.5);
		DrawSprite(ball);
		UpdateSprite(ball);
		
		RefreshScreen();
	until (WindowCloseRequested() OR (p = 1));
end;

begin
	Main();
end.