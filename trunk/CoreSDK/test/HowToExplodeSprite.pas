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
	bitmap explosion := LoadBitmapNamed('explosion', 'explosion_pro.png');
    bitmap_set_cell_details(explosion, 72, 72, 6, 7, 45);
    load_animation_script_named("explosion", "explosion.txt");
	
	ball := CreateSprite(BitmapNamed('ball'));
	SpriteSetX(ball, 50);
	SpriteSetY(ball, 100);

	ClearScreen(ColorWhite);
	
	CheckExplosion(ball, explosion);
	
	DrawSprite(ball);
	
	
	
	UpdateSprite(ball);
	
	RefreshScreen();
	
end;

begin
	Main();
end.