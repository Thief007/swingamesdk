program HowToExplodeASprite;
uses sgGraphics, sgSprites, sgTypes, sgImages, sgUtils, sgInput, sgAudio, sgAnimations;

procedure CheckExplosion(ball, explosion : sprite);
begin
	
end;

procedure Main();
var
	ball, explosion: Sprite;
	explosionBmp: Bitmap;
begin
    OpenAudio();
	OpenGraphicsWindow('Exploding a Sprite', 800, 600);
	
	LoadBitmapNamed('ball', 'ball_small.png');
	explosionBmp := LoadBitmapNamed('explosion', 'explosion_pro.png');
    BitmapSetCellDetails(explosionBmp, 72, 72, 6, 7, 45);
    LoadAnimationScriptNamed('explosion', 'explosion.txt');
	
	ball := CreateSprite(BitmapNamed('ball'));
	SpriteSetX(ball, 50);
	SpriteSetY(ball, 100);
	
	explosion := CreateSprite(explosionBmp, AnimationScriptNamed('explosion'));
	SpriteStartAnimation(explosion, 'explode');
	
	SpriteSetX(explosion, SpriteX(ball) + SpriteWidth(ball) / 2.0 - SpriteWidth(explosion) / 2.0);
    SpriteSetY(explosion, SpriteY(ball) + SpriteHeight(ball) / 2.0 - SpriteHeight(explosion) / 2.0);

	ClearScreen(ColorWhite);
	
	DrawSprite(ball);
	DrawSprite(explosion);
	
	RefreshScreen();
	Delay(1000);
	
	CloseAudio();
	ReleaseAllResources();  
end;

begin
	Main();
end.
