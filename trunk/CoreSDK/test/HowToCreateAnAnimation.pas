program HowToCreateAnAnimation;
uses sgGraphics, sgSprites, sgTypes, sgImages, sgUtils, sgInput, sgAudio, sgAnimations, sgResources;

procedure Main();
var
	explosion: Sprite;
	explosionBmp: Bitmap;
begin
    OpenAudio();
	OpenGraphicsWindow('Exploding a Sprite', 800, 600);

	explosionBmp := LoadBitmapNamed('explosion', 'explosion_pro.png');
    BitmapSetCellDetails(explosionBmp, 72, 72, 6, 7, 45);
    LoadAnimationScriptNamed('explosionScrpt', 'explosion.txt');
	
	explosion := CreateSprite(explosionBmp, AnimationScriptNamed('explosionScrpt'));
	SpriteStartAnimation(explosion, 'FireExplosion');
	
	SpriteSetX(explosion, 400);
    SpriteSetY(explosion, 300);
	
	repeat
		ClearScreen(ColorWhite);
		DrawSprite(explosion);
		RefreshScreen();
		
		UpdateSprite(explosion);
		
		ProcessEvents();
	until SpriteAnimationHasEnded(explosion);
	
	Delay(800);
	
	CloseAudio();
	ReleaseAllResources();
end;

begin
	Main();
end.