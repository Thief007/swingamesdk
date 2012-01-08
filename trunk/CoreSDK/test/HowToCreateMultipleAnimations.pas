program HowToCreateAnAnimation;
uses sgGraphics, sgSprites, sgTypes, sgImages, sgUtils, sgInput, sgAudio, sgAnimations;

procedure Main();
var
	explosion: Sprite;
	implosion: Sprite;
	explosion_loop: Sprite;
	explosionBmp: Bitmap;
begin
    OpenAudio();
	OpenGraphicsWindow('Miltiple Animations', 800, 600);

	explosionBmp := LoadBitmapNamed('explosion', 'explosion_pro.png');
    BitmapSetCellDetails(explosionBmp, 72, 72, 6, 7, 45);
    LoadAnimationScriptNamed('explosionScrpt', 'explosion.txt');
	
	explosion := CreateSprite(explosionBmp, AnimationScriptNamed('explosionScrpt'));
	implosion := CreateSprite(explosionBmp, AnimationScriptNamed('explosionScrpt'));
	explosion_loop := CreateSprite(explosionBmp, AnimationScriptNamed('explosionScrpt'));
	
	SpriteStartAnimation(explosion, 'explosion');
	SpriteStartAnimation(implosion, 'implosion');
	SpriteStartAnimation(explosion_loop, 'explosion_loop');
	
	SpriteSetX(explosion, 200);
    SpriteSetY(explosion, 100);
	
    SpriteSetX(implosion, 400);
	SpriteSetY(implosion, 300);
	
	SpriteSetX(explosion_loop, 600);
	SpriteSetY(explosion_loop, 500);
	
	ClearScreen(ColorWhite);
	
	repeat
		ClearScreen(ColorWhite);
		DrawSprite(explosion);
		RefreshScreen();
		UpdateSprite(explosion);
		ProcessEvents();
	until SpriteAnimationHasEnded(explosion);
	
	delay(1500);
	
	repeat
		ClearScreen(ColorWhite);
		DrawSprite(implosion);
		RefreshScreen();
		UpdateSprite(implosion);
		ProcessEvents();
	until SpriteAnimationHasEnded(implosion);
	
	delay(1500);
	
	repeat
		ClearScreen(ColorWhite);
		DrawSprite(explosion_loop);
		RefreshScreen();
		UpdateSprite(explosion_loop);
		ProcessEvents();
	until WindowCloseRequested();
	
	CloseAudio();
	ReleaseAllResources();
end;

begin
	Main();
end.