program HowToMoveASprite;
uses sgGraphics, sgText, sgSprites, sgTypes, sgImages, sgUtils, sgInput, sgResources, sgPhysics, sgAnimations;

procedure DrawAndUpdate(sprt: Sprite);
begin
    DrawSprite(sprt);
    UpdateSprite(sprt);
end;

procedure Main();
var
	ball1, ball2, explosion: Sprite;
	explosionBmp: Bitmap;
begin
    OpenGraphicsWindow('Collision with animation', 800, 600);
	
	LoadBitmapNamed('ball', 'ball_small.png');
    LoadBitmapNamed('background', 'backgroundImage.png');
    
    explosionBmp := LoadBitmapNamed('explosion', 'red_explosion.png');
    BitmapSetCellDetails(explosionBmp, 38, 38, 8, 2, 16);
    LoadAnimationScriptNamed('explosionScrpt', 'RedExplosion.txt');
	
	explosion := nil;
    
    RefreshScreen();
    
	ball1 := CreateSprite(BitmapNamed('ball'));
    ball2 := CreateSprite(BitmapNamed('ball'));
    
	SpriteSetX(ball1, 400);
	SpriteSetY(ball1, 0);
	SpriteSetDx(ball1, 0);
	SpriteSetDy(ball1, 0.5);
    
    SpriteSetX(ball2, 400);
	SpriteSetY(ball2, 560);
	SpriteSetDx(ball2, 0);
	SpriteSetDy(ball2, -0.5);
	
	repeat
        ClearScreen(ColorWhite);
        
        DrawLineprogram HowToMoveASprite;
uses sgGraphics, sgText, sgSprites, sgTypes, sgImages, sgUtils, sgInput, sgResources, sgPhysics, sgAnimations;

procedure DrawAndUpdate(sprt: Sprite);
begin
    DrawSprite(sprt);
    UpdateSprite(sprt);
end;

procedure Main();
var
	ball1, ball2, explosion: Sprite;
	explosionBmp: Bitmap;
    lines: array [0..3] of LineSegment;
begin
    OpenGraphicsWindow('Collision with animation', 800, 600);
	
	LoadBitmapNamed('ball', 'ball_small.png');
    LoadBitmapNamed('background', 'backgroundImage.png');
    
    explosionBmp := LoadBitmapNamed('explosion', 'red_explosion.png');
    BitmapSetCellDetails(explosionBmp, 38, 38, 8, 2, 16);
    LoadAnimationScriptNamed('explosionScrpt', 'RedExplosion.txt');
	
	explosion := nil;
    
    RefreshScreen();
    
	ball1 := CreateSprite(BitmapNamed('ball'));
    ball2 := CreateSprite(BitmapNamed('ball'));
    
	SpriteSetX(ball1, 400);
	SpriteSetY(ball1, 0);
	SpriteSetDx(ball1, 0);
	SpriteSetDy(ball1, 0.5);
    
    SpriteSetX(ball2, 400);
	SpriteSetY(ball2, 560);
	SpriteSetDx(ball2, 0);
	SpriteSetDy(ball2, -0.5);
	
	repeat
        ClearScreen();
        DrawBitmap ('background', 0, 0);

        if(SpriteCollision(ball1, ball2)) then
        begin
            CollideCircles(ball1, ball2);
            if explosion <> nil then FreeSprite(explosion);
            
            explosion := CreateSprite(explosionBmp, AnimationScriptNamed('explosionScrpt'));
            SpriteStartAnimation(explosion, 'FireExplosion');
            SpriteSetX(explosion, ((SpriteX(ball1)) + 15));
            SpriteSetY(explosion, ((SpriteY(ball1)) + 15));
        end;
        
        if(SpriteY(ball1) < 1) then
            SpriteSetDy(ball1, 0.5);
            
        if(SpriteY(ball2) > 500) then
            SpriteSetDy(ball2, -0.5);
        
        DrawAndUpdate(ball1);
        DrawAndUpdate(ball2);
        if explosion <> nil then
            DrawAndUpdate(explosion);
        
        RefreshScreen();
        ProcessEvents();
	until WindowCloseRequested();

	ReleaseAllResources();  
end;

begin
	Main();
end.

        if(SpriteCollision(ball1, ball2)) then
        begin
            if explosion <> nil then ReleaseSprite(explosion);
            explosion := CreateSprite(explosionBmp, AnimationScriptNamed('explosionScrpt'));
            SpriteStartAnimation(explosion, 'FireExplosion');
            SpriteSetX(explosion, ((SpriteX(ball1)) + 100));
            SpriteSetY(explosion, ((SpriteY(ball1)) + 200));
        end;
        
        if(SpriteY(ball1) < 1) then
            SpriteSetDy(ball1, 0.5);
            
        if(SpriteY(ball2) > 500) then
            SpriteSetDy(ball2, -0.5);
        
        DrawAndUpdate(ball1);
        DrawAndUpdate(ball2);
        DrawAndUpdate(explosion);
        
        RefreshScreen();
        ProcessEvents();
	until WindowCloseRequested();

	ReleaseAllResources();  
end;

begin
	Main();
end.