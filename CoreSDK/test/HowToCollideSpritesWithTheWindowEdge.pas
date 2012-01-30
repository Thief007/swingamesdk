program HowToCreateASprite;
uses
    sgTypes, sgAudio, sgText, sgGraphics, sgGeometry, sgResources, sgSprites, sgInput, sgPhysics, sgImages;
	
procedure KeepOnScreen(s: Sprite);
begin
  if SpriteX(s) > ScreenWidth() - SpriteWidth(s) then
  begin
    SpriteSetDX(s, -SpriteDX(s));
    SpriteSetX(s, ScreenWidth() - SpriteWidth(s));
  end;
  if SpriteY(s) > ScreenHeight() - SpriteHeight(s) then
  begin
    SpriteSetDY(s, -SpriteDY(s));
    SpriteSetY(s, ScreenHeight() - SpriteHeight(s));
  end;
  
  if SpriteX(s) < 0 then
  begin
    SpriteSetDX(s, -SpriteDX(s));
    SpriteSetX(s, 0);
  end;
  if SpriteY(s) < 0 then
  begin
    SpriteSetDY(s, -SpriteDY(s));
    SpriteSetY(s, 0);
  end;
end;

procedure Main();
var
    earth: Sprite;
    asteroid: Sprite;
begin
    OpenGraphicsWindow('Create a Sprite', 800, 600);

    ClearScreen(ColorWhite);

    LoadBitmapNamed('earth', 'earth.png');
    LoadBitmapNamed('asteroid', 'asteroid.png');

    earth := CreateSprite(BitmapNamed('earth'));
    SpriteSetX(earth, 700);
    SpriteSetY(earth, 100);
    SpriteSetMass(earth, 10);
    SpriteSetVelocity(earth, VectorTo(-0.8, 0.6));

    asteroid := CreateSprite(BitmapNamed('asteroid'));
    SpriteSetX(asteroid, 100);
    SpriteSetY(asteroid, 500);
    SpriteSetMass(asteroid, 1);
    SpriteSetVelocity(asteroid, VectorTo(1, -0.6));

    repeat
        ProcessEvents();
        ClearScreen(ColorWhite);
        DrawSprite(earth);
        UpdateSprite(earth);
        DrawSprite(asteroid);
        UpdateSprite(asteroid);
		
		KeepOnScreen(earth);
		KeepOnScreen(asteroid);
		
        if SpriteCollision(earth, asteroid) then CollideCircles(asteroid, earth);

        RefreshScreen();
    until WindowCloseRequested();

    ReleaseAllResources();
end;

begin
    Main();
end.