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
    asteroid: Sprite;
begin
    OpenGraphicsWindow('Create a Sprite', 800, 600);

    ClearScreen(ColorWhite);

    LoadBitmapNamed('asteroid', 'asteroid.png');

    asteroid := CreateSprite(BitmapNamed('asteroid'));
    SpriteSetX(asteroid, 100);
    SpriteSetY(asteroid, 500);
    SpriteSetMass(asteroid, 1);
    SpriteSetVelocity(asteroid, VectorTo(1, -0.6));

    repeat
        ProcessEvents();
        ClearScreen(ColorWhite);
        DrawSprite(asteroid);
        UpdateSprite(asteroid);
		
		KeepOnScreen(asteroid);
		RefreshScreen(60);
    until WindowCloseRequested();

    ReleaseAllResources();
end;

begin
    Main();
end.