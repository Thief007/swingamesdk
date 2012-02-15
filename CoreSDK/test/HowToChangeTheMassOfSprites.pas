program HowToCreateASprite;
uses
    sgTypes, sgAudio, sgText, sgGraphics, sgGeometry, sgResources, sgSprites, sgInput, sgPhysics, sgImages;

procedure Main();
var
    earth: Sprite;
    asteroid: Sprite;
begin
    OpenGraphicsWindow('Sprites with different mass', 800, 600);

    ClearScreen(ColorWhite);

    LoadBitmapNamed('earth', 'earth.png');
    LoadBitmapNamed('asteroid', 'asteroid.png');

    earth := CreateSprite(BitmapNamed('earth'));
    SpriteSetX(earth, 700);
    SpriteSetY(earth, 100);
    SpriteSetVelocity(earth, VectorTo(-0.8, 0.6));
    SpriteSetMass(earth, 10);

    asteroid := CreateSprite(BitmapNamed('asteroid'));
    SpriteSetX(asteroid, 100);
    SpriteSetY(asteroid, 500);
    SpriteSetVelocity(asteroid, VectorTo(1, -0.6));
    SpriteSetMass(asteroid, 1);

    repeat
        ProcessEvents();
        ClearScreen(ColorWhite);
        DrawSprite(earth);
        UpdateSprite(earth);
        DrawSprite(asteroid);
        UpdateSprite(asteroid);
        if SpriteCollision(earth, asteroid) then CollideCircles(asteroid, earth);

        RefreshScreen();
    until WindowCloseRequested();

    ReleaseAllResources();
end;

begin
    Main();
end.