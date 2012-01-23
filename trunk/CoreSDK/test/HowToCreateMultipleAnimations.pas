program HowToCreateMultipleAnimation;
uses
    sgGraphics, sgSprites, sgTypes, sgImages, sgUtils, sgInput, sgAudio, sgAnimations, sgResources, sgText;

procedure Main();
var
    explosion: Sprite;
    explosionBmp: Bitmap;
begin
    OpenAudio();
    OpenGraphicsWindow('Miltiple Animations', 200, 200);

    explosionBmp := LoadBitmapNamed('explosion', 'explosion_pro.png');
    BitmapSetCellDetails(explosionBmp, 72, 72, 6, 7, 45);
    LoadAnimationScriptNamed('explosionScrpt', 'explosion.txt');

    explosion := CreateSprite(explosionBmp, AnimationScriptNamed('explosionScrpt'));

    SpriteSetX(explosion, 64);
    SpriteSetY(explosion, 64);

    repeat
        ClearScreen(ColorWhite);
        DrawText('[E]xplosion', ColorBlack, 0, 0);
        DrawText('[I]mplosion', ColorBlack, 0, 10);
        DrawText('[L]oop', ColorBlack, 0, 20);
        DrawSprite(explosion);
        RefreshScreen(60);

        UpdateSprite(explosion);

        ProcessEvents();
        if KeyTyped(VK_E) then SpriteStartAnimation(explosion, 'explosion');
        if KeyTyped(VK_I) then SpriteStartAnimation(explosion, 'implosion');
        if KeyTyped(VK_L) then SpriteStartAnimation(explosion, 'explosion_loop');

    until WindowCloseRequested();

    CloseAudio();
    ReleaseAllResources();
end;

begin
    Main();
end.