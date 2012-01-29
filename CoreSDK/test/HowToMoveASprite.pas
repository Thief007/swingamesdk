program HowToMoveASprite;
uses 
    sgGraphics, sgSprites, sgTypes, sgImages, sgUtils, sgInput, sgResources;

procedure Main();
var
    ball: Sprite;
begin
    OpenGraphicsWindow('Create a Sprite', 150, 150);

    LoadBitmapNamed('ball', 'ball_small.png');

    ball := CreateSprite(BitmapNamed('ball'));
    SpriteSetX(ball, 60);
    SpriteSetY(ball, 0);

    SpriteSetDx(ball, 0);
    SpriteSetDy(ball, 0.05);

    repeat
        ClearScreen(ColorWhite);
        if SpriteOffscreen(ball) then
        begin
            SpriteSetX(ball, 60);
            SpriteSetY(ball, -30);
        end;
        DrawSprite(ball);

        UpdateSprite(ball);
        RefreshScreen();
        ProcessEvents();
    until WindowCloseRequested();

    ReleaseAllResources();  
end;

begin
    Main();
end.