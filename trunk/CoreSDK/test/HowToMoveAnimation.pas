program HowToMoveAnAnimation;
uses
    sgGraphics, sgSprites, sgTypes, sgImages, sgUtils, sgInput, sgAudio, sgAnimations, sgResources;

procedure DoWalking(sprt: Sprite; key: KeyCode; animationName: String; dx, dy: Single);
begin
    if KeyDown(key) then
    begin
        SpriteStartAnimation(sprt, animationName);
        SpriteSetDX(sprt, dx);
        SpriteSetDY(sprt, dy);		
    end;
end;

procedure Main();
var
    walking: Sprite;
    myFrog: Bitmap;
begin
    OpenAudio();
    OpenGraphicsWindow('Walking Frog', 800, 600);

    myFrog := LoadBitmapNamed('walking', 'frog.png');
    BitmapSetCellDetails(myFrog, 73, 105, 4, 4, 16);
    LoadAnimationScriptNamed('WalkingScrpt', 'kermit.txt');

    walking := CreateSprite(myFrog, AnimationScriptNamed('WalkingScrpt'));
    SpriteStartAnimation(walking, 'StandFront');

    SpriteSetX(walking, 382);
    SpriteSetY(walking, 274);

    repeat
        ClearScreen(ColorWhite);
        DrawSprite(walking);
        RefreshScreen(60);
        UpdateSprite(walking);

        ProcessEvents();
        if SpriteAnimationHasEnded(walking) then
        begin
            SpriteSetDX(walking, 0);
            SpriteSetDY(walking, 0);
            
			DoWalking(walking, VK_UP, 	 'WalkBack',  0, -0.25);
            DoWalking(walking, VK_DOWN,  'WalkFront', 0, +0.25);
            DoWalking(walking, VK_LEFT,  'WalkLeft',  -0.25, 0);
            DoWalking(walking, VK_RIGHT, 'WalkRight',  +0.25, 0);
			
			DoWalking(walking, VK_W, 	 'MoonWalkBack',  0, -0.25);
            DoWalking(walking, VK_S,  'MoonWalkFront', 0, +0.25);
            DoWalking(walking, VK_A,  'MoonWalkLeft',  -0.25, 0);
            DoWalking(walking, VK_D, 'MoonWalkRight',  +0.25, 0);
			
			DoWalking(walking, VK_SPACE, 'DancingFrog', 0, 0);
		end;
    until WindowCloseRequested();

    CloseAudio();
    ReleaseAllResources();
end;

begin
    Main();
end.