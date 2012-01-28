program HowToCreateASprite;
uses
    sgGraphics, sgInput, sgPhysics, sgSprites, sgTypes, sgImages, sgUtils, sgResources;

procedure Main();
var
    ball: Sprite;
begin
    OpenGraphicsWindow('Create a Sprite', 800, 600);

    ClearScreen(ColorWhite);

    LoadBitmapNamed('ball', 'ball_small.png');

    ball := CreateSprite(BitmapNamed('ball'));
    SpriteSetX(ball, 385);
    SpriteSetY(ball, 285);

	repeat
	ProcessEvents();
	SpriteSetDx(ball, 0);
    SpriteSetDy(ball, 0.02);
	
	ClearScreen();
	
	if KeyDown(VK_RIGHT) then
	begin
        SpriteSetDx(ball, 0.2);
        SpriteSetDy(ball, 0);
	end
	
	else if KeyDown(VK_LEFT) then
	begin
        SpriteSetDx(ball, -0.2);
        SpriteSetDy(ball, 0);
	end

    else if KeyDown(VK_UP) then
	begin
        SpriteSetDx(ball, 0);
        SpriteSetDy(ball, -0.2);
	end
	
	else if KeyDown(VK_DOWN) then
	begin
        SpriteSetDx(ball, 0);
        SpriteSetDy(ball, 0.2);
	end
	
	else
	begin
	    SpriteSetDx(ball, 0);
        SpriteSetDy(ball, 0);
	end;
		
    DrawSprite(ball);
    UpdateSprite(ball);
    RefreshScreen();
    until WindowCloseRequested();

    ReleaseAllResources();
end;

begin
    Main();
end.