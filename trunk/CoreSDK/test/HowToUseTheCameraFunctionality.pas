program HowToUseTheCameraFunctionality;
uses
  sgInput, sgGraphics, sgResources, sgText, sgTypes, sgGeometry, sgUtils, sgCamera, sgImages, sgSprites;  
  
procedure Main();
var	
	leftCPos, rightCPos, topCPos, lowCpos : Single;
	sSprite : Sprite;
begin 
  OpenGraphicsWindow('Explore with camera', 800, 600);
	
	LoadBitmapNamed('image', 'SwinGame.jpg');
	
	sSprite := CreateSprite(BitmapNamed('image'));
	SpriteSetX(sSprite, 60);
	SpriteSetY(sSprite, 60);	
	
  repeat // The game loop...
    ProcessEvents();	
		
		//Move the camera
    if KeyDown(vk_UP) then MoveCameraBy(0, -1);
    if KeyDown(vk_DOWN) then MoveCameraBy(0, +1);
    if KeyDown(vk_LEFT) then MoveCameraBy(-1, 0);
    if KeyDown(vk_RIGHT) then MoveCameraBy(+1, 0);
    if KeyTyped(vk_SPACE) then CenterCameraOn(sSprite, 0, 0); 
		if KeyTyped(vk_S) then MoveCameraTo(0, 0);
		if KeyTyped(vk_E) then MoveCameraTo(1007, 998);
		if MouseClicked() then 
		begin
			if PointOnScreen(MousePosition()) then 
			begin
			
			end
			else
			begin
			
			end;
		end;
		
    ClearScreen(ColorWhite);
    
		//Camera current coordinate
		DrawText(PointToString(CameraPos()), ColorBlack, CameraX+700, CameraY+2);
		
		DrawRectangle(ColorBlue, CameraScreenRect()); // Draw rectangle that encompases the area of the game world that is curretnly on the screen
    FillCircle(ColorRed, 0, 0, 4);
    DrawText('Starting position of Game World at 0,0', ColorRed, 7, -2);		
		
		//Draw a Sprite    
		DrawSprite(sSprite);
		
		FillCircle(ColorRed, 1000, 1000, 4);
    DrawText('End position of Game World at 1000,1000', ColorRed, 1007, 998);		
		
    RefreshScreen();    
    
  until WindowCloseRequested();
  
  ReleaseAllResources();
end;

begin
  Main();
end.