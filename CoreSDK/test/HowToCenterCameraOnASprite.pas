program HowToCenterCameraOnASprite;
uses
  sgInput, sgGraphics, sgResources, sgText, sgTypes, sgGeometry, sgUtils, sgCamera, sgImages, sgSprites;  
  
procedure Main();
var		
	sSprite : Sprite;	
begin 
  OpenGraphicsWindow('Center Camera On A Sprite', 800, 600);
	
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
		
		// Center camera on a certain point
    if KeyTyped(vk_SPACE) then CenterCameraOn(sSprite, 0, 0); 		
		
    ClearScreen(ColorWhite);
    
		//Camera current coordinate
		DrawText(PointToString(CameraPos()), ColorBlack, CameraX+700, CameraY+2);
		
		DrawRectangle(ColorBlue, CameraScreenRect()); // Draw rectangle that encompases the area of the game world that is curretnly on the screen   
		
		//Draw a Sprite    
		DrawSprite(sSprite);	
		
    RefreshScreen();    
    
  until WindowCloseRequested();
  
  ReleaseAllResources();
end;

begin
  Main();
end.