program HowToExploreUsingCamera;
uses
  sgInput, sgGraphics, sgResources, sgText, sgTypes, sgGeometry, sgPhysics, sgUtils, sgCamera;  
  
procedure Main();
begin 
  OpenGraphicsWindow('Explore with camera', 800, 600);
  
  repeat // The game loop...
    ProcessEvents();      
      
    //Move the camera
    if(KeyDown(vk_UP)) then MoveCameraBy(0, -1);
    if(KeyDown(vk_DOWN)) then MoveCameraBy(0, +1);
    if(KeyDown(vk_LEFT)) then MoveCameraBy(-1, 0);
    if(KeyDown(vk_RIGHT)) then MoveCameraBy(+1, 0);
    
    //Draw the scene
    ClearScreen(ColorWhite);
    
    FillCircle(ColorRed, 0, 0, 4);
    DrawText('Starting position of Game World at 0,0', ColorRed, 7, -2);    
    
    FillRectangle(RGBColor(205,201,201), -150, 250, 1150, 20);    
    FillRectangle(RGBColor(205,201,201), -150, 330, 1150, 20);
    
    FillRectangle(RGBColor(255,255,0), -150, 290, 50, 20);
    FillRectangle(RGBColor(124,252,0), -50, 290, 50, 20);
    FillRectangle(RGBColor(184,134,11), 50, 290, 50, 20);
    FillRectangle(RGBColor(0,255,255), 150, 290, 50, 20);
    FillRectangle(RGBColor(255,165,0), 250, 290, 50, 20);
    FillRectangle(RGBColor(255,192,203), 350, 290, 50, 20);
    
    FillRectangle(RGBColor(160,32,240), 450, 290, 50, 20);
    FillRectangle(RGBColor(165,42,42), 550, 290, 50, 20);
    FillRectangle(RGBColor(240,230,140), 650, 290, 50, 20);
    FillRectangle(RGBColor(0,0,128), 750, 290, 50, 20);
    FillRectangle(RGBColor(245,255,250), 850, 290, 50, 20);
    FillRectangle(RGBColor(255,228,225), 950, 290, 50, 20);  
    
    DrawCircle(ColorBlue, 105, 420, 30);
    FillRectangle(ColorBlack, 100, 450, 10, 50);        
    
    FillEllipse(ColorBlue, 300, 50, 60, 30);
    DrawText('Ellipse in blue at position 300,50', ColorRed, 380, 60);    
    
    DrawText(PointToString(CameraPos()), ColorBlack, CameraPos());    
    
    RefreshScreen();    
    
  until WindowCloseRequested();
  
  ReleaseAllResources();
end;

begin
  Main();
end.