program HowToMouseInput;
uses
  sgInput, sgGraphics, sgResources, sgText, sgTypes, sgUtils, sgAudio, sysutils;
  
procedure Main();
var
	str : String;
	mPos : Point2D;
begin  
  OpenGraphicsWindow('Mouse Input', 400, 240); 
  
  repeat // The game loop...
    ProcessEvents();
    
    ClearScreen(ColorWhite);
    
    if KeyTyped(vk_h) then HideMouse();
		if KeyTyped(vk_s) then ShowMouse();
		if MouseShown () then DrawText('visible', ColorBlue, 'Arial', 14, 120, 90);
		if NOT MouseShown () then DrawText('not visible', ColorBlue, 'Arial', 14, 120, 90);
		if MouseClicked(LeftButton) then str := FloatToStr(MouseX) + ',' + FloatToStr(MouseY);
		if MouseDown(LeftButton) then DrawText('is down', ColorBlue, 'Arial', 14, 200, 140);
		if MouseUp(LeftButton) then DrawText('is up', ColorBlue, 'Arial', 14, 200, 140);
		if MouseDown(RightButton) then DrawText('is down', ColorBlue, 'Arial', 14, 200, 165);
		if MouseUp(RightButton) then DrawText('is up', ColorBlue, 'Arial', 14, 200, 165);
		if KeyDown(vk_DOWN) then 
		begin
			mPos := MousePosition();
			mPos.y += 1;
			MoveMouse(mPos);		
		end;
		if KeyDown(vk_UP) then 
		begin
			mPos := MousePosition();
			mPos.y -= 1;
			MoveMouse(mPos);		
		end;
		if KeyDown(vk_LEFT) then 
		begin
			mPos := MousePosition();
			mPos.X -= 1;
			MoveMouse(mPos);		
		end;
		if KeyDown(vk_RIGHT) then 
		begin
			mPos := MousePosition();
			mPos.X += 1;
			MoveMouse(mPos);		
		end;
    
    DrawText('Mouse Input', ColorRed, 'Arial', 18, 125, 15);
		DrawText('Press H to hide the cursor', ColorBlue, 'Arial', 14, 20, 40);
		DrawText('Press S to show the cursor', ColorBlue, 'Arial', 14, 20, 65);
		DrawText('Cursor status :', ColorBlue, 'Arial', 14, 20, 90);
		DrawText('Mouse Position at: ' + FloatToStr(MousePosition.x) + ',' + FloatToStr(MouseY), ColorGreen, 'Arial', 10, 0, 0);
		DrawText('Mouse last click at position : ' + str, ColorBlue, 'Arial', 14, 20, 115);
		DrawText('Mouse LeftButton status :', ColorBlue, 'Arial', 14, 20, 140);
		DrawText('Mouse RightButton status :', ColorBlue, 'Arial', 14, 20, 165);
		DrawText('Press Space to change mouse cursor', ColorBlue, 'Arial', 14, 20, 190);
		DrawText('Use Up, Down, Left, Right to move the mouse cursor', ColorBlue, 'Arial', 14, 20, 215);
    
    RefreshScreen(60);    
  until WindowCloseRequested() OR KeyTyped(vk_ESCAPE) OR KeyTyped(VK_Q);  
  
  ReleaseAllResources();
end;

begin
  Main();
end.