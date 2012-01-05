program HowToDrawPrimitiveShapes;
uses sgAudio, sgGraphics, sgUtils, sgResources;

procedure Main();
begin    
    OpenGraphicsWindow('Primitive shapes', 800, 600);
    
	ClearScreen();
	
	DrawRectangle(ColorGreen, 25, 100, 200, 100 );
	FillRectangle(ColorGreen, 25, 400, 200, 100 );
	
	DrawTriangle(ColorYellow, 275, 200, 475, 200, 375, 100);
	FillTriangle(ColorYellow, 275, 500, 475, 500, 375, 400);
	
	DrawCircle(ColorWhite, 625, 150, 100);
	
	RefreshScreen();
    
    Delay(5000);

    ReleaseAllResources();    
end;
	
begin
	Main();
end.