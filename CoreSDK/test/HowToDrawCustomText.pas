program HowToDrawCustomText;
uses 
    sgText, sgGraphics, sgUtils, sgResources, sgTypes;

procedure Main();
begin    
    OpenGraphicsWindow('Drawing Text', 800, 600);
    
    ClearScreen();

    DrawText('You Win!!!', ColorGreen, 300, 200);
    DrawTextLines('OR DO YOU???', ColorRed, ColorWhite, 'Arial', 18, AlignCenter, 400, 400, 160, 100);

    RefreshScreen();
    
    Delay(5000);

    ReleaseAllResources();    
end;
	
begin
    Main();
end.