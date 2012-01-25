program HowToDrawRandomShape;
uses
sgTypes, sgInput, sgGraphics, sgResources, sgUtils, sgText;

procedure Main();
begin	
  OpenGraphicsWindow('How To Read Text To Screen', 800, 600);		

  ClearScreen(ColorWhite);

  repeat // The game loop...
    ProcessEvents();
	
		if NOT ReadingText() THEN
		begin
			StartReadingText(ColorRed, 50, 'Arial', 290, 10);
		end;
		
    RefreshScreen(60);
  until WindowCloseRequested() OR KeyTyped(VK_Q);

  ReleaseAllResources();
end;

begin
  Main();
end.