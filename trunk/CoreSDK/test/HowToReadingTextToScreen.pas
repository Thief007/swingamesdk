program HowToDrawRandomShape;
uses
sgTypes, sgInput, sgGraphics, sgResources, sgUtils, sgText;

procedure Main();
var
  fnt : Font;
begin  
  OpenGraphicsWindow('How To Read Text To Screen', 240, 160);  

  fnt := LoadFont('Arial',12);
  repeat // The game loop...
    ProcessEvents();
  
    ClearScreen(ColorWhite);
    
    if NOT ReadingText() THEN
    begin
      StartReadingText(ColorRed, 40, fnt, 10, 10);            
    end;
    
    RefreshScreen(60);
  until WindowCloseRequested();

  ReleaseAllResources();
end;

begin
  Main();
end.