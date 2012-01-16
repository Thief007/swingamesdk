program HowToMoveAShape;
uses
  sgInput, sgGraphics, sgResources, sgText, sgTypes;

procedure Main();
var 
  x,y : Single;	
begin
  x := 140;
  y := 110;

  OpenGraphicsWindow('How To Move A Shape', 320, 240);

  repeat // The game loop...
    ProcessEvents();

    ClearScreen(ColorWhite);

    FillRectangle(ColorGreen, x, y, 40, 20);

    if(KeyDown(vk_UP)) then
    begin
      y -= 1;
      if (y = 0) then y := 240;
    end;
    if(KeyDown(vk_DOWN)) then
    begin
      y += 1;
      if (y = 240) then y := 0;
    end;
    if(KeyDown(vk_LEFT)) then
    begin
      x -= 1;
      if (x = 0) then x := 320;
    end;
    if(KeyDown(vk_RIGHT)) then
    begin
      x += 1;
      if (x = 320) then x := 0;
    end;

    RefreshScreen(60);
  until WindowCloseRequested();

  ReleaseAllResources();
end;

begin
  Main();
end.