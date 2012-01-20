program HowToDrawSomethingToTheScreen;
uses sgGraphics, sgUtils, sgResources;

procedure Main();
begin
  OpenGraphicsWindow('How To Draw Something To The Screen');
  
  ClearScreen(ColorWhite);    // clear the background canvas
  RefreshScreen();  // make it appear on screen...
  Delay(2000);      // wait for 2 seconds  
  
  ReleaseAllResources();
end;

begin
  Main();
end.