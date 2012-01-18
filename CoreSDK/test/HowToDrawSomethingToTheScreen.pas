program HowToDrawSomethingToTheScreen;
uses sgGraphics, sgUtils, sgResources;

procedure Main();
begin
  OpenGraphicsWindow('How To Draw Something To The Screen');
  
  ClearScreen();    // clear the background canvas
  RefreshScreen();  // make it appear on screen...
  Delay(2000);      // wait for 2 seconds
  
  ClearScreen(ColorYellow);   // clear the background canvas
  RefreshScreen();            // make it appear on screen...
  ClearScreen(ColorWhite);    // clear the background canvas
  Delay(2000);                // wait for 2 seconds... notice yellow screen!
  
  RefreshScreen();            // make the white screen appear...
  Delay(2000);
  
  ReleaseAllResources();
end;

begin
  Main();
end.