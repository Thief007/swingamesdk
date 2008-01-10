unit GameLogic;

interface
  procedure Main();

implementation
uses
  GameResources,
  SysUtils,
  SGSDK_Core, SGSDK_Font, SGSDK_Audio, SGSDK_Graphics, SGSDK_Input, SGSDK_Physics,
  SGSDK_KeyCodes;

  //The main procedure that controlls the game logic.
  //
  // SIDE EFFECTS:
  // - Creates the screen, and displays a message
  procedure Main();
  begin
    OpenGraphicsWindow('My Game', 640, 480);
    
    LoadResources();
      
    repeat
      ProcessEvents();
  
      //Draw screen
	    ClearScreen(ColorBlack);

      FillRectangle(ColorRed, 20, 200, 200, 100);
      FillRectangle(ColorGreen, 220, 200, 200, 100);
      FillRectangle(ColorBlue, 420, 200, 200, 100);

      DrawText('Hello World', ColorRed, GameFont('Courier'), 20, 310);
      DrawText('Hello World', ColorGreen, GameFont('Courier'), 220, 310);
      DrawText('Hello World', ColorBlue, GameFont('Courier'), 420, 310);

 	    DrawFramerate(0, 0, GameFont('Courier'));
      DrawText('Hello World', ColorWhite, GameFont('ArialLarge'), 50, 50);
  
      RefreshScreen();
    until WindowCloseRequested();
    
    FreeResources();
  end;

end.
