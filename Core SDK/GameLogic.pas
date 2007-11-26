unit GameLogic;

interface
  procedure Main();

implementation
uses
  GameResources,
  SysUtils,
  SwinGameAPI,
  KeyCodes;

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
    DrawFramerate(0, 0, GameFont(courier));
    DrawText('Hello World', ColorWhite, GameFont(ArialLarge), 50, 50);
  
      RefreshScreen();
    until WindowCloseRequested();
    
    FreeResources();
  end;

end.
