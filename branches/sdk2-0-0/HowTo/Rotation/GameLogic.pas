///
/// GameLogic.pas
///
/// --------------------------------------
///  Demonstrates the use of multiple images to simulate
///  rotation of a sprite. 
/// --------------------------------------
unit GameLogic;

interface
  procedure Main();

implementation
uses
  GameResources, SysUtils,
  SGSDK_Core, SGSDK_Font, SGSDK_Audio, SGSDK_Graphics, SGSDK_Input, SGSDK_Physics,
  SGSDK_KeyCodes;

  const 
    CHANGE_COUNT = 120; // how many cycles before it changes direction (auto)
    SCREEN_WIDTH = 800;
    SCREEN_HEIGHT = 600;

  type GameState = record
      aircraft: Sprite;   // The aircraft moving
      shadow: Sprite;     // The aircraft's shadow
      count: Integer;     // A count of the number of cycles - for auto movement
      manual: Boolean;    // Is this manual or automatic movement
    end;

  /// Align the aircraft's shadow with the aircraft sprite
  procedure AlignShadow(var aircraft, shadow: Sprite);
  begin
    shadow.x := aircraft.x + 30;
    shadow.y := aircraft.y + 80;
    shadow.currentFrame := aircraft.currentFrame;    
  end;

  //Change the direction of the aircraft - select the correct frame
  // and alter the direction of its movement
  procedure ChangeDirection(var aircraft: Sprite; angle: Integer);
  var
    m: Matrix2D;
  begin
    if angle <> 0 then
    begin
      m := RotationMatrix(angle);
      aircraft.movement := Multiply(m, aircraft.movement);
      if angle = -30 then 
      begin
        aircraft.currentFrame := (aircraft.currentFrame + 1) mod 12;
      end
      else 
      begin
        aircraft.currentFrame := (aircraft.currentFrame - 1) mod 12;
        if aircraft.currentFrame < 0 then aircraft.currentFrame := 11;
      end;
    end;    
  end;
  
  // Wrap the sprite around the screen once it is completely off screen.
  procedure WrapSprite(var sprt: Sprite);
  begin
    // Wrap the sprite when offscreen
    if sprt.x < -sprt.width then sprt.x := sprt.x + SCREEN_WIDTH;
    if sprt.x > SCREEN_WIDTH + sprt.width then sprt.x := sprt.x - SCREEN_WIDTH;
    if sprt.y < -sprt.height then sprt.y := sprt.y + SCREEN_HEIGHT;
    if sprt.y > SCREEN_HEIGHT + sprt.height then sprt.y := sprt.y - SCREEN_HEIGHT;
  end;

  // Update the game. Move aircraft if automatic, move the sprite,
  //  and align the shadow.
  procedure UpdateGame(var gameData: GameState);
  begin
    if gameData.manual = false then
    begin
      gameData.count := gameData.count + 1;
      if gameData.count mod 120 = 0 then
      begin
        ChangeDirection(gameData.aircraft, -30);
        gameData.count := 0;
      end;
    end;
    
    MoveSprite(gameData.aircraft);
    AlignShadow(gameData.aircraft, gameData.shadow);
    
    WrapSprite(gameData.aircraft);
  end;
  
  // Draw the sprite and perform the wrap - drawing it twice
  // if it is partially off the screen.
  procedure DrawWrappingSprite(sprt: Sprite);
  var
    offsetX, offsetY: Integer; 
  begin
    offsetX := 0;
    offsetY := 0;
    
    if sprt.x < 0 then
    begin
      offsetX := SCREEN_WIDTH;
    end
    else if sprt.x + sprt.width > SCREEN_WIDTH then
    begin
      offsetX := -SCREEN_WIDTH;
    end;
    
    if sprt.y < 0 then
    begin
      offsetY := SCREEN_HEIGHT;
    end
    else if sprt.Y + sprt.height > SCREEN_HEIGHT then
    begin
      offsetY := -SCREEN_HEIGHT;
    end;
    
    //Draw the sprite in its current position.
    DrawSprite(sprt);
    
    if (offsetX <> 0) or (offsetY <> 0) then
    begin
      //Draw it offset
      DrawSprite(sprt, offsetX, offsetY);
    end;
    
  end;
  
  // Draw the game onto the screen
  procedure Draw(const gameData: GameState);
  begin
    //Draw screen
    ClearScreen(ColorBlack);
    DrawBitmap(GameImage('Background'), 0, 0);
    
    DrawWrappingSprite(gameData.shadow);
    DrawWrappingSprite(gameData.aircraft);
    
    
    RefreshScreen(65);
  end;
  
  // Handle user input - allowing the user to switch between automatic
  //  and manual control, and to control the aircraft when manual.
  procedure HandleUserInput(var gameData: GameState);
  var
    angle: Integer;
  begin
    angle := 0;
    if gameData.manual then
    begin
      if WasKeyTyped(VK_LEFT) then 
        angle := -30;
      if WasKeyTyped(VK_RIGHT) then
        angle := angle + 30;
      
      if angle <> 0 then
      begin
        ChangeDirection(gameData.aircraft, angle);
      end;
    end;
    
    if WasKeyTyped(VK_T) then
    begin
      gameData.manual := not gameData.manual;
    end;
  end;
  
  // Setup the game data.
  procedure InitialiseData(var gameData: GameState);
  begin
    gameData.aircraft := CreateSprite(GameImage('Aircraft'), 120, 12, 80, 80);
    gameData.shadow := CreateSprite(GameImage('AircraftShadow'), 120, 12, 40, 40);
    gameData.aircraft.x := 610;
    gameData.aircraft.y := 300;
    gameData.aircraft.movement := CreateVector(0, -1);
    
    gameData.count := 0;
  end;

  //The main procedure that controlls the game logic.
  //
  // SIDE EFFECTS:
  // - Creates the screen, and displays a message
  procedure Main();
  var
    gameData: GameState;
  begin
    OpenGraphicsWindow('Rotation and Wrapping', 800, 600);
    
    LoadResources();
        
    InitialiseData(gameData);
    
    repeat
      ProcessEvents();
      HandleUserInput(gameData);
      UpdateGame(gameData);
      Draw(gameData);
    until WindowCloseRequested();
    
    FreeResources();
  end;

end.
