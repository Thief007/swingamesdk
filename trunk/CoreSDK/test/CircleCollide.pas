program HelloWorld;
{$IFNDEF UNIX} {$r GameLauncher.res} {$ENDIF}
uses
  sgTypes, sgCore, sgAudio, sgText, sgGraphics, sgGeometry, sgResources, sgSprites, sgInput;

procedure KeepOnScreen(s: Sprite);
begin
  if s^.x > ScreenWidth() - CurrentWidth(s) then
  begin
    s^.movement.x := -s^.movement.x;
    s^.x := ScreenWidth() - CurrentWidth(s);
  end;
  if s^.y > ScreenHeight() - CurrentHeight(s) then
  begin
    s^.movement.y := -s^.movement.y;
    s^.y := ScreenHeight() - CurrentHeight(s);
  end;
  if s^.x < 0 then
  begin
    s^.movement.x := -s^.movement.x;
    s^.x := 0;
  end;
  if s^.y < 0 then
  begin
    s^.movement.y := -s^.movement.y;
    s^.y := 0;
  end;
end;

procedure Main();
var
  c1, rectPt, ptOut, temp: Point2D;
  r1, r2: LongInt;
  rect: Rectangle;
  s1: Sprite;
  mouseMvmt, mouseOut: Vector;
  nrm, rm: Matrix2D;
begin
  OpenAudio();
  
  OpenGraphicsWindow('Hello World', 800, 600);
  
  MapBitmap('ball', 'ball_small.png');
  
  rm := RotationMatrix(5.0);
  nrm := RotationMatrix(-5.0);
  
  c1 := PointAt(50, 50);
  r1 := 25;
  r2 := 20;
  
  s1 := CreateSprite(GetBitmap('ball'));
  s1^.x := 400;
  s1^.y := 300;
  s1^.Mass := 10;
  s1^.Movement := VectorFrom(3, 3);
  
  mouseMvmt := VectorFrom(3,3);
  
  rect := RectangleFrom(500, 500, 200, 100);
  
  repeat // The game loop...
    ProcessEvents();
    
    ClearScreen();
    
    DrawRectangle(ColorRed, rect);
    DrawCircle(ColorRed, c1, r1);
    
    temp := GetMousePosition();
    rectPt := ClosestPointOnRectFromCircle(temp, r2, rect);
    
    ptOut := DeepestPointOnCircleVsRectWithMovement(temp, r2, rect, mouseMvmt);
    
    mouseOut := VectorOutOfRectFromCircle(temp, r2, rect, mouseMvmt);
    
    if KeyDown(vk_Right) then mouseMvmt := MatrixMultiply(rm, mouseMvmt);
    if KeyDown(vk_Left) then mouseMvmt := MatrixMultiply(nrm, mouseMvmt);
    
    DrawCircle(ColorGreen, temp, r2);
    DrawCircle(ColorYellow, ptOut, 2);
    //DrawLine(ColorWhite, temp, rectPt);
    //DrawLine(ColorYellow, temp, ptOut);
    
    
    DrawLine(ColorBlue, temp.x, temp.y, temp.x + (mouseMvmt.x * 10), temp.y + (mouseMvmt.y * 10));
    //DrawLine(ColorYellow, temp.x, temp.y, temp.x + mouseOut.x, temp.y + mouseOut.y);
    
    temp := PointAt(temp, mouseOut);
    DrawCircle(ColorYellow, temp, r2);
    
    DrawSprite(s1);
    
    temp := CenterPoint(s1);
    DrawLine(ColorBlue, temp.x, temp.y, temp.x + (s1^.Movement.x * 10), temp.y + (s1^.Movement.y * 10));
    
    UpdateSprite(s1);
    KeepOnScreen(s1);
    
    RefreshScreen();
  until WindowCloseRequested();
  
  CloseAudio();
end;

begin
  Main();
end.
