program HelloWorld;
{$IFNDEF UNIX} {$r GameLauncher.res} {$ENDIF}
uses
  sgTypes, sgCore, sgAudio, sgText, sgGraphics, sgGeometry, sgResources, sgSprites, sgInput, sgPhysics;

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
  c1, rectPt, ptOut, temp, tmp, edge: Point2D;
  r1, r2: LongInt;
  rect: Rectangle;
  s1, s2: Sprite;
  found: LineSegment;
  mouseMvmt, mouseOut, ptOnCircle, toCenter, oppositePt, toCircle: Vector;
  nrm, rm: Matrix2D;
  t1: Triangle;
  
  i: Integer;
  dist, dotProd: Single;
  checkPoints: array [0..2] of Point2D;
begin
  OpenAudio();
  
  OpenGraphicsWindow('Circle Collisions', 800, 600);
  
  MapBitmap('ball', 'ball_small.png');
  
  rm := RotationMatrix(1.0);
  nrm := RotationMatrix(-1.0);
  
  c1 := PointAt(100, 100);
  r1 := 50;
  r2 := 20;
  
  s1 := CreateSprite(GetBitmap('ball'));
  s1^.x := 400;
  s1^.y := 300;
  s1^.Mass := 10;
  s1^.Movement := VectorFrom(4, 4);
  
  s2 := CreateSprite(GetBitmap('ball'));
  s2^.x := 200;
  s2^.y := 200;
  s2^.Mass := 10;
  s2^.Movement := VectorFrom(1, -5);
  
  t1 := TriangleFrom(600, 100, 550, 200, 670, 175);
  
  mouseMvmt := VectorFrom(3,3);
  
  rect := RectangleFrom(400, 400, 200, 100);
  ShowMouse(False);
  
  repeat // The game loop...
    ProcessEvents();
    
    ClearScreen();
    
    DrawRectangle(ColorRed, rect);
    DrawCircle(ColorRed, c1, r1);
    DrawTriangle(ColorRed, t1);
    
    temp := GetMousePosition();
    

    if CircleRectCollision(temp, r2, rect) then
    begin
      DrawCircle(ColorBlue, temp, r2);
      if LineCircleHit(temp, r2, mouseMvmt, rect, found) then
      begin
        DrawLine(ColorWhite, found);
        mouseOut := VectorOutOfRectFromCircle(temp, r2, rect, mouseMvmt);
        mouseOut := AddVectors(temp, mouseOut);
        DrawCircle(ColorGreen, mouseOut, r2);
      end;
    end
    else if CircleCircleCollision(temp, r2, c1, r1) then
    begin
      DrawCircle(ColorWhite, c1, r1);
      DrawCircle(ColorBlue, temp, r2);
      mouseOut := VectorOutOfCircleFromCircle(temp, r2, c1, r1, mouseMvmt);
      mouseOut := AddVectors(temp, mouseOut);
      DrawCircle(ColorGreen, mouseOut, r2);
    end
    else if CircleTriangleCollision(temp, r2, t1) then
    begin
      //DrawTriangle(ColorWhite, t1);
      DrawCircle(ColorBlue, temp, r2);
      
      LineOfLinesCircleHit(temp, r2, mouseMvmt, LinesFrom(t1), found);
    end else begin
      DrawCircle(ColorGreen, temp, r2);
    end;
    
    // rectPt := ClosestPointOnRectFromCircle(temp, r2, rect);
    // 
    // ptOut := DeepestPointOnCircleVsRectWithMovement(temp, r2, rect, mouseMvmt);
    // 
    // mouseOut := VectorOutOfRectFromCircle(temp, r2, rect, mouseMvmt);
    
    if KeyDown(vk_Right) then mouseMvmt := MatrixMultiply(rm, mouseMvmt);
    if KeyDown(vk_Left) then mouseMvmt := MatrixMultiply(nrm, mouseMvmt);
      
    if KeyDown(vk_R) then 
    begin
      tmp := AddVectors(InvertVector(mouseMvmt), temp);
      MoveMouse(Round(tmp.x), Round(tmp.y));      
    end else if KeyDown(vk_F) then 
    begin
      tmp := AddVectors(mouseMvmt, temp);
      MoveMouse(Round(tmp.x), Round(tmp.y));      
    end;
    
    // DrawCircle(ColorYellow, ptOut, 2);
    // //DrawLine(ColorWhite, temp, rectPt);
    // //DrawLine(ColorYellow, temp, ptOut);
    // 
    // 
    DrawLine(ColorBlue, temp.x, temp.y, temp.x + (mouseMvmt.x * 10), temp.y + (mouseMvmt.y * 10));
    // //DrawLine(ColorYellow, temp.x, temp.y, temp.x + mouseOut.x, temp.y + mouseOut.y);
    // 
    // temp := PointAt(temp, mouseOut);
    // DrawCircle(ColorYellow, temp, r2);
    // 
    DrawSprite(s1);
    temp := CenterPoint(s1);
    DrawLine(ColorBlue, temp.x, temp.y, temp.x + (s1^.Movement.x * 10), temp.y + (s1^.Movement.y * 10));
    UpdateSprite(s1);
    KeepOnScreen(s1);
    if CircleRectCollision(CenterPoint(s1), CurrentWidth(s1) / 2, rect) then CollideCircleRectangle(s1, rect)
    else if CircleCircleCollision(CenterPoint(s1), CurrentWidth(s1) / 2, c1, r1) then CollideCircleCircle(s1, c1, r1);
    
    DrawSprite(s2);
    temp := CenterPoint(s2);
    DrawLine(ColorYellow, temp.x, temp.y, temp.x + (s2^.Movement.x * 10), temp.y + (s2^.Movement.y * 10));
    UpdateSprite(s2);
    KeepOnScreen(s2);
    if CircleRectCollision(CenterPoint(s2), CurrentWidth(s2) / 2, rect) then CollideCircleRectangle(s2, rect)
    else if CircleCircleCollision(CenterPoint(s2), CurrentWidth(s2) / 2, c1, r1) then CollideCircleCircle(s2, c1, r1);
    
    if SpritesCollided(s1, s2) then CollideCircles(s1, s2);
    
    DrawFramerate(0,0);
    RefreshScreen(60);
  until WindowCloseRequested();
  
  CloseAudio();
end;

begin
  Main();
end.
