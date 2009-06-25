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
  c1, rectPt, ptOut, temp, tmp, edge: Point2D;
  r1, r2: LongInt;
  rect: Rectangle;
  s1: Sprite;
  found: LineSegment;
  mouseMvmt, mouseOut, ptOnCircle, toCenter, oppositePt, toCircle: Vector;
  nrm, rm: Matrix2D;
  
  i: Integer;
  dist, dotProd: Single;
  checkPoints: array [0..2] of Point2D;
begin
  OpenAudio();
  
  OpenGraphicsWindow('Hello World', 800, 600);
  
  MapBitmap('ball', 'ball_small.png');
  
  rm := RotationMatrix(1.0);
  nrm := RotationMatrix(-1.0);
  
  c1 := PointAt(50, 50);
  r1 := 25;
  r2 := 20;
  
  s1 := CreateSprite(GetBitmap('ball'));
  s1^.x := 400;
  s1^.y := 300;
  s1^.Mass := 10;
  s1^.Movement := VectorFrom(3, 3);
  
  mouseMvmt := VectorFrom(3,3);
  
  rect := RectangleFrom(400, 400, 200, 100);
  
  repeat // The game loop...
    ProcessEvents();
    
    ClearScreen();
    
    DrawRectangle(ColorRed, rect);
    DrawCircle(ColorRed, c1, r1);
    
    temp := GetMousePosition();
    DrawCircle(ColorGreen, temp, r2);

    if LineCircleHit(temp, r2, mouseMvmt, rect, found) then
    begin
      DrawLine(ColorWhite, found);
      
      // Determine far edge of Circle
      // 1: Get the closest point on the line
      tmp := ClosestPointOnLine(temp, found);
      DrawCircle(ColorRed, tmp, 1);
      // 2: Get the closest point on the circle
      ptOnCircle := ClosestPointOnCircle(tmp, temp, r2);
      DrawCircle(ColorWhite, ptOnCircle, 1);
      // 3: Get other side...
      toCircle := VectorFromPoints(tmp, ptOnCircle);
      edge := AddVectors(ptOnCircle, VectorMultiply(UnitVector(toCircle), r2 * 2));
      
      // Get the point on the line we hit... from far edge
      
      // edge := temp;
      // edge.x += r2;
      DrawCircle(ColorGreen, edge, 2);
      
      //Get the intersection point
      GetLineIntersectionPoint(LineFromVector(edge, mouseMvmt), found, tmp);
      DrawCircle(ColorGreen, tmp, 2);
      
      //Move back to line
      tmp := ClosestPointOnLine(tmp, found);
      DrawCircle(ColorYellow, tmp, 2);
      
      //Cast ray to find distant point on circle //TODO: change heading to unitvector itself
      dist := GetRayCircleIntersectDistance(tmp, UnitVector(mouseMvmt), temp, r2);
      
      // Get pt on circle by moving back from tmp dist distance in mouseMvmt direction
      ptOnCircle := AddVectors(VectorMultiply(UnitVector(mouseMvmt), dist), tmp);
      DrawLine(ColorMagenta, tmp, ptOnCircle);
      DrawCircle(ColorMagenta, ptOnCircle, 2);
      
      //Project the ray to the other side of the circle
      toCenter := VectorFromPoints(ptOnCircle, temp);
      dotProd := DotProduct(toCenter, UnitVector(mouseMvmt));
      //WriteLn(dotProd:4:2);
      
      oppositePt := AddVectors(ptOnCircle, VectorMultiply(UnitVector(mouseMvmt), 2 * dotProd));
      DrawCircle(ColorRed, oppositePt, 2);
      
      mouseOut := VectorOutOfRectFromPoint(oppositePt, rect, mouseMvmt);
      mouseOut := AddVectors(temp, mouseOut);
      DrawCircle(ColorBlue, mouseOut, r2);
      
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
    // DrawSprite(s1);
    // 
    // temp := CenterPoint(s1);
    // DrawLine(ColorBlue, temp.x, temp.y, temp.x + (s1^.Movement.x * 10), temp.y + (s1^.Movement.y * 10));
    // 
    // UpdateSprite(s1);
    // KeepOnScreen(s1);
    
    RefreshScreen();
  until WindowCloseRequested();
  
  CloseAudio();
end;

begin
  Main();
end.
