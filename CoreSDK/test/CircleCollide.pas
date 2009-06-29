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

procedure DoLineTest(testLines: LinesArray; const center: Point2D; radius: Single; movement: Vector);
type
  DoublePt = record ptOnCircle, ptOnLine: Point2D; end;
var
  pt1, pt2, ptOnLine, ptOnCircle, hitPt, maxHitPtOnLine, maxHitPtOnCircle: Point2D;
  tmp: Array [0..3] of Point2D;
  chkPts: Array [0..3] of DoublePt;
  lineVec, normalMvmt, normalLine, toEdge, edge, ray, vOut: Vector;
  i, j, maxIdx, hits: Integer;
  dotProd, dist, maxDist: Single;
begin
  exit;
  
  // Cast ray searching for points back from shape
  ray := InvertVector(movement);
  normalMvmt := VectorNormal(movement);
  
  maxIdx := -1;
  maxDist := -1;
  
  //Search all lines for hit points
  for i := 0 to High(testLines) do
  begin
    lineVec := LineAsVector(testLines[i]);
    //Get the normal of the line we hit
    normalLine := VectorNormal(lineVec);
    hits := 0;
    
    //tmp 0 and tmp 1 are the widest points to detect collision with line
    GetWidestPoints(center, radius, normalMvmt, tmp[0], tmp[1]);
    //tmp 2 and tmp 3 are the closest and furthest points from the line
    GetWidestPoints(center, radius, normalLine, tmp[2], tmp[3]);
    
    // for both points...
    for j := 0 to 3 do
    begin
      //DrawCircle(ColorWhite, tmp[j], 2);
      
      // Cast a ray back from the test points to find line pts... out on ptOnLine
      if GetRayIntersectionPoint(tmp[j], ray, testLines[i], ptOnLine) then
      begin
        //DrawCircle(ColorRed, ptOnLine, 1);
        //DrawLine(ColorRed, tmp[j], ptOnLine);
        
        chkPts[hits].ptOnLine := ptOnLine;
        chkPts[hits].ptOnCircle := tmp[j];
        hits := hits + 1;
      end;
    end;
    
    // for each of the hits on this line...
    // search for the longest hit.
    for j := 0 to hits - 1 do
    begin
      //DrawCircle(ColorWhite, chkPts[j].ptOnCircle, 1);
      toEdge := VectorFromPoints(center, chkPts[j].ptOnCircle);
      //DrawLine(ColorRed, center, chkPts[j].ptOnCircle);
      dotProd := DotProduct(toEdge, normalLine);
      
      // 2d: Most distant edge pt is on a line this distance (dotProd) from the center
      edge := AddVectors(center, VectorMultiply(normalLine, dotProd));
      //DrawPixel(ColorWhite, edge); // Draws pt on line to distant pt
      
      //  Find the point we hit on the line... ptOnLine receives intersection point...
      if not GetLineIntersectionPoint(LineFromVector(edge, movement), testLines[i], ptOnLine) then continue;
      // Move back onto line segment... linePt -> closest point on line to intersect point
      //DrawCircle(ColorRed, ptOnLine, 1); // point on line, but not necessarily the line segment
      //DrawLine(ColorWhite, edge, ptOnLine);
      
      ptOnLine := ClosestPointOnLine(ptOnLine, testLines[i]);
      //FillCircle(ColorBlue, ptOnLine, 1); // point on segment
      
      // Find the most distant point on the circle, given the movement vector
      if not DistantPointOnCircleWithVector(ptOnLine, center, radius, movement, ptOnCircle) then continue;
      //FillCircle(ColorBlue, ptOnCircle, 2); // point on segment
      //DrawLine(ColorBlue, ptOnLine, ptOnCircle);
      
      // GetTangentPoints(testLines[i].endPoint, center, radius, pt1, pt2);
      // DrawCircle(ColorRed, pt1, 2);
      // DrawCircle(ColorRed, pt2, 2);
      
      dist := PointPointDistance(ptOnLine, ptOnCircle);
      //WriteLn(dist);
      if (dist > maxDist) or (maxIdx = -1) then
      begin
        maxDist := dist;
        maxIdx := i;
        vOut := VectorFromPoints(ptOnCircle, ptOnLine);
        //dotProd := DotProduct(UnitVector(vOut), VectorFrom(0.5,0.5));
        vOut := VectorMultiply(UnitVector(vOut), VectorMagnitude(vOut) + 1.42);
        WriteLn(dotProd:4:2);
      end;      
    end;
  end;
  
  if maxIdx > -1 then
  begin
    WriteLn('---');
    DrawLine(ColorGreen, LineFromVector(center, vOut));
    DrawCircle(ColorGreen, AddVectors(center, vOut), Round(radius));
  end;
end;

procedure CheckCollisionWithLine(s: Sprite; const l: LineSegment);
var
  mvmtLine: LineSegment;
  outVec: Vector;
  lines: LinesArray;
begin
  //mvmtLine := LineFromVector(CenterPoint(s), InvertVector(s^.movement));
  
  //todo: check LineSegmentsIntersect
  
  if CircleLineCollision(s, l) then
  begin
    // the have collided
    SetLength(lines, 1);
    lines[0] := l;
    
    //outVec := VectorOverLinesFromCircle(CenterPoint(s), CurrentWidth(s) / 2, lines, s^.movement);
    //MoveSprite(s, outVec);
    CollideCircleLines(s, lines);
    //WriteLn('hit');
    //MoveSprite(s);
  end;
end;

procedure CheckCollisionWithTriangle(s: Sprite; const t: Triangle);
var lines: LinesArray;
begin
  if CircleTriangleCollision(CenterPoint(s), CurrentWidth(s) / 2, t) then
  begin
    lines := LinesFrom(t);
    
    CollideCircleLines(s, lines);
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
  
  i, maxIDx: Integer;
  dist, dotProd: Single;
  checkPoints: array [0..2] of Point2D;
  
  testLines: LinesArray;
begin
  OpenAudio();
  
  OpenGraphicsWindow('Circle Collisions', 800, 600);
  
  MapBitmap('ball', 'ball_small.png');
  
  SetLength(testLines, 1);
  testLines[0] := LineFrom(300, 250, 500, 350);
  
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
    
    for i := 0 to High(testLines) do
    begin
      DrawLine(ColorRed, testLines[i]);
    end;
    
    temp := GetMousePosition();
    DoLineTest(testLines, temp, r2, mouseMvmt);

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
      
      mouseOut := VectorOverLinesFromCircle(temp, r2, LinesFrom(t1), mouseMvmt, maxIdx);
      
      mouseOut := AddVectors(temp, mouseOut);
      DrawCircle(ColorGreen, mouseOut, r2);
      
      DrawLine(ColorWhite, LinesFrom(t1)[maxIdx]);
      
      //LineOfLinesCircleHit(temp, r2, mouseMvmt, LinesFrom(t1), found);
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
    
    CheckCollisionWithLine(s1, testLines[0]);
    CheckCollisionWithLine(s2, testLines[0]);
    
    CheckCollisionWithTriangle(s1, t1);
    CheckCollisionWithTriangle(s2, t1);
    
    temp := GetMousePosition();
    if CircleCircleCollision(temp, r2, CenterPoint(s1), CurrentWidth(s1) / 2) then
      CollideCircleCircle(s1, temp, r2);
    
    if CircleCircleCollision(temp, r2, CenterPoint(s2), CurrentWidth(s2) / 2) then
      CollideCircleCircle(s2, temp, r2);
    
    if SpritesCollided(s1, s2) then CollideCircles(s1, s2);
    
    DrawFramerate(0,0);
    RefreshScreen(20);
  until WindowCloseRequested();
  
  CloseAudio();
end;

begin
  Main();
end.