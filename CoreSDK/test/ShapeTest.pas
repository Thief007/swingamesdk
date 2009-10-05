program HelloWorld;
{$IFNDEF UNIX} {$r GameLauncher.res} {$ENDIF}
uses
  sgTypes, sgCore, sgAudio, sgText, sgGraphics, sgGeometry, sgResources, sgInput;

procedure Main();
var
  pts: ArrayOfPoint2D;
  p: ShapePrototype;
  s, s1: Shape;
  t: Timer;
  time: Integer;
  filled: Boolean;
begin
  OpenAudio();
  
  OpenGraphicsWindow('Shape Test', 640, 480);
  
  SetLength(pts, 9);
  pts[0] := PointAt(0, 0);
  pts[1] := PointAt(0, 50);
  pts[2] := PointAt(50, 0);
  pts[3] := PointAt(50, 50);
  pts[4] := PointAt(100, 0);
  pts[5] := PointAt(100, 50);
  pts[6] := PointAt(150, 0);
  pts[7] := PointAt(150, 50);
  pts[8] := PointAt(200, 0);
  
  p := PrototypeFrom(pts, pkTriangleList);
  
  s := ShapeAtPoint(p, PointAt(200,200));
  ShapeSetAngle(s, 0);
  ShapeSetScale(s, PointAt(1.0, 1.0));
  
  s1 := ShapeAtPoint(p, PointAt(0,100));
  ShapeSetAngle(s1, 15);
  ShapeSetScale(s1, PointAt(0.5, 0.5));
  
  ShapeAddSubShape(s, s1);
  
  t := CreateTimer();
  StartTimer(t);
  
  filled := false;
  
  repeat // The game loop...
    ClearScreen();

    ProcessEvents();
    
    time := TimerTicks(t);
    
    if KeyTyped(vk_p) then PrototypeSetKind(p, pkPoint);
    if KeyTyped(vk_c) then PrototypeSetKind(p, pkCircle);
    if KeyTyped(vk_e) then PrototypeSetKind(p, pkEllipse);
    if KeyTyped(vk_k) then PrototypeSetKind(p, pkLine);
    if KeyTyped(vk_t) then PrototypeSetKind(p, pkTriangle);
    if KeyTyped(vk_j) then PrototypeSetKind(p, pkLineList);
    if KeyTyped(vk_h) then PrototypeSetKind(p, pkLineStrip);
    if KeyTyped(vk_o) then PrototypeSetKind(p, pkPolygon);
    if KeyTyped(vk_l) then PrototypeSetKind(p, pkTriangleList);
    if KeyTyped(vk_s) then PrototypeSetKind(p, pkTriangleStrip);
    if KeyTyped(vk_f) then PrototypeSetKind(p, pkTriangleFan);
    
    if KeyTyped(vk_space) then filled := not filled;
    
    if filled then
      FillShape(ColorGreen, s)
    else
      DrawShape(ColorGreen, s);
    
    RefreshScreen();
  until WindowCloseRequested();
  
  ReleaseAllResources();
  CloseAudio();
  
  FreeShape(s);
  FreePrototype(p);
end;

begin
  Main();
end.
