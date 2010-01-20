program MapEditorTest;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}
uses
  sgTypes, sgMaps, sgCore, sgCamera,
  sgInput, sgAudio, sgShared, SysUtils,
  StrUtils, sgResources, sgGraphics, sgText,sgNamedIndexCollection;
//Main procedure
procedure UpdateCamera();
begin
  if KeyDown(VK_LEFT) then
    MoveCameraBy(-2,0)
  else if KeyDown(VK_RIGHT) then
    MoveCameraBy(2,0)
  else if KeyDown(VK_UP) then
    MoveCameraBy(0,-2)
  else if KeyDown(VK_DOWN) then
    MoveCameraBy(0,2)
end;
//updates all event driven actions.
procedure UpdateActions(map:Map);
begin
  UpdateCamera();
  
  if MouseClicked(LeftButton) then
    UpdateSelect(map);
end;
//updates camera position based on user input

procedure Main();
var
  myMap: Map;
begin
  OpenAudio();
  OpenGraphicsWindow('Maps Tests', 640, 480);
  writeln('Map Loading');
  myMap := LoadMap('test1.txt');
  writeln('Map Loaded');
  AllocateValue(myMap, @myMap^.Tiles[2,2], 'test', 0.8);
  repeat // The game loop...
    ProcessEvents();
    UpdateActions(myMap);
    ClearScreen(ColorBlack);
    DrawMap(myMap);
    DrawFramerate(0,0);
    RefreshScreen();
  until WindowCloseRequested();
  ReleaseAllResources();
  CloseAudio();
end;
begin
  Main();
end.
