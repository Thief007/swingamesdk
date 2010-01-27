program MapEditorTest;
//{IFNDEF UNIX} {r GameLauncher.res} {ENDIF}
uses
  sgTypes, sgMaps, sgCore, sgCamera,
  sgInput, sgAudio, sgShared, SysUtils,
  StrUtils, sgResources, sgGraphics, sgText,sgNamedIndexCollection, sgCharacters, sgAnimations, sgSprites, sgGeometry;
//Main procedure
const
	speed = 5;

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
if not KeyDown(VK_LSHIFT) then
  UpdateCamera();


  
  if MouseClicked(LeftButton) then
    UpdateSelect(map);
end;
//updates camera position based on user input

procedure Main();
var
  myMap: Map;
	c: Character;
  p2d : Point2DArray;
begin
  OpenAudio();
  OpenGraphicsWindow('Maps Tests', 640, 480);
  
	SetLength(p2d, 5);
	
	p2d[0].x := 0;
	p2d[0].y := -speed;	
	p2d[1].x := 0;
	p2d[1].y := speed;	
	p2d[2].x := -speed;
	p2d[3].y := 0;	
	p2d[3].x := speed;
	p2d[3].y := 0;
	p2d[4].x := 0;
	p2d[4].y := 0;
	
  c := LoadCharacter('test2.txt');
  writeln('Map Loading');
  myMap := LoadMap('test1.txt');
  writeln('Map Loaded');
  AllocateValue(myMap, @myMap^.Tiles[2,2], 'test', 0.8);
  repeat // The game loop...
    ProcessEvents();
    UpdateActions(myMap);
    CenterCameraOn(c, VectorTo(0,0));
    ClearScreen(ColorBlack);
    DrawMap(myMap);

		DrawCharacterWithStationary(c, 0, 1);
//		DrawCharacter(c);
		UpdateSpriteAnimation(c^.CharSprite);
		MoveSprite(c^.CharSprite);
		if KeyDown(vk_LShift) then
    begin
      if KeyDown(vk_Up) then c^.CharSprite^.velocity:= p2d[0]
      else if KeyDown(vk_Down) then c^.CharSprite^.velocity:= p2d[1]
      else if KeyDown(vk_Left) then c^.CharSprite^.velocity:= p2d[2]
      else if KeyDown(vk_Right) then c^.CharSprite^.velocity:= p2d[3]
      else c^.CharSprite^.velocity:= p2d[4];
    end;
		if KeyTyped(vk_1) then ToggleLayerVisibility(c, 1);

    
    DrawFramerate(0,0);
    RefreshScreen();
  until WindowCloseRequested();
  ReleaseAllResources();
  CloseAudio();
end;
begin
  Main();
end.
