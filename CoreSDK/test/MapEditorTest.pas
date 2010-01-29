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
  i,j : LongInt;
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
  AllocateValue(myMap, TileAt(myMap, 2,2), 'test', 0.8);
  repeat // The game loop...
    ProcessEvents();
    UpdateActions(myMap);

      if KeyDown(vk_Up) then c^.CharSprite^.velocity:= p2d[0]
      else if KeyDown(vk_Down) then c^.CharSprite^.velocity:= p2d[1]
      else if KeyDown(vk_Left) then c^.CharSprite^.velocity:= p2d[2]
      else if KeyDown(vk_Right) then c^.CharSprite^.velocity:= p2d[3]
      else c^.CharSprite^.velocity:= p2d[4];

		if KeyTyped(vk_1) then ToggleLayerVisibility(c, 1);

    UpdateSpriteAnimation(c^.CharSprite);
    //if KeyDown(vk_m) then
      MoveSprite(c^.CharSprite);

    CenterCameraOn(c, VectorTo(0,0));
    ClearScreen(ColorBlack);
    DrawMap(myMap);

		DrawCharacterWithStationary(c, 0, 1);
    
    if SpriteHasCollidedWithTile(myMap, 2, c^.CharSprite, i, j) then
    begin
      //HighLightTile(@myMap^.Tiles[j, i], myMap);
      //WriteLn('Character Velocity: ', PointToString(c^.CharSprite^.velocity));
      MoveOut(myMap,CharacterSprite(c), i, j);
    end;
    
    
    DrawFramerate(0,0);
    RefreshScreen();
  until WindowCloseRequested();
  ReleaseAllResources();
  CloseAudio();
end;
begin
  Main();
end.
