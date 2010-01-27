program CharacterTest;

uses
  sgCore, sgAudio, sgGraphics, sgResources, sgText, sgGeometry, sgTypes, sgCharacters, sgNamedIndexCollection, sgSprites, sgInput, sgUserinterface;
	  
const
	speed = 5;
		
procedure Main();
var
	c: Character;
	i: integer;
	p2d : Array of Point2D;
	charDetails: Panel
	
begin
  OpenAudio();
  
  OpenGraphicsWindow('Hello World', 800, 600);
   
	c:= LoadCharacter('test2.txt'); 
	
	
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
	 
	 
  repeat
    ProcessEvents();
    ClearScreen(ColorBlack);
		
		DrawCharacterWithStationary(c, 0, 1);
//		DrawCharacter(c);
		UpdateSpriteAnimation(c^.CharSprite);
		MoveSprite(c^.CharSprite);
		
		if RegionClickedBool then WriteLn('Worky');
		
		
		if KeyDown(vk_Up) then c^.CharSprite^.velocity:= p2d[0]
		else if KeyDown(vk_Down) then c^.CharSprite^.velocity:= p2d[1]
		else if KeyDown(vk_Left) then c^.CharSprite^.velocity:= p2d[2]
		else if KeyDown(vk_Right) then c^.CharSprite^.velocity:= p2d[3]
		else c^.CharSprite^.velocity:= p2d[4];
		
		if KeyTyped(vk_1) then ToggleLayerVisibility(c, 1);
		    
    DrawFramerate(0,0);
    RefreshScreen(60);
  until WindowCloseRequested();
  
  ReleaseAllResources();
  
  CloseAudio();
end;

begin
  Main();
end.
