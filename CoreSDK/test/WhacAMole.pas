program WhacAMole;
uses 
SwinGame, sgTypes, sgUserInterface, SysUtils;

Const 
  numOfMole = 20;
  Level = 10;
  numberOfHoles = 9;
  TimePerRound = 60;
  moleWidth = 245;
  moleHeight = 163;

Type
  State = (Empty, Mole, MoleUp, MoleDown, Wack, Bam);
  
  ProgressBar = record
    partRect: Rectangle;
    fullBar: Bitmap;
    emptyBar: Bitmap;    
  end;  
  
  HoleData = record    
    moleSprite, kapow : Sprite;
    holeState   : State;
    nextAt      : Single;    
  end;
  
  GameData = record
    timeComplete, Score, MoleRemaining, Level, Life : Integer;    
    lifeBitmap  : Bitmap;
    timeRemainingBar : ProgressBar;    
    gTimer : Timer;    
    hole  : array [1..numberOfHoles] of HoleData;    
  end;
  
procedure DisplayScore(score : Integer);
begin
    case score of 
    0..9 : DrawText(IntToStr(score), ColorBlack, 'Arial', 40, 277, 144);
    10..99 : DrawText(IntToStr(score), ColorBlack, 'Arial', 40, 255, 144);
    100..999 : DrawText(IntToStr(score), ColorBlack, 'Arial', 40, 232, 144);
    1000..9999 : DrawText(IntToStr(score), ColorBlack, 'Arial', 40, 209, 144);
    10000..99999 : DrawText(IntToStr(score), ColorBlack, 'Arial', 40, 186, 144);
    100000..999999 : DrawText(IntToStr(score), ColorBlack, 'Arial', 40, 163, 144);
    1000000..9999999 : DrawText(IntToStr(score), ColorBlack, 'Arial', 40, 140, 144);
    else;
      DrawText(IntToStr(9999999), ColorBlack, 'Arial', 40, 140, 144);      
    end;    
end;

procedure DisplayMoleRemaining(moleRemain : Integer);
begin
  case moleRemain of 
    0..9 : DrawText(IntToStr(moleRemain), ColorBlack, 'Arial', 40, 448, 144);
    else
      DrawText(IntToStr(moleRemain), ColorBlack, 'Arial', 40, 426, 144);
  end;    
end;

procedure DisplayCurrentLevel(level : Integer);
begin  
  DrawText(IntToStr(level), ColorBlack, 'Arial', 40, 678, 144);
end;

procedure DrawLife(var gData : GameData);
var  
  i, hGap, vGap : Integer;
begin  
  hGap := 73;
  vGap := 89;  
  
  for i := 1 to gData.life do
  begin
    DrawBitmap(gData.lifeBitmap, hGap, vGap);    
    hGap += 38;
  end;  
end;

procedure DrawBackGroundImage();
begin  
  DrawBitmap ('bgImage', 0, 0);  
end;

procedure UpdateProgressBar(var gData : GameData; percent : Double);
begin
  PauseTimer(gData.gTimer);  
  gData.timeRemainingBar.partRect := RectangleFrom(0, 0, 34, round(percent * 279));
  DrawBitmapPart(gData.timeRemainingBar.emptyBar, gData.timeRemainingBar.partRect, 32, 439);    
  ResumeTimer(gData.gTimer);
end;

procedure DrawProgressBar(var gData : GameData);
var
  i : Integer;
begin
  DrawBitmap(gData.timeRemainingBar.fullBar, 32, 439);  
  gData.timeRemainingBar.partRect := RectangleFrom(0, 0, 34, 279);  
  for i := 1 to TimePerRound do if TimerTicks(gData.gTimer) >= (i*1000) then UpdateProgressBar(gData, i/TimePerRound);  
end;

procedure InitMolesSprite(var gData : GameData);
var  
  i, y, xM, vGap, hGap : Integer;  
begin
  xM := 260;  
  hGap := 0;
  vGap := 0;  
  y := 252;
    
  for i := 1 to numberOfHoles  do
  begin
    gData.hole[i].moleSprite := CreateSprite(BitmapNamed('WhacAMole'), AnimationScriptNamed('WhacAMole_temp'));    
    gData.hole[i].kapow := CreateSprite(BitmapNamed('Kapow'), AnimationScriptNamed('Kapow_temp'));    
    if i = 4 then
    begin
      hGap := 0;
      vGap := 170;      
    end;
    if i = 7 then 
    begin
      hGap := 0;
      vGap := 333;
    end;
    SpriteSetX(gData.hole[i].moleSprite, xM+hGap);
    SpriteSetY(gData.hole[i].moleSprite, y+vGap);    
    SpriteSetX(gData.hole[i].kapow, xM+hGap);    
    SpriteSetY(gData.hole[i].kapow, y+vGap);    
    gData.hole[i].holeState := Empty;    
    gData.hole[i].nextAt :=0;
    hGap +=250;    
  end; 
end;

procedure UpDateScore(var gData: GameData; i: Integer);
begin
  gData.MoleRemaining -= 1;
  gData.Score += 10;
  gData.hole[i].holeState := Bam;
  SpriteStartAnimation(gData.hole[i].moleSprite, 'MoleDown', true);
  SpriteStartAnimation(gData.hole[i].kapow, 'Kapow', true);
end;

procedure PlayGame(var gData : GameData);
var
  i : Integer;
begin    
    for i := 1 to numberOfHOles do
    begin
      UpdateSprite(gData.hole[i].moleSprite);
      UpdateSprite(gData.hole[i].kapow);
      
      case gData.hole[i].holeState of
        Empty: 
        Begin
          if (TimerTicks(gData.gTimer) > gData.hole[i].nextAt) and (Rnd() < 1/900) then
          Begin
            gData.hole[i].holeState := MoleUp;
            gData.hole[i].nextAt := TimerTicks(gData.gTimer) + (rnd(500)+500);
            SpriteStartAnimation(gData.hole[i].moleSprite, 'MoleUp', true);    
          End;
        End;
        MoleUp: if SpriteAnimationHasEnded(gData.hole[i].moleSprite) then gData.hole[i].holeState := Mole;        
        Mole: 
        Begin
          if TimerTicks(gData.gTimer) > gData.hole[i].nextAt then
          begin
            gData.hole[i].holeState := MoleDown;            
            SpriteStartAnimation(gData.hole[i].moleSprite, 'MoleDown', true);
          end;         
        End;
        MoleDown: if SpriteAnimationHasEnded(gData.hole[i].moleSprite) then gData.hole[i].holeState := Empty;        
        Wack: if (gData.MoleRemaining > 0) then UpdateScore(gData, i);        
        Bam:  if SpriteAnimationHasEnded(gData.hole[i].moleSprite) then gData.hole[i].holeState := Empty;        
      end;      
    end;    
    if (TimerTicks(gData.gTimer) >= (TimePerRound*1000)) or (gData.MoleRemaining = 0) then
    begin      
      StopTimer(gData.gTimer);
      gData.hole[i].holeState := Empty;            
    end    
end;

procedure DrawGame(var gData : GameData);
begin
  DrawBackGroundImage();
  DrawLife(gData);
  DrawProgressBar(gData);
  DisplayScore(gData.Score);
  DisplayMoleRemaining(gData.MoleRemaining);
  DisplayCurrentLevel(gData.Level);   
  DrawFramerate(0,0);             
end;

procedure InitGame(var gData : GameData);  
begin
  gData.Score := 0;
  gData.Life := 3;
  gData.MoleRemaining := numOfMole;
  gData.Level := 1;
  gData.timeComplete := 0;
  gData.lifeBitmap := LoadBitmap('heart.png');  
  
  LoadBitmapNamed('bgImage', 'whac-a-mole-background.png');
  gData.timeRemainingBar.emptyBar := LoadBitmapNamed('progressEmpty', 'progress_emptyVertical.png');
  gData.timeRemainingBar.fullBar := LoadBitmapNamed('progressFull', 'progress_fullVertical.png');    
  
  InitMolesSprite(gData);
  
  gData.gTimer := CreateTimer();
  StartTimer(gData.gTimer);
  
  DrawGame(gData);
end;

procedure HandleInput(var gData : GameData);
var
  mousePos : Point2D;
  i : Integer;
begin
  if MouseClicked(LeftButton) then
  begin
    mousePos := MousePosition();
    for i := 1 to numberOfHoles do  
      if SpriteOnScreenAt(gData.hole[i].moleSprite, mousePos) then  
        if not (gData.hole[i].holeState = Bam) then gData.hole[i].holeState := Wack;    
  end
end;

procedure Main(); 
var
  gData : GameData;  
  i:  Integer;
begin
  OpenAudio();
  OpenGraphicsWindow('Whack A Mole', 1024, 768);
  LoadDefaultColors();  
  LoadResourceBundle('WhacAMole.txt');  
  InitGame(gData);
  
  repeat // The game loop...
    ProcessEvents();    
    ClearScreen(ColorWhite);
    
    DrawGame(gData);
    PlayGame(gData);    
    HandleInput(gData);

    for i := 1 to numberOfHoles do
    begin      
      if gData.hole[i].holeState = Bam then 
      begin
        DrawSprite(gData.hole[i].kapow);      
      end      
      else DrawSprite(gData.hole[i].moleSprite);    
    end;        
    
    RefreshScreen();    
  until WindowCloseRequested();
  
  ReleaseAllSprites();
  ReleaseAllResources();
end;

begin
  Main();
end.