program WhacAMole;
uses 
SwinGame, sgTypes, sgUserInterface, SysUtils;

Const 
  numOfMole = 20;
  Level = 10;
  numberOfHoles = 9;
  TimePerRound = 15;

Type
  State = (Empty, Mole, MoleUp, MoleDown, Wack);
  
  ProgressBar = record
    partRect: Rectangle;
    fullBar: Bitmap;
    emptyBar: Bitmap;    
  end;  
  
  HoleData = record    
    moleSprite : Sprite;
    holeState   : State;    
  end;
  
  GameData = record
    Score, MoleRemaining, Level, Life : Integer;    
    lifeBitmap  : Bitmap;
    timeRemainingBar : ProgressBar;    
    gTimer : Timer;    
    hole  : array [1..numberOfHoles] of HoleData;
    nextAt  : Single;  
    //lastUpdateTime, timePassed : Single;    
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
begin
  DrawBitmap(gData.timeRemainingBar.fullBar, 32, 439);  
  gData.timeRemainingBar.partRect := RectangleFrom(0, 0, 34, 279);  
  
  if TimerTicks(gData.gTimer) >= 1000  then UpdateProgressBar(gData, 1/TimePerRound);
  if TimerTicks(gData.gTimer) >= 2000  then UpdateProgressBar(gData, 2/TimePerRound);
  if TimerTicks(gData.gTimer) >= 3000  then UpdateProgressBar(gData, 3/TimePerRound);
  if TimerTicks(gData.gTimer) >= 4000  then UpdateProgressBar(gData, 4/TimePerRound);  
  if TimerTicks(gData.gTimer) >= 5000  then UpdateProgressBar(gData, 5/TimePerRound);
  if TimerTicks(gData.gTimer) >= 6000  then UpdateProgressBar(gData, 6/TimePerRound);
  if TimerTicks(gData.gTimer) >= 7000  then UpdateProgressBar(gData, 7/TimePerRound);
  if TimerTicks(gData.gTimer) >= 8000  then UpdateProgressBar(gData, 8/TimePerRound);
  if TimerTicks(gData.gTimer) >= 9000  then UpdateProgressBar(gData, 9/TimePerRound);
  if TimerTicks(gData.gTimer) >= 10000 then UpdateProgressBar(gData, 10/TimePerRound);
  if TimerTicks(gData.gTimer) >= 11000 then UpdateProgressBar(gData, 11/TimePerRound);
  if TimerTicks(gData.gTimer) >= 12000 then UpdateProgressBar(gData, 12/TimePerRound);
  if TimerTicks(gData.gTimer) >= 13000 then UpdateProgressBar(gData, 13/TimePerRound);
  if TimerTicks(gData.gTimer) >= 14000 then UpdateProgressBar(gData, 14/TimePerRound);
  if TimerTicks(gData.gTimer) >= 15000 then UpdateProgressBar(gData, 15/TimePerRound);
end;

procedure InitMolesSprite(var gData : GameData);
var  
  i, xM, vGap, hGap : Integer;  
begin
  xM := 260;  
  hGap := 0;
  vGap := 170;  
  
  //Generate first row
  for i := 1 to 3 do
  begin
    gData.hole[i].moleSprite := CreateSprite(BitmapNamed('WhacAMole'), AnimationScriptNamed('WhacAMole_temp'));       
    SpriteSetX(gData.hole[i].moleSprite, xM+hGap);
    SpriteSetY(gData.hole[i].moleSprite, 252);    
    gData.hole[i].holeState := Empty;      
    hGap +=250;    
  end;  
  
  hGap := 0;  
  
  for i := 4 to 6 do
  begin
    gData.hole[i].moleSprite := CreateSprite(BitmapNamed('WhacAMole'), AnimationScriptNamed('WhacAMole_temp'));    
    SpriteSetX(gData.hole[i].moleSprite, xM+hGap);
    SpriteSetY(gData.hole[i].moleSprite, 252+vGap);        
    gData.hole[i].holeState := Empty;    
    hGap +=250;    
  end;
  
  hGap := 0;  
  vGap := 333;
  
  for i := 7 to 9 do
  begin
    gData.hole[i].moleSprite := CreateSprite(BitmapNamed('WhacAMole'), AnimationScriptNamed('WhacAMole_temp'));        
    SpriteSetX(gData.hole[i].moleSprite, xM+hGap);
    SpriteSetY(gData.hole[i].moleSprite, 252+vGap);
    gData.hole[i].holeState := Empty;    
    hGap +=250;    
  end;  
end;

procedure PlayGame(var gData : GameData);
var
  i : Integer;
begin    
    for i := 1 to numberOfHOles do
    begin
      UpdateSprite(gData.hole[i].moleSprite);
      
      case gData.hole[i].holeState of
        Empty: 
        Begin
          if (TimerTicks(gData.gTimer) > gData.nextAt) and (Rnd() > 1/9) then
          Begin
            gData.hole[i].holeState := MoleUp;
            gData.nextAt := TimerTicks(gData.gTimer) + Rnd(1000);
            SpriteStartAnimation(gData.hole[i].moleSprite, 'MoleUp');    
          End;
        End;
        MoleUp:
        Begin
          if SpriteAnimationHasEnded(gData.hole[i].moleSprite) then gData.hole[i].holeState := Mole;
          {if (TimerTicks(gData.gTimer) > gData.hole[i].nextAt[i]) and (Rnd() > 1/9) then
          begin
            gData.hole[i].nextAt[i] := TimerTicks(gData.gTimer) + Rnd(2000);
            SpriteStartAnimation(gData.hole[i].moleSprite, 'MoleDown');
          end;}
        End;
        Mole:
        Begin
          if SpriteAnimationHasEnded(gData.hole[i].moleSprite) then gData.hole[i].holeState := MoleDown;
          {if (TimerTicks(gData.gTimer) > gData.hole[i].nextAt[i]) and (Rnd() > 1/9) then
          begin
            gData.hole[i].nextAt[i] := TimerTicks(gData.gTimer) + Rnd(2000);
            SpriteStartAnimation(gData.hole[i].moleSprite, 'MoleDown');
          end;}
        End;
        MoleDown: 
        Begin
          if SpriteAnimationHasEnded(gData.hole[i].moleSprite) then gData.hole[i].holeState := Empty;
          {if (TimerTicks(gData.gTimer) > gData.hole[i].nextAt[i]) and (Rnd() > 1/9) then
          begin
            gData.hole[i].nextAt[i] := TimerTicks(gData.gTimer) + Rnd(2000);
          end;}
        End;
        //Wack:        
      end;      
    end;
    

end;

procedure InitGame(var gData : GameData);  
begin
  gData.Score := 0;
  gData.Life := 3;
  gData.MoleRemaining := numOfMole;
  gData.Level := 1;
  gData.nextAt := 1000;  
  gData.lifeBitmap := LoadBitmap('heart.png');  
  
  LoadBitmapNamed('bgImage', 'whac-a-mole-background.png');
  gData.timeRemainingBar.emptyBar := LoadBitmapNamed('progressEmpty', 'progress_emptyVertical.png');
  gData.timeRemainingBar.fullBar := LoadBitmapNamed('progressFull', 'progress_fullVertical.png');    
  
  InitMolesSprite(gData);
  
  gData.gTimer := CreateTimer();
  StartTimer(gData.gTimer);
  
  DrawBackGroundImage();  
  DrawLife(gData);
  DrawProgressBar(gData);
  DisplayScore(gData.Score);
  DisplayMoleRemaining(gData.MoleRemaining);
  DisplayCurrentLevel(gData.Level);
end;

procedure DrawGame(var gData : GameData);
begin
   DrawBackGroundImage();
   DrawLife(gData);
   DrawProgressBar(gData);
   DisplayScore(gData.Score);
   DisplayMoleRemaining(gData.MoleRemaining);
   DisplayCurrentLevel(gData.Level);   
end;

procedure Main(); 
var
  data : GameData;
  i : Integer;
begin
  OpenAudio();
  OpenGraphicsWindow('Whack A Mole', 1024, 768);
  LoadDefaultColors();  
  LoadResourceBundle('WhacAMole.txt');  
  InitGame(data);
  
  repeat // The game loop...
    ProcessEvents();    
    
    PlayGame(data);
    
    ClearScreen(ColorWhite);
    DrawGame(data);          
    for i := 1 to numberOfHoles do
      DrawSprite(data.hole[i].moleSprite);      
    
    DrawFramerate(0,0);      
    RefreshScreen();
    
    //UpdateGame(data);    
  until WindowCloseRequested();
  
  ReleaseAllSprites();
  ReleaseAllResources();
end;

begin
  Main();
end.