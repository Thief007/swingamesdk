program AnimationTests;

uses sgTypes, sgShared, sgAnimations, SysUtils, sgCore, sgAudio, sgResources, sgImages, sgGraphics, sgText;

var
  explosions: AnimationTemplate;
  boom: Array [0..1] of Animation;
  expl: Bitmap;
  i, currentAnim: Integer;
begin
  OpenAudio();
  OpenGraphicsWindow('Animation Tests', 640, 480);
  
  LoadResourceBundle('Explosion.txt');
  
  boom[0] := CreateAnimation('explosion', FetchAnimationTemplate('explosion_temp'), False);
  boom[1] := CreateAnimation('implosion', FetchAnimationTemplate('explosion_temp'), False);
  
  expl := FetchBitmap('explosion_bmp');
  
  WriteLn(HexStr(boom[0]), ' - ', HexStr(expl));
  currentAnim := -1;
  
  repeat // The game loop...
    ProcessEvents();
    
    if (currentAnim = -1) or AnimationEnded(boom[currentAnim]) then
    begin
      currentAnim := (currentAnim + 1) mod Length(boom);
      RestartAnimation(boom[currentAnim]);
    end
    else
      UpdateAnimation(boom[currentAnim]);
    
    ClearScreen(ColorBlack);
    
    if assigned(boom[currentAnim]^.currentFrame) then
      WriteLn(currentAnim, ' - ', boom[currentAnim]^.currentFrame^.cellIndex);
    
    DrawAnimation(boom[currentAnim], expl, 50, 50);
    
    DrawFramerate(0,0);
    
    RefreshScreen(60);
  until WindowCloseRequested();
  
  ReleaseAllResources();
  CloseAudio();
end.
