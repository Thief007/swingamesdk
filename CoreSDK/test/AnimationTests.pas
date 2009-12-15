program AnimationTests;

uses sgTypes, sgShared, sgAnimations, SysUtils, sgCore, sgAudio, sgResources, sgImages, sgGraphics, sgText;

var
  explosions: AnimationTemplate;
  boom: Array [0..1] of Animation;
  expl: Bitmap;
  i: Integer;
begin
  OpenAudio();
  OpenGraphicsWindow('Animation Tests', 640, 480);
  
  LoadResourceBundle('Explosion.txt');
  
  boom[0] := CreateAnimation('explosion', FetchAnimationTemplate('explosion_temp'), True);
  boom[1] := CreateAnimation('implosion', FetchAnimationTemplate('explosion_temp'), False);
  
  expl := FetchBitmap('explosion_bmp');
  
  WriteLn(HexStr(boom[0]), ' - ', HexStr(expl));
  
  //RestartAnimation(boom[0]);
  repeat // The game loop...
    ProcessEvents();
    
    UpdateAnimation(boom[0]);
    
    ClearScreen(ColorBlack);
    
    DrawAnimation(boom[0], expl, 50, 50);
    
    
    DrawFramerate(0,0);
    
    RefreshScreen();
  until WindowCloseRequested();
  
  ReleaseAllResources();
  CloseAudio();
end.