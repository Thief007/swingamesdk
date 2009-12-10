program AnimationTests;

uses cmem, sgTypes, sgShared, sgAnimations, SysUtils, sgCore;

var
  explosions: AnimationTemplate;
  i: Integer;
begin
  for i := 0 to 100 do
  begin
    explosions := MapAnimationTemplate('e' + IntToStr(i), 'explosion.txt');
  end;
  
  ReleaseAllAnimationTemplates();
  
  Delay(2000);
  
end.