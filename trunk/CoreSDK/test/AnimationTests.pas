program AnimationTests;

uses sgTypes, sgShared, sgAnimations;

var
  explosions: AnimationTemplate;
begin
  explosions := LoadAnimationTemplate('explosion.txt');
  
  FreeAnimationTemplate(explosions);
  ReadLn();
end.