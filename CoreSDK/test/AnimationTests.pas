program AnimationTests;

uses sgTypes, sgShared, sgSprites, sgAnimations, SysUtils, sgCore, sgAudio, sgResources, sgImages, sgGraphics, sgGeometry, sgText, sgInput;

var
  explosions: AnimationTemplate;
  boom: Array [0..1] of Animation;
  expl: Bitmap;
  i, currentAnim: Integer;
  
  //Test animated sprites
  s: Sprite;
begin
  OpenAudio();
  OpenGraphicsWindow('Animation Tests', 640, 480);
  
  LoadResourceBundle('Explosion.txt');
  
  boom[0] := CreateAnimation('explosion', FetchAnimationTemplate('explosion_temp'), False);
  boom[1] := CreateAnimation('implosion', FetchAnimationTemplate('explosion_temp'), False);
  
  expl := FetchBitmap('explosion_bmp');
  
  WriteLn('ID of boom[0] = ', HexStr(boom[0]));
  WriteLn('ID of expl = ', HexStr(expl));
  
  currentAnim := -1;
  
  s := CreateSprite(FetchBitmap('red_explosion'),'explosion',FetchAnimationTemplate('explosion_temp'));
  SpriteAddLayer(s, FetchBitmap('count'), 'count');
  SpriteShowLayer(s, 'count');
  SpriteSetLayerOffset(s, 'count', PointAt(3,3));
  SpriteStartAnimation(s, 'explosion_loop');
  SpriteSetPosition(s, PointAt(200,200));
  
  repeat // The game loop...
    ProcessEvents();
    
    if MouseClicked(LeftButton) then SpriteToggleLayerVisible(s, 'count');
    if MouseClicked(RightButton) then SpriteToggleLayerVisible(s, 'explosion');
    if KeyTyped(vk_Up) then SpriteBringLayerForward(s, 1);
    if KeyTyped(vk_Down) then SpriteSendLayerBackward(s, 0);
    
    UpdateSprite(s);
    
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
    DrawSprite(s);
    
    DrawRectangle(ColorGreen, SpriteLayerRectangle(s, 'count'));
    
    DrawCell(FetchBitmap('red_explosion'), 1, 200, 50);
    
    DrawFramerate(0,0);
    
    RefreshScreen(60);
  until WindowCloseRequested();
  
  ReleaseAllResources();
  CloseAudio();
end.
