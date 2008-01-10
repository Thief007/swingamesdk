program MyGame;
{$R 'MyGame.res' 'MyGame.rc'}

uses
  GameLogic in 'GameLogic.pas',
  GameResources in 'GameResources.pas',
  SGSDK_Audio;

begin
  OpenAudio();
  Main();
  CloseAudio();
end.
