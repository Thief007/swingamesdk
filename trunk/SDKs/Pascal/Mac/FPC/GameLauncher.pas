program MyGame;
uses
  SGSDK_Audio, GameLogic in 'GameLogic.pas'; {$IFNDEF UNIX} {$r GameLauncher.res} {$ENDIF} 

begin
  OpenAudio();
  Main();
  CloseAudio();
end.
