program GameMain;
uses
  sgAudio, GameLogic in 'GameLogic.pas'; {$IFNDEF UNIX} {$r GameLauncher.res} {$ENDIF} 

begin
  OpenAudio();
  Main();
  CloseAudio();
end.
