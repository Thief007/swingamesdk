program MyGame;
uses
  GameLogic in 'GameLogic.pas'; {$IFNDEF UNIX} {$r GameLauncher.res} {$ENDIF} 

begin
  Main();
end.
