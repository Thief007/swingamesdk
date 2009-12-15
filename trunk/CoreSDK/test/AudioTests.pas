program AudioTests;
uses
  sgTypes, sgCore, sgAudio, sgText, sgGraphics, sgResources;

procedure Main();
var
  snd, snd2, snd3: SoundEffect;
  mus, mus1: Music;
  i: Integer;
begin
  OpenAudio();
  //OpenGraphicsWindow('Audio Tests', 640, 480);
  
  //Test releasing of resources - not assigned
  ReleaseSoundEffect('fred');
  ReleaseMusic('fred');
  
  mus := MapMusic('fast', 'Fast.mp3');
  mus1 := LoadMusic('menu.ogg');
  snd := MapSoundEffect('shock', 'shock.wav');
  snd2 := MapSoundEffect('menu', 'menu.ogg');
  snd3 := MapSoundEffect('menu3', 'menu.ogg');
  
  WriteLn(HexStr(mus), ' = Loaded ', MusicName(mus), ' for file ', MusicFilename(mus));
  WriteLn(HexStr(snd), ' = Loaded ', SoundEffectName(snd), ' for file ', SoundEffectFilename(snd));
  WriteLn(HexStr(snd2), ' = Loaded ', SoundEffectName(snd2), ' for file ', SoundEffectFilename(snd2));
  WriteLn(HexStr(snd3), ' = Loaded ', SoundEffectName(snd3), ' for file ', SoundEffectFilename(snd3));
  
  FadeMusicIn(mus, 500);
  Delay(1000);
  
  PlaySoundEffect(snd);
  Delay(200);
  
  PlaySoundEffect(snd2);
  Delay(1000);
  
  FadeMusicOut(500);
  Delay(500);
  
  for i := 0 to 3 do
  begin
    PlayMusic(mus);
    Delay(1000);
    StopMusic();
  end;
  
  PlayMusic(mus1);
  Delay(2000);
  
  ReleaseAllResources();
  CloseAudio();
  Delay(2000);
end;

begin
  Main();
end.
