program AudioTests;
uses
  sgTypes, sgCore, sgAudio, sgText, sgGraphics, sgResources;

procedure Main();
var
  snd: SoundEffect;
  mus: Music;
  i: Integer;
begin
  OpenAudio();
  //OpenGraphicsWindow('Audio Tests', 640, 480);
  
  //Test releasing of resources - not assigned
  ReleaseSoundEffect('fred');
  ReleaseMusic('fred');
  
  mus := MapMusic('fast', 'Fast.mp3');
  snd := MapSoundEffect('shock', 'shock.wav');
  
  WriteLn('Loaded ', MusicName(mus), ' for file ', MusicFilename(mus));
  WriteLn('Loaded ', SoundEffectName(snd), ' for file ', SoundEffectFilename(snd));
  
  FadeMusicIn(mus, 500);
  Delay(1000);
  
  PlaySoundEffect(snd);
  Delay(200);
  
  PlaySoundEffect(snd);
  Delay(1000);
  
  FadeMusicOut(500);
  Delay(500);
  
  for i := 0 to 10 do
  begin
    PlayMusic(mus);
    Delay(1000);
    StopMusic();
  end;
  
  ReleaseAllResources();
  CloseAudio();
end;

begin
  Main();
end.
