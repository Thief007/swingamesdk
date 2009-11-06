program AudioTests;
uses
  sgTypes, sgCore, sgAudio, sgText, sgGraphics, sgResources;

procedure Main();
var
  snd: SoundEffect;
  mus: Music;
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
  Sleep(1000);
  
  PlaySoundEffect(snd);
  Sleep(200);
  
  PlaySoundEffect(snd);
  Sleep(1000);
  
  FadeMusicOut(500);
  Sleep(500);
  
  ReleaseAllResources();
  CloseAudio();
end;

begin
  Main();
end.
