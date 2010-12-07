program AudioTests;
uses
  sgTypes, sgCore, sgAudio, sgText, sgGraphics, sgResources;

procedure Main();
var
  snd, snd2, snd3: SoundEffect;
  mus, mus1, miditest: Music;
  i: Integer;
begin
  OpenAudio();
  //OpenGraphicsWindow('Audio Tests', 640, 480);
  
  //Test releasing of resources - not assigned
  ReleaseSoundEffect('fred');
  ReleaseMusic('fred');
  
  mus := LoadMusicNamed('fast', 'Fast.mp3');
  mus1 := LoadMusic('menu.ogg');
  snd := LoadSoundEffectNamed('shock', 'shock.wav');
  snd2 := LoadSoundEffectNamed('menu', 'menu.ogg');
  snd3 := LoadSoundEffectNamed('menu3', 'menu.ogg');
  miditest := LoadMusicNamed('ASNY', 'aintseennothingyet.mid');
  
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
  StopSoundEffect(snd2);
  
  WriteLn('Fading music out...');
  FadeMusicOut(500);
  Delay(500);
  
  WriteLn('Press enter');
  ReadLn();
  
  WriteLn('Stopping...');
  StopMusic();
  WriteLn('Press enter');
  ReadLn();
  
  
  WriteLn('Should be playing midi any second...');
  FadeMusicIn(miditest, 500);
  Delay(5000);
  WriteLn('Midi is finished...');
  FadeMusicOut(500);
  WriteLn('Press enter');
  ReadLn();
  
  for i := 0 to 3 do
  begin
    PlayMusic(mus);
    Delay(1000);
    StopMusic();
  end;
  
  WriteLn('Press enter');
  ReadLn();
  
  PlayMusic(mus1);
  Delay(2000);
  StopMusic();
  WriteLn('Music stopped...');
  
  WriteLn('Press enter');
  ReadLn();
  
  
  WriteLn('Playing many channels...');
  for i := 0 to 30 do
  begin
    PlaySoundEffect(snd);
    Delay(100);
  end;
  
  WriteLn('Press enter');
  ReadLn();
  
  PlayMusic(mus1);
  
  WriteLn('Press enter');
  ReadLn();
  
  ReleaseAllResources();
  CloseAudio();
  Delay(2000);
end;

begin
  Main();
end.
