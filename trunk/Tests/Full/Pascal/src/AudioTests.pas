unit AudioTests;

interface
	uses TestFramework;

	procedure AddAudioSuite(var suites: TestSuites); 

implementation
	uses GameResources, sgAudio, sgInput, sgGraphics, sgTypes, sgGeometry, sgText, SysUtils;
	
	procedure TestPlaySound(const drawIn: Rectangle);
	begin
		if KeyTyped(VK_A) then PlaySoundEffect(SoundEffectNamed('Shock'));
		if KeyTyped(VK_I) then 
		begin	
			if false = SoundEffectPlaying(SoundEffectNamed('Shock')) then PlaySoundEffect(SoundEffectNamed('Shock'));
		end;
		if KeyTyped(VK_S) then StopSoundEffect(SoundEffectNamed('Shock'));
		if KeyTyped(VK_L) then PlaySoundEffect(SoundEffectNamed('Shock'), -1);
	end;
	
	procedure TestPlayMusic(const drawIn: Rectangle);
	begin
		if KeyTyped(VK_A) then PlayMusic(MusicNamed('Fast'), 1);
		if KeyTyped(VK_I) then 
		begin	
			if false = MusicPlaying() then PlayMusic(MusicNamed('Fast'));
		end;
		if KeyTyped(VK_S) then StopMusic();
		if KeyTyped(VK_L) then PlayMusic(MusicNamed('Fast'));
		
		if KeyDown(VK_UP) then 
		begin
		  SetMusicVolume(MusicVolume() + 0.01);
		end;
		if KeyDown(VK_DOWN) then 
		begin
		  SetMusicVolume(MusicVolume() - 0.01);
		end;
		
		DrawText('Volume ' + FloatToStr(MusicVolume()), ColorWhite, 100, 100);
	end;
	
	function GetAudioTests(): TestSuite;
	var
		i: Integer;
	begin
		result.Title := 'Audio Tests';
		SetLength(result.Tests, 2);
		
		for i := 0 to High(result.Tests) do
		begin
			InitTest(result.Tests[i]);
		end;
		
		with result.Tests[0] do
		begin
			MethodBeingTested := 'PlaySoundEffect, IsSoundEffectPlaying, StopSoundEffect';
			Instructions := 'pl[a]y sound once' + LineEnding + '' + LineEnding + 'play [i]f not playing' + LineEnding + '[s]top sound effect' + LineEnding + '[l]oop sound';
			ToRun := @TestPlaySound;
		end;
		with result.Tests[1] do
		begin
			MethodBeingTested := 'PlayMusic, MusicPlaying, StopMusic';
			Instructions := 'pl[a]y music once' + LineEnding + 'play [i]f not playing' + LineEnding + '[s]top music' + LineEnding + '[l]oop music';
			ToRun := @TestPlayMusic;
		end;
	end;
	
	procedure AddAudioSuite(var suites: TestSuites);
	begin
		suites[High(suites)] := GetAudioTests();
	end;

end.