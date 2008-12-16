unit AudioTests;

interface
	uses TestFramework;

	procedure AddAudioSuite(var suites: TestSuites); 

implementation
	uses GameResources, SGSDK_Core, SGSDK_Audio, SGSDK_Input, SGSDK_Graphics, SGSDK_KeyCodes, SGSDK_Shapes, SGSDK_Font, SysUtils;
	
	procedure TestPlaySound(const drawIn: Rectangle);
	begin
		if WasKeyTyped(VK_A) then PlaySoundEffect(GameSound('Shock'));
		if WasKeyTyped(VK_I) then 
		begin	
			if false = IsSoundEffectPlaying(GameSound('Shock')) then PlaySoundEffect(GameSound('Shock'));
		end;
		if WasKeyTyped(VK_S) then StopSoundEffect(GameSound('Shock'));
		if WasKeyTyped(VK_L) then PlaySoundEffect(GameSound('Shock'), -1);
	end;
	
	procedure TestPlayMusic(const drawIn: Rectangle);
	begin
		if WasKeyTyped(VK_A) then PlayMusic(GameMusic('Fast'), 1);
		if WasKeyTyped(VK_I) then 
		begin	
			if false = IsMusicPlaying() then PlayMusic(GameMusic('Fast'));
		end;
		if WasKeyTyped(VK_S) then StopMusic();
		if WasKeyTyped(VK_L) then PlayMusic(GameMusic('Fast'));
		
		if IsKeyPressed(VK_UP) then 
		begin
		  SetMusicVolume(MusicVolume() + 0.01);
		end;
		if IsKeyPressed(VK_DOWN) then 
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
			Instructions := 'pl[a]y sound once' + EOL + '' + EOL + 'play [i]f not playing' + EOL + '[s]top sound effect' + EOL + '[l]oop sound';
			ToRun := @TestPlaySound;
		end;
		with result.Tests[1] do
		begin
			MethodBeingTested := 'PlayMusic, IsMusicPlaying, StopMusic';
			Instructions := 'pl[a]y music once' + EOL + 'play [i]f not playing' + EOL + '[s]top music' + EOL + '[l]oop music';
			ToRun := @TestPlayMusic;
		end;
	end;
	
	procedure AddAudioSuite(var suites: TestSuites);
	begin
		suites[High(suites)] := GetAudioTests();
	end;

end.