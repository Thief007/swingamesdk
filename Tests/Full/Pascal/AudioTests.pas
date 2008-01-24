unit AudioTests;

interface
	uses TestFramework;

	procedure AddAudioSuite(var suites: TestSuites); 

implementation
	uses GameResources, SGSDK_Core, SGSDK_Audio, SGSDK_Input, SGSDK_Graphics, SGSDK_KeyCodes, SGSDK_Shapes;
	
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
	
	function GetAudioTests(): TestSuite;
	begin
		result.Title := 'Audio Tests';
		SetLength(result.Tests, 1);
		
		InitTest(result.Tests[0]);
		
		with result.Tests[0] do
		begin
			MethodBeingTested := 'PlaySoundEffect, IsSoundEffectPlaying, StopSoundEffect';
			Instructions := 'pl[a]y sound once' + EOL + 'play [i]f not playing' + EOL + '[s]top sound effect' + EOL + '[l]oop sound';
			ToRun := @TestPlaySound;
		end;
	end;
	
	procedure AddAudioSuite(var suites: TestSuites);
	begin
		suites[High(suites)] := GetAudioTests();
	end;

end.