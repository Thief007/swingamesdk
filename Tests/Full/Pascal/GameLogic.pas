unit GameLogic;

interface
	procedure Main();

implementation
	uses
	GameResources,
	SysUtils,
	SGSDK_Core, TestFramework, AudioTests;
	
	
	type SuiteAdder = procedure (var suites: TestSuites);
	
	procedure AddSuite(var suites: TestSuites; adder: SuiteAdder);
	begin
		SetLength(suites, Length(suites) + 1);
		adder(suites);
	end;
	
	procedure LoadSuites(var suites: TestSuites);
	begin
		SetLength(suites, 0);
		
		AddSuite(suites, @AddAudioSuite);
	end;
		
	//The main procedure that controlls the game logic.
	//
	// SIDE EFFECTS:
	// - Creates the screen, and displays a message
	procedure Main();
	var
		suites: TestSuites;
		i : Integer;
	begin
		OpenGraphicsWindow('Testing SwinGame SDK - Pascal', 800, 600);
		
		LoadResources();
		
		LoadSuites(suites);
		
		for i := 0 to High(suites) do
		begin
			RunTestSuite(suites[i]);
		end;
		
		WriteReport(suites);
		
		FreeResources();
	end;
end.