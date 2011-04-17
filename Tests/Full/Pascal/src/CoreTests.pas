unit CoreTests;

interface
	uses TestFramework;

	procedure AddCoreSuite(var suites: TestSuites); 

implementation
	uses GameResources, sgInput, sgGraphics, sgTypes,sgGeometry,sgText,SysUtils,sgTimers;
	
	var
		_Timer: Timer;
        _IsTimerRunning: Boolean;
        _HasTimerStarted: Boolean;
	
	procedure ScreenTest(const drawIn: Rectangle);
	begin
		DrawText('Current Screen Resolution: ' + IntToStr(ScreenWidth()) + 'x' + IntToStr(ScreenHeight()), ColorWhite, FontNamed('Courier'), 10, 10);

        if KeyTyped(VK_3) then ChangeScreenSize(300, 200);
        if KeyTyped(VK_6) then ChangeScreenSize(640, 480);
        if KeyTyped(VK_8) then ChangeScreenSize(800, 600);

        if KeyTyped(VK_S) then ToggleFullScreen();
	end;
	
	procedure TimerTest(const drawIn: Rectangle);
		procedure StartTimerI();
		begin
			if not _HasTimerStarted then
			begin
				StartTimer(_Timer);
				_HasTimerStarted := true;
				_IsTimerRunning := true;
			end;
		end;
		
		procedure ToggleTimer();
		begin
            if _IsTimerRunning then PauseTimer(_Timer)
            	else ResumeTimer(_Timer);

            _IsTimerRunning := not _IsTimerRunning;
		end;
	begin
		StartTimerI();

        if KeyTyped(VK_S) then ToggleTimer();
        if KeyTyped(VK_R) then StartTimer(_Timer);

        DrawText(IntToStr(TimerTicks(_Timer)), ColorWhite, FontNamed('Courier'), 10, 10);
	end;
	
	function GetCoreTests(): TestSuite;
	var
		i: Integer;
	begin
		result.Title := 'Core Tests';
		SetLength(result.Tests, 2);
		
		for i := 0 to High(result.Tests) do
		begin
			InitTest(result.Tests[i]);
		end;
		
		with result.Tests[0] do
		begin
			MethodBeingTested := 'Screen Tests';
			Instructions := 
		        '[3]00 x 200' + LineEnding +
		        '[6]40 x 480' + LineEnding +
		        '[8]00 x 600' + LineEnding +
		        'Toggle Full [S]creen';
			ToRun := @ScreenTest;
		end;
		
		with result.Tests[1] do
		begin
			MethodBeingTested := 'All of Timer';
			Instructions := '[S]tart and Stop the Timer' + LineEnding +
							'[R]estart Timer';
			_Timer := CreateTimer();
			_IsTimerRunning := false;
			_HasTimerStarted := false;
			ToRun := @TimerTest;
		end;
	end;
	
	procedure AddCoreSuite(var suites: TestSuites);
	begin
		suites[High(suites)] := GetCoreTests();
	end;

end.