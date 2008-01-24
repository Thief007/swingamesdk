unit TestFramework;

interface
	 uses SGSDK_Shapes;
	
	type
		TestMethod = procedure (const drawIn: Rectangle);
		
		TestSet = record
				MethodBeingTested: String;
				Instructions: String;
				ToRun: TestMethod;
				Passed: Boolean;
				Skipped: Boolean;
				Done: Boolean;
				ClearScreen: Boolean;
			end;
			
		TestSuite = record
				Title: String;
				Tests: Array of TestSet;
			end;
			
		TestSuites = Array of TestSuite;

		procedure RunTestSuite(const suite: TestSuite);
		procedure InitTest(var test: TestSet);
		procedure WriteReport(const suites: TestSuites);
implementation
	uses SGSDK_Input, SGSDK_Graphics, SGSDK_Font, SGSDK_Core, SGSDK_KeyCodes, SGSDK_Camera, GameResources;
	
	const
		TITLE_TEXT_TOP = 26;
		TITLE_TEXT_LEFT = 55;
		
		TITLE_HEIGHT = 50;
		SCREEN_WIDTH = 800;
		SCREEN_HEIGHT = 600;
		METHOD_TOP = 91;
		METHOD_LEFT = 38;
		METHOD_HEIGHT = 17;
		METHOD_WIDTH = 600;
		INSTRUCTION_WIDTH = 280;
		
		GENERAL_INST_HEIGHT = 20;

		INSTRUCTION_LEFT = 480;
		INSTRUCTION_TOP = 145;
		INSTRUCTION_HEIGHT = 280;
		GENERAL_INST_LEFT = 38;
		GENERAL_INST_TOP = 569;
		//GENERAL_INST_WIDTH = SCREEN_WIDTH;
		TEST_IN_LEFT = 23;
		TEST_IN_TOP = 129;
		TEST_IN_WIDTH = 418;
		TEST_IN_HEIGHT = 418;

	procedure InitTest(var test: TestSet);
	begin
		test.Passed := false;
		test.Skipped := true;
		test.ClearScreen := true;
		test.Done := false;
	end;
	
	procedure DrawTitle(title : String);
	begin
		//FillRectangleOnScreen(ColorBlack, 0, 0, SCREEN_WIDTH, TITLE_HEIGHT);
	    DrawTextOnScreen(title, ColorWhite, GameFont('CourierLarge'), TITLE_TEXT_LEFT, TITLE_TEXT_TOP);
	end;

	procedure DrawMethodBeingTested(methodBeingTested : String);
	begin
		FillRectangleOnScreen(ColorBlack, METHOD_LEFT, METHOD_TOP, METHOD_WIDTH, METHOD_HEIGHT);
	    DrawTextOnScreen(methodBeingTested, ColorWhite, GameFont('Courier'), METHOD_LEFT, METHOD_TOP);
	end;

	procedure DrawInstructions(instructions: String);
	var
		rect: Rectangle;
	begin
		rect := CreateRectangle(INSTRUCTION_LEFT, INSTRUCTION_TOP, INSTRUCTION_WIDTH, INSTRUCTION_HEIGHT);
		FillRectangle(ColorBlack, rect);
		DrawTextLines(instructions, ColorWhite, ColorTransparent, GameFont('Courier'), AlignLeft, rect);
	end;
	
	procedure DrawGeneralInstructions();
	const
		INST = 'Press: [p]ass, [f]ail, [esc] skip suite, [n]ext test';
	var
		fg: Color;
	begin
		fg := GetColor(33, 118, 182, 255);
	    DrawTextOnScreen(INST, fg, GameFont('Courier'), GENERAL_INST_LEFT, GENERAL_INST_TOP);
	end;
	

	procedure RunTest(var test: TestSet; const drawIn: Rectangle);
	begin
		if test.ClearScreen then DrawBitmapOnScreen(GameImage('BGA'), Round(drawIn.x), Round(drawIn.y));	
		
		DrawMethodBeingTested(test.MethodBeingTested);		
		DrawInstructions(test.Instructions);
		
		if WasKeyTyped(VK_P) then
		begin
			test.Passed := true;
			test.Skipped := false;
			test.Done := true;
		end;
		if WasKeyTyped(VK_F) then
		begin
			test.Passed := false;
			test.Skipped := false;
			test.Done := true;
		end;
		if WasKeyTyped(VK_N) then
		begin
			test.Skipped := true;
			test.Done := true;
		end;
		
		SetClip(drawIn);
		SetScreenOffset(-drawIn.x, -drawIn.y);
		test.ToRun(drawIn);
		SetScreenOffset(0, 0);
		ResetClip();
	end;

	procedure RunTestSuite(const suite: TestSuite);
	var
		i: Integer;
		testDrawIn: Rectangle;
		skip: Boolean;
	begin
		skip := false;
		testDrawIn := CreateRectangle(TEST_IN_LEFT, TEST_IN_TOP, TEST_IN_WIDTH, TEST_IN_HEIGHT);
		
		DrawBitmapOnScreen(GameImage('BG'), 0, 0);
		DrawTitle(suite.Title);
		DrawGeneralInstructions();
		
		for i := 0 to High(suite.Tests) do
		begin
			repeat
				ProcessEvents();
												
				RunTest(suite.Tests[i], testDrawIn);
								
				if WasKeyTyped(VK_ESCAPE) then skip := true;
			
				RefreshScreen();
			until WindowCloseRequested() or suite.Tests[i].Done or skip;
			
			if WindowCloseRequested() or skip then break;
		end;
	end;

	procedure WriteSuiteToReport(const suite: TestSuite; var output: Text);
	var
		i: Integer;
	begin
		WriteLn(output, 'Suite: ', suite.Title);
		for i := 0 to High(suite.Tests) do
		begin
			with suite.Tests[i] do
			begin
				if Skipped then 	Write(output, ' --  skip  --')
				else if Passed then Write(output, '    passed   ')
				else 				Write(output, ' ** FAILED **');
				WriteLn(output, ' - ', MethodBeingTested);
			end;
		end;
	end;
	
	procedure WriteReport(const suites: TestSuites);
	var
		output: Text;
		i: Integer;
	begin
		Assign(output, 'results.log');
		ReWrite(output);
		
		for i := 0 to High(suites) do
		begin
			WriteSuiteToReport(suites[i], output);
		end;
		
		Close(output);
	end;

end.