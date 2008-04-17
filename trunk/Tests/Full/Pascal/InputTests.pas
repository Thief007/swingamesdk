unit InputTests;

interface
	uses TestFramework;

	procedure AddInputSuite(var suites: TestSuites); 

implementation
	uses GameResources, SGSDK_Input, SGSDK_Core, SGSDK_Graphics, SGSDK_KeyCodes, SGSDK_Shapes, SGSDK_Font, SysUtils;
	
	var
		scroll: Integer;
		MouseMovement: Vector;
		_EnteredText: String;
	
	procedure InputTest(const drawIn: Rectangle);
	begin
        MouseMovement := GetMouseMovement();
       
        DrawText('Mouse Position : ' + FloatToStr(GetMousePosition().X) + ', ' + FloatToStr(GetMousePosition().Y), ColourWhite, GameFont('Courier'), 10, 10);
        DrawText('Mouse Movement : ' + FloatToStr(MouseMovement.X) + ', ' + FloatToStr(MouseMovement.Y), ColourWhite, GameFont('Courier'), 10, 30);

        if WasKeyTyped(VK_H) then HideMouse();
        if WasKeyTyped(VK_S) then ShowMouse();
        if IsKeyPressed(VK_C) then MoveMouse(400, 300);
				if MouseWasClicked(WheelUpButton) then scroll += 1;
				if MouseWasClicked(WheelDownButton) then scroll -= 1;

        DrawLineOnScreen(GetColor(120, 177, 250), CreateLine(GetMousePosition().X, 0, GetMousePosition().X, 600));
        DrawLineOnScreen(GetColor(120, 177, 250), CreateLine(0, GetMousePosition().Y, 800, GetMousePosition().Y));

        DrawText('Is Left Mouse Button Down : ' + BoolToStr(IsMouseDown(LeftButton)), ColourWhite, GameFont('Courier'), 10, 50);
        DrawText('Was Left Mouse Button Clicked : ' + BoolToStr(MouseWasClicked(LeftButton)), ColourWhite, GameFont('Courier'), 10, 70);
        DrawText('Is Mouse Cursor Shown : ' + BoolToStr(IsMouseShown()), ColourWhite, GameFont('Courier'), 10, 90);

				DrawText('Scroll : ' + IntToStr(scroll), ColourWhite, GameFont('Courier'), 10, 140);	
	end;
	
	procedure KeyboardInputTest(const drawIn: Rectangle);
	begin
		DrawText('A Key Down : ' + BoolToStr(IsKeyPressed(VK_A)), ColourWhite, GameFont('Courier'), 10, 10);
		DrawText('A Key Up : ' + BoolToStr(not IsKeyPressed(VK_A)), ColourWhite, GameFont('Courier'), 10, 30);
		DrawText('A Key Typed : ' + BoolToStr(WasKeyTyped(VK_A)), ColourWhite, GameFont('Courier'), 10, 50);

		DrawText('Is Reading Text : ' + BoolToStr(IsReadingText()), ColourWhite, GameFont('Courier'), 10, 90);

		if (not IsReadingText()) and (WasKeyTyped(VK_S)) then
		begin
		    StartReadingText(ColourWhite, 10, GameFont('Courier'), 35, 240);
		end;
		
		if IsReadingText() and MouseWasClicked(LeftButton) then
			_EnteredText := EndReadingText()
		else
			_EnteredText := TextReadAsASCII();
		
		DrawText('You Entered : ' + _EnteredText, ColourWhite, GameFont('Courier'), 10, 130);
	end;
	
	function GetInputTests(): TestSuite;
	var
		i: Integer;
	begin
		result.Title := 'Keyboard and Mouse Input Tests';
		SetLength(result.Tests, 2);
		
		for i := 0 to High(result.Tests) do
		begin
			InitTest(result.Tests[i]);
		end;
		
		with result.Tests[0] do
		begin
			MethodBeingTested := 'All Mouse';
			Instructions := '[H]ide Mouse' + EOL +
	        				'[S]how Mouse' + EOL +
	        				'Move Mouse to [C]enter'  + EOL +
	        				'Click the Left Mouse Button';
			ToRun := @InputTest;
		end;
		
		with result.Tests[1] do
		begin
			MethodBeingTested := 'All Keyboard';
			Instructions := 'Hit the [A] Key' + EOL +
            				'[S]tart Reading Text' + EOL + 
										'Click mouse to end reading text';
			ToRun := @KeyboardInputTest;
		end;
	end;
	
	procedure AddInputSuite(var suites: TestSuites);
	begin
		scroll := 0;
		
		suites[High(suites)] := GetInputTests();
	end;

end.