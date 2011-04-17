unit InputTests;

interface
	uses TestFramework;

	procedure AddInputSuite(var suites: TestSuites); 

implementation
	uses GameResources, sgInput, sgGraphics, sgTypes,sgGeometry,sgText,SysUtils;
	
	var
		scroll: Integer;
		_MouseMovement: Vector;
		_EnteredText: String;
	
	procedure InputTest(const drawIn: Rectangle);
	begin
        _MouseMovement := MouseMovement();
       
        DrawText('Mouse Position : ' + FloatToStr(MousePosition().X) + ', ' + FloatToStr(MousePosition().Y), ColorWhite, FontNamed('Courier'), 10, 10);
        DrawText('Mouse Movement : ' + FloatToStr(_MouseMovement.X) + ', ' + FloatToStr(_MouseMovement.Y), ColorWhite, FontNamed('Courier'), 10, 30);

        if KeyTyped(VK_H) then HideMouse();
        if KeyTyped(VK_S) then ShowMouse();
        if KeyDown(VK_C) then MoveMouse(400, 300);
				if MouseClicked(WheelUpButton) then scroll += 1;
				if MouseClicked(WheelDownButton) then scroll -= 1;

        DrawLineOnScreen(RGBColor(120, 177, 250), LineFrom(MousePosition().X, 0, MousePosition().X, 600));
        DrawLineOnScreen(RGBColor(120, 177, 250), LineFrom(0, MousePosition().Y, 800, MousePosition().Y));

        DrawText('Is Left Mouse Button Down : ' + BoolToStr(MouseDown(LeftButton)), ColorWhite, FontNamed('Courier'), 10, 50);
        DrawText('Was Left Mouse Button Clicked : ' + BoolToStr(MouseClicked(LeftButton)), ColorWhite, FontNamed('Courier'), 10, 70);
        DrawText('Is Mouse Cursor Shown : ' + BoolToStr(MouseShown()), ColorWhite, FontNamed('Courier'), 10, 90);
        
				DrawText('Scroll : ' + IntToStr(scroll), ColorWhite, FontNamed('Courier'), 10, 140);	
				
				DrawText('A key was pressed : ' + BoolToStr(AnyKeyPressed()), ColorWhite, 10, 160);
	end;
	
	procedure KeyboardInputTest(const drawIn: Rectangle);
	begin
		DrawText('A Key Down : ' + BoolToStr(KeyDown(VK_A)), ColorWhite, FontNamed('Courier'), 10, 10);
		DrawText('A Key Up : ' + BoolToStr(not KeyDown(VK_A)), ColorWhite, FontNamed('Courier'), 10, 30);
		DrawText('A Key Typed : ' + BoolToStr(KeyTyped(VK_A)), ColorWhite, FontNamed('Courier'), 10, 50);

		DrawText('Is Reading Text : ' + BoolToStr(ReadingText()), ColorWhite, FontNamed('Courier'), 10, 90);

		if (not ReadingText()) and (KeyTyped(VK_S)) then
		begin
		    StartReadingTextWithText('Hello', ColorWhite, 10, FontNamed('Courier'), 35, 240);
		end;
		
		if ReadingText() and MouseClicked(LeftButton) then
			_EnteredText := EndReadingText()
		else
			_EnteredText := TextReadAsASCII();
		
		DrawText('You Entered : ' + _EnteredText, ColorWhite, FontNamed('Courier'), 10, 130);
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
			Instructions := '[H]ide Mouse' + LineEnding +
	        				'[S]how Mouse' + LineEnding +
	        				'Move Mouse to [C]enter'  + LineEnding +
	        				'Click the Left Mouse Button';
			ToRun := @InputTest;
		end;
		
		with result.Tests[1] do
		begin
			MethodBeingTested := 'All Keyboard';
			Instructions := 'Hit the [A] Key' + LineEnding +
            				'[S]tart Reading Text' + LineEnding + 
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