unit FontTests;

interface
	uses TestFramework;

	procedure AddFontSuite(var suites: TestSuites); 

implementation
	uses GameResources, SGSDK_Core, SGSDK_Input, SGSDK_Graphics, SGSDK_KeyCodes, SGSDK_Shapes, SGSDK_Font, SGSDK_Physics, SysUtils;
	
	var
		_TextColor: Colour;
		_x, _y, _z: Integer;
		draw: Bitmap;
	
	procedure TestFont(const drawIn: Rectangle);
	begin
		if IsKeyPressed(VK_C)then
		begin
		  _x := _x + 1;
		  _y := _y + 1;
		  _z := _z + 1;

		  if _x > 255 then _x := 0;
		  if _x < 0 then _x := 255;
		  if _y > 255 then _y := 0;
		  if _y < 0 then _y := 255;
		  if _z > 255 then _z := 0;
		  if _z < 0 then _z := 255;

		 _TextColor := GetColor(_x, _y*_x, _z*_x*_y);
		end;

		//Draw Text
		DrawText('1 This is some Text', _TextColor, GameFont('Courier'), 10, 10);
		DrawText('2 This is some Text', _TextColor, GameFont('Courier'), CreatePoint(10, 30));

		//Draw Text On Bitmap
		ClearSurface(draw, ColourBlack);
		DrawText(draw, '3 This is some Text', _TextColor, GameFont('Courier'), CreatePoint(0, 0));
		DrawBitmap(draw, 10, 50);
		ClearSurface(draw, GetColour(100, 100, 100));
		DrawText(draw, '4 This is some Text', _TextColor, GameFont('Courier'), 0, 0);
		DrawBitmap(draw, 10, 70);

		//Draw Text On Screen
		DrawTextOnScreen('5 This is some Text', _TextColor, GameFont('Courier'), 32, 220);
		DrawTextOnScreen('6 This is some Text', _TextColor, GameFont('Courier'), CreatePoint(32, 240));

		//Draw Text Lines
		DrawTextLines('7 This is some Text Lines' + EOL + '** This is some Text Lines **', _TextColor, ColourTransparent, GameFont('Courier'), AlignCenter, 10, 130, 100, 100);
		DrawTextLines('8 This is some Text Lines' + EOL + '** This is some Text Lines **', _TextColor, ColourGreen, GameFont('Courier'), AlignCenter, CreateRectangle(10, 170, 100, 100));

		//Draw Text Lines on Bitmap
		ClearSurface(draw, ColourTransparent);
		DrawTextLines(draw, '9 This is some Text Lines' + EOL + '** This is some Text Lines **', _TextColor, GetColour(100, 100, 100), GameFont('Courier'), AlignCenter, CreateRectangle(0, 0, 100, 100));
		DrawBitmap(draw, 10, 210);
		ClearSurface(draw, ColourTransparent);
		DrawTextLines(draw, 'A This is some Text Lines' + EOL + '** This is some Text Lines **', _TextColor, ColourBlue, GameFont('Courier'), AlignCenter, 0, 0, 100, 100);
		DrawBitmap(draw, 10, 250);

		//Draw Text Lines on Screen
		DrawTextLinesOnScreen('B This is some Text Lines' + EOL + '** This is some Text Lines **', _TextColor, ColourTransparent, GameFont('Courier'), AlignCenter, 32, 440, 100, 100);
		DrawTextLinesOnScreen('C This is some Text Lines' + EOL + '** This is some Text Lines **', _TextColor, ColourYellow, GameFont('Courier'), AlignCenter, CreateRectangle(32, 480, 100, 100));
	end;

	function GetFontTests(): TestSuite;
	var
		i: Integer;
	begin
		result.Title := 'Font Test';
		SetLength(result.Tests, 1);
		
		for i := 0 to High(result.Tests) do
		begin
			InitTest(result.Tests[i]);
		end;
		
		with result.Tests[0] do
		begin
			MethodBeingTested := 'DrawText, DrawTextOnScreen, DrawTextLines, DrawTextLinesOnScreen, GetColor';
			Instructions := '[C]hange Color';
			_TextColor := ColourWhite;
			_x := 255;
			_y := 255;
			_z := 255;
			draw := CreateBitmap(300, 32);
			ToRun := @TestFont;
		end;
	end;
	
	procedure AddFontSuite(var suites: TestSuites);
	begin
		suites[High(suites)] := GetFontTests();
	end;

end.