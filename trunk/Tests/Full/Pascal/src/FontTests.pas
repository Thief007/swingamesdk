unit FontTests;

interface
	uses TestFramework;

	procedure AddFontSuite(var suites: TestSuites); 

implementation
	uses GameResources, sgInput, sgGraphics, sgTypes,sgGeometry,sgText,sgPhysics, SysUtils, sgImages;
	
	var
		_TextColor: Color;
		_x, _y, _z: Integer;
		draw: Bitmap;
	
	procedure TestFont(const drawIn: Rectangle);
	begin
		if KeyDown(VK_C)then
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

		 _TextColor := RGBColor(_x, _y*_x, _z*_x*_y);
		end;

		//Draw Text
		DrawText('1 This is some Text', _TextColor, FontNamed('Courier'), 10, 10);
		DrawText('2 This is some Text', _TextColor, FontNamed('Courier'), PointAt(10, 30));

		//Draw Text On Bitmap
		ClearSurface(draw, ColorBlack);
		DrawText(draw, '3 This is some Text', _TextColor, FontNamed('Courier'), PointAt(0, 0));
		DrawBitmap(draw, 10, 50);
		ClearSurface(draw, RGBColor(100, 100, 100));
		DrawText(draw, '4 This is some Text', _TextColor, FontNamed('Courier'), 0, 0);
		DrawBitmap(draw, 10, 70);

		//Draw Text On Screen
		DrawTextOnScreen('5 This is some Text', _TextColor, FontNamed('Courier'), 32, 220);
		DrawTextOnScreen('6 This is some Text', _TextColor, FontNamed('Courier'), PointAt(32, 240));

		//Draw Text Lines
		DrawTextLines('7 This is some Text Lines' + LineEnding + '** This is some Text Lines **', _TextColor, ColorTransparent, FontNamed('Courier'), AlignCenter, 10, 130, 200, 10);
		DrawTextLines('8 This is some Text Lines' + LineEnding + '** This is some Text Lines **', _TextColor, ColorGreen, FontNamed('Courier'), AlignCenter, RectangleFrom(10, 170, 200, 100));

		//Draw Text Lines on Bitmap
		ClearSurface(draw, ColorTransparent);
		DrawTextLines(draw, '9 This is some Text Lines' + LineEnding + '** This is some Text Lines **', _TextColor, RGBColor(100, 100, 100), FontNamed('Courier'), AlignCenter, RectangleFrom(0, 0, 200, 100));
		DrawBitmap(draw, 10, 210);
		ClearSurface(draw, ColorTransparent);
		DrawTextLines(draw, 'A This is some Text Lines' + LineEnding + '** This is some Text Lines **', _TextColor, ColorBlue, FontNamed('Courier'), AlignCenter, 0, 0, 200, 100);
		DrawBitmap(draw, 10, 250);
		//Draw Text Lines on Screen
		DrawTextLinesOnScreen('B This is some Text Lines' + LineEnding + '** This is some Text Lines **', _TextColor, ColorTransparent, FontNamed('Courier'), AlignCenter, 32, 440, 200, 100);
		DrawTextLinesOnScreen('C This is some Text Lines' + LineEnding + '** This is some Text Lines **', _TextColor, ColorYellow, FontNamed('Courier'), AlignCenter, RectangleFrom(32, 480, 200, 100));
	
		DrawText('D This is some simple text', _TextColor, 32, 380);
		ClearSurface(draw, ColorBlack);
		DrawText(draw, 'E This is some simple text', _TextColor, 0, 0);
		DrawBitmap(draw, 200, 400);
		DrawTextOnScreen('F This is some simple text', _TextColor, 32, 500);

    // DrawUnicode(' G =  快来和我一起享受TOM免费邮箱吧！ 看看除了1.5G，还有什么', _TextColor, FontNamed('Courier'), 0, 200);
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
			MethodBeingTested := 'DrawText, DrawTextOnScreen, DrawTextLines, DrawTextLinesOnScreen, RGBColor';
			Instructions := '[C]hange Color';
			_TextColor := ColorWhite;
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