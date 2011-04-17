unit MappyTests;

interface
	uses TestFramework;

	procedure AddMappySuite(var suites: TestSuites); 

implementation
	uses GameResources, sgInput, sgGraphics, sgTypes,sgGeometry,sgText,sgPhysics, sgMappyloader, SysUtils;
	
	var
		_Map: Map;
		_Ball: Sprite;
		_TileX, _TileY: Integer;
	
	procedure TestMappy(const drawIn: Rectangle);
	begin
		DrawMap(_Map);

		_Ball.Movement := VectorTo(0, 0);

		//Move Rectangle
		if KeyDown(VK_UP) then _Ball.Movement := VectorTo(-0, -1);
		if KeyDown(VK_DOWN) then _Ball.Movement := VectorTo(0, 1);
		if KeyDown(VK_LEFT) then _Ball.Movement := VectorTo(-1, 0);
		if KeyDown(VK_RIGHT) then _Ball.Movement := VectorTo(1, 0);

		MoveSprite(_Ball);
		DrawSprite(_Ball);

		if SpriteHasCollidedWithMapTile(_Map, _Ball, _TileX, _TileY) then
		begin
		    DrawRectangle(ColorRed, RectangleFrom(_Ball));
		    DrawRectangle(ColorRed, RectangleFrom(150, 150, 50, 50));

		    case WillCollideOnSide(_Map, _Ball) of
		        Left: 
		            DrawLine(ColorYellow, LineFrom(_Ball.X - 10, _Ball.Y - 10, _Ball.X - 10, _Ball.Y + _Ball.Height + 10));
		        Right:
		            DrawLine(ColorYellow, LineFrom(_Ball.X + 10 + _Ball.Width, _Ball.Y - 10, _Ball.X + 10 + _Ball.Width, _Ball.Y + _Ball.Height + 10));
		        Top:
		            DrawLine(ColorYellow, LineFrom(_Ball.X - 10, _Ball.Y - 10, _Ball.X + _Ball.Width + 10, _Ball.Y - 10));
		        Bottom:
		            DrawLine(ColorYellow, LineFrom(_Ball.X - 10, _Ball.Y + _Ball.Height + 10, _Ball.X + _Ball.Width + 10, _Ball.Y + _Ball.Height + 10));
		    end;

		    if KeyTyped(VK_M) then MoveSpriteOutOfTile(_Map, _Ball, _TileX, _TileY);
		end
		else
		begin
		    DrawRectangle(ColorWhite, RectangleFrom(_Ball));
		    DrawRectangle(ColorWhite, RectangleFrom(150, 150, 50, 50));
		end;
	end;

	function GetMappyTests(): TestSuite;
	var
		i: Integer;
	begin
		result.Title := 'Mappy Test';
		SetLength(result.Tests, 1);
		
		for i := 0 to High(result.Tests) do
		begin
			InitTest(result.Tests[i]);
		end;
		
		with result.Tests[0] do
		begin
			MethodBeingTested := 'Mappy Collisions';
			Instructions := '[Arrow Keys] Move Ball' + LineEnding +
	        				'[M]ove Ball out of Map';
			_Map := GameMap('test3');
			ToRun := @TestMappy;
			_Ball := CreateSprite(BitmapNamed('SmallBall'));
			_TileX := 0; _TileY := 0;
		end;
	end;
	
	procedure AddMappySuite(var suites: TestSuites);
	begin
		suites[High(suites)] := GetMappyTests();
	end;

end.