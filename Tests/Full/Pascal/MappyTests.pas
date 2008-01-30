unit MappyTests;

interface
	uses TestFramework;

	procedure AddMappySuite(var suites: TestSuites); 

implementation
	uses GameResources, SGSDK_Core, SGSDK_Input, SGSDK_Graphics, SGSDK_KeyCodes, SGSDK_Shapes, SGSDK_Font, SGSDK_Physics, SGSDK_Mappyloader, SysUtils;
	
	var
		draw: Bitmap;
		_Map: Map;
		_Ball: Sprite;
		_BoundingRect: Rectangle;
		_TileX, _TileY: Integer;
	
	procedure TestMappy(const drawIn: Rectangle);
	begin
		DrawMap(_Map);

		_Ball.Movement := CreateVector(0, 0);

		//Move Rectangle
		if IsKeyPressed(VK_UP) then _Ball.Movement := CreateVector(-0, -1);
		if IsKeyPressed(VK_DOWN) then _Ball.Movement := CreateVector(0, 1);
		if IsKeyPressed(VK_LEFT) then _Ball.Movement := CreateVector(-1, 0);
		if IsKeyPressed(VK_RIGHT) then _Ball.Movement := CreateVector(1, 0);

		MoveSprite(_Ball);
		DrawSprite(_Ball);

		_BoundingRect.X := Round(_Ball.X);
		_BoundingRect.Y := Round(_Ball.Y);

		if SpriteHasCollidedWithMapTile(_Map, _Ball, _TileX, _TileY) then
		begin
		    DrawRectangle(ColourRed, CreateRectangle(_Ball));
		    DrawRectangle(ColourRed, CreateRectangle(150, 150, 50, 50));

		    case WillCollideOnSide(_Map, _Ball) of
		        Left: 
		            DrawLine(ColourYellow, CreateLine(_Ball.X - 10, _Ball.Y - 10, _Ball.X - 10, _Ball.Y + _Ball.Height + 10));
		        Right:
		            DrawLine(ColourYellow, CreateLine(_Ball.X + 10 + _Ball.Width, _Ball.Y - 10, _Ball.X + 10 + _Ball.Width, _Ball.Y + _Ball.Height + 10));
		        Top:
		            DrawLine(ColourYellow, CreateLine(_Ball.X - 10, _Ball.Y - 10, _Ball.X + _Ball.Width + 10, _Ball.Y - 10));
		        Bottom:
		            DrawLine(ColourYellow, CreateLine(_Ball.X - 10, _Ball.Y + _Ball.Height + 10, _Ball.X + _Ball.Width + 10, _Ball.Y + _Ball.Height + 10));
		    end;

		    if WasKeyTyped(VK_M) then MoveSpriteOutOfTile(_Map, _Ball, _TileX, _TileY);
		end
		else
		begin
		    DrawRectangle(ColourWhite, CreateRectangle(_Ball));
		    DrawRectangle(ColourWhite, CreateRectangle(150, 150, 50, 50));
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
			Instructions := '[Arrow Keys] Move Ball' + EOL +
	        				'[M]ove Ball out of Map';
			draw := CreateBitmap(300, 32);
			_Map := GameMap('test3');
			ToRun := @TestMappy;
			_Ball := CreateSprite(GameImage('SmallBall'));
			_BoundingRect := CreateRectangle(0, 0, 30, 30);
			_TileX := 0; _TileY := 0;
		end;
	end;
	
	procedure AddMappySuite(var suites: TestSuites);
	begin
		suites[High(suites)] := GetMappyTests();
	end;

end.