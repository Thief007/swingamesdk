unit SGSDK_MappyLoader;

interface
	uses	SGSDK_Graphics, SGSDK_Camera, SGSDK_Physics,
			SDL, SGSDK_Core, Classes, SysUtils, SDL_image,
			SDL_Mixer, SDL_TTF, SDLEventProcessing;
			
	type
		CollisionSide = (
			Top,
			Bottom,
			Left,
			Right,
			TopLeft,
			TopRight,
			BottomLeft,
			BottomRight,
			LeftRight,
			TopBottom,
			None
		);
		
		Event = (
			Event1 = 0, Event2 = 1, Event3 = 2, Event4 = 3, Event5 = 4, Event6 = 5, Event7 = 6, Event8 = 7, Event9 = 8,
			Event10 = 9, Event11 = 10, Event12 = 11, Event13 = 12, Event14 = 13, Event15 = 14, Event16 = 15, 
			Event17 = 16, Event18 = 17, Event19 = 18, Event20 = 19, Event21 = 20, Event22 = 21, Event23 = 22, 
			Event24 = 23
		);
		
		MapData = record
            MapWidth: Integer;
            MapHeight: Integer;
            BlockWidth: Integer;
            BlockHeight: Integer;
            NumberOfBlocks: Integer;
            NumberOfLayers: Integer;
            NumberOfAnimations: Integer;
            CollisionLayer: Integer;
            EventLayer: Integer;
        end;
		
        AnimationData = record
            AnimationNumber: Integer;
            Delay: Integer;
            NumberOfFrames: Integer;
            Frame: Array of Integer;
            CurrentFrame : Integer;
        end;
        
        LayerData = record
            Animation: Array of Array of Integer;
            Value: Array of Array of Integer;
        end;
        
        CollisionData = record
            Collidable: Array of Array of Boolean;
        end;
        
        EventData = record
            Event: Array of Array of Event;
        end;
        
        Map = record
        	MapInfo : MapData;
        	AnimationInfo : Array of AnimationData;
        	Layerinfo : Array of LayerData;
        	CollisionInfo : CollisionData;
        	EventInfo : EventData;
        	Tiles : Sprite;     	
        	Animate : Boolean;
        	Frame : Integer;
        end;
        
		function LoadMap(fileName : String): Map;
		procedure DrawMap(var m : Map);
		function CollisionWithMap(m: Map; var spr: Sprite; vec: Vector): CollisionSide; overload;
		function CollisionWithMap(m: Map; var spr: Sprite): CollisionSide; overload;
		
		function EventCount(m : Map; event : Event): Integer;
		function EventPositionX(m : Map; event : Event; eventnumber : Integer): Integer;
		function EventPositionY(m : Map; event : Event; eventnumber : Integer): Integer;
		
		procedure FreeMap(m: Map);
		
implementation
	function ReadInt(var stream : text): UInt16;
	var
		c : char;
		c2 : char;
		i : Integer;
		i2 : Integer;
	begin
		Read(stream ,c);
		Read(stream ,c2);
		
		i := Integer(c);
		i2 := Integer(c2) * 256;
		
		result := i + i2;	
	end;
	
	procedure LoadMapInformation(var m : Map; var stream : text);
	begin
		m.MapInfo.MapWidth := ReadInt(stream);
        m.MapInfo.MapHeight := ReadInt(stream);
        m.MapInfo.BlockWidth := ReadInt(stream);
        m.MapInfo.BlockHeight := ReadInt(stream);
        m.MapInfo.NumberOfBlocks := ReadInt(stream);
        m.MapInfo.NumberOfAnimations := ReadInt(stream);
        m.MapInfo.NumberOfLayers := ReadInt(stream);
        m.MapInfo.CollisionLayer := ReadInt(stream);
   		m.MapInfo.EventLayer := ReadInt(stream);
   		
   		{
   		//Debug
   		WriteLn('MapInformation');
   		WriteLn('');
   		WriteLn(m.MapInfo.MapWidth);
   		WriteLn(m.MapInfo.MapHeight);
   		WriteLn(m.MapInfo.BlockWidth);
   		WriteLn(m.MapInfo.BlockHeight);
   		WriteLn(m.MapInfo.NumberOfBlocks);
   		WriteLn(m.MapInfo.NumberOfAnimations);
   		WriteLn(m.MapInfo.NumberOfLayers);
   		WriteLn(m.MapInfo.CollisionLayer);
   		WriteLn(m.MapInfo.EventLayer);
   		WriteLn('');
   		ReadLn();
   		}
	end;
	
	procedure LoadAnimationInformation(var m : Map; var stream : text);
	var
		i, j : Integer;
	begin
	
		if m.MapInfo.NumberOfAnimations > 0 then
		begin
		
			SetLength(m.AnimationInfo, m.MapInfo.NumberOfAnimations);
			
			for i := 0 to m.MapInfo.NumberOfAnimations - 1 do
			begin
				
				m.AnimationInfo[i].AnimationNumber := i + 1;
				m.AnimationInfo[i].Delay := ReadInt(stream);
				m.AnimationInfo[i].NumberOfFrames := ReadInt(stream);
				
				SetLength(m.AnimationInfo[i].Frame, m.AnimationInfo[i].NumberOfFrames);
				
				for j := 0 to m.AnimationInfo[i].NumberOfFrames - 1 do
				begin
					m.AnimationInfo[i].Frame[j] := ReadInt(stream);
				end;
				
				m.AnimationInfo[i].CurrentFrame := 0;
				
			end;
			
			{
			//Debug
			WriteLn('Animation Information');
			WriteLn('');
			for i := 0 to m.MapInfo.NumberOfAnimations - 1 do
			begin
				WriteLn(m.AnimationInfo[i].AnimationNumber);
				WriteLn(m.AnimationInfo[i].Delay);
				WriteLn(m.AnimationInfo[i].NumberOfFrames);
				
				for j := 0 to m.AnimationInfo[i].NumberOfFrames - 1 do
				begin
					WriteLn(m.AnimationInfo[i].Frame[j]);
				end;
			end;
			WriteLn('');
			ReadLn();		
			}
		end;
	end;
	
	procedure LoadLayerData(var m : Map; var stream : text);
	var
		l, y, x : Integer;
	begin
	
		SetLength(m.LayerInfo, m.MapInfo.NumberOfLayers - m.MapInfo.Collisionlayer - m.MapInfo.EventLayer);
		
		for y := 0 to Length(m.LayerInfo) - 1 do
		begin
			SetLength(m.LayerInfo[y].Animation, m.MapInfo.MapHeight);
			SetLength(m.LayerInfo[y].Value, m.MapInfo.MapHeight);
			
			for x := 0 to m.MapInfo.MapHeight - 1 do
			begin
				SetLength(m.LayerInfo[y].Animation[x], m.MapInfo.MapWidth);
				SetLength(m.LayerInfo[y].Value[x], m.MapInfo.MapWidth);
			end;
		end;
		
		for l := 0 to m.MapInfo.NumberOfLayers - m.MapInfo.Collisionlayer - m.MapInfo.Eventlayer - 1 do
		begin
			for y := 0 to m.MapInfo.MapHeight - 1 do
			begin
				for x := 0 to m.MapInfo.MapWidth - 1 do
				begin
					m.LayerInfo[l].Animation[y][x] := ReadInt(stream);
					m.LayerInfo[l].Value[y][x] := ReadInt(stream);
				end;
			end;
		end;
		
		{
		//Debug
		WriteLn('Layer Information');
		WriteLn(Length(m.Layerinfo));
		WriteLn('');
		
		for l := 0 to Length(m.LayerInfo) - 1 do
		begin
			for y := 0 to m.MapInfo.MapHeight - 1 do
			begin
				for x := 0 to m.MapInfo.MapWidth - 1 do
				begin
					Write(m.LayerInfo[l].Animation[y][x]);
					Write(',');
					Write(m.LayerInfo[l].Value[y][x]);
					Write(' ');
				end;
			end;
			WriteLn('');
			ReadLn();
		end;
		}
		
		
	end;
	
	procedure LoadCollisionData(var m : Map; var stream : text);
	var
		y, x: Integer;
	begin
		if m.MapInfo.CollisionLayer = 1 then
		begin
			SetLength(m.CollisionInfo.Collidable, m.MapInfo.MapHeight);
			
			for y := 0 to m.MapInfo.MapHeight - 1 do
			begin
				SetLength(m.CollisionInfo.Collidable[y], m.MapInfo.MapWidth);
			end;
			
			for y := 0 to m.MapInfo.MapHeight - 1 do
			begin
				for x := 0 to m.MapInfo.MapWidth - 1 do
				begin
					if ReadInt(stream) <> 0 then
						m.CollisionInfo.Collidable[y][x] := true
					else
						m.CollisionInfo.Collidable[y][x] := false
				end;
			end;
			
			
			//Debug
			{
			for y := 0 to m.MapInfo.MapHeight - 1 do
			begin
				for x := 0 to m.MapInfo.MapWidth - 1 do
				begin
					if m.CollisionInfo.Collidable[y][x] = true then
						Write('1')
					else
						Write('0')
				end;
				WriteLn('');
			end;
			ReadLn();
			}
		end;
	end;
	
	procedure LoadEventData(var m : Map; var stream : text);
	var
		y, x, smallest, temp: Integer;
	begin
		SetLength(m.EventInfo.Event, m.MapInfo.MapHeight);
		
		for y := 0 to m.MapInfo.MapHeight - 1 do
		begin
			SetLength(m.EventInfo.Event[y], m.MapInfo.MapWidth);
		end;
		
		smallest := m.MapInfo.NumberOfBlocks - 23;
		temp := 0;
		
		for y := 0 to m.MapInfo.MapHeight - 1 do
		begin
			for x := 0 to m.MapInfo.MapWidth - 1 do
			begin
				temp := ReadInt(stream);
				
				if (temp - smallest) >= 0 then
					m.EventInfo.Event[y][x] := Event(temp - smallest)
				else
					m.EventInfo.Event[y][x] := Event(25);
			end;
		end;
		
		
		//Debug
		{
		for y := 0 to m.MapInfo.MapHeight - 1 do
		begin
			for x := 0 to m.MapInfo.MapWidth - 1 do
			begin
				Write(' ');
				Write(Integer(m.EventInfo.Event[y][x]));
			end;
			WriteLn('');
		end;
		ReadLn();
		}
	end;
	
	procedure LoadBlockSprites(var m : Map; fileName : String);
	var
		fpc : Array of Integer;
	begin
		SetLength(fpc, m.MapInfo.NumberOfBlocks);
		m.Tiles := CreateSprite(LoadBitmap(GetPathToResource(fileName + '.png', MapResource)), true, fpc, m.MapInfo.BlockWidth, m.MapInfo.BlockHeight);
		m.Tiles.currentFrame := 0;
	end;
	
	procedure DrawMap(var m : Map);
	var
		l, y ,x : Integer;
		XStart, YStart, XEnd, YEnd : Integer;
		f : Integer;
	begin
		//Screen Drawing Starting Point
		XStart := round((GameX(0) / m.MapInfo.BlockWidth) - (m.MapInfo.BlockWidth * 3));
		YStart := round((GameY(0) / m.MapInfo.BlockHeight) - (m.MapInfo.BlockHeight * 3));
		
		//Screen Drawing Ending point
		XEnd := round(XStart + (SGSDK_Core.ScreenWidth() / m.MapInfo.BlockWidth) + (m.MapInfo.BlockWidth * 3));
		YEnd := round(YStart + (SGSDK_Core.ScreenHeight() / m.MapInfo.BlockHeight) + (m.MapInfo.BlockHeight * 3));
		
		for l := 0 to m.MapInfo.NumberOfLayers - m.MapInfo.CollisionLayer - m.MapInfo.EventLayer - 1 do
		begin
			for y := YStart  to YEnd do
			begin
				if (y < m.MapInfo.MapHeight) and (y > -1) then
				begin
					for x := XStart  to XEnd do
					begin
						if (x < m.MapInfo.MapWidth) and (x > -1) then
						begin
					
							if (m.LayerInfo[l].Animation[y][x] = 0) and (m.LayerInfo[l].Value[y][x] > 0) then
							begin
								m.Tiles.currentFrame := m.LayerInfo[l].Value[y][x] - 1;
								
								m.Tiles.xPos := x * m.MapInfo.BlockWidth;
								m.Tiles.yPos := y * m.MapInfo.BlockHeight;
								//DrawSprite(m.Tiles, CameraX, CameraY, SGSDK_Core.ScreenWidth(), SGSDK_Core.ScreenHeight());
								DrawSprite(m.Tiles);
							end
							else if (m.LayerInfo[l].Animation[y][x] = 1) then
							begin
								
								m.Tiles.xPos := x * m.MapInfo.BlockWidth;
                            	m.Tiles.yPos := y * m.MapInfo.BlockHeight;
								
                            	
                            	f := round(m.Frame/10) mod (m.AnimationInfo[m.LayerInfo[l].Value[y][x]].NumberOfFrames);
                            	m.Tiles.currentFrame := m.AnimationInfo[m.LayerInfo[l].Value[y][x]].Frame[f] - 1;		
								
								DrawSprite(m.Tiles);
							
							end;
							
						end;
					end;
				end;
			end;
		end;
		
		m.Frame := m.Frame + 1;
		
		if m.Frame = 100 then
		begin
			m.Frame := 0
		end;
	end;
	
	function LoadMap(fileName: String): Map;
	var
		filestream : text;
		m : Map;
	begin
		//Get File
		assign(filestream, GetPathToResource(fileName + '.sga', MapResource));
		reset(filestream);
		
		//Load Map Content
		LoadMapInformation(m, filestream);
		LoadAnimationInformation(m, filestream);
		LoadLayerData(m, filestream);
		LoadCollisionData(m, filestream);
		LoadEventData(m, filestream);	
		//Closes File
		close(filestream);	
		LoadBlockSprites(m, fileName);
		m.Frame := 0;
		result := m;
	end;
	
	//Gets the number of Event of the specified type
	function EventCount(m : Map; event : Event): Integer;
	var
		y, x, count : Integer;
	begin
		count := 0;
		
		for y := 0 to m.MapInfo.MapWidth - 1 do
		begin
			for x := 0 to m.MapInfo.MapHeight - 1 do
			begin
				if event = m.EventInfo.Event[y][x] then
					count := count + 1;
			end;
		end;
		result := count;
	end;
	
	// Gets the Top Left X Coordinate of the Event
	function EventPositionX(m : Map; event : Event; eventnumber : Integer): Integer;
	var
		y, x, count : Integer;
	begin
		count := 0;
		
		for y := 0 to m.MapInfo.MapWidth - 1 do
		begin
			for x := 0 to m.MapInfo.MapHeight - 1 do
			begin
				if event = m.EventInfo.Event[y][x] then
				begin
					if eventnumber = count then
					begin
						result := x * m.MapInfo.BlockWidth;
						exit;
					end;
					count := count + 1;
				end;
			end;
		end;
		result := 0;
	end;
	
	// Gets the Top Left Y Coordinate of the Event
	function EventPositionY(m : Map; event : Event; eventnumber : Integer): Integer;
	var
		y, x, count : Integer;
	begin
		count := 0;
		
		for y := 0 to m.MapInfo.MapWidth - 1 do
		begin
			for x := 0 to m.MapInfo.MapHeight - 1 do
			begin
				if event = m.EventInfo.Event[y][x] then
				begin
					if eventnumber = count then
					begin
						result := y * m.MapInfo.BlockHeight;
						exit;
					end;
					count := count + 1;
				end;
			end;
		end;
		result := 0;
	end;
	
	function BruteForceDetection(m : Map; var spr : Sprite): Boolean;
	const
		SEARCH_RANGE = 0;
	var
		XStart, XEnd, YStart, YEnd : Integer;
		y, x: Integer;
	begin
		XStart := round((spr.xPos / m.MapInfo.BlockWidth) - ((spr.width / m.MapInfo.BlockWidth) + SEARCH_RANGE));
		XEnd := round((spr.xPos / m.MapInfo.BlockWidth) + ((spr.width / m.MapInfo.BlockWidth) + SEARCH_RANGE));
		YStart := round((spr.yPos / m.MapInfo.BlockHeight) - ((spr.height / m.MapInfo.BlockHeight) + SEARCH_RANGE));
		YEnd := round((spr.yPos / m.MapInfo.BlockHeight) + ((spr.height / m.MapInfo.BlockHeight) + SEARCH_RANGE));

		for y := YStart to YEnd do
		begin
			if (y < m.MapInfo.MapHeight - 1) and (y > -1) then
			begin
				for x := XStart to XEnd do
				begin
					if (x < m.MapInfo.MapWidth - 1) and (x > -1) then
					begin
						if m.CollisionInfo.Collidable[y][x] = true then
						begin
							if HasSpriteCollidedWithRect(spr, 
														 x * m.MapInfo.BlockWidth, 
														 y * m.MapInfo.BlockHeight, 
														 m.MapInfo.BlockWidth, 
														 m.MapInfo.BlockHeight) then
							begin
								result := true;
								exit;
							end;
						end;
					end;
				end;
			end;
		end;
		result := false;
	end;
	
	function BruteForceDetectionComponent(m : Map; var spr: Sprite; xOffSet, yOffSet: Integer): Boolean;
	begin
		spr.xPos := spr.xPos + xOffset;
		spr.yPos := spr.yPos + yOffset;

		if BruteForceDetection(m, spr) then
		begin
			result := true;
		end
		else
			result := false;

		spr.xPos := spr.xPos - xOffset;
		spr.yPos := spr.yPos - yOffset;
	end;
	
	procedure PushSpriteOut(m : Map; var spr : Sprite; vec : Vector);
	const
		SEARCH_RANGE = 0;
	var
		XStart, XEnd, YStart, YEnd : Integer;
		y, x: Integer;
		tempVector: Vector;
	begin
		XStart := round((spr.xPos / m.MapInfo.BlockWidth) - ((spr.width / m.MapInfo.BlockWidth) + SEARCH_RANGE));
		XEnd := round((spr.xPos / m.MapInfo.BlockWidth) + ((spr.width / m.MapInfo.BlockWidth) + SEARCH_RANGE));
		YStart := round((spr.yPos / m.MapInfo.BlockHeight) - ((spr.height / m.MapInfo.BlockHeight) + SEARCH_RANGE));
		YEnd := round((spr.yPos / m.MapInfo.BlockHeight) + ((spr.height / m.MapInfo.BlockHeight) + SEARCH_RANGE));

		for y := YStart to YEnd do
		begin
			if (y < m.MapInfo.MapHeight - 1) and (y > 0) then
			begin
				for x := XStart to XEnd do
				begin
					if (x < m.MapInfo.MapWidth - 1) and (x > 0) then
					begin
						if m.CollisionInfo.Collidable[y][x] = true then
						begin
							if HasSpriteCollidedWithRect(spr, 
														 x * m.MapInfo.BlockWidth, 
														 y * m.MapInfo.BlockHeight, 
														 m.MapInfo.BlockWidth, 
														 m.MapInfo.BlockHeight) then
							begin
								MoveSprite(spr, InvertVector(vec));
								tempVector := vec;
								repeat
									MoveSprite(spr, tempVector);
									if HasSpriteCollidedWithRect(spr, 
																 x * m.MapInfo.BlockWidth, 
																 y * m.MapInfo.BlockHeight, 
																 m.MapInfo.BlockWidth, 
																 m.MapInfo.BlockHeight) then
									begin
										MoveSprite(spr, InvertVector(tempVector));
										tempVector := MultiplyVector(tempVector, 0.5);
									end;
								until Magnitude(tempVector) < 0.1;
							end;
						end;
					end;
				end;
			end;
		end;
	end;
	
	function CollisionWithMap(m: Map; var spr: Sprite; vec: vector): CollisionSide; overload;
	type
		Collisions = record
			Top, Bottom, Left, Right: Boolean;
		end;
	var
		xOffset, yOffset: Integer;
		col: Collisions;
	begin
		xOffset := m.MapInfo.BlockWidth div 2;
		yOffset := m.MapInfo.BlockHeight div 2;

		result := None;
		
		if BruteForceDetection(m, spr) then
		begin
			PushSpriteOut(m, spr, vec);
		
			if vec.x > 0 then begin
				//Right
				if BruteForceDetectionComponent(m, spr, xOffset, 0) then
					col.Right := true
				else col.Right := false;
			end else col.Right := false;

			if vec.x < 0 then begin
				//Left
				if BruteForceDetectionComponent(m, spr, xOffset * -1, 0) then
					col.Left := true
				else col.Left := false;
			end else col.Left := false;

			if vec.y < 0 then begin
				//Top
				if BruteForceDetectionComponent(m, spr, 0, yOffset * -1) then
					col.Top := true
				else col.Top := false;
			end else col.Top := false;

			if vec.y > 0 then begin
				//Bottom
				if BruteForceDetectionComponent(m, spr, 0, yOffset) then
					col.Bottom := true
				else col.Bottom := false;
			end else col.Bottom := false;

			//BottomRight
			if (col.Right = col.Bottom) and (vec.x > 0) and (vec.y > 0) then
			begin
				result := BottomRight;
				exit;
			end;

			//BottomLeft
			if (col.Left = col.Bottom) and (vec.x < 0) and (vec.y > 0) then
			begin
				result := BottomLeft;
				exit;
			end;

			//TopRight
			if (col.Right = col.Top) and (vec.x > 0) and (vec.y < 0) then
			begin
				result := TopRight;
				exit;
			end;

			//TopLeft
			if (col.Left = col.Top) and (vec.x < 0) and (vec.y < 0) then
			begin
				result := TopLeft;
				exit;
			end;

			//Left
			if col.Left then
			begin
				result := Left;
				exit;
			end;

			//Right
			if col.Right then
			begin
				result := Right;
				exit;
			end;

			//Top
			if col.Top then
			begin
				result := Top;
				exit;
			end;

			//Bottom
			if col.Bottom then
			begin
				result := Bottom;
				exit;
			end;
		end;
	end;
	
	function CollisionWithMap(m: Map; var spr: Sprite): CollisionSide; overload;
	begin
		result := CollisionWithMap(m, spr, spr.movement);
	end;
	
	procedure FreeMap(m: Map);
	begin
		FreeBitmap(m.Tiles.bitmaps[0]);
	end;
end.