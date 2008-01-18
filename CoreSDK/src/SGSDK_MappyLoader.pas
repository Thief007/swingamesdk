///-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
// 					SGSDK_MappyLoader.pas
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// The MappyLoader unit is responsible for loading and
// processing a Mappy data file exported using the Lua
// script specifically written for SwinGame.
//
// Change History:
//
// Version 1.1:
// - 2008-01-17: Aki + Andrew: Refactor
//  
// Version 1.0:
// - Various

unit SGSDK_MappyLoader;

interface
	uses	SGSDK_Core, SGSDK_Physics;
			
	type
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
        
		EventDetails = record
			x: Integer;
			y: Integer;
		end;

        MapRecord = record
        	MapInfo : MapData;
        	AnimationInfo : Array of AnimationData;
        	Layerinfo : Array of LayerData;
        	CollisionInfo : CollisionData;
        	EventInfo : Array [Event] of Array of EventDetails;
        	Tiles : Sprite;     	
        	Animate : Boolean;
        	Frame : Integer;
        end;
		Map = ^MapRecord;
        
		function LoadMap(mapName : String): Map;
		function LoadMapFiles(mapFile, imgFile: String): Map;
		procedure DrawMap(m : Map);
		function CollisionWithMap(m: Map; spr: Sprite; vec: Vector): CollisionSide; overload;
		function CollisionWithMap(m: Map; spr: Sprite): CollisionSide; overload;
		
		function EventCount(m : Map; eventType : Event): Integer;
		function EventPositionX(m : Map; eventType : Event; eventnumber : Integer): Integer;
		function EventPositionY(m : Map; eventType : Event; eventnumber : Integer): Integer;
		
		procedure FreeMap(var m: Map);
		
implementation
	uses SysUtils, Classes, SGSDK_Graphics, SGSDK_Camera;

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
	
	procedure LoadMapInformation(m : Map; var stream : text);
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
	
	procedure LoadAnimationInformation(m : Map; var stream : text);
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
	
	procedure LoadLayerData(m : Map; var stream : text);
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
	
	procedure LoadCollisionData(m : Map; var stream : text);
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
	
	procedure LoadEventData(m : Map; var stream : text);
	var
		py, px, smallestEventIdx, temp: Integer;
		evt: Event;
	begin
		//SetLength(m.EventInfo, High(Events));
		
		//SetLength(m.EventInfo.Event, m.MapInfo.MapHeight);
		
		{for y := 0 to m.MapInfo.MapHeight - 1 do
		begin
			SetLength(m.EventInfo.Event[y], m.MapInfo.MapWidth);
		end;}
		
		//The smallest "non-graphics" tile, i.e. the events
		smallestEventIdx := m.MapInfo.NumberOfBlocks - 23;
		
		for py := 0 to m.MapInfo.MapHeight - 1 do
		begin
			for px := 0 to m.MapInfo.MapWidth - 1 do
			begin
				temp := ReadInt(stream);
				evt := Event(temp - smallestEventIdx);
				
				if (evt >= Event1) and (evt <= Event24) then 
				begin
					SetLength(m.EventInfo[evt], Length(m.EventInfo[evt]) + 1);
					
					with m.EventInfo[evt][High(m.EventInfo[evt])] do
					begin
						x := px;
						y := py;
					end;
				end
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
	
	procedure LoadBlockSprites(m : Map; fileName : String);
	var
		fpc : Array of Integer;
	begin
		SetLength(fpc, m.MapInfo.NumberOfBlocks);
		m.Tiles := CreateSprite(LoadBitmap(fileName), true, fpc, m.MapInfo.BlockWidth, m.MapInfo.BlockHeight);
		m.Tiles.currentFrame := 0;
	end;
	
	procedure DrawMap(m : Map);
	var
		l, y ,x : Integer;
		XStart, YStart, XEnd, YEnd : Integer;
		f : Integer;
	begin
		if m = nil then raise Exception.Create('No Map supplied (nil)');
		
		//WriteLn('GX, GY: ', GameX(0), ',' ,GameY(0));
		//WriteLn('bw, bh: ', m.MapInfo.BlockWidth, ', ', m.MapInfo.BlockHeight);
		
		//Screen Drawing Starting Point
		XStart := round((GameX(0) / m.MapInfo.BlockWidth) - (m.MapInfo.BlockWidth * 1));
		YStart := round((GameY(0) / m.MapInfo.BlockHeight) - (m.MapInfo.BlockHeight * 1));
		
		//Screen Drawing Ending point
		XEnd := round(XStart + (SGSDK_Core.ScreenWidth() / m.MapInfo.BlockWidth) + (m.MapInfo.BlockWidth * 1));
		YEnd := round(YStart + (SGSDK_Core.ScreenHeight() / m.MapInfo.BlockHeight) + (m.MapInfo.BlockHeight * 1));

		
		//WriteLn('DrawMap ', XStart, ',', YStart, ' - ',  XEnd, ',', YEnd);
		
		if YStart < 0 then YStart := 0;
		if YStart >= m.MapInfo.MapHeight then exit;
		if YEnd < 0 then exit;
		if YEnd >= m.MapInfo.MapHeight then YEnd := m.MapInfo.MapHeight - 1;
				
		if XStart < 0 then XStart := 0;
		if XStart >= m.MapInfo.MapWidth then exit;
		if XEnd < 0 then exit;
		if XEnd >= m.MapInfo.MapWidth then XEnd := m.MapInfo.MapWidth - 1;
			
		for y := YStart  to YEnd do
		begin
			m.Tiles.yPos := y * m.MapInfo.BlockHeight;
			for x := XStart  to XEnd do
			begin
				m.Tiles.xPos := x * m.MapInfo.BlockWidth;
				for l := 0 to m.MapInfo.NumberOfLayers - m.MapInfo.CollisionLayer - m.MapInfo.EventLayer - 1 do
				begin
					if (m.LayerInfo[l].Animation[y][x] = 0) and (m.LayerInfo[l].Value[y][x] > 0) then
					begin
						m.Tiles.currentFrame := m.LayerInfo[l].Value[y][x] - 1;
						//DrawSprite(m.Tiles, CameraX, CameraY, SGSDK_Core.ScreenWidth(), SGSDK_Core.ScreenHeight());
						DrawSprite(m.Tiles);
					end
					else if (m.LayerInfo[l].Animation[y][x] = 1) then
					begin
                        f := round(m.Frame/10) mod (m.AnimationInfo[m.LayerInfo[l].Value[y][x]].NumberOfFrames);
                        m.Tiles.currentFrame := m.AnimationInfo[m.LayerInfo[l].Value[y][x]].Frame[f] - 1;		
						DrawSprite(m.Tiles);
					end;
				end;
			end;
		end;
		
		m.Frame := (m.Frame + 1) mod 1000;
	end;

	function LoadMap(mapName: String): Map;
	var
		mapFile, imgFile: String;
	begin
		mapFile := GetPathToResource(mapName + '.sga', MapResource);
		imgFile := GetPathToResource(mapName + '.png', MapResource);
      	
		result := LoadMapFiles(mapFile, imgFile);
	end;
	
	function LoadMapFiles(mapFile, imgFile: String): Map;
	var
		filestream : text;
		m : Map;
	begin
		if not FileExists(mapFile) then raise Exception.Create('Unable to locate map: ' + mapFile);
		if not FileExists(imgFile) then raise Exception.Create('Unable to locate images: ' + imgFile);
					
		//Get File
		assign(filestream, mapFile);
		reset(filestream);
		
		//Create Map
		New(m);
		
		//Load Map Content
		LoadMapInformation(m, filestream);
		LoadAnimationInformation(m, filestream);
		LoadLayerData(m, filestream);
		LoadCollisionData(m, filestream);
		LoadEventData(m, filestream);
			
		//Closes File
		close(filestream);	
		
		LoadBlockSprites(m, imgFile);
		m.Frame := 0;
		result := m;
	end;
	
	//Gets the number of Event of the specified type
	function EventCount(m : Map; eventType : Event): Integer;
	begin
		if m = nil then raise Exception.Create('No Map supplied (nil)');
		if (eventType < Event1) or (eventType > Event23) then raise Exception.Create('EventType is out of range');
		
		result := Length(m.EventInfo[eventType]);
		
		{count := 0;
		
		for y := 0 to m.MapInfo.MapWidth - 1 do
		begin
			for x := 0 to m.MapInfo.MapHeight - 1 do
			begin
				if event = m.EventInfo.Event[y][x] then
					count := count + 1;
			end;
		end;
		result := count;}
	end;
	
	// Gets the Top Left X Coordinate of the Event
	function EventPositionX(m : Map; eventType : Event; eventnumber : Integer): Integer;
	begin
		if (eventnumber < 0) or (eventnumber > EventCount(m, eventType)) then raise Exception.Create('Event number is out of range');

		result := m.EventInfo[eventType][eventnumber].x * m.MapInfo.BlockWidth;
		
		{count := 0;
		
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
		result := 0;}
	end;
	
	// Gets the Top Left Y Coordinate of the Event
	function EventPositionY(m : Map; eventType : Event; eventnumber : Integer): Integer;
	begin
		if (eventnumber < 0) or (eventnumber > EventCount(m, eventType)) then raise Exception.Create('Event number is out of range');
			
		result := m.EventInfo[eventType][eventnumber].y * m.MapInfo.BlockHeight;
		
		{count := 0;
		
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
		result := 0;}
	end;
	
	function BruteForceDetection(m: Map; spr: Sprite): Boolean;
	const
		SEARCH_RANGE = 0;
	var
		XStart, XEnd, YStart, YEnd : Integer;
		y, x, yCache: Integer;
	begin
		result := false;
		
		XStart := round((spr.xPos / m.MapInfo.BlockWidth) - ((spr.width / m.MapInfo.BlockWidth) - SEARCH_RANGE));
		XEnd := round((spr.xPos / m.MapInfo.BlockWidth) + ((spr.width / m.MapInfo.BlockWidth) + SEARCH_RANGE));
		YStart := round((spr.yPos / m.MapInfo.BlockHeight) - ((spr.height / m.MapInfo.BlockHeight) - SEARCH_RANGE));
		YEnd := round((spr.yPos / m.MapInfo.BlockHeight) + ((spr.height / m.MapInfo.BlockHeight) + SEARCH_RANGE));

		if YStart < 0 then YStart := 0;
		if YStart >= m.MapInfo.MapHeight then exit;
		if YEnd < 0 then exit;
		if YEnd >= m.MapInfo.MapHeight then YEnd := m.MapInfo.MapHeight - 1;
				
		if XStart < 0 then XStart := 0;
		if XStart >= m.MapInfo.MapWidth then exit;
		if XEnd < 0 then exit;
		if XEnd >= m.MapInfo.MapWidth then XEnd := m.MapInfo.MapWidth - 1;

		for y := YStart to YEnd do
		begin
			yCache := y * m.MapInfo.BlockHeight;
			
			for x := XStart to XEnd do
			begin
				if m.CollisionInfo.Collidable[y][x] = true then
				begin
					if HasSpriteCollidedWithRect(spr, 
							 x * m.MapInfo.BlockWidth, 
							 yCache, 
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
	

	
	procedure MoveOut(sprt: Sprite; movement: Vector; x, y, width, height: Integer);
	var
		side: CollisionSide;
		kickVector, unitMove: Vector;
		dotprd: Single;
	begin
{		side := GetCollisionFromVector(movement);
		
		case side of
			Top, Left, TopLeft:	kickVector := CreateVector(sprt.xPos - x, sprt.yPos - y);
			Bottom, BottomLeft:	kickVector := CreateVector(sprt.xPos - x, sprt.yPos - (y + height));
			TopRight:			kickVector := CreateVector(sprt.xPos - (x + width), sprt.yPos - y);
			Right, BottomRight:	kickVector := CreateVector(sprt.xPos - (x + width), sprt.yPos - (y + height));
		end;
		
		kickVector := InvertVector(kickVector);
		unitMove := GetUnitVector(movement);
		
		dotprd := DotProduct(kickVector, unitMove);

		if side = BottomRight then
		begin
			WriteLn('Movement: ', movement.x:4:2, ',', movement.y:4:2);
			WriteLn('Sprt X,Y: ', sprt.xPos:4:2, ',', sprt.yPos:4:2);
			WriteLn('Sprt W,H: ', sprt.width, ',', sprt.height);
			WriteLn('Block X,Y: ', x, ',', y);
			WriteLn('Block W,H: ', width, ',', height);
			WriteLn('Side: ', Integer(side));
			WriteLn('To Top of Sprt: ', kickVector.x:4:2, ',', kickVector.y:4:2);
			WriteLn('Dot Product: ', dotprd:4:2);
		end;

		kickVector := InvertVector(MultiplyVector(unitMove, dotprd));

		if side = BottomRight then WriteLn('Kick: ', kickVector.x:4:2, ',', kickVector.y:4:2);
		
		MoveSprite(sprt, kickVector);
}	end;
	
	procedure PushSpriteOut(m : Map; spr : Sprite; vec : Vector);
	const
		SEARCH_RANGE = 0;
	var
		XStart, XEnd, YStart, YEnd : Integer;
		y, x, yCache, xCache: Integer;
	begin
		XStart := round((spr.xPos / m.MapInfo.BlockWidth) - ((spr.width / m.MapInfo.BlockWidth) - SEARCH_RANGE));
		XEnd := round((spr.xPos / m.MapInfo.BlockWidth) + ((spr.width / m.MapInfo.BlockWidth) + SEARCH_RANGE));
		YStart := round((spr.yPos / m.MapInfo.BlockHeight) - ((spr.height / m.MapInfo.BlockHeight) - SEARCH_RANGE));
		YEnd := round((spr.yPos / m.MapInfo.BlockHeight) + ((spr.height / m.MapInfo.BlockHeight) + SEARCH_RANGE));

		if YStart < 0 then YStart := 0;
		if YStart >= m.MapInfo.MapHeight then exit;
		if YEnd < 0 then exit;
		if YEnd >= m.MapInfo.MapHeight then YEnd := m.MapInfo.MapHeight - 1;
				
		if XStart < 0 then XStart := 0;
		if XStart >= m.MapInfo.MapWidth then exit;
		if XEnd < 0 then exit;
		if XEnd >= m.MapInfo.MapWidth then XEnd := m.MapInfo.MapWidth - 1;

		for y := YStart to YEnd do
		begin
			yCache := y * m.MapInfo.BlockHeight;
			
			for x := XStart to XEnd do
			begin
				if m.CollisionInfo.Collidable[y][x] = true then
				begin
					xCache := x * m.MapInfo.BlockWidth;
					
					if HasSpriteCollidedWithRect(spr, 
							 xCache, 
							 yCache, 
							 m.MapInfo.BlockWidth, 
							 m.MapInfo.BlockHeight) then
					begin
						MoveOut(spr, vec, x * m.MapInfo.BlockWidth, y * m.MapInfo.BlockHeight, m.MapInfo.BlockWidth, m.MapInfo.BlockHeight);
						
{						if spr.usePixelCollision then
						begin
							MoveSprite(spr, InvertVector(vec));
							
							tempVector := vec;
							repeat
								MoveSprite(spr, tempVector);
								if HasSpriteCollidedWithRect(spr, 
									 xCache, 
									 yCache, 
									 m.MapInfo.BlockWidth, 
									 m.MapInfo.BlockHeight) then
								begin
									MoveSprite(spr, InvertVector(tempVector));
									tempVector := MultiplyVector(tempVector, 0.5);
								end;
							until Magnitude(tempVector) < 0.1;
						end;}
					end;
				end;
			end;
		end;
	end;
	
	function CollisionWithMap(m: Map; spr: Sprite; vec: vector): CollisionSide; overload;
	type
		Collisions = record
			Top, Bottom, Left, Right: Boolean;
		end;
	var
		xOffset, yOffset: Integer;
		col: Collisions;
	begin
		if m = nil then raise Exception.Create('No Map supplied (nil)');
		if spr = nil then raise Exception.Create('No Sprite suppled (nil)');
		
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
	
	function CollisionWithMap(m: Map; spr: Sprite): CollisionSide; overload;
	begin
		result := CollisionWithMap(m, spr, spr.movement);
	end;
	
	procedure FreeMap(var m: Map);
	begin
		FreeBitmap(m.Tiles.bitmaps[0]);
		FreeSprite(m.Tiles);
		Dispose(m);
		m := nil;
	end;
end.