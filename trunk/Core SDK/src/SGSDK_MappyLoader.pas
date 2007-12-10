unit SGSDK_MappyLoader;

interface
	uses	SGSDK_Graphics,
			SDL, SGSDK_Core, Classes, SysUtils, SDL_image,
			SDL_Mixer, SDL_TTF, SDLEventProcessing;
	
			
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
        	AnimatedTiles : Array of Sprite;
        	
        	Animate : Boolean;
        	Frame : Integer;
        end;

        
        
		function Loadmap(): Map;
		procedure Drawmap(var m : Map; CameraX, CameraY : Integer);
	
	
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
			
			{
			//Debug
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
		
		{
		//Debug
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
	
	procedure LoadBlockSprites(var m : Map);
	var
		tilefile : String;
		fpc : Array of Integer;
	begin
	
		SetLength(fpc, m.MapInfo.NumberOfBlocks);
		m.Tiles := CreateSprite(LoadBitmap(GetPathToResource('TileSet.png')), true, fpc, m.MapInfo.BlockWidth, m.MapInfo.BlockHeight);
		m.Tiles.currentFrame := 0;
	end;
	
	procedure LoadAnimatedSprites(var m : Map);
	var
		aob : Array of Bitmap;
		fpc : Array of Integer;
		a, f : Integer;
		srcX, srcY : Integer;
	begin
	
		SetLength(m.AnimatedTiles, m.MapInfo.NumberOfAnimations);	
	
		for a := 0 to m.MapInfo.NumberOfAnimations - 1 do
		begin
			
			SetLength(aob, m.AnimationInfo[a].NumberOfFrames);
			SetLength(fpc, m.AnimationInfo[a].NumberOfFrames);
			
			for f := 0 to m.AnimationInfo[a].NumberOfFrames - 1 do
			begin
				//srcX := m.Tiles.bitmaps[0].Width div m.MapInfo.BlockWidth;
				//srcY := m.Tiles.bitmaps[0].Height div m.MapInfo.BlockHeight;

				//srcY := (m.AnimationInfo[a].Frame[f] div m.Tiles.cols) * m.MapInfo.BlockHeight;
				//srcX := (m.AnimationInfo[a].Frame[f] mod m.Tiles.cols) * m.MapInfo.BlockWidth;
				
				//srcY := round(m.AnimationInfo[a].Frame[f] / m.Tiles.cols) * m.MapInfo.BlockHeight;
				//srcX := round(m.AnimationInfo[a].Frame[f] / m.Tiles.row) * m.MapInfo.BlockWidth;
				//new(aob[f]);
				//DrawBitmapPart(aob[f], m.Tiles.bitmaps[0] ,srcX, srcY, m.MapInfo.BlockWidth, m.MapInfo.BlockHeight, 0 ,0);
													 
				//aob[f] := m.Tiles.bitmaps[m.AnimationInfo[a].Frame[f]];
				//fpc[f] := 2;
			end;
			
			m.AnimatedTiles[a] := CreateSprite(aob, fpc, Loop); 
			
			SetLength(aob, 0); 
		end;
	
	end;
	
	procedure DrawMap(var m : Map; CameraX, CameraY : Integer);
	var
		l, y ,x : Integer;
		XStart, YStart, XEnd, YEnd : Integer;
		dx, dy : Integer;
	begin
		//Screen Drawing Starting Point
		XStart := round((CameraX / m.MapInfo.BlockWidth) - (m.MapInfo.BlockWidth * 3));
		YStart := round((CameraY / m.MapInfo.BlockHeight) - (m.MapInfo.BlockHeight * 3));
		
		//Screen Drawing Ending point
		XEnd := round(XStart + (SGSDK_Core.ScreenWidth() / m.MapInfo.BlockWidth) + (m.MapInfo.BlockWidth * 3));
		YEnd := round(YStart + (SGSDK_Core.ScreenHeight() / m.MapInfo.BlockHeight) + (m.MapInfo.BlockHeight * 3));
		
		for l := 0 to m.MapInfo.NumberOfLayers - m.MapInfo.CollisionLayer - m.MapInfo.EventLayer - 1 do
		begin
			for y := YStart  to YEnd do
			begin
				if (y < m.MapInfo.MapHeight - 1) and (y > -1) then
				begin
					for x := XStart  to XEnd do
					begin
						if (x < m.MapInfo.MapWidth - 1) and (x > -1) then
						begin
					
							if (m.LayerInfo[l].Animation[y][x] = 0) and (m.LayerInfo[l].Value[y][x] > 0) then
							begin
								m.Tiles.currentFrame := m.LayerInfo[l].Value[y][x] - 1;
								
								m.Tiles.xPos := x * m.MapInfo.BlockWidth;
								m.Tiles.yPos := y * m.MapInfo.BlockHeight;
								DrawSprite(m.Tiles, CameraX, CameraY, SGSDK_Core.ScreenWidth(), SGSDK_Core.ScreenHeight());
							end
							else if (m.LayerInfo[l].Animation[y][x] = 1) then
							begin
								{m.Tiles.xPos := x * m.MapInfo.BlockWidth;
                            	m.Tiles.yPos := y * m.MapInfo.BlockHeight;
								m.Tiles.currentFrame := m.AnimationInfo[m.LayerInfo[l].Value[y][x]].Frame[m.Frame];
								
								DrawSprite(m.Tiles,CameraX, CameraY, SGSDK_Core.ScreenWidth(), SGSDK_Core.ScreenHeight());
							}
							
								{DrawBitmapPart(m.Tiles.bitmaps[0], 
									round(m.AnimationInfo[m.LayerInfo[l].Value[y][x]].Frame[m.Frame] / m.Tiles.cols) * m.MapInfo.BlockHeight,
									round(m.AnimationInfo[m.LayerInfo[l].Value[y][x]].Frame[m.Frame] / m.Tiles.row) * m.MapInfo.BlockWidth,
              						m.MapInfo.BlockWidth,
              						m.Mapinfo.BlockHeight,
              						x * m.MapInfo.BlockWidth - CameraX,
              						y * m.MapInfo.BlockHeight - CameraY);
              						}									
																		
                                m.AnimatedTiles[m.LayerInfo[l].Value[y][x]].xPos := x * m.MapInfo.BlockWidth;
                            	m.AnimatedTiles[m.LayerInfo[l].Value[y][x]].yPos := y * m.MapInfo.BlockHeight;
                            	DrawSprite(m.AnimatedTiles[m.LayerInfo[l].Value[y][x]], CameraX, CameraY, SGSDK_Core.ScreenWidth(), SGSDK_Core.ScreenHeight());
								
							end;
							
						end;
					end;
				end;
			end;
		end;
		
		//m.Frame := m.Frame + 1;
	end;
	
	
	{

	}
	
	
	
	
	
	
	
	function LoadMap(): Map;
	var
		filestream : text;
		m : Map;
	begin
		//Get File
		assign(filestream, 'test.txt');
		reset(filestream);
		
		//Load Map Content
		LoadMapInformation(m, filestream);
		LoadAnimationInformation(m, filestream);
		LoadLayerData(m, filestream);
		LoadCollisionData(m, filestream);
		LoadEventData(m, filestream);
		
		//Closes File
		close(filestream);
		
		LoadBlockSprites(m);
		//LoadAnimatedSprites(m);
		m.Frame := 0;
		result := m;
	end;
	


end.