program HowToDetectShapeCollision;
uses
  sgInput, sgGraphics, sgResources, sgText, sgTypes, sgGeometry, sgPhysics, sgUtils;  

procedure BouncingCircle(var cPoint : Point2D; c : Circle; l : LinesArray);
var	
	random : Integer;
begin
	random := Rnd(8);
	
	if CircleLinesCollision(c, l) = false then
	begin	
		case random of
			0: begin // Top
					cPoint.x += 0; 
					cPoint.y -= 1;
				end;
			1:	begin // Bottom
					cPoint.x += 0; 
					cPoint.y += 1;
				end;
			2:	begin // Left
					cPoint.x -= 1; 
					cPoint.y += 0;
				end;
			3:	begin // Right
					cPoint.x += 1; 
					cPoint.y += 0;
				end;
			4:	begin // Top Left
					cPoint.x -= 1; 
					cPoint.y -= 1;
				end;
			5: begin // Top Right
					cPoint.x += 1; 
					cPoint.y -= 1;
				end;		
			6: 	begin // Bottom Left
					cPoint.x -= 1; 
					cPoint.y += 1;
				end;		
			7:	begin // Bottom Right
					cPoint.x += 1; 
					cPoint.y += 1;
				end;		
		end;
	end
	else
		
end;
  
procedure Main();
var
	c : Color;
	c1 : Circle;	
	lineArray : Array[0..3] of LineSegment;		
begin 
	OpenGraphicsWindow('Shape Collision Movement', 800, 600);
	
	c := ColorGreen;
	c1 := CircleAt(400, 300, 15); // return a circle
	lineArray[0] := LineFrom(350, 250, 350, 350);
	lineArray[1] := LineFrom(450, 250, 450, 350);
	lineArray[2] := LineFrom(350, 250, 450, 250);
	lineArray[3] := LineFrom(350, 350, 450, 350);	
	
	repeat // The game loop...
		ProcessEvents();
    	
		ClearScreen(ColorWhite);
		
		FillCircle(c, c1);
		DrawLine(ColorGreen, lineArray[0]);
		DrawLine(ColorGreen, lineArray[1]);
		DrawLine(ColorGreen, lineArray[2]);
		DrawLine(ColorGreen, lineArray[3]);		
		BouncingCircle(c1.center, c1, lineArray);
		
		RefreshScreen();
	until WindowCloseRequested();
  
	ReleaseAllResources();
end;

begin
  Main();
end.