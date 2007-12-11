unit SGSDK_Camera;

interface
	uses	SDL, SGSDK_Core, Classes, SysUtils, SDL_image,
			SDL_Mixer, SDL_TTF, SDLEventProcessing;

	//*****
	//
	// Visible Window management routines.
	//
	//*****
	//
	// These routines are used to move the visual window.
	//

	function XOffset(): Integer;
	function YOffset(): Integer;
	
	function ScreenX(x: Single): Integer;
	function ScreenY(y: Single): Integer;
	function GameX(x: Integer) : Single;
	function GameY(y: Integer) : Single;
	function ToGameCoordinates(screenVector: Vector): Vector;
		
	procedure MoveVisualArea(v: Vector); overload;
	procedure MoveVisualArea(dx, dy: Single); overload;
	procedure SetScreenOffset(x, y: Single);
	
	procedure FollowSprite(spr : Sprite; Xoffset, Yoffset : Integer);
	
implementation

	///
	/// The screen offset variables
	///
	var 
		ScreenOffsetX : Single = 0.0;
		ScreenOffsetY : Single = 0.0;
	
	function XOffset(): Integer;
	begin
		result := Round(ScreenOffsetX);
	end;
	
	function YOffset(): Integer;
	begin
		result := Round(ScreenOffsetY);
	end;
	
	function ScreenX(x: Single): Integer;
	begin
		result := Round(x - ScreenOffsetX);
	end;
	
	function ScreenY(y: Single): Integer;
	begin
		result := Round(y - ScreenOffsetY);
	end;
	
	function GameX(x: Integer) : Single;
	begin
		result := x + ScreenOffsetX;
	end;
	
	function GameY(y: Integer) : Single;
	begin
		result := y + ScreenOffsetY;
	end;
	
	procedure MoveVisualArea(v: Vector); overload;
	begin
		ScreenOffsetX += v.x;
		ScreenOffsetY += v.y;
	end;
	
	procedure MoveVisualArea(dx, dy: Single); overload;
	begin
		ScreenOffsetX += dx;
		ScreenOffsetY += dy;		
	end;
	
	procedure SetScreenOffset(x, y: Single);
	begin
		ScreenOffsetX := x;
		ScreenOffsetY := y;
	end;
	
	function ToGameCoordinates(screenVector: Vector): Vector;
	begin
		result.x := screenVector.x + ScreenOffsetX;
		result.y := screenVector.y + ScreenOffsetY;
		result.w := screenVector.w;
	end;
	
	procedure FollowSprite(spr : Sprite; Xoffset, Yoffset : Integer);
	begin
		MoveVisualArea(Round(ScreenX(spr.xPos) + spr.width / 2 - ScreenWidth() / 2) + Xoffset, 
					   Round(ScreenY(spr.yPos) + spr.height / 2 - ScreenHeight() / 2) + Yoffset);
	end;

end.