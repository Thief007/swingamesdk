///-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
// 					SGSDK_Camera.pas
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// The Camera unit is used to change the view port (ie 
// the camera location.)
//
// Change History:
//
// Version 1.1:
// - 2008-01-17: Aki + Andrew: Refactor
//  
// Version 1.0:
// - Various

unit SGSDK_Camera;

interface
	uses SGSDK_Core;

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
	uses Classes, SysUtils;

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
		ScreenOffsetX := ScreenOffsetX + v.x;
		ScreenOffsetY := ScreenOffsetY + v.y;
	end;

	procedure MoveVisualArea(dx, dy: Single); overload;
	begin
		ScreenOffsetX := ScreenOffsetX + dx;
		ScreenOffsetY := ScreenOffsetY + dy;
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
		if spr = nil then begin
			raise Exception.Create('FollowSprite requires a target sprite. No sprite was provided (nil supplied)');
		end;
		MoveVisualArea(Round(ScreenX(spr.x) + spr.width / 2 - ScreenWidth() / 2) + Xoffset, 
					   Round(ScreenY(spr.y) + spr.height / 2 - ScreenHeight() / 2) + Yoffset);
	end;

end.