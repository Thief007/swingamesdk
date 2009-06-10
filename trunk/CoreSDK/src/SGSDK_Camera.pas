//---------------------------------------------------/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
//          SGSDK_Camera.pas
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// The Camera unit is used to change the view port (ie 
// the camera location.)
//
// Change History:
//
// Version 2:
// - 2008-12-17: Andrew: Moved all integers to LongInt
//
// Version 1.1:
// - 2008-01-23: Andrew: Changed ToGameCoordinates to use Point2D
//                 Added Point2D overload for SetScreenOffset
// - 2008-01-21: Andrew: Added const to vector parameters.
// - 2008-01-17: Aki + Andrew: Refactor
//  
// Version 1.0:
// - Various

///@module Camera
unit SGSDK_Camera;

interface
  uses SGSDK_Core, SGSDK_Shapes;

  //*****
  //
  // Visible Window management routines.
  //
  //*****
  //
  // These routines are used to move the visual window.
  //
  
  function XOffset(): LongInt;
  function YOffset(): LongInt;
  
  function ScreenX(x: Single): LongInt;
  function ScreenY(y: Single): LongInt;
  function GameX(x: LongInt) : Single;
  function GameY(y: LongInt) : Single;
  function ToGameCoordinates(const screenPoint: Point2D): Point2D;
    
  procedure MoveVisualArea(const v: Vector); overload;
  procedure MoveVisualArea(dx, dy: Single); overload;
  procedure SetScreenOffset(x, y: Single); overload;
  procedure SetScreenOffset(pt: Point2D); overload;
  
  procedure FollowSprite(spr: Sprite; Xoffset, Yoffset: LongInt); overload;
  procedure FollowSprite(spr: Sprite; const offset: Vector); overload;
  
implementation
  uses Classes, SysUtils;

  ///
  /// The screen offset variables
  ///
  var 
    ScreenOffsetX : Single = 0.0;
    ScreenOffsetY : Single = 0.0;
  
  function XOffset(): LongInt;
  begin
    result := Round(ScreenOffsetX);
  end;
  
  function YOffset(): LongInt;
  begin
    result := Round(ScreenOffsetY);
  end;
  
  function ScreenX(x: Single): LongInt;
  begin
    result := Round(x - ScreenOffsetX);
  end;
  
  function ScreenY(y: Single): LongInt;
  begin
    result := Round(y - ScreenOffsetY);
  end;
  
  function GameX(x: LongInt) : Single;
  begin
    result := x + ScreenOffsetX;
  end;
  
  function GameY(y: LongInt) : Single;
  begin
    result := y + ScreenOffsetY;
  end;
  
  procedure MoveVisualArea(const v: Vector); overload;
  begin
    ScreenOffsetX := ScreenOffsetX + v.x;
    ScreenOffsetY := ScreenOffsetY + v.y;
  end;

  procedure MoveVisualArea(dx, dy: Single); overload;
  begin
    ScreenOffsetX := ScreenOffsetX + dx;
    ScreenOffsetY := ScreenOffsetY + dy;
  end;
  
  procedure SetScreenOffset(x, y: Single); overload;
  begin
    ScreenOffsetX := x;
    ScreenOffsetY := y;
  end;

  procedure SetScreenOffset(pt: Point2D); overload;
  begin
    ScreenOffsetX := pt.x;
    ScreenOffsetY := pt.y;
  end;
  
  function ToGameCoordinates(const screenPoint: Point2D): Point2D;
  begin
    result.x := screenPoint.x + ScreenOffsetX;
    result.y := screenPoint.y + ScreenOffsetY;
  end;
  
  procedure FollowSprite(spr : Sprite; Xoffset, Yoffset : LongInt);
  begin
    if spr = nil then begin
      raise Exception.Create('FollowSprite requires a target sprite. No sprite was provided (nil supplied)');
    end;
    MoveVisualArea(Round(ScreenX(spr.x) + spr.width / 2 - ScreenWidth() / 2) + Xoffset, 
             Round(ScreenY(spr.y) + spr.height / 2 - ScreenHeight() / 2) + Yoffset);
  end;
  
  procedure FollowSprite(spr: Sprite; const offset: Vector); overload;
  begin
    FollowSprite(spr, Round(offset.x), Round(offset.y));
  end;

end.