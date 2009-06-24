//----------------------------------------------------------------------------
//          sgCamera.pas
//----------------------------------------------------------------------------
//
// The Camera unit is used to change the view port (ie the camera location.)
//
// Change History:
//
// Version 3:
// - 2009-06-17: Clinton: Added CameraPos, reordered methods, 
//                        Renamed ToScreen/ToWorld (removed "Coordinates")
// - 2009-06-16: Clinton: Renaming for consistent World/Camera/Screen use
//                        Comment cleanup/formatting + new comments
//                        Added ToScreenCoordinates
// - 2009-06-15: Andrew: Added meta tags
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
//----------------------------------------------------------------------------

///@module Camera
unit sgCamera;

//----------------------------------------------------------------------------
interface
//----------------------------------------------------------------------------
  
  uses sgTypes;
  
  //---------------------------------------------------------------------------
  // Camera - position
  //---------------------------------------------------------------------------

  /// @returns the current camera x axis (offset) value
  /// @lib
  function CameraX(): LongInt;

  /// @returns the current camera y axis (offset) value
  /// @lib
  function CameraY(): LongInt;
  
  /// @returns the current camera position as a point 
  /// @lib
  function CameraPos(): Point2D;
  
  //---------------------------------------------------------------------------
  // Camera - movement
  //---------------------------------------------------------------------------
  
  /// Move the camera (offset its world x and y values) using the specified 
  /// vector. For example, if you move the camera by the same speed vector of 
  /// a sprite the camera will "track" (be locked on to) the sprite as it moves.
  ///
  /// @param offset The offset vector to move the camera world position by.
  /// @lib
  procedure MoveCameraBy(const offset: Vector); overload;
  
  /// Move the camera (offset its world x and y values) using the specified 
  /// dx (change in x) and dy (change in x) values. 
  ///
  /// @param dx the amount of x axis offset to apply 
  /// @param dy the amount of x axis offset to apply 
  /// @lib MoveCameraByXY
  procedure MoveCameraBy(dx, dy: Single); overload;
  
  /// Move the camera view to a world location specified by the x and y values.
  ///
  /// @param x The world x axis value to move the camera to.
  /// @param y The world y axis value to move the camera to
  /// @lib MoveCameraToXY
  procedure MoveCameraTo(x, y: Single); overload;
  
  /// Move the camera view to a world location specified as a Point2D.
  ///
  /// @param pt The point to move the camera view to.
  /// @lib
  procedure MoveCameraTo(pt: Point2D); overload;
  
  //---------------------------------------------------------------------------
  // Camera - sprite tracking
  //---------------------------------------------------------------------------  
  
  /// Set the camera view to be centered over the specified sprite, with an 
  /// offset from the center of the sprite if needed. The sprites size (width
  /// and height) are taken into account. Use x and y offset of 0.0 if you want 
  /// the camera to be exaclty over the center of the sprite.
  ///
  /// @param s The sprite to center the camera on 
  /// @param offsetX The amount of x axis offset for the camaera to use 
  /// @param offsetY The amount of y axis offset for the camaera to use 
  /// @lib CenterCameraOnWithXYOffset
  procedure CenterCameraOn(s: Sprite; offsetX, offsetY: LongInt); overload;

  /// @param offset The amount of offset from sprite center for the camera to use.  
  /// @lib
  procedure CenterCameraOn(s: Sprite; const offset: Vector); overload;
  
  //---------------------------------------------------------------------------
  // World-To-Screen Translation
  //---------------------------------------------------------------------------
  
  /// Translate a world x value to the current screen x value which is based on
  /// the camera position.
  ///
  /// @param worldX The world x value to be translated
  /// @returns The translated screen x value
  /// @lib
  function ToScreenX(worldX: Single): LongInt;

  /// Translate a world y value to the current screen y value set by the camera.
  ///
  /// @param worldY The world y value to be converted
  /// @returns A screen y value
  /// @lib
  function ToScreenY(worldY: Single): LongInt;

  /// Translate a Point2D from world coordinates to screen coordinates.
  ///
  /// @param worldPoint The screen coordinate to translate
  /// @returns A screen coordinate point
  /// @lib
  function ToScreen(const worldPoint: Point2D): Point2D;
  
  //---------------------------------------------------------------------------
  // Screen-To-World Translation
  //---------------------------------------------------------------------------  
  
  /// Translate a screen x value (based on the camera) to a world x value
  ///
  /// @param screenX The current screen x value to be converted
  /// @returns A world x value 
  /// @lib
  function ToWorldX(screenX: LongInt) : Single;

  /// Translate a screen y value (based on the camera) to a world y value
  ///
  /// @param screenY The current screen y value to be converted
  /// @returns A world y value 
  /// @lib
  function ToWorldY(screenY: LongInt) : Single;

  /// Translate a Point2D from screen coordinates to world coordinates.
  ///
  /// @param screenPoint The screen coordinate to translate
  /// @returns A world coordinate point
  /// @lib
  function ToWorld(const screenPoint: Point2D): Point2D;  
  
  
//----------------------------------------------------------------------------
implementation
//----------------------------------------------------------------------------

  uses Classes, SysUtils, sgCore, sgShapes;

  ///
  /// The screen offset variables
  ///
  var 
    _cameraX : Single = 0.0;
    _cameraY : Single = 0.0;
  
  //---------------------------------------------------------------------------
  // Camera - position
  //---------------------------------------------------------------------------
  
  function CameraX(): LongInt;
  begin
    result := Round(_cameraX);
  end;
  
  function CameraY(): LongInt;
  begin
    result := Round(_cameraY);
  end;
  
  function CameraPos(): Point2D;
  begin
    result.x := _cameraX;
    result.y := _cameraY;
  end;
  
  //---------------------------------------------------------------------------
  // Camera - movement
  //---------------------------------------------------------------------------
  
  procedure MoveCameraBy(const offset: Vector); overload;
  begin
    _cameraX := _cameraX + offset.x;
    _cameraY := _cameraY + offset.y;
  end;

  procedure MoveCameraBy(dx, dy: Single); overload;
  begin
    _cameraX := _cameraX + dx;
    _cameraY := _cameraY + dy;
  end;
  
  procedure MoveCameraTo(x, y: Single); overload;
  begin
    _cameraX := x;
    _cameraY := y;
  end;

  procedure MoveCameraTo(pt: Point2D); overload;
  begin
    _cameraX := pt.x;
    _cameraY := pt.y;
  end;

  //---------------------------------------------------------------------------
  // Camera - Sprite tracking
  //---------------------------------------------------------------------------
  
  procedure CenterCameraOn(s: Sprite; offsetX, offsetY: LongInt);
  begin
    if s = nil then begin
      raise Exception.Create('CenterCameraOn requires a target sprite. No sprite was provided (nil supplied)');
    end;
    MoveCameraTo(Round(ToScreenX(s^.x) + s^.width / 2 - ScreenWidth() / 2) + offsetX, 
                 Round(ToScreenY(s^.y) + s^.height / 2 - ScreenHeight() / 2) + offsetY);
  end;
  
  procedure CenterCameraOn(s: Sprite; const offset: Vector); overload;
  begin
    CenterCameraOn(s, Round(offset.x), Round(offset.y));
  end;
  
  //---------------------------------------------------------------------------
  // World-To-Screen Translation
  //---------------------------------------------------------------------------

  function ToScreenX(worldX: Single): LongInt;
  begin
    result := Round(worldX - _cameraX);
  end;
  
  function ToScreenY(worldY: Single): LongInt;
  begin
    result := Round(worldY - _cameraY);
  end;  
  
  function ToScreen(const worldPoint: Point2D): Point2D;
  begin
    result.x := _cameraX - worldPoint.x;
    result.y := _cameraY - worldPoint.y;
  end;
  
  //---------------------------------------------------------------------------
  // Screen-To-World Translation
  //---------------------------------------------------------------------------
  
  function ToWorldX(screenX: LongInt) : Single;
  begin
    result := screenX + _cameraX;
  end;
  
  function ToWorldY(screenY: LongInt) : Single;
  begin
    result := screenY + _cameraY;
  end;
  
  function ToWorld(const screenPoint: Point2D): Point2D;
  begin
    result.x := screenPoint.x + _cameraX;
    result.y := screenPoint.y + _cameraY;
  end;  
  



end.