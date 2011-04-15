//=============================================================================
// sgGraphics.pas
//=============================================================================
//
// The Graphics unit is responsible for all of the drawing of anything to the
// screen or other surfaces. The ...OnScreen routines draw directly onto the
// screen ignoring the camera settings. The standard draw routines draw to the
// screen using the camera settings. Finally the overloaded drawing methods
// with a destination Bitmap will draw onto the supplied bitmap.
//
// Change History:
//
// Version 3.0:
// - 2010-02-02: Aaron  : Added PushClip,PopClip,SetClip,ResetClip
// - 2010-01-13: Aaron  : Made all Draw Shapes  draw with an offset and made those that does not have a destination Bitmap have  an offset of cameraX and cameraY
// - 2010-01-04: Andrew : Added PutPixel
// - 2009-12-10: Andrew : Moved out remaining bitmap function
// - 2009-11-06: Andrew : Moved out bitmap function
// - 2009-10-16: Andrew : Added shapes and shape prototypes
// - 2009-07-14: Andrew : Removed loading and freeing code.
// - 2009-07-10: Andrew : Fixed missing const modifier on struct parameters
// - 2009-06-29: Andrew : Using circle
// - 2009-06-24: Andrew : Moved Sprite routines to Sprites.
// - 2009-06-23: Clinton: Comment format/cleanup
// - 2009-06-05: Andrew : Using sgShared
//
// Version 2.0:
// - 2008-12-17: Andrew : Moved all integers to Longint
// - 2008-12-10: Andrew : Moved primitive drawing to SDL_gfx
//                      : Added rotation and zoom to Sprite + Sprite Drawing
//                      : Added RotateZoomBitmap
//                      : Added MakeOpaque and MakeTransparent to allow multiple blending
//                      : Added extra triangle drawing code
// - 2008-12-09: Andrew : Started transition to SDL_gfx
//
// Version 1.1:
// - 2008-04-08: Stephen: Added DrawTriangle()
// - 2008-04-02: Andrew : Fixed issues related to freeing images
//                      : Fixed transparent pixels for non 32bit images
// - 2008-03-09: Andrew : Fixed DrawSprite with Offset
// - 2008-02-16: Andrew : Added GetPixel and GetPixelFromScreen
// - 2008-01-31: Andrew : Fixed Line Drawing Issue
// - 2008-01-30: Andrew : Fixed DrawRectangle
// - 2008-01-25: Andrew : Fixed compiler hints for pointer use
// - 2008-01-24: Andrew : Added Clipping
// - 2008-01-24: James  : Version 1.1 overloads
// - 2008-01-21: Aki    : 40 overloads added for Point2D and
// - 2008-01-17: Aki + Andrew: Refactor Rectangle support
//
// Version 1.0:
// - Various
//=============================================================================

{$I sgTrace.inc}

/// The graphics code of SwinGame is used to draw primitive shapes to the screen
/// or onto bitmaps.
/// 
/// @module Graphics
/// @static
unit sgGraphics;

//=============================================================================
interface
  uses sgTypes;
//=============================================================================
  
  
//----------------------------------------------------------------------------
// Window management
//----------------------------------------------------------------------------
  
  /// Sets the icon for the window. This must be called before openning the
  /// graphics window. The icon is loaded as a bitmap, though this can be from
  /// any kind of bitmap file.
  ///
  /// @param filename The name of the file to load as the images icon
  ///
  ///Side Effects
  /// - The icon will be loaded and used as the windows icon when the window
  /// is opened.
  ///
  /// @lib
  procedure SetIcon(filename: String);
  
  /// Opens the graphical window so that it can be drawn onto. You can set the
  /// icon for this window using `SetIcon`. The window itself is only drawn when
  /// you call `RefreshScreen`. All windows are opened at 32 bits per pixel. You
  /// can toggle fullscreen using `ToggleFullScreen`. The window is closed when
  /// the application terminates.
  ///
  /// @param caption The caption for the window
  /// @param width The width of the window
  /// @param height The height of the window
  ///
  /// Side Effects:
  /// - A graphical window is opened
  ///
  /// @lib
  /// @uname OpenGraphicsWindow
  /// @sn openGraphicsWindow:%s width:%s height:%s
  procedure OpenGraphicsWindow(caption: String; width, height: Longint); overload;

  /// Opens the graphical window as an 800 x 600 window. See OpenGramhicsWinddow
  /// for more options.
  /// @param caption: The caption for the window
  ///
  /// Side Effects:
  /// - A graphical window is opened
  ///
  /// @lib OpenGraphicsWindow(caption, 800, 600)
  /// @uname OpenGraphicsWindow800x600
  /// @sn openGraphicsWindow:%s
  procedure OpenGraphicsWindow(caption: String); overload;

  /// Changes the size of the screen.
  ///
  /// @param width, height: The new width and height of the screen
  ///
  /// Side Effects:
  /// - The screen changes to the specified size
  ///
  /// @lib
  /// @sn changeScreenSizeToWidth:%s height:%s
  procedure ChangeScreenSize(width, height: Longint);

  /// Switches the application to full screen or back from full screen to
  /// windowed.
  ///
  /// Side Effects:
  /// - The window switched between fullscreen and windowed
  ///
  /// @lib
  procedure ToggleFullScreen();
  
  /// Toggle the Window border mode. This enables you to toggle from a bordered
  /// window to a borderless window.
  ///
  /// @lib
  procedure ToggleWindowBorder();
  
  /// Returns the width of the screen currently displayed.
  ///
  /// @returns: The screen's width
  ///
  /// @lib
  ///
  /// @class Graphics
  /// @static
  /// @getter ScreenWidth
  function ScreenWidth(): Longint;

  /// Returns the height of the screen currently displayed.
  ///
  /// @returns: The screen's height
  ///
  /// @lib
  ///
  /// @class Graphics
  /// @static
  /// @getter ScreenHeight
  function ScreenHeight(): Longint;

  /// Saves the current screen a bitmap file. The file will be saved into the
  /// current directory.
  ///
  /// @param basename   The base name for the screen shot. e.g. "GemCollector"
  ///
  /// Side Effects:
  /// - Saves the current screen image to a bitmap file.
  ///
  /// @lib TakeScreenshot
  procedure TakeScreenshot(basename: String);
  
  
  
//----------------------------------------------------------------------------
// Refreshing the screen
//----------------------------------------------------------------------------
  
  /// Draws the current drawing to the screen. This must be called to display
  /// anything to the screen. This will draw all drawing operations, as well
  /// as the text being entered by the user.
  ///
  /// Side Effects:
  /// - The current drawing is shown on the screen.
  ///
  /// @lib RefreshScreen
  procedure RefreshScreen(); overload;
  
  /// Refresh with a target FPS. This will delay a period of time that will 
  /// approximately meet the targetted frames per second.
  ///
  /// @lib RefreshScreenRestrictFPS
  procedure RefreshScreen(TargetFPS: Longword); overload;
  
  
  
//----------------------------------------------------------------------------
// Color
//----------------------------------------------------------------------------

  /// Maps a color from a given bitmap. This is used when determining color
  /// keys for transparent images.
  ///
  /// @param bmp:   the bitmap to get the color for
  /// @param apiColor:     The api color to match
  /// @returns:           The color matched to the bitmaps pixel format
  ///
  /// @lib ColorFromBitmap
  /// @sn colorFrom:%s apiColor:%s
  ///
  /// @doc_group colors
  function ColorFrom(bmp: Bitmap; apiColor: Color): Color;

  /// Creates and returns a random color where R, G, B and A are all randomised.
  ///
  /// @lib
  ///
  /// @doc_group colors
  function RandomColor(): Color;

  /// Creates and returns a random color where R, G, and B are all randomised, and A is set
  /// to the passed in value.
  ///
  /// @param alpha: the opacity of the random color
  ///
  /// @lib
  ///
  /// @doc_group colors
  function RandomRGBColor(alpha: Byte): Color;

  /// Gets a color given its RGBA components.
  ///
  /// @param red, green, blue, alpha:  Components of the color
  /// @returns: The matching colour
  ///
  /// @lib
  /// @sn rgbaColorRed:%s green:%s blue:%s alpha:%s
  ///
  /// @doc_group colors
  function RGBAColor(red, green, blue, alpha: Byte): Color;

  /// Gets a color given its RGB components.
  ///
  /// @param red, green, blue:   Components of the color
  /// @returns:                 The matching colour
  ///
  /// @lib RGBAColor(red, green, blue, 255)
  /// @uname RGBColor
  /// @sn rgbColorRed:%s green:%s blue:%s
  ///
  /// @doc_group colors
  function RGBColor(red, green, blue: Byte): Color;

  /// Gets a color given its RGBA components.
  ///
  /// @lib
  /// @sn colorComponentsOf:%s red:%s green:%s blue:%s alpha:%s
  ///
  /// @doc_group colors
  procedure ColorComponents(c: Color; out r, g, b, a: byte);


  /// returns color to string.
  ///
  /// @lib
  ///
  /// @doc_group colors
  function  ColorToString(c: Color): string;

  /// Returns a color from a floating point RBG value set.
  ///
  /// @param r,g,b: Components for color 0 = none 1 = full
  ///
  /// @lib
  /// @sn rgbFloatColorRed:%s green:%s blue:%s
  ///
  /// @doc_group colors
  function RGBFloatColor(r,g,b: Single): Color;

  /// Returns a color from a floating point RBGA value set.
  ///
  /// @param r,g,b,a: Components for color 0 = none 1 = full
  ///
  /// @lib
  /// @sn rgbaFloatColorRed:%s green:%s blue:%s alpha:%s
  ///
  /// @doc_group colors
  function RGBAFloatColor(r,g,b, a: Single): Color;

  /// Returs a color from the HSB input.
  ///
  /// @param hue, saturation, brightness: Values between 0 and 1
  /// @returns The matching color
  ///
  /// @lib
  /// @sn hsbColorHue:%s sat:%s bri:%s
  ///
  /// @doc_group colors
  function HSBColor(hue, saturation, brightness: Single): Color;

  /// Get the transpareny value of ``color``.
  ///
  /// @lib
  ///
  /// @doc_group colors
  function TransparencyOf(c: Color): byte;

  /// Get the red value of ``color``.
  ///
  /// @lib
  ///
  /// @doc_group colors
  function RedOf(c: Color): byte;

  /// Get the green value of ``color``.
  ///
  /// @lib
  ///
  /// @doc_group colors
  function GreenOf(c: Color): byte;

  /// Get the blue value of ``color``.
  ///
  /// @lib
  ///
  /// @doc_group colors
  function BlueOf(c: Color): byte;

  /// Gets the hue ``h``, saturation ``s``, and brightness ``b`` values from
  /// the color.
  ///
  /// @lib
  /// @sn hsbValueOf:%s hue:%s sat:%s bri:%s
  ///
  /// @doc_group colors
  procedure HSBValuesOf(c: Color; out h, s, b: Single);

  /// Get the hue of the ``color``.
  ///
  /// @lib
  ///
  /// @doc_group colors
  function HueOf(c: Color): Single;

  /// Get the saturation of the ``color``.
  ///
  /// @lib
  ///
  /// @doc_group colors
  function SaturationOf(c: Color) : Single;

  /// Get the brightness of the ``color``.
  ///
  /// @lib
  ///
  /// @doc_group colors
  function BrightnessOf(c: Color) : Single;
  
  
  
//---------------------------------------------------------------------------
// Circle drawing code
//---------------------------------------------------------------------------
  
  /// Draw a circle onto a destination (filled or outline).
  /// 
  /// @lib DrawOrFillPtCircleOnto
  /// @sn drawOnto:%s color:%s filled:%s circleX:%s y:%s radius:%s
  procedure DrawCircle(dest: Bitmap; clr: Color; filled: Boolean; xc, yc: Single; radius: Longint); overload;
  
  /// Draw a circle onto a destination.
  /// 
  /// @lib DrawOrFillPtCircleOnto(dest, clr, False, xc, yc, radius)
  /// @uname DrawPtCircleOnto
  /// @sn drawOnto:%s color:%s circleX:%s y:%s radius:%s
  procedure DrawCircle(dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint); overload;
  
  /// Fill a circle onto a destination.
  /// 
  /// @lib DrawOrFillPtCircleOnto(dest, clr, True, xc, yc, radius)
  /// @uname FillPtCircleOnto
  /// @sn fillOnto:%s color:%s circleX:%s y:%s radius:%s
  procedure FillCircle(dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint); overload;
  
  /// Draw a circle onto a destination (filled or outline).
  /// 
  /// @lib DrawOrFillPtCircleAtPointOnto
  /// @sn drawOnto:%s color:%s filled:%s circle:%s radius:%s
  procedure DrawCircle(dest: Bitmap; clr: Color; filled: Boolean; const point: Point2D; radius: Longint); overload;
  
  /// Draw a circle onto a bitmap.
  /// 
  /// @lib DrawOrFillPtCircleAtPointOnto(dest, clr, False, point, radius)
  /// @uname DrawCircleAtPointOnto
  /// @sn drawOnto:%s color:%s circle:%s radius:%s
  procedure DrawCircle(dest: Bitmap; clr: Color; const point: Point2D; radius: Longint); overload;
  
  /// Fill a circle onto a destination bitmap.
  /// 
  /// @lib DrawOrFillPtCircleAtPointOnto(dest, clr, True, point, radius)
  /// @uname FillCircleAtPointOnto
  /// @sn fillOnto:%s color:%s circle:%s radius:%s
  procedure FillCircle(dest: Bitmap; clr: Color; const point: Point2D; radius: Longint); overload;
  
  /// Draw a circle onto a bitmap (filled or outline).
  /// 
  /// @lib DrawOrFillCircleOnto
  /// @sn drawOnto:%s color:%s filled:%s circle:%s
  procedure DrawCircle(dest: Bitmap; clr: Color; filled: Boolean; const c: Circle); overload;
  
  /// Draw a circle onto a bitmap.
  /// 
  /// @lib DrawOrFillCircleOnto(dest, clr, False, c)
  /// @uname DrawCircleOnto
  /// @sn drawOnto:%s color:%s circle:%s
  procedure DrawCircle(dest: Bitmap; clr: Color; const c: Circle); overload;
  
  /// Fill a circle onto a destination.
  /// 
  /// @lib DrawOrFillCircleOnto(dest, clr, True, c)
  /// @uname FillCircleOnto
  /// @sn fillOnto:%s color:%s circle:%s
  procedure FillCircle(dest: Bitmap; clr: Color; const c: Circle); overload;
  
  /// Draw a circle in the game (filled or outline).
  /// 
  /// @lib DrawOrFillPtCircle
  /// @sn draw:%s filled:%s circleX:%s y:%s radius:%s
  procedure DrawCircle(clr: Color; filled: Boolean; xc, yc: Single; radius: Longint); overload;
  
  /// Draw a circle in the game.
  /// 
  /// @lib DrawOrFillPtCircle(clr, False, xc, yc, radius)
  /// @uname DrawPtCircle
  /// @sn draw:%s circleX:%s y:%s radius:%s
  procedure DrawCircle(clr: Color; xc, yc: Single; radius: Longint); overload;
  
  /// Fill a circle in the game.
  /// 
  /// @lib DrawOrFillPtCircle(clr, True, xc, yc, radius)
  /// @uname FillPtCircle
  /// @sn fill:%s circleX:%s y:%s radius:%s
  procedure FillCircle(clr: Color; xc, yc: Single; radius: Longint); overload;
  
  /// Draw a circle in the game (filled or outline).
  /// 
  /// @lib DrawOrFillPtCircleAtPoint
  /// @sn draw:%s filled:%s circle:%s radius:%s
  procedure DrawCircle(clr: Color; filled: Boolean; const position: Point2D; radius: Longint); overload;
  
  /// Draw circle in the game.
  /// 
  /// @lib DrawOrFillPtCircleAtPoint(clr, False, position, radius)
  /// @uname DrawPtCircleAtPoint
  /// @sn draw:%s circle:%s radius:%s
  procedure DrawCircle(clr: Color; const position: Point2D; radius: Longint); overload;
  
  /// Fill a circle in the game.
  /// 
  /// @lib DrawOrFillPtCircleAtPoint(clr, True, position, radius)
  /// @uname FillPtCircleAtPoint
  /// @sn fill:%s circle:%s radius:%s
  procedure FillCircle(clr: Color; const position: Point2D; radius: Longint); overload;
  
  /// Draw a circle in the game (filled or outline).
  /// 
  /// @lib DrawOrFillCircle
  /// @sn draw:%s filled:%s circle:%s
  procedure DrawCircle(clr: Color; filled: Boolean; const c: Circle); overload;
  
  /// Draw a circle in the game.
  /// 
  /// @lib DrawOrFillCircle(clr, False, c)
  /// @uname DrawCircle
  /// @sn draw:%s circle:%s
  procedure DrawCircle(clr: Color; const c: Circle); overload;
  
  /// Fill a circle in the game.
  /// 
  /// @lib DrawOrFillCircle(clr, True, c)
  /// @uname FillCircle
  /// @sn fill:%s circle:%s
  procedure FillCircle(clr: Color; const c: Circle); overload;
  
  /// Draw a circle on the screen (filled or outline).
  /// 
  /// @lib DrawOrFillPtCircleOnScreen
  /// @sn draw:%s filled:%s circleOnScreenX:%s y:%s radius:%s
  procedure DrawCircleOnScreen(clr: Color; filled: Boolean; xc, yc: Single; radius: Longint); overload;
  
  /// Draw a circle on the screen.
  /// 
  /// @lib DrawOrFillPtCircleOnScreen(clr, False, xc, yc, radius)
  /// @uname DrawCirclePtOnScreen
  /// @sn draw:%s circleOnScreenX:%s y:%s radius:%s
  procedure DrawCircleOnScreen(clr: Color; xc, yc: Single; radius: Longint); overload;
  
  /// Fill a circle on the screen.
  /// 
  /// @lib DrawOrFillPtCircleOnScreen(clr, True, xc, yc, radius)
  /// @uname FillCirclePtOnScreen
  /// @sn fill:%s circleOnScreenX:%s y:%s radius:%s
  procedure FillCircleOnScreen(clr: Color; xc, yc: Single; radius: Longint); overload;
  
  /// Draw a circle onto the screen (filled or outline).
  /// 
  /// Draw a circle onto the screen
  /// @lib DrawOrFillCircleAtPointOnScreen
  /// @sn draw:%s filled:%s circleOnScreen:%s radius:%s
  procedure DrawCircleOnScreen(clr: Color; filled: Boolean; const position: Point2D; radius: Longint); overload;
  
  /// Draw a circle onto the screen.
  /// 
  /// @lib DrawOrFillCircleAtPointOnScreen(clr, False, position, radius)
  /// @uname DrawCircleAtPointOnScreen
  /// @sn draw:%s circleOnScreen:%s radius:%s
  procedure DrawCircleOnScreen(clr: Color; const position: Point2D; radius: Longint); overload;
  
  /// Fills a circle onto the screen.
  /// 
  /// @lib DrawOrFillCircleAtPointOnScreen(clr, True, position, radius)
  /// @uname FillCircleAtPointOnScreen
  /// @sn fill:%s circleOnScreen:%s radius:%s
  procedure FillCircleOnScreen(clr: Color; const position: Point2D; radius: Longint); overload;
  
  /// Draw a circle on the screen (filled or outline)
  /// 
  /// @lib DrawOrFillCircleOnScreen
  /// @sn draw:%s filled:%s circleOnScreen:%s
  procedure DrawCircleOnScreen(clr: Color; filled: Boolean; const c: Circle); overload;
  
  /// Draw the circel onto the screen.
  /// 
  /// @lib DrawOrFillCircleOnScreen(clr, False, c)
  /// @uname DrawCircleOnScreen
  /// @sn draw:%s circleOnScreen:%s
  procedure DrawCircleOnScreen(clr: Color; const c: Circle); overload;
  
  /// Fill the circle onto the screen.
  /// 
  /// @lib DrawOrFillCircleOnScreen(clr, True, c)
  /// @uname FillCircleOnScreen
  /// @sn fill:%s circleOnScreen:%s
  procedure FillCircleOnScreen(clr: Color; const c: Circle); overload;
  
  
  
//---------------------------------------------------------------------------
// Triangle drawing code
//---------------------------------------------------------------------------
  
  /// Draw the triangle onto the destination (filled or outline).
  /// 
  /// @lib DrawOrFillTriangleOnto
  /// @sn drawOnto:%s color:%s filled:%s triangle:%s
  procedure DrawTriangle(dest: Bitmap; clr: Color; filled: Boolean; const tri: Triangle); overload;
  
  /// Draw the triangle onto the destination.
  /// 
  /// @lib DrawOrFillTriangleOnto(dest, clr, False, tri)
  /// @uname DrawTriangleOnto
  /// @sn drawOnto:%s color:%s triangle:%s
  procedure DrawTriangle(dest: Bitmap; clr: Color; const tri: Triangle); overload;
  
  /// Fill the triangle onto the destination.
  ///
  /// @lib DrawOrFillTriangleOnto(dest, clr, True, tri)
  /// @uname FillTriangleOnto
  /// @sn fillOnto:%s color:%s triangle:%s
  procedure FillTriangle(dest: Bitmap; clr: Color; const tri: Triangle); overload;
  
  /// Draw the triangle onto the destination.
  /// 
  /// @lib DrawTriangleFromPointsOnto
  /// @sn drawOnto:%s color:%s triangleX1:%s y1:%s x2:%s y2:%s x3:%s y3:%s
  procedure DrawTriangle(dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  
  /// Fill the triangle onto the destination.
  ///
  /// @lib FillTriangleFromPointsOnto
  /// @sn fillOnto:%s color:%s triangleX1:%s y1:%s x2:%s y2:%s x3:%s y3:%s
  procedure FillTriangle(dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  
  /// Draw a triangle in the game.
  ///
  /// @lib DrawOrFillTriangle
  /// @sn draw:%s filled:%s triangle:%s
  procedure DrawTriangle(clr: Color; filled: Boolean; const tri: Triangle); overload;
  
  /// Draw a triangle in the game.
  ///
  /// @lib DrawOrFillTriangle(clr, False, tri)
  /// @uname DrawTriangle
  /// @sn draw:%s triangle:%s
  procedure DrawTriangle(clr: Color; const tri: Triangle); overload;
  
  /// Fill a triangle in the game.
  /// 
  /// @lib DrawOrFillTriangle(clr, True, tri)
  /// @uname FillTriangle
  /// @sn fill:%s triangle:%s
  procedure FillTriangle(clr: Color; const tri: Triangle); overload;
  
  /// Draw a triangle in the game.
  ///
  /// @lib DrawTriangleFromPoints
  /// @sn draw:%s triangleX1:%s y1:%s x2:%s y2:%s x3:%s y3:%s
  procedure DrawTriangle(clr: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  
  /// Fill a triangle in the game.
  ///
  /// @lib FillTriangleFromPoints
  /// @sn fill:%s triangleX1:%s y1:%s x2:%s y2:%s x3:%s y3:%s
  procedure FillTriangle(clr: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  
  /// Draw a triangle (filled or outline) onto the screen.
  /// 
  /// @lib DrawOrFillTriangleOnScreen
  /// @sn draw:%s filled:%s triangleOnScreen:%s
  procedure DrawTriangleOnScreen(clr: Color; filled: Boolean; const tri: Triangle); overload;
  
  /// Draw a triangle onto the screen.
  ///
  /// @lib DrawOrFillTriangleOnScreen(clr, False, tri)
  /// @uname DrawTriangleOnScreen
  /// @sn draw:%s triangleOnScreen:%s
  procedure DrawTriangleOnScreen(clr: Color; const tri: Triangle); overload;
  
  /// Fills a triangle on the screen.
  /// 
  /// @lib DrawOrFillTriangleOnScreen(clr, True, tri)
  /// @uname FillTriangleOnScreen
  /// @sn fill:%s triangleOnScreen:%s
  procedure FillTriangleOnScreen(clr: Color; const tri: Triangle); overload;
  
  /// Draws the outline of a triangle on the screen.
  /// 
  /// @lib DrawTriangleFromPointsOnScreen
  /// @sn draw:%s triangleOnScreenX1:%s y1:%s x2:%s y2:%s x3:%s y3:%s
  procedure DrawTriangleOnScreen(clr: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  
  /// Fills a triangle on the screen.
  /// 
  /// @lib FillTriangleFromPointsOnScreen
  /// @sn fill:%s triangleOnScreenX1:%s y1:%s x2:%s y2:%s x3:%s y3:%s
  procedure FillTriangleOnScreen(clr: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  
  
  
//---------------------------------------------------------------------------
// Shape drawing code
//---------------------------------------------------------------------------
  
  /// Draw the Shape s onto the destination bitmap. The filled boolean indicates
  /// if the Shape should be filled.
  /// 
  /// @lib DrawOrFillShapeOnto
  /// @sn drawOnto:%s shape:%s filled:%s
  /// 
  /// @class Shape
  /// @overload Draw DrawOrFillOnto
  /// @self 2
  /// @csn drawOnto:%s filled:%s
  procedure DrawShape(dest: Bitmap; s: Shape; filled: Boolean); overload;
  
  /// Draw the Shape s onto the destination bitmap.
  /// 
  /// @lib DrawShapeOnto
  /// @sn drawOnto:%s shape:%s
  /// 
  /// @class Shape
  /// @overload Draw DrawOnto
  /// @self 2
  /// @csn drawOnto:%s
  procedure DrawShape(dest: Bitmap; s: Shape); overload;
  
  /// Fill the Shape s onto the destination bitmap.
  ///
  /// @lib FillShapeOnto
  /// @sn fillOnto:%s shape:%s
  /// 
  /// @class Shape
  /// @overload Fill FillOnto
  /// @self 2
  /// @csn fillOnto:%s
  procedure FillShape(dest: Bitmap; s: Shape); overload;
  
  /// Draw or fill the Shape s onto the screen at the 
  /// shapes game coordinates.
  /// 
  /// @lib DrawOrFillShape
  /// @sn drawShape:%s filled:%s
  /// 
  /// @class Shape
  /// @overload Draw DrawOrFill
  /// @csn drawFilled:%s
  procedure DrawShape(s: Shape; filled: Boolean); overload;
  
  /// Draw the Shape s onto the screen at the 
  /// shapes game coordinates.
  /// 
  /// @lib DrawShape
  /// 
  /// @class Shape
  /// @method Draw
  procedure DrawShape(s: Shape); overload;
  
  /// Fill the Shape s.
  /// 
  /// @lib FillShape
  /// 
  /// @class Shape
  /// @method Fill
  procedure FillShape(s: Shape); overload;
  
  /// Draw or fill the Shape s onto the screen using the
  /// Shape's location as screen coordinates.
  /// 
  /// @lib DrawOrFillShapeOnScreen
  /// @sn drawShapeOnScreen:%s filled:%s
  /// 
  /// @class Shape
  /// @overload DrawOnScreen DrawOrFillOnScreen
  /// @csn drawOnScreenFilled:%s
  procedure DrawShapeOnScreen(s: Shape; filled: Boolean); overload;
  
  /// Draw the Shape s onto the screen using the
  /// Shape's location as screen coordinates.
  ///
  /// @lib DrawShapeOnScreen
  ///
  /// @class Shape
  /// @method DrawOnScreen
  procedure DrawShapeOnScreen(s: Shape); overload;
  
  /// Fill the Shape s onto the screen using the
  /// Shape's location as screen coordinates.
  /// 
  /// @lib FillShapeOnScreen
  /// 
  /// @class Shape
  /// @method FillOnScreen
  procedure FillShapeOnScreen(s: Shape); overload;
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This version
  /// Draws the first point of the shape as a pixel.
  ///
  /// @lib
  /// @sn drawOnto:%s pointShape:%s filled:%s offset:%s
  ///
  /// @class Shape
  /// @method DrawAsPoint
  /// @self 2
  /// @csn drawPointOnto:%s filled:%s offset:%s
  procedure DrawShapeAsPoint(dest: Bitmap; s:Shape; filled: Boolean; const offset:Point2D); overload;
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This version
  /// draws the shape as a circle, centered on the first point with a radius defined
  /// by the distance to the second point.
  /// 
  /// @lib
  /// @sn drawOnto:%s circleShape:%s filled:%s offset:%s
  /// 
  /// @class Shape
  /// @method DrawAsCircle
  /// @self 2
  /// @csn drawCircleOnto:%s filled:%s offset:%s
  procedure DrawShapeAsCircle(dest: Bitmap; s:Shape; filled: Boolean; const offset:Point2D); overload;
  
  //   /// Draw the passed in shape to the specified bitmap. If filled the shape
  //   /// is drawn with a fill rather than drawn as a series of lines.
  //   ///
  //   /// @lib
  //   /// @class Shape
  //   /// @method DrawAsEllipse
  //   /// @self 2
  // procedure DrawShapeAsEllipse(dest: Bitmap; s:Shape; filled: Boolean); overload;
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This version
  /// draws a line from the first point of the shape to the second point.
  /// 
  /// @lib
  /// @sn drawOnto:%s lineShape:%s filled:%s offset:%s
  /// 
  /// @class Shape
  /// @method DrawAsLine
  /// @self 2
  /// @csn drawLineOnto:%s filled:%s offset:%s
  procedure DrawShapeAsLine(dest: Bitmap; s:Shape; filled: Boolean; const offset:Point2D); overload;
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This version
  /// draws the shape as a triangle based on the first three points of the shape.
  /// 
  /// @lib
  /// @sn drawOnto:%s triangleShape:%s filled:%s offset:%s
  ///
  /// @class Shape
  /// @method DrawAsTriangle
  /// @self 2
  /// @csn drawTriangleOnto:%s filled:%s offset:%s
  procedure DrawShapeAsTriangle(dest: Bitmap; s:Shape; filled: Boolean; const offset:Point2D); overload;
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This version
  /// draws the points as a list of lines. A shape with 4 points has two lines in its
  /// list. If an odd numer of points are supplied then the extra point will be skipped.
  /// In this way a shape with 5 points also has 2 lines.
  /// 
  /// @lib
  /// @sn drawOnto:%s lineListShape:%s filled:%s offset:%s
  /// 
  /// @class Shape
  /// @method DrawAsLineList
  /// @self 2
  /// @csn drawLineListOnto:%s filled:%s offset:%s
  procedure DrawShapeAsLineList(dest: Bitmap; s: Shape; filled: Boolean; const offset:Point2D);
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This version
  /// draws the shape as a line strip. A shape with three points has two lines, one 
  /// from pt[0] to pt[1] and a second from pt[1] to pt[2].
  /// 
  /// @lib
  /// @sn drawOnto:%s lineStripShape:%s filled:%s offset:%s
  /// 
  /// @class Shape
  /// @method DrawAsLineStrip
  /// @self 2
  /// @csn drawLineStripOnto:%s filled:%s offset:%s
  procedure DrawShapeAsLineStrip(dest: Bitmap; s: Shape; filled: Boolean; const offset:Point2D);
  
  // / Draw the passed in shape to the specified bitmap. If filled the shape
  // / is drawn with a fill rather than drawn as a series of lines. This draws
  // / as a polygon where each point is connected to its neighbour and the
  // / first point is reconnected to the last point.
  // / 
  // / @lib
  // / @sn drawOnto:%s polygonShape:%s filled:%s offset:%s
  // / 
  // / @class Shape
  // / @method DrawAsPolygon
  // / @self 2
  // / @csn drawPolygonOnto:%s filled:%s offset:%s
  //procedure DrawShapeAsPolygon(dest: Bitmap; s: Shape; filled: Boolean; const offset:Point2D);
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This version
  /// draws the shape as a tan of triangles where each triangle is made up of
  /// the first point and two neighbouring points from the shape.
  /// 
  /// @lib
  /// @sn drawOnto:%s triangleFanShape:%s filled:%s offset:%s
  /// 
  /// @class Shape
  /// @method DrawAsTriangleFan
  /// @self 2
  /// @csn drawTriangleFonOnto:%s filled:%s offset:%s
  procedure DrawShapeAsTriangleFan(dest: Bitmap; s: Shape; filled: Boolean; const offset:Point2D);
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This version
  /// draws as a strip of triangles where each triangle is made up of the 
  /// three neighbouring points. In this way 4 points gives two triangles.
  /// 
  /// @lib
  /// @sn drawOnto:%s triangleStripShape:%s filled:%s offset:%s
  /// 
  /// @class Shape
  /// @method DrawAsTriangleStrip
  /// @self 2
  /// @csn drawTriangleStripOnto:%s filled:%s offset:%s
  procedure DrawShapeAsTriangleStrip(dest: Bitmap; s: Shape; filled: Boolean; const offset:Point2D);
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This version
  /// draws as a triangle list, where each set of three points is drawn as an
  /// individual triangle and extra points are ignored. So 6, 7, and 8 points 
  /// all create 2 triangles (pt[0] + pt[1] + pt[2] and pt[3] + pt[4] + pt[5]). 
  /// 
  /// @lib
  /// @sn drawOnto:%s triangleListShape:%s filled:%s offset:%s
  /// 
  /// @class Shape
  /// @method DrawAsTriangleList
  /// @self 2
  /// @csn drawTriangleListOnto:%s filled:%s offset:%s
  procedure DrawShapeAsTriangleList(dest: Bitmap; s: Shape; filled: Boolean; const offset:Point2D);
  
  
  
//---------------------------------------------------------------------------
// Screen clearing routines
//---------------------------------------------------------------------------
  
  /// Clear the screen black.
  /// 
  /// @lib ClearScreen
  /// @sn clearScreen
  procedure ClearScreen(); overload;
  
  /// Clear the screen to a specified color.
  /// 
  /// @lib ClearScreenTo
  /// @sn clearScreen:%s
  procedure ClearScreen(toColor : Color); overload;
  
  
  
//---------------------------------------------------------------------------
// Pixel drawing
//---------------------------------------------------------------------------
  
  /// Sets the color of the pixel to the specified value.
  ///
  /// @lib
  /// @sn bitmap:%s putPixelX:%s y:%s color:%s
  procedure PutPixel(bmp: Bitmap; value: Color; x, y: Longint);
  
  /// Draw a pixel onto a destination.
  /// 
  /// @lib DrawPixelOnto
  /// @sn drawOnto:%s color:%s pixelX:%s y:%s
  procedure DrawPixel(dest: Bitmap; clr: Color; x, y: Longint); overload;
  
  /// Draw a pixel onto a destination.
  /// 
  /// @lib DrawPixelAtPointOnto
  /// @sn drawOnto:%s color:%s pixel:%s
  procedure DrawPixel(dest: Bitmap; clr: Color; const position: Point2D); overload;
  
  /// Draw a pixel in the game.
  ///
  /// @lib
  /// @sn draw:%s pixelX:%s y:%s
  procedure DrawPixel(clr: Color; x, y: Single); overload;
  
  /// Draw a pixel in the game.
  ///
  /// @lib DrawPixelAtPoint
  /// @sn draw:%s pixelAt:%s
  procedure DrawPixel(clr: Color; const position: Point2D); overload;
  
  /// Draw a pixel on the screen.
  /// 
  /// @lib
  /// @sn draw:%s pixelOnScreenX:%s y:%s
  procedure DrawPixelOnScreen(clr: Color; x, y: Longint); overload;
  
  /// Draw a pixel on the screen.
  ///
  /// @lib DrawPixelAtPointOnScreen
  /// @sn draw:%s pixelOnScreenAt:%s
  procedure DrawPixelOnScreen(clr: Color; const position: Point2D); overload;
  
  
  
//---------------------------------------------------------------------------
// Rectangle drawing
//---------------------------------------------------------------------------
  
  /// Draw a rectangle onto a destination.
  /// 
  /// @lib DrawOrFillRectangleOnto
  /// @sn drawOnto:%s color:%s filled:%s rectangleX:%s y:%s width:%s height:%s
  procedure DrawRectangle(dest: Bitmap; clr : Color; filled : Boolean; xPos, yPos, width, height : Longint); overload;
  
  /// Draw a rectangle onto a destination (filled or outline).
  /// 
  /// @lib DrawOrFillRectangleRectOnto
  /// @sn drawOnto:%s color:%s filled:%s rectangle:%s
  procedure DrawRectangle(dest: Bitmap; clr : Color; filled : Boolean; const source: Rectangle); overload;
  
  /// Draw a rectangle onto a destination.
  /// 
  /// @lib DrawOrFillRectangleOnto(dest, clr, False, xPos, yPos, width, height)
  /// @uname DrawRectangleOnto
  /// @sn drawOnto:%s color:%s rectangleX:%s y:%s width:%s height:%s
  procedure DrawRectangle(dest: Bitmap; clr : Color; xPos, yPos, width, height: Longint); overload;
  
  /// Draw a rectangle onto a destination.
  /// 
  /// @lib DrawOrFillRectangleRectOnto(dest, clr, False, source)
  /// @uname DrawRectangleRectOnto
  /// @sn drawOnto:%s color:%s rectangle:%s
  procedure DrawRectangle(dest: Bitmap; clr : Color; const source: Rectangle); overload;
  
  /// Fill a rectangle onto a destination.
  /// 
  /// @lib DrawOrFillRectangleOnto(dest, clr, True, xPos, yPos, width, height)
  /// @uname FillRectangleOnto
  /// @sn fillOnto:%s color:%s rectangleX:%s y:%s width:%s height:%s
  procedure FillRectangle(dest: Bitmap; clr : Color; xPos, yPos, width, height : Longint); overload;
  
  /// Fill a rectangle onto a destination.
  /// 
  /// @lib DrawOrFillRectangleRectOnto(dest, clr, True, source)
  /// @uname FillRectangleRectOnto
  /// @sn fillOnto:%s color:%s rectangle:%s
  procedure FillRectangle(dest: Bitmap; clr : Color; const source: Rectangle); overload;
  
  /// Draw a rectangle in the game (filled or outline).
  ///
  /// @lib DrawOrFillRectangle
  /// @sn draw:%s filled:%s rectangleX:%s y:%s width:%s height:%s
  procedure DrawRectangle(clr: Color; filled: Boolean; xPos, yPos: Single; width, height: Longint); overload;
  
  /// Draw a rectangle in the game (filled or outline).
  /// 
  /// @lib DrawOrFillRectangleRect
  /// @sn draw:%s filled:%s rectangle:%s
  procedure DrawRectangle(clr: Color; filled: Boolean; const source: Rectangle); overload;
  
  /// Draw a rectangle in the game.
  ///
  /// @lib DrawOrFillRectangle(clr, False, xPos, yPos, width, height)
  /// @uname DrawRectangle
  /// @sn draw:%s rectangleX:%s y:%s width:%s height:%s
  procedure DrawRectangle(clr: Color; xPos, yPos: Single; width, height: Longint); overload;
  
  /// Draw rectangle in the game.
  ///
  /// @lib DrawOrFillRectangleRect(clr, False, source)
  /// @uname DrawRectangleRect
  /// @sn draw:%s rectangle:%s
  procedure DrawRectangle(clr: Color; const source: Rectangle); overload;
  
  /// Fill rectangle.
  ///
  /// @lib DrawOrFillRectangle(clr, True, xPos, yPos, width, height)
  /// @uname FillRectangle
  /// @sn fill:%s rectangleX:%s y:%s width:%s height:%s
  procedure FillRectangle(clr: Color; xPos, yPos: Single; width, height: Longint); overload;
  
  /// Fill a rectangle in the game.
  ///
  /// @lib DrawOrFillRectangleRect(clr, True, source)
  /// @uname FillRectangleRect
  /// @sn fill:%s rectangle:%s
  procedure FillRectangle(clr: Color; const source: Rectangle); overload;
  
  /// Draw a rectangle on the screen (filled or outline).
  ///
  /// @lib DrawOrFillRectangleOnScreen
  /// @sn draw:%s filled:%s rectangleOnScreenX:%s y:%s width:%s height:%s
  procedure DrawRectangleOnScreen(clr : Color; filled : Boolean; xPos, yPos, width, height : Longint); overload;
  
  /// Draw a rectangle on the screen.
  ///
  /// @lib DrawOrFillRectangleOnScreen(clr, False, xPos, yPos, width, height)
  /// @uname DrawRectangleOnScreen
  /// @sn draw:%s rectangleOnScreenX:%s y:%s width:%s height:%s
  procedure DrawRectangleOnScreen(clr : Color; xPos, yPos, width, height : Longint); overload;
  
  /// Fill a rectangle on the screen.
  ///
  /// @lib DrawOrFillRectangleOnScreen(clr, True, xPos, yPos, width, height)
  /// @uname FillRectangleOnScreen
  /// @sn fill:%s rectangleOnScreenX:%s y:%s width:%s height:%s
  procedure FillRectangleOnScreen(clr : Color; xPos, yPos, width, height : Longint); overload;
  
  /// Draw a rectangle on the screen (fill or outline).
  ///
  /// @lib DrawOrFillRectangleRectOnScreen
  /// @sn draw:%s filled:%s rectangleOnScreen:%s
  procedure DrawRectangleOnScreen(clr : Color; filled : Boolean; const source : Rectangle); overload;
  
  /// Draw a rectangle on the screen.
  ///
  /// @lib DrawOrFillRectangleRectOnScreen(clr, False, source)
  /// @uname DrawRectangleRectOnScreen
  /// @sn draw:%s rectangleOnScreen:%s
  procedure DrawRectangleOnScreen(clr : Color; const source : Rectangle); overload;
  
  /// Fill a rectangle on the screen.
  /// 
  /// @lib DrawOrFillRectangleRectOnScreen(clr, True, source)
  /// @uname FillRectangleRectOnScreen
  /// @sn fill:%s rectangleOnScreen:%s
  procedure FillRectangleOnScreen(clr : Color; const source : Rectangle); overload;
  
  
  
//---------------------------------------------------------------------------
// Line drawing
//---------------------------------------------------------------------------
  
  /// Draw a line onto a destination bitmap.
  /// 
  /// @lib DrawLineOnto
  /// @sn drawOnto:%s color:%s lineX1:%s y1:%s x2:%s y2:%s
  procedure DrawLine(dest: Bitmap; clr: Color; xPosStart, yPosStart, xPosEnd, yPosEnd: Longint); overload;
  
  /// Draw a line onto a destination bitmap.
  /// 
  /// @lib DrawLineSegmentOnto
  /// @sn drawOnto:%s color:%s line:%s
  procedure DrawLine(dest: Bitmap; clr: Color; const line: LineSegment); overload;
  
  /// Draw a line onto a destination.
  /// 
  /// @lib DrawLinePtsOnto
  /// @sn drawOnto:%s color:%s lineFromPt:%s toPt:%s
  procedure DrawLine(dest: Bitmap; clr: Color; const startPt, endPt: Point2D); overload;
  
  /// Draw a horizontal line onto a destination.
  /// 
  /// @lib DrawHorizontalLineOnto
  /// @sn drawOnto:%s color:%s horizontalLineY:%s x1:%s x2:%s
  procedure DrawHorizontalLine(dest: Bitmap; clr: Color; y, x1, x2: Longint); overload;
  
  /// Draw a vertical line onto a destination.
  ///
  /// @lib DrawVerticalLineOnto
  /// @sn drawOnto:%s color:%s verticalLineX:%s y1:%s y2:%s
  procedure DrawVerticalLine(dest: Bitmap; clr: Color; x, y1, y2: Longint); overload;
  
  /// Draw a collection of lines.
  /// 
  /// @lib DrawLineSegments
  /// @sn draw:%s lines:%s
  procedure DrawLines(clr: Color; const lines: LinesArray); //overload;
  
  /// Draw a line in the game.
  /// 
  /// @lib
  /// @sn draw:%s lineX1:%s y1:%s x2:%s y2:%s
  procedure DrawLine(clr: Color; xPosStart, yPosStart, xPosEnd, yPosEnd: Single); overload;
  
  /// Draw a line in the game.
  ///
  /// @lib DrawLineSegment
  /// @sn draw:%s line:%s
  procedure DrawLine(clr: Color; const line: LineSegment); overload;
  
  /// Draw a line in the game.
  ///
  /// @lib DrawLinePts
  /// @sn draw:%s lineFromPt:%s toPt:%s
  procedure DrawLine(clr: Color; const startPt, endPt: Point2D); overload;
  
  /// Draw a horizontal line.
  /// 
  /// @lib
  /// @sn draw:%s horizontalLineY:%s x1:%s x2:%s
  procedure DrawHorizontalLine(clr: Color; y, x1, x2: Single); overload;
  
  /// Draw a vertical line in the game.
  /// 
  /// @lib
  /// @sn draw:%s verticalLineX:%s y1:%s y2:%s
  procedure DrawVerticalLine(clr: Color; x, y1, y2: Single); overload;
  
  /// Draw a line on the screen.
  /// 
  /// @lib
  /// @sn draw:%s onScreenX1:%s y1:%s x2:%s y2:%s
  procedure DrawLineOnScreen(clr: Color; xPosStart, yPosStart, xPosEnd, yPosEnd: Longint); overload;
  
  /// Draw a line on the screen.
  ///
  /// @lib DrawLineSegmentOnScreen
  /// @sn draw:%s lineOnScreen:%s
  procedure DrawLineOnScreen(clr: Color; const line: LineSegment); overload;
  
  /// Draw a line on the screen.
  ///
  /// @lib DrawLinePtsOnScreen
  /// @sn draw:%s lineOnScreenFromPt:%s toPt:%s
  procedure DrawLineOnScreen(clr: Color; const startPt, endPt: Point2D); overload;
  
  /// Draw a horizontal line on the screen between x1, x2
  ///
  /// @lib
  /// @sn draw:%s horizontalLineOnScreenY:%s x1:%s x2:%s
  procedure DrawHorizontalLineOnScreen(clr: Color; y, x1, x2: Longint);
  
  /// Draw a vertical line on the screen between y1 and y2.
  ///
  /// @lib
  /// @sn draw:%s verticalLineOnScreenX:%s y1:%s y2:%s
  procedure DrawVerticalLineOnScreen(clr: Color; x, y1, y2: Longint);
  
  
  
//---------------------------------------------------------------------------
// Ellipse drawing
//---------------------------------------------------------------------------
  
  /// Draw the ellipse onto the destination.
  /// 
  /// @lib DrawOrFillEllipseOnto
  /// @sn drawOnto:%s color:%s filled:%s ellipseX:%s y:%s width:%s height:%s
  procedure DrawEllipse(dest: Bitmap; clr: Color; filled: Boolean; xPos, yPos, width, height: Longint); overload;
  
  /// Draw the ellipse onto the destination (filled or outline).
  /// 
  /// @lib DrawOrFillEllipseInRectOnto
  /// @sn drawOnto:%s color:%s filled:%s ellipse:%s
  procedure DrawEllipse(dest: Bitmap; clr: Color; filled: Boolean; const source: Rectangle); overload;
  
  /// Drawthe ellipse onto the destination.
  ///
  /// @lib DrawOrFillEllipseOnto(dest, clr, False, xPos, yPos, width, height)
  /// @uname DrawEllipseOnto
  /// @sn drawOnto:%s color:%s ellipseX:%s y:%s width:%s height:%s
  procedure DrawEllipse(dest: Bitmap; clr: Color; xPos, yPos, width, height: Longint); overload;
  
  /// Draw the ellipse onto the destination.
  /// 
  /// @lib DrawOrFillEllipseInRectOnto(dest, clr, False, source)
  /// @uname DrawEllipseInRectOnto
  /// @sn drawOnto:%s color:%s ellipse:%s
  procedure DrawEllipse(dest: Bitmap; clr: Color; const source: Rectangle); overload;
  
  /// Fill the ellipse onto the destination.
  /// 
  /// @lib DrawOrFillEllipseOnto(dest, clr, True, xPos, yPos, width, height)
  /// @uname FillEllipseOnto
  /// @sn fillOnto:%s color:%s ellipseX:%s y:%s width:%s height:%s
  procedure FillEllipse(dest: Bitmap; clr: Color; xPos, yPos, width, height: Longint); overload;
  
  /// Fill the ellipse onto the destination.
  /// 
  /// @lib DrawOrFillEllipseInRectOnto(dest, clr, True, source)
  /// @uname FillEllipseInRectOnto
  /// @sn fillOnto:%s color:%s ellipse:%s
  procedure FillEllipse(dest: Bitmap; clr: Color; const source: Rectangle); overload;
  
  /// Draw an ellipse in the game (filled or outline).
  ///
  /// @lib DrawOrFillEllipse
  /// @sn draw:%s filled:%s x:%s y:%s width:%s height:%s
  procedure DrawEllipse(clr: Color; filled: Boolean; xPos, yPos: Single; width, height: Longint); overload;
  
  /// Draw an ellipse in the game (filled or outline).
  ///
  /// @lib DrawOrFillEllipseInRect
  /// @sn draw:%s filled:%s ellipse:%s
  procedure DrawEllipse(clr: Color; filled: Boolean; const source: Rectangle); overload;
    
  /// Draw an ellipse on the screen.
  ///
  /// @lib DrawOrFillEllipse(clr, False, xPos, yPos, width, height)
  /// @uname DrawEllipse
  /// @sn draw:%s x:%s y:%s width:%s height:%s
  procedure DrawEllipse(clr: Color; xPos, yPos: Single; width, height: Longint); overload;
  
  /// Draw an ellipse in the game.
  /// 
  /// @lib DrawOrFillEllipseInRect(clr, False, source)
  /// @uname DrawEllipseInRect
  /// @sn draw:%s ellipse:%s
  procedure DrawEllipse(clr: Color; const source: Rectangle); overload;
  
  /// Fill a ellipse in the game.
  ///
  /// @lib DrawOrFillEllipse(clr, True, xPos, yPos, width, height)
  /// @uname FillEllipse
  /// @sn fill:%s x:%s y:%s width:%s height:%s
  procedure FillEllipse(clr: Color; xPos, yPos: Single; width, height: Longint); overload;
  
  /// Fill a ellipse in the game.
  ///
  /// @lib DrawOrFillEllipseInRect(clr, True, source)
  /// @uname FillEllipseInRect
  /// @sn fill:%s ellipse:%s
  procedure FillEllipse(clr: Color; const source: Rectangle); overload;
  
  /// Draw an ellipse on the screen (filled or outline).
  /// 
  /// @lib DrawOrFillEllipseOnScreen
  /// @sn draw:%s filled:%s ellipseOnScreenX:%s y:%s width:%s height:%s
  procedure DrawEllipseOnScreen(clr: Color; filled: Boolean; xPos, yPos, width, height: Longint); overload;
  
  /// Draw an ellipse on screen.
  ///
  /// @lib DrawOrFillEllipseOnScreen(clr, False, xPos, yPos, width, height)
  /// @uname DrawEllipseOnScreen
  /// @sn draw:%s ellipseOnScreenX:%s y:%s width:%s height:%s
  procedure DrawEllipseOnScreen(clr: Color; xPos, yPos, width, height: Longint); overload;
  
  /// Fills an ellipse on the screen.
  ///
  /// @lib DrawOrFillEllipseOnScreen(clr, True, xPos, yPos, width, height)
  /// @uname FillEllipseOnScreen
  /// @sn fill:%s ellipseOnScreenX:%s y:%s width:%s height:%s
  procedure FillEllipseOnScreen(clr: Color; xPos, yPos, width, height: Longint); overload;
  
  /// Draw an ellpse on the screen (filled or outline).
  ///
  /// @lib DrawOrFillEllipseInRectOnScreen
  /// @sn draw:%s filled:%s ellipseOnScreen:%s
  procedure DrawEllipseOnScreen(clr: Color; filled: Boolean; const source: Rectangle); overload;
  
  /// Draw an ellipse on the screen.
  ///
  /// @lib DrawOrFillEllipseInRectOnScreen(clr, False, source)
  /// @uname DrawEllipseInRectOnScreen
  /// @sn draw:%s ellipseOnScreen:%s
  procedure DrawEllipseOnScreen(clr: Color; const source: Rectangle); overload;
  
  /// Fills the ellipse on screen.
  /// 
  /// @lib DrawOrFillEllipseInRectOnScreen(clr, True, source)
  /// @uname FillEllipseInRectOnScreen
  /// @sn fill:%s ellipseOnScreen:%s
  procedure FillEllipseOnScreen(clr: Color; const source: Rectangle); overload;
  
  
  
//---------------------------------------------------------------------------
// Clipping
//---------------------------------------------------------------------------
  
  /// Push a clip rectangle to the screen. This can be undone using PopClip.
  ///
  /// @lib PushClipXY
  /// @sn pushClipX:%s y:%s width:%s height:%s
  procedure PushClip(x, y, w, h: Longint); overload;
  
  /// Push a clip rectangle to the screen. This can be undone using PopClip.
  ///
  /// @lib PushClipRect
  /// @sn pushClip:%s
  procedure PushClip(const r: Rectangle); overload;

  /// Add the clipping rectangle of a bitmap and uses the intersect between the new rectangle and previous clip.
  /// 
  /// @lib PushClipRectForBitmap
  /// @sn bitmap:%s PushClipRect:%s
  ///
  /// @class Bitmap
  /// @overload PushClip PushClipRect
  /// @csn pushClip:%s
  procedure PushClip(bmp: Bitmap; const r: Rectangle); overload;  
  
  /// Reset the clipping rectangle of the screen.
  /// 
  /// @lib
  procedure ResetClip(); overload;
  
  /// Reset the clipping rectangle on a bitmap.
  ///
  /// @lib ResetClipForBitmap
  ///
  /// @class Bitmap
  /// @method ResetClip
  procedure ResetClip(bmp: Bitmap); overload;

  /// Set the clip rectangle of the bitmap.
  ///
  /// @lib SetBmpClip
  /// @sn bitmap:%s setClip:%s
  ///
  /// @class Bitmap
  /// @method SetClip
  procedure SetClip(bmp: Bitmap; const r: Rectangle); overload;

  /// Set the clip rectangle of the screen.
  ///
  /// @lib SetClip
  /// @sn setClip:%s
  procedure SetClip(const r: Rectangle); overload;
  
  /// Set the clip rectangle of the screen.
  ///
  /// @lib SetClipXY
  /// @sn setClipX:%s y:%s width:%s height:%s
  procedure SetClip(x, y, w, h: Longint); overload;

  /// Set the clip rectangle of the bitmap.
  ///
  /// @lib SetBmpClipXY
  /// @sn bitmap:%s setClipX:%s y:%s width:%s height:%s
  ///
  /// @class Bitmap
  /// @overload SetClip SetClipXY
  /// @csn setClipX:%s y:%s width:%s height:%s
  procedure SetClip(bmp: Bitmap; x, y, w, h: Longint); overload;
  
  /// Pop the clip rectangle of the screen.
  ///
  /// @lib PopClipScreen
  procedure PopClip(); overload;

  /// Pop the clipping rectangle of a bitmap.
  /// 
  /// @lib PopClipBmp
  /// @sn PopClipBitmap:%s
  ///
  /// @class Bitmap
  /// @method PopClip
  procedure PopClip(bmp: Bitmap); overload;

  /// Returns the rectangle of the currentl clip of bitmap
  ///
  /// @lib CurrentBmpClip
  /// @sn currentClip:%s
  ///
  /// @class Bitmap
  /// @getter CurrentClip
  function CurrentClip(bmp: Bitmap): Rectangle; overload;

  /// Returns the rectangle of the currentl clip of bitmap
  ///
  /// @lib CurrentScreenClip
  ///
  function CurrentClip(): Rectangle; overload;
  
  
  
//---------------------------------------------------------------------------
// Pixel reading functions
//---------------------------------------------------------------------------
  
  /// Returns the color of the pixel at the x,y location on
  /// the supplied bitmap.
  /// 
  /// @lib
  /// @sn bitmap:%s colorAtX:%s y:%s
  ///
  /// @class Bitmap
  /// @method GetPixel
  /// @csn colorAtX:%s y:%s
  function GetPixel(bmp: Bitmap; x, y: Longint): Color;
  
  /// Returns the color of the pixel at the given x,y location.
  ///
  /// @lib
  /// @sn colorOnScreenAtX:%s y:%s
  function GetPixelFromScreen(x, y: Longint): Color;
  
  
  var
    //Preset colours, do not change these values.
    ColorBlue, ColorGreen, ColorRed, ColorWhite, ColorBlack, ColorYellow,
    ColorPink, ColorTurquoise, ColorGrey, ColorMagenta, ColorTransparent,
    ColorLightGrey: Color;
  
//=============================================================================
implementation
//=============================================================================

  uses Math, Classes, SysUtils, // system
       SDL_gfx, SDL, SDL_Image, // sdl
       sgSavePNG, 
       sgTrace, 
       sgCamera, sgPhysics, sgShared, sgGeometry, sgResources, sgImages, sgUtils, sgUserInterface;

  /// Clears the surface of the screen to the passed in color.
  ///
  /// @param toColor: The colour to clear the bitmap to
  ///
  /// Side Effects:
  /// - Screen's surface is set to the toColor
  procedure ClearScreen(toColor : Color); overload;
  begin
    ClearSurface(screen, toColor);
  end;

  /// Clears the screen to Black.
  ///
  /// Side Effects:
  /// - screen's surface is set to black
  procedure ClearScreen(); overload;
  begin
    ClearScreen(ColorBlack);
  end;

  function GetPixel(bmp: Bitmap; x, y: Longint): Color;
  begin
    if not Assigned(bmp) then begin RaiseException('No bitmap supplied'); exit; end;

    if (x < 0) or (x >= bmp^.width) or (y < 0) or (y >= bmp^.height) then
    begin
      result := 0;
      exit;
    end;

    result := GetPixel32(bmp^.surface, x, y);
  end;

  function GetPixelFromScreen(x, y: Longint): Color;
  begin
    result := GetPixel(screen, x, y);
  end;

  procedure PutPixel(bmp: Bitmap; value: Color; x, y: Longint);
  var
    clr:  Color;
    p:    ^Color;
    bpp:  Longint;
  begin
    if not assigned(bmp) then exit;
    
    clr := ColorFrom(bmp, value);
    bpp := bmp^.surface^.format^.BytesPerPixel;
    // Here p is the address to the pixel we want to set
    p := bmp^.surface^.pixels + y * bmp^.surface^.pitch + x * bpp;
    
    if bpp <> 4 then RaiseException('PutPixel only supported on 32bit images.');
    p^ := clr;
  end;
  
  /// Draws a pixel onto the screen.
  ///
  /// @param clr:     The color to draw the pixel
  /// @param x,y:         The x,y location to draw the pixel at
  ///
  /// Side Effects:
  /// - Sets one pixel on the screen
  procedure DrawPixelOnScreen(clr: Color; x, y: Longint);
  begin
    DrawPixel(screen, clr, x, y);
  end;

  procedure DrawPixel(clr: Color; x, y: Single); overload;
  begin
    DrawPixelOnScreen(clr, sgCamera.ToScreenX(x), sgCamera.ToScreenY(y));
  end;

  /// Draws a rectangle on the screen.
  ///
  /// @param clr:     The color to draw the rectangle
  /// @param filled:       True to draw a filled rectangle, false for outline
  /// @param xPos,yPos:   The x,y location to draw the rectangle at
  /// @param width,height: The width and height of the rectangle
  ///
  /// Side Effects:
  /// - Draws a rectangle in the dest bitmap
  procedure DrawRectangleOnScreen(clr : Color; filled : Boolean; xPos, yPos, width, height : Longint); overload;
  begin
    DrawRectangle(screen, clr, filled, xPos, yPos, width, height);
  end;

  procedure DrawRectangle(clr : Color; filled : Boolean; xPos, yPos: Single; width, height : Longint); overload;
  begin
    DrawRectangle(screen, clr, filled, sgCamera.ToScreenX(xPos), sgCamera.ToScreenY(yPos), width, height);
  end;

  /// Draws the outline of a rectangle on the screen.
  ///
  /// @param clr:     The color to draw the rectangle
  /// @param xPos,yPos:   The x,y location to draw the rectangle at
  /// @param width,height: The width and height of the rectangle
  ///
  /// Side Effects:
  /// - Draws a rectangle on the screen
  procedure DrawRectangleOnScreen(clr : Color; xPos, yPos, width, height : Longint); overload;
  begin
    DrawRectangle(screen, clr, xPos, yPos, width, height);
  end;
  
  procedure DrawRectangle(clr: Color; xPos, yPos: Single; width, height : Longint); overload;
  begin
    DrawRectangle(screen, clr, sgCamera.ToScreenX(xPos), sgCamera.ToScreenY(yPos), width, height);
  end;

  /// Draws a filled rectangle on the screen.
  ///
  /// @param clr:     The color to draw the rectangle
  /// @param xPos,yPos:   The x,y location to draw the rectangle at
  /// @param width,height: The width and height of the rectangle
  ///
  /// Side Effects:
  /// - Draws a rectangle on the screen
  procedure FillRectangleOnScreen(clr : Color; xPos, yPos, width, height : Longint); overload;
  begin
    FillRectangle(screen, clr, xPos, yPos, width, height);
  end;

  procedure FillRectangle(clr : Color; xPos, yPos: Single; width, height : Longint); overload;
  begin
    FillRectangle(screen, clr, sgCamera.ToScreenX(xPos), sgCamera.ToScreenY(yPos), width, height);
  end;

  /// Draws a line on the screen.
  ///
  /// @param clr:     The color to draw the line
  /// @param xPosStart,yPosStart: The x,y location to start the line at
  /// @param xPosEnd, yPosEnd:    The x,y location to end the line at
  ///
  /// Side Effects:
  /// - Draws a line in the screen
  procedure DrawLineOnScreen(clr: Color; xPosStart, yPosStart, xPosEnd, yPosEnd: Longint); overload;
  begin
    DrawLine(screen, clr, xPosStart, yPosStart, xPosEnd, yPosEnd);
  end;
  
  procedure DrawLine(clr: Color; xPosStart, yPosStart, xPosEnd, yPosEnd: Single); overload;
  begin
    DrawLine(screen, clr, sgCamera.ToScreenX(xPosStart), sgCamera.ToScreenY(yPosStart), sgCamera.ToScreenX(xPosEnd), sgCamera.ToScreenY(yPosEnd));
  end;
  
  procedure DrawLine(clr: Color; const line: LineSegment); overload;
  begin
    DrawLine(clr, line.startPoint.x, line.startPoint.y, line.endPoint.x, line.endPoint.y);
  end;
  
  procedure DrawLine(clr: Color; const startPt, endPt: Point2D); overload;
  begin
    DrawLine(clr, startPt.x, startPt.y, endPt.x, endPt.y);
  end;
  
  procedure DrawLine(dest: Bitmap; clr: Color; const line: LineSegment); overload;
  begin
    DrawLine(dest, clr, Round(line.startPoint.x), Round(line.startPoint.y), Round(line.endPoint.x), Round(line.endPoint.y));
  end;
  
  procedure DrawLine(dest: Bitmap; clr: Color; const startPt, endPt: Point2D); overload;
  begin
    DrawLine(dest, clr, Round(startPt.x), Round(startPt.y), Round(endPt.x), Round(endPt.y));
  end;
  
  procedure DrawTriangle(clr: Color; filled: Boolean; const tri: Triangle); overload;
  begin
    if filled then FillTriangle(clr, tri)
    else DrawTriangle(clr, tri);
  end;

  procedure DrawTriangle(dest: Bitmap; clr: Color; filled: Boolean; const tri: Triangle); overload;
  begin
    if filled then FillTriangle(dest, clr, tri)
    else DrawTriangle(dest, clr, tri);
  end;
  
  procedure DrawTriangle(dest: Bitmap; clr: Color; const tri: Triangle); overload;
  begin
    DrawTriangle(dest, clr, tri[0].x, tri[0].y, tri[1].x, tri[1].y, tri[2].x, tri[2].y);
  end;

  procedure DrawTriangleOnScreen(clr: Color; filled: Boolean; const tri: Triangle); overload;
  begin
    if filled then FillTriangleOnScreen(clr, tri) 
    else DrawTriangleOnScreen(clr, tri);
  end;

  procedure DrawTriangleOnScreen(clr: Color; const tri: Triangle); overload;
  begin
    DrawTriangle(screen, clr, tri[0].x, tri[0].y, tri[1].x, tri[1].y, tri[2].x, tri[2].y);
  end;

  procedure DrawTriangle(clr: Color; const tri: Triangle); overload;
  begin
    DrawTriangle(clr, tri[0].x, tri[0].y, tri[1].x, tri[1].y, tri[2].x, tri[2].y);
  end;
  
  procedure DrawTriangle(clr: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  begin
    DrawTriangle(screen, clr, sgCamera.ToScreenX(x1), sgCamera.ToScreenY(y1), sgCamera.ToScreenX(x2), sgCamera.ToScreenY(y2), sgCamera.ToScreenX(x3), sgCamera.ToScreenY(y3));
  end;
  
  procedure DrawTriangleOnScreen(clr: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  begin
    DrawTriangle(screen, clr, x1, y1, x2, y2, x3, y3);
  end;

  procedure DrawTriangle(dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  begin
    trigonColor(dest^.surface, Round(x1), Round(y1), Round(x2), Round(y2), Round(x3), Round(y3), ToGfxColor(clr));
  end;

  procedure FillTriangle(dest: Bitmap; clr: Color; const tri: Triangle); overload;
  begin
    FillTriangle(dest, clr, tri[0].x, tri[0].y, tri[1].x, tri[1].y, tri[2].x, tri[2].y);
  end;
  
  procedure FillTriangle(clr: Color; const tri: Triangle); overload;
  begin
    FillTriangle(clr, tri[0].x, tri[0].y, tri[1].x, tri[1].y, tri[2].x, tri[2].y);
  end;

  procedure FillTriangle(clr: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  begin
    FillTriangle(screen, clr, sgCamera.ToScreenX(x1), sgCamera.ToScreenY(y1), sgCamera.ToScreenX(x2), sgCamera.ToScreenY(y2), sgCamera.ToScreenX(x3), sgCamera.ToScreenY(y3));
  end;

  procedure FillTriangleOnScreen(clr: Color; const tri: Triangle); overload;
  begin
    FillTriangle(screen, clr, tri[0].x, tri[0].y, tri[1].x, tri[1].y, tri[2].x, tri[2].y);
  end;
  
  procedure FillTriangleOnScreen(clr: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  begin
    FillTriangle(screen, clr, x1, y1, x2, y2, x3, y3);
  end;

  procedure FillTriangle(dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  begin
    filledTrigonColor(dest^.surface, Round(x1), Round(y1), Round(x2), Round(y2), Round(x3), Round(y3), ToGfxColor(clr));
  end;

  //=============================================================================
  
  procedure DrawHorizontalLineOnScreen(clr: Color; y, x1, x2: Longint); overload;
  begin
    DrawHorizontalLine(screen, clr, y, x1, x2);
  end;

  procedure DrawHorizontalLine(clr: Color; y, x1, x2: Single); overload;
  begin
    DrawHorizontalLine(screen, clr, sgCamera.ToScreenY(y), sgCamera.ToScreenX(x1), sgCamera.ToScreenX(x2));
  end;

  procedure DrawVerticalLineOnScreen(clr: Color; x, y1, y2: Longint); overload;
  begin
    DrawVerticalLine(screen, clr, x, y1, y2);
  end;

  procedure DrawVerticalLine(clr: Color; x, y1, y2: Single); overload;
  begin
    DrawVerticalLine(screen, clr, sgCamera.ToScreenX(x), sgCamera.ToScreenY(y1), sgCamera.ToScreenY(y2));
  end;
  
  //=============================================================================
  
  procedure DrawCircleOnScreen(clr: Color; filled: Boolean; xc, yc: Single; radius: Longint); overload;
  begin
    DrawCircle(screen, clr, filled, xc, yc, radius);
  end;
  
  procedure DrawCircle(clr: Color; filled: Boolean; xc, yc: Single; radius: Longint); overload;
  begin
    DrawCircle(screen, clr, filled, sgCamera.ToScreenX(xc), sgCamera.ToScreenY(yc), radius);
  end;
  
  procedure DrawCircleOnScreen(clr: Color; xc, yc: Single; radius: Longint); overload;
  begin
    DrawCircle(screen, clr, xc, yc, radius);
  end;
  
  procedure DrawCircle(clr: Color; xc, yc: Single; radius: Longint); overload;
  begin
    DrawCircle(screen, clr, sgCamera.ToScreenX(xc), sgCamera.ToScreenY(yc), radius);
  end;

  procedure FillCircleOnScreen(clr: Color; xc, yc: Single; radius: Longint); overload;
  begin
    FillCircle(screen, clr, xc, yc, radius);
  end;

  procedure FillCircle(clr: Color; xc, yc: Single; radius: Longint); overload;
  begin
    FillCircle(screen, clr, sgCamera.ToScreenX(xc), sgCamera.ToScreenY(yc), radius);
  end;
  
  procedure DrawCircle(dest: Bitmap; clr: Color; filled: Boolean; xc, yc: Single; radius: Longint); overload;
  begin
    if filled then FillCircle(dest, clr, xc, yc, radius)
    else DrawCircle(dest, clr, xc, yc, radius);
  end;

  procedure DrawCircle(dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint); overload;
  begin
    aacircleColor(dest^.surface, Round(xc), Round(yc), Abs(radius), ToGfxColor(clr));
  end;

  procedure FillCircle(dest: Bitmap; clr: Color; xc, yc: Single; radius: Longint);
  begin
    filledCircleColor(dest^.surface, Round(xc), Round(yc), Abs(radius), ToGfxColor(clr));
  end;
  
  procedure DrawCircle(dest: Bitmap; clr: Color; filled: Boolean; const c: Circle); overload;
  begin
    DrawCircle(dest, clr, filled, c.center.x, c.center.y, c.radius);
  end;
  
  procedure DrawCircle(dest: Bitmap; clr: Color; const c: Circle); overload;
  begin
    DrawCircle(dest, clr, False, c.center.x, c.center.y, c.radius)
  end;
  
  procedure FillCircle(dest: Bitmap; clr: Color; const c: Circle); overload;
  begin
    DrawCircle(dest, clr, True, c.center.x, c.center.y, c.radius)
  end;
  
  procedure DrawCircle(clr: Color; filled: Boolean; const c: Circle); overload;
  begin
    DrawCircle(screen, clr, filled, sgCamera.ToScreenX(c.center.x), sgCamera.ToScreenY(c.center.y), c.radius);
  end;

  procedure DrawCircle(clr: Color; const c: Circle); overload;
  begin
    DrawCircle(screen, clr, False, sgCamera.ToScreenX(c.center.x), sgCamera.ToScreenY(c.center.y), c.radius);
  end;
  
  procedure FillCircle(clr: Color; const c: Circle); overload;
  begin
    DrawCircle(screen, clr, True, sgCamera.ToScreenX(c.center.x), sgCamera.ToScreenY(c.center.y), c.radius);
  end;
  
  procedure DrawCircleOnScreen(clr: Color; filled: Boolean; const c: Circle); overload;
  begin
    DrawCircle(screen, clr, filled, c.center.x, c.center.y, c.radius);
  end;
  
  procedure DrawCircleOnScreen(clr: Color; const c: Circle); overload;
  begin
    DrawCircle(screen, clr, False, c.center.x, c.center.y, c.radius);
  end;

  procedure FillCircleOnScreen(clr: Color; const c: Circle); overload;
  begin
    DrawCircle(screen, clr, True, c.center.x, c.center.y, c.radius);
  end;
  
  //=============================================================================
  
  procedure DrawEllipseOnScreen(clr: Color; filled: Boolean; xPos, yPos, width, height: Longint); overload;
  begin
    DrawEllipse(screen, clr, filled, xPos, yPos, width, height);
  end;

  procedure DrawEllipse(clr: Color; filled: Boolean; xPos, yPos: Single; width, height: Longint); overload;
  begin
    DrawEllipse(screen, clr, filled, sgCamera.ToScreenX(xPos), sgCamera.ToScreenY(yPos), width, height);
  end;

  procedure DrawEllipseOnScreen(clr: Color; xPos, yPos, width, height: Longint); overload;
  begin
    DrawEllipse(screen, clr, xPos, yPos, width, height);
  end;

  procedure DrawEllipse(clr: Color; xPos, yPos: Single; width, height: Longint); overload;
  begin
    DrawEllipse(screen, clr, sgCamera.ToScreenX(xPos), sgCamera.ToScreenY(yPos), width, height);
  end;
  
  procedure FillEllipseOnScreen(clr: Color;  xPos, yPos, width, height: Longint); overload;
  begin
    FillEllipse(screen, clr, xPos, yPos, width, height);
  end;
  
  procedure FillEllipse(clr: Color;  xPos, yPos: Single; width, height: Longint); overload;
  begin
    FillEllipse(screen, clr, sgCamera.ToScreenX(xPos), sgCamera.ToScreenY(yPos), width, height);
  end;
  
  procedure DrawEllipse(dest: Bitmap; clr: Color; filled: Boolean; xPos, yPos, width, height: Longint); overload;
  begin
    if filled then FillEllipse(dest, clr, xPos, yPos, width, height)
    else DrawEllipse(dest, clr, xPos, yPos, width, height);
  end;
  
  procedure DrawEllipse(dest: Bitmap; clr: Color;  xPos, yPos, width, height: Longint); overload;
  var
    halfWidth, halfHeight: Sint16;
  begin
    if width < 0 then
    begin
      xPos += width;
      width := -width;
    end;
    if height < 0 then
    begin
      yPos += height;
      height := -height;
    end;

    halfWidth := width div 2;
    halfHeight := height div 2;
    
    aaellipseColor(dest^.surface, xPos + halfWidth, yPos + halfHeight, halfWidth, halfHeight, ToGfxColor(clr));
  end;

  procedure FillEllipse(dest: Bitmap; clr: Color; xPos, yPos, width, height: Longint);
  var
    halfWidth, halfHeight: Sint16;
  begin
    if width < 0 then
    begin
      xPos += width;
      width := -width;
    end;
    if height < 0 then
    begin
      yPos += height;
      height := -height;
    end;

    halfWidth := width div 2;
    halfHeight := height div 2;
    
    filledEllipseColor(dest^.surface, xPos + halfWidth, yPos + halfHeight, halfWidth, halfHeight, ToGfxColor(clr));
  end;

  //=============================================================================
  
  procedure DrawRectangle(dest: Bitmap; clr: Color; filled : Boolean; xPos, yPos, width, height : Longint); overload;
  begin
    if filled then FillRectangle(dest, clr, xPos, yPos, width, height)
    else DrawRectangle(dest, clr, xPos, yPos, width, height);
  end;

  procedure DrawRectangle(dest: Bitmap; clr : Color; xPos, yPos, width, height : Longint); overload;
  begin
    if dest = nil then begin RaiseException('No destination bitmap supplied'); exit; end;
    rectangleColor(dest^.surface, xPos, yPos, xPos + width - 1, yPos + height - 1, ToGfxColor(clr));
  end;

  procedure FillRectangle(dest: Bitmap; clr : Color;  xPos, yPos, width, height : Longint);
  var
    rect: SDL_Rect;
  begin
    if dest = nil then begin RaiseException('No destination bitmap supplied'); exit; end;
    
    if width < 0 then
    begin
      rect.x := xPos + width; //move back by width
      width := -width;
    end else rect.x := xPos;
    
    if height < 0 then
    begin
      rect.y := yPos + height; //move up by height
      height := -height;
    end else rect.y := yPos;
    
    rect.w := width;
    rect.h := height;
    
    //SDL_FillRect(dest^.surface, @rect, clr);
    boxColor(dest^.surface, rect.x, rect.y, rect.x + width - 1, rect.y + height - 1, ToGfxColor(clr));
  end;
  
  /// Draws a vertical line on the destination bitmap.
  ///
  /// @param dest:         The destination bitmap - not optimised!
  /// @param clr:     The color to draw the line
  /// @param x:           The x location of the line
  /// @param y1, y2:       The starting and ending y value of the line
  ///
  /// Side Effects:
  /// - Draws a line in the dest bitmap
  procedure DrawVerticalLine(dest: Bitmap; clr: Color; x, y1, y2: Longint);
  begin
    if dest = nil then begin RaiseException('The destination bitmap to draw a vertical line is nil'); exit; end;
    vlineColor(dest^.surface, x, y1, y2, ToGfxColor(clr));
  end;

  /// Draws a horizontal line on the destination bitmap.
  ///
  /// @param dest:         The destination bitmap - not optimised!
  /// @param clr:     The color to draw the line
  /// @param y:           The y location of the line
  /// @param x1, x2:       The starting and ending x value of the line
  ///
  /// Side Effects:
  /// - Draws a line in the dest bitmap
  procedure DrawHorizontalLine(dest: Bitmap; clr: Color; y, x1, x2: Longint);
  begin
    if dest = nil then begin RaiseException('The destination bitmap to draw a vertical line is nil'); exit; end;
      
    hlineColor(dest^.surface, x1, x2, y, ToGfxColor(clr));
  end;

  /// Draws a line on the destination bitmap.
  ///
  /// @param dest:         The destination bitmap - not optimised!
  /// @param clr:     The color to draw the line
  /// @param xPosStart,yPosStart: The x,y location to start the line at
  /// @param xPosEnd, yPosEnd:    The x,y location to end the line at
  ///
  /// Side Effects:
  /// - Draws a line in the dest bitmap
  procedure DrawLine(dest: Bitmap; clr: Color; xPosStart, yPosStart, xPosEnd, yPosEnd: Longint);
  begin
    try
      aalineColor(dest^.surface, xPosStart, yPosStart, xPosEnd, yPosEnd, ToGfxColor(clr));
    except
    end;
  end;

  /// Draws a pixel onto the destination bitmap.
  ///
  /// @param dest:         The destination bitmap - not optimised!
  /// @param clr:     The color to draw the pixel
  /// @param x,y:         The x,y location to draw the pixel at
  ///
  /// Side Effects:
  /// - Sets one pixel on the destination bitmap
  procedure DrawPixel(dest: Bitmap; clr: Color; x, y: Longint); overload;
  begin
    if dest = nil then begin RaiseException('The destination bitmap to draw a pixel is nil'); exit; end;
    
    if (x < 0) or (x >= dest^.surface^.w) or (y < 0) or (y >= dest^.surface^.h) then exit;
    
    //if SDL_MUSTLOCK(dest^.surface) then SDL_LockSurface(dest^.surface);
    
    pixelColor(dest^.surface, x, y, ToGfxColor(clr));
    
    //if SDL_MUSTLOCK(dest^.surface) then SDL_UnlockSurface(dest^.surface);
  end;
  
  
  procedure DrawPixel(dest: Bitmap; clr: Color; const position : Point2D); overload;
  begin
    DrawPixel(dest, clr, Round(position.x), Round(position.y));
  end;

  procedure DrawRectangle(dest: Bitmap; clr : Color; filled : Boolean; const source: Rectangle); overload;
  begin
    DrawRectangle(dest, clr, filled, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure DrawRectangle(dest: Bitmap; clr : Color; const source: Rectangle); overload;
  begin
    DrawRectangle(dest, clr, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure FillRectangle(dest: Bitmap; clr : Color; const source: Rectangle); overload;
  begin
    FillRectangle(dest, clr, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure DrawCircle(dest: Bitmap; clr: Color; filled: Boolean; const point: Point2D; radius: Longint); overload;
  begin
    DrawCircle(dest, clr, filled, Round(point.x), Round(point.y), radius);
  end;

  procedure DrawCircle(dest: Bitmap; clr: Color; const point: Point2D; radius: Longint); overload;
  begin
    DrawCircle(dest, clr, Round(point.x), Round(point.y), radius);
  end;

  procedure FillCircle(dest: Bitmap; clr: Color; const point: Point2D; radius: Longint); overload;
  begin
    FillCircle(dest, clr, Round(point.x), Round(point.y), radius);
  end;

  procedure DrawEllipse(dest: Bitmap; clr: Color; filled: Boolean; const source: Rectangle); overload;
  begin
    DrawEllipse(dest, clr, filled, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure DrawEllipse(dest: Bitmap; clr: Color; const source: Rectangle); overload;
  begin
    DrawEllipse(dest, clr, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure FillEllipse(dest: Bitmap; clr: Color; const source: Rectangle); overload;
  begin
    FillEllipse(dest, clr, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure DrawPixel(clr: Color; const position: Point2D); overload;
  begin
    DrawPixel(clr, Round(position.x), Round(position.y));
  end;

  procedure DrawRectangle(clr : Color; filled : Boolean; const source : Rectangle); overload;
  begin
    DrawRectangle(clr, filled, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure DrawRectangle(clr : Color; const source : Rectangle); overload;
  begin
    DrawRectangle(clr, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure FillRectangle(clr : Color; const source : Rectangle); overload;
  begin
    FillRectangle(clr, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure DrawCircle(clr: Color; filled: Boolean; const position: Point2D; radius: Longint); overload;
  begin
    DrawCircle(clr, filled, Round(position.x), Round(position.y), radius);
  end;

  procedure DrawCircle(clr: Color; const position: Point2D; radius: Longint); overload;
  begin
    DrawCircle(clr, Round(position.x), Round(position.y), radius);
  end;

  procedure FillCircle(clr: Color; const position: Point2D; radius: Longint); overload;
  begin
    FillCircle(clr, position.x, position.y, radius);
  end;

  procedure DrawEllipse(clr: Color; filled: Boolean; const source: Rectangle); overload;
  begin
    DrawEllipse(clr, filled, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure DrawEllipse(clr: Color; const source: Rectangle); overload;
  begin
    DrawEllipse(clr, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure FillEllipse(clr: Color; const source: Rectangle); overload;
  begin
    FillEllipse(clr, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure DrawPixelOnScreen(clr: Color; const position: Point2D); overload;
  begin
    DrawPixelOnScreen(clr, Round(position.x), Round(position.y));
  end;

  procedure DrawRectangleOnScreen(clr : Color; filled : Boolean; const source : Rectangle); overload;
  begin
    DrawRectangleOnScreen(clr, filled, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure DrawRectangleOnScreen(clr : Color; const source : Rectangle); overload;
  begin
    DrawRectangleOnScreen(clr, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure FillRectangleOnScreen(clr : Color; const source : Rectangle); overload;
  begin
    FillRectangleOnScreen(clr, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure DrawLineOnScreen(clr: Color; const line: LineSegment); overload;
  begin
    DrawLineOnScreen(clr, Round(line.startPoint.x), Round(line.startPoint.y), Round(line.endPoint.x), Round(line.endPoint.y));
  end;
  
  procedure DrawLineOnScreen(clr: Color; const startPt, endPt: Point2D); overload;
  begin
    DrawLineOnScreen(clr, Round(startPt.x), Round(startPt.y), Round(endPt.x), Round(endPt.y));
  end;
  
  procedure DrawCircleOnScreen(clr: Color; filled: Boolean; const position: Point2D; radius: Longint); overload;
  begin
    DrawCircleOnScreen(clr, filled, Round(position.x), Round(position.y), radius);
  end;

  procedure DrawCircleOnScreen(clr: Color; const position: Point2D; radius: Longint); overload;
  begin
    DrawCircleOnScreen(clr, Round(position.x), Round(position.y), radius);
  end;

  procedure FillCircleOnScreen(clr: Color; const position: Point2D; radius: Longint); overload;
  begin
    FillCircleOnScreen(clr, Round(position.x), Round(position.y), radius);
  end;

  procedure DrawEllipseOnScreen(clr: Color; filled: Boolean; const source: Rectangle); overload;
  begin
    DrawEllipseOnScreen(clr, filled, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure DrawEllipseOnScreen(clr: Color; const source: Rectangle); overload;
  begin
    DrawEllipseOnScreen(clr, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure FillEllipseOnScreen(clr: Color; const source: Rectangle); overload;
  begin
    FillEllipseOnScreen(clr, Round(source.x), Round(source.y), source.width, source.height);
  end;
  
  procedure ResetClip(bmp: Bitmap); overload;
  begin
    if bmp = nil then exit;
    
    SetLength(bmp^.clipStack, 0);
    SDL_SetClipRect(bmp^.surface, nil);
  end;

  procedure ResetClip(); overload;
  begin
    ResetClip(screen);
  end;
  
  procedure DoSetClip(bmp: Bitmap; const r: Rectangle); overload;
  var
    rect: SDL_Rect;
  begin
    if bmp = nil then begin RaiseException('Cannot set clip, bmp must not be nil'); exit; end;
    
    rect := NewSDLRect(Round(r.x), Round(r.y), r.width, r.height);
    SDL_SetClipRect(bmp^.surface, @rect);
  end;
  
  procedure PushClip(bmp: Bitmap; const r: Rectangle); overload;
  begin
    SetLength(bmp^.clipStack, Length(bmp^.clipStack) + 1);
    
    if Length(bmp^.clipStack) > 1 then
    begin
      // WriteLn('Adding clip ', RectangleToString(r));
      // WriteLn('Was         ', RectangleToString(bmp^.clipStack[High(bmp^.clipStack) - 1]));
      bmp^.clipStack[high(bmp^.clipStack)] := Intersection(r, bmp^.clipStack[High(bmp^.clipStack) - 1]);
      // WriteLn('Now         ', RectangleToString(bmp^.clipStack[High(bmp^.clipStack)]));
    end
    else
      bmp^.clipStack[high(bmp^.clipStack)] := r;
    
    DoSetClip(bmp, bmp^.clipStack[high(bmp^.clipStack)]);
  end;

  procedure SetClip(x, y, w, h: Longint); overload;
  begin
    SetClip(screen, RectangleFrom(x, y, w, h));
  end;
  
  procedure SetClip(bmp: Bitmap; x, y, w, h: Longint); overload;
  begin
    SetClip(bmp, RectangleFrom(x, y, w, h));
  end;

  procedure PushClip(x, y, w, h: Longint); overload;
  begin
    PushClip(screen, RectangleFrom(x, y, w, h));
  end;
  
  procedure PushClip(const r: Rectangle); overload;
  begin
    PushClip(screen, r);
  end;

  procedure SetClip(bmp: Bitmap; const r: Rectangle); overload;
  begin
    SetLength(bmp^.clipStack, 0);
    PushClip(bmp, r);
  end;

  procedure SetClip(const r: Rectangle); overload;
  begin
    SetClip(screen, r);
  end;

  procedure PopClip(); overload;
  begin
    PopClip(screen);
  end;
  
  procedure PopClip(bmp: Bitmap); overload;
  begin
    Setlength(bmp^.clipStack, Length(bmp^.clipStack)-1);
    if Length(bmp^.clipStack) > 0 then
      DoSetClip(bmp, bmp^.clipStack[High(bmp^.clipStack)])
    else
      ResetClip(bmp);
  end;

  function CurrentClip(bmp: Bitmap): Rectangle; overload;
  begin
    if Length(bmp^.clipStack) <> 0 then result:= bmp^.clipStack[high(bmp^.clipStack)]
    else
      result:=BitmapRectangle(0, 0, bmp);
  end;

  function CurrentClip(): Rectangle; overload;
  begin
    result := CurrentClip(screen);
  end;

  
  
  //=============================================================================
  
  procedure DrawLines(clr: Color; const lines: LinesArray); //TODO: overload;
  var
    i: Longint;
  begin
    for i := 0 to High(lines) do
    begin
      DrawLine(clr, lines[i]);
    end;
  end;

  //=============================================================================
  
  procedure DrawShapeAsPoint(dest: Bitmap; s:Shape; filled: Boolean; const offset: Point2D); overload;
  var
    pts: Point2DArray;
  begin
    pts := ShapePoints(s);
    if length(pts) = 0 then exit;
    
    DrawPixel(dest, s^.color, PointAdd(pts[0], offset));
  end;
  
  procedure DrawShapeAsCircle(dest: Bitmap; s:Shape; filled: Boolean; const offset: point2D); overload;
  var
    // r: Single;
    // pts: Point2DArray;
    c: Circle;
  begin
    c := ShapeCircle(s);
    
    //pts := ShapePoints(s);
    //if length(pts) < 2 then exit;
    //r := PointPointDistance(pts[0], pts[1]);
    
    DrawCircle(dest, s^.color, filled, c); //PointAdd(pts[0], offset), Round(r));
  end;
  
  // procedure DrawShapeAsEllipse(dest: Bitmap; s:Shape; filled: Boolean; const offset: Point2D); overload;
  // var
  //   pts: Point2DArray;
  // begin
  //   pts := ShapePoints(s);
  //   if length(pts) < 2 then exit;
  //   
  //   DrawEllipse(dest, s^.color, filled, 
  //     Round(pts[0].x+offset.X),
  //     Round(pts[0].y+offset.Y), 
  //     Round(pts[1].x) - Round(pts[0].x), 
  //     Round(pts[1].y) - Round(pts[0].y));
  // end;
  
  procedure DrawShapeAsLine(dest: Bitmap; s:Shape; filled: Boolean; const offset: Point2D); overload;
  var
    //pts: Point2DArray;
    ln: LineSegment;
  begin
    ln := ShapeLine(s);
    // pts := ShapePoints(s);
    // if length(pts) < 2 then exit;
    
    DrawLine(dest, s^.color, ln); //PointAdd(pts[0], offset), PointAdd(pts[1], offset));
  end;
  
  procedure _DrawTriangles(dest: Bitmap; s: Shape; filled: Boolean; const offset: Point2D; kind: ShapeKind);
  var
    i: Longint;
    tri: TriangleArray;
  begin
    tri := ShapeTriangles(s, kind);
    
    for i := 0 to High(tri) do
    begin
      if filled then FillTriangle(s^.color, tri[i])
      else DrawTriangle(s^.color, tri[i]);
    end;
  end;
  
  procedure DrawShapeAsTriangle(dest: Bitmap; s:Shape; filled: Boolean; const offset: Point2D); overload;
  // var
  //   //pts: Point2DArray;
  //   tri: Triangle;
  begin
    _DrawTriangles(dest, s, filled, offset, pkTriangle);
    // tri := ShapeTriangle(s);
    // 
    // // pts := ShapePoints(s);
    // // if length(pts) < 3 then exit;
    // 
    // if filled then
    //   FillTriangle(dest, s^.color, tri)
    //   //FillTriangle(dest, s^.color, pts[0].x+offset.X, pts[0].y+offset.Y, pts[1].x+offset.X, pts[1].y+offset.Y, pts[2].x+offset.X, pts[2].y+offset.Y)
    // else
    //   DrawTriangle(dest, s^.color, tri);
    //   //DrawTriangle(dest, s^.color, pts[0].x+offset.X, pts[0].y+offset.Y, pts[1].x+offset.X, pts[1].y+offset.Y, pts[2].x+offset.X, pts[2].y+offset.Y);
  end;
  
  procedure _DrawLines(dest: Bitmap; s: Shape; const offset: Point2D; kind: ShapeKind);
  var
    lines: LinesArray;
    i: Longint;
  begin
    lines := ShapeLines(s, kind, offset);
    for i := 0 to High(lines) do
    begin
      DrawLine(dest, s^.color, lines[i]);
    end;
  end;
  
  procedure DrawShapeAsLineList(dest: Bitmap; s: Shape; filled: Boolean; const offset: Point2D);
  begin
    _DrawLines(dest, s, offset, pkLineList);
  end;
  // var
  //   i: Longint;
  //   pts: Point2DArray;
  // begin
  //   pts := ShapePoints(s);
  //   
  //   for i := 0 to Length(pts) div 2 - 1 do
  //   begin
  //     DrawLine(dest, s^.color, PointAdd(pts[i * 2], offset), PointAdd(pts[i * 2 + 1], offset));
  //   end;
  // end;
  
  procedure DrawShapeAsLineStrip(dest: Bitmap; s: Shape; filled: Boolean; const offset: Point2D);
  begin
    _DrawLines(dest, s, offset, pkLineStrip);
  end;
  // var
  //   i: Longint;
  //   pts: Point2DArray;
  // begin
  //   pts := ShapePoints(s);
  //   
  //   for i := 0 to Length(pts) - 2 do
  //   begin
  //     DrawLine(dest, s^.color, PointAdd(pts[i], offset), PointAdd(pts[i+ 1], offset));
  //   end;
  // end;
  
  {procedure DrawShapeAsPolygon(dest: Bitmap; s: Shape; filled: Boolean; const offset: Point2D);
  var
    i, l: Longint;
    pts: Point2DArray;
  begin
    pts := ShapePoints(s);
    if Length(pts) < 3 then exit;
    
    l := Length(pts);
    
    if filled then
    begin
      for i := 0 to Length(pts) - 3 do
      begin
        FillTriangle(dest, s^.color,
          pts[i].x,pts[i].y,
          pts[i + 1].x, pts[i + 1].y,
          pts[(i + 2) mod l].x, pts[(i + 2) mod l].y);
      end;
    end
    else
    begin
      for i := 0 to Length(pts) - 2 + 1 do
      begin
        DrawLine(dest, s^.color, pts[i], pts[(i+ 1) mod l]);
      end;
    end;
    
  end;}
  
  // procedure DrawTriangleFan(dest: Bitmap; s: Shape; const offset: Point2D);
  // // var
  // //   i: Longint;
  // //   pts: Point2DArray;
  // begin
  //   DrawTriangleFan(dest, s, false, offset);
  //   // pts := ShapePoints(s);
  //   // 
  //   // for i := 0 to Length(pts) - 3 do
  //   // begin
  //   //   DrawTriangle(dest, s^.color,
  //   //     pts[0].x+offset.X,pts[0].y+offset.Y,
  //   //     pts[i + 1].x+offset.X, pts[i + 1].y+offset.Y,
  //   //     pts[i + 2].x+offset.X, pts[i + 2].y+offset.Y);
  //   // end;
  // end;
  // 
  // procedure FillTriangleFan(dest: Bitmap; s: Shape; const offset: Point2D);
  // // var
  // //   i: Longint;
  // //   pts: Point2DArray;
  // begin
  //   DrawTriangleFan(dest, s, true, offset);
  //   // pts := ShapePoints(s);
  //   // 
  //   // for i := 0 to Length(pts) - 3 do
  //   // begin
  //   //   FillTriangle(dest, s^.color,
  //   //     pts[0].x+offset.X,pts[0].y+offset.Y,
  //   //     pts[i + 1].x+offset.X, pts[i + 1].y+offset.Y,
  //   //     pts[i + 2].x+offset.X, pts[i + 2].y+offset.Y);
  //   // end;
  // end;
  
  procedure DrawShapeAsTriangleFan(dest: Bitmap; s: Shape; filled: Boolean; const offset: Point2D);
  begin
    //if filled then FillTriangleFan(dest, s, offset) else DrawTriangleFan(dest, s, offset);
    _DrawTriangles(dest, s, filled, offset, pkTriangleFan);
  end;
  
  // procedure DrawTriangleStrip(dest: Bitmap; s: Shape; const offset: Point2D);
  // begin
  //   DrawTriangleStrip(dest, s, false, offset);
  // end;
  // // var
  // //   i: Longint;
  // //   pts: Point2DArray;
  // // begin
  // //   pts := ShapePoints(s);
  // //   
  // //   for i := 0 to Length(pts) - 3 do
  // //   begin
  // //     DrawTriangle(dest, s^.color,
  // //       pts[i].x+offset.X,pts[i].y+offset.Y,
  // //       pts[i + 1].x+offset.X, pts[i + 1].y+offset.Y,
  // //       pts[i + 2].x+offset.X, pts[i + 2].y+offset.Y);
  // //   end;
  // // end;
  // 
  // procedure FillTriangleStrip(dest: Bitmap; s: Shape; const offset: Point2D);
  // begin
  //   DrawTriangleStrip(dest, s, true, offset);
  // end;
  // 
  // // var
  // //   i: Longint;
  // //   pts: Point2DArray;
  // // begin
  // //   pts := ShapePoints(s);
  // //   
  // //   for i := 0 to Length(pts) - 3 do
  // //   begin
  // //     FillTriangle(dest, s^.color,
  // //       pts[i].x+offset.X,pts[i].y+offset.Y,
  // //       pts[i + 1].x+offset.X, pts[i + 1].y+offset.Y,
  // //       pts[i + 2].x+offset.X, pts[i + 2].y+offset.Y);
  // //   end;
  // // end;
  
  procedure DrawShapeAsTriangleStrip(dest: Bitmap; s: Shape; filled: Boolean; const offset: Point2D);
  begin
    _DrawTriangles(dest, s, filled, offset, pkTriangleStrip);
    //if filled then FillTriangleStrip(dest, s, offset) else DrawTriangleStrip(dest, s, offset);
  end;
  
  // procedure DrawTriangleList(dest: Bitmap; s: Shape; const offset: Point2D);
  // begin
  //   DrawTriangleList(dest, s, false, offset);
  // end;
  // // var
  // //   i: Longint;
  // //   pts: Point2DArray;
  // // begin
  // //   pts := ShapePoints(s);
  // //   
  // //   for i := 0 to Length(pts) div 3 - 1 do
  // //   begin
  // //     DrawTriangle(dest, s^.color,
  // //       pts[i * 3].x+offset.X, pts[i * 3].y+offset.Y,
  // //       pts[i * 3 + 1].x+offset.X, pts[i * 3 + 1].y+offset.Y,
  // //       pts[i * 3 + 2].x+offset.X, pts[i * 3 + 2].y+offset.Y);
  // //   end;
  // // end;
  // 
  // procedure FillTriangleList(dest: Bitmap; s: Shape; const offset: Point2D);
  // begin
  //   DrawTriangleList(dest, s, true, offset);
  // end;
  // // var
  // //   i: Longint;
  // //   pts: Point2DArray;
  // // begin
  // //   pts := ShapePoints(s);
  // //   
  // //   for i := 0 to Length(pts) div 3 - 1 do
  // //   begin
  // //     FillTriangle(dest, s^.color,
  // //       pts[i * 3].x+offset.X, pts[i * 3].y+offset.Y,
  // //       pts[i * 3 + 1].x+offset.X, pts[i * 3 + 1].y+offset.Y,
  // //       pts[i * 3 + 2].x+offset.X, pts[i * 3 + 2].y+offset.Y);
  // //   end;
  // // end;
  
  procedure DrawShapeAsTriangleList(dest: Bitmap; s: Shape; filled: Boolean; const offset: Point2D);
  begin
    _DrawTriangles(dest, s, filled, offset, pkTriangleList);
    //if filled then FillTriangleList(dest, s, offset) else DrawTriangleList(dest, s, offset);
  end;
  
  
  procedure DrawShape(dest: Bitmap; s: Shape; filled: Boolean; const offset: Point2D); overload;
  var
    i: Longint;
  begin
    s^.prototype^.drawWith(dest, s, filled, offset);
    
    for i := 0 to High(s^.subShapes) do
    begin
      DrawShape(dest, s^.subShapes[i], filled, offset);
    end;
  end;

  procedure DrawShape(dest: Bitmap; s: Shape; filled: Boolean); overload;
  begin
    DrawShape(dest, s, filled, PointAt(0,0));
  end;
  
  procedure DrawShape(dest: Bitmap; s: Shape); overload;
  begin
    DrawShape(dest, s, false, PointAt(0,0));
  end;
  
  procedure FillShape(dest: Bitmap; s: Shape); overload;
  begin
    DrawShape(dest, s, true, PointAt(0,0));
  end;
  
  procedure DrawShape(s: Shape; filled: Boolean); overload;
  begin
    DrawShape(screen, s, filled, PointAt(-CameraX(), -CameraY()));
  end;
  
  procedure DrawShape(s: Shape); overload;
  begin
    DrawShape(screen, s, false, PointAt(-CameraX(), -CameraY()));
  end;
  
  procedure FillShape(s: Shape); overload;
  begin
    DrawShape(screen, s, true, PointAt(-CameraX, -CameraY));
  end;
  
  procedure DrawShapeOnScreen(s: Shape; filled: Boolean); overload;
  begin
    DrawShape(screen, s, filled);
  end;

  procedure DrawShapeOnScreen(s: Shape); overload;
  begin
    DrawShapeOnScreen(s, false);
  end;
  
  procedure FillShapeOnScreen(s: Shape); overload;
  begin
    DrawShapeOnScreen(s, true);
  end;
  
//----------------------------------------------------------------------------
// Set Icon / Window Open / Screen Size / Resize
//----------------------------------------------------------------------------

  procedure _SetupScreen();
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', '_SetupScreen');
    {$ENDIF}
    if screen = nil then New(screen)
    else if (screen^.surface <> nil) then SDL_FreeSurface(screen^.surface);

    with _screen^.format^ do
    begin
      screen^.surface := SDL_CreateRGBSurface(SDL_HWSURFACE,
                                             ScreenWidth(), ScreenHeight(), 32,
                                             RMask, GMask, BMask, 
                                             $ffffffff and not RMask
                                                and not GMask
                                                and not BMask);
      
      //WriteLn(RMask, ':', GMask, ':', BMask, ':', screen^.surface^.format^.AMask);
      
      //Turn off alpha blending for when scr is blit onto _screen
      SDL_SetAlpha(screen^.surface, 0, 255);
      SDL_FillRect(screen^.surface, @screen^.surface^.clip_rect, 0);

      baseSurface := screen^.surface;

      screen^.width := _screen^.w;
      screen^.height := _screen^.h;
      screenRect := BitmapRectangle(0,0,screen);
    end;
    {$IFDEF TRACE}
      TraceExit('sgGraphics', '_SetupScreen');
    {$ENDIF}
  end;

  /// Sets up the graphical window for the specified width and height.
  /// Sets the caption of the window, and the icon if one is specified.
  procedure _InitSDL(caption: String; screenWidth, screenHeight: Longint);
  var
    icon: PSDL_Surface;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'InitSDL');
    {$ENDIF}

    if (screenWidth < 1) or (screenHeight < 1) then
    begin
      RaiseException('Screen Width and Height must be greater then 0 when opening a Graphical Window');
      exit;
    end;

    if Length(iconFile) > 0 then
    begin
      try
        icon := IMG_Load(PChar(iconFile));
        SDL_WM_SetIcon(icon, 0);
        SDL_FreeSurface(icon);
      except
        RaiseException('The icon file specified could not be loaded');
        exit;
      end;
    end;

    _screen := SDL_SetVideoMode(screenWidth, screenHeight, 32, SDL_HWSURFACE or SDL_DOUBLEBUF);
    if _screen = nil then
    begin
      RaiseException('Unable to create window drawing surface... ' + SDL_GetError());
      exit;
    end;

    _SetupScreen();
    if length(caption) > 0 then
      SDL_WM_SetCaption(PChar(caption), nil);

    {$IFDEF TRACE}
      TraceExit('sgGraphics', '_InitSDL');
    {$ENDIF}
  end;

  procedure SetIcon(filename: String);
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'SetIcon');
    {$ENDIF}
    iconFile := filename;
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'SetIcon');
    {$ENDIF}
  end;
  
  procedure OpenGraphicsWindow(caption: String; width: Longint; height: Longint); overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'OpenGraphicsWindow', caption + ': W' + IntToStr(width) + ': H' + IntToStr(height));
    {$ENDIF}

    if screen <> nil then
    begin
      RaiseException('Screen has been created. Cannot create multiple windows.');
      exit;
    end;

    try         
      _InitSDL(caption, width, height);
      
      //Init the colors
      ColorWhite := RGBAColor(255, 255, 255, 255);
      ColorGreen := RGBAColor(0, 255, 0, 255);
      ColorBlue := RGBAColor(0, 0, 255, 255);
      ColorBlack := RGBAColor(0, 0, 0, 255);
      ColorRed := RGBAColor(255, 0, 0, 255);
      ColorYellow := RGBAColor(255, 255, 0, 255);
      ColorPink := RGBAColor(255, 20, 147, 255);
      ColorTurquoise := RGBAColor(0, 206, 209, 255);
      ColorGrey := RGBAColor(128, 128, 128, 255);
      ColorMagenta := RGBAColor(255, 0, 255, 255);
      ColorTransparent := RGBAColor(0, 0, 0, 0);
      ColorLightGrey := RGBAColor(200, 200, 200, 255);
      
      GUISetForegroundColor(ColorGreen);
      GUISetBackgroundColor(ColorBlack);
      
      SDL_FillRect(screen^.surface, nil, ColorGrey);
      stringColor(screen^.surface, screenWidth div 2 - 30, screenHeight div 2, PChar('Loading ...'), ToGFXColor(ColorWhite));
      RefreshScreen();
    except on e: Exception do
      begin
        RaiseException('Error in OpenGraphicsWindow: ' + e.Message);
        exit;
      end;
    end;
    
    {$IFDEF TRACE}
      TraceIf(tlInfo, 'sgGraphics', 'Info', 'OpenGraphicsWindow', 'Window is open (' + caption + ' ' + IntToStr(width) + 'x' + IntToStr(height) + ')');
    {$ENDIF}
    
    ShowLogos();
    LoadResourceBundle('FileDialog.txt');
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'OpenGraphicsWindow');
    {$ENDIF}
  end;
  

  procedure OpenGraphicsWindow(caption: String); overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'OpenGraphicsWindow');
    {$ENDIF}
    OpenGraphicsWindow(caption, 800,600);
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'OpenGraphicsWindow');
    {$ENDIF}
  end;

  procedure ToggleFullScreen();
  var
    oldScr: PSDL_Surface;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'ToggleFullScreen');
    {$ENDIF}
    oldScr := _screen;

    try
      //Remember... _screen is a pointer to screen buffer, not a "surface"!
      _screen := SDL_SetVideoMode(oldScr^.w, oldScr^.h, 32, oldScr^.flags xor SDL_FULLSCREEN);
    except on exc: Exception do
    end;
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'ToggleFullScreen');
    {$ENDIF}
  end;
  
  procedure ToggleWindowBorder();
  var
    oldScr: PSDL_Surface;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'ToggleWindowBorder');
    {$ENDIF}
    oldScr := _screen;
    
    try
      //Remember... _screen is a pointer to screen buffer, not a "surface"!
      _screen := SDL_SetVideoMode(oldScr^.w, oldScr^.h, 32, oldScr^.flags xor SDL_NOFRAME);
    except on exc: Exception do
    end;
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'ToggleWindowBorder');
    {$ENDIF}
  end;

  procedure ChangeScreenSize(width, height: Longint);
  var
    oldScr: PSDL_Surface;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'ChangeScreenSize');
    {$ENDIF}
    if (screen = nil) then
    begin
      RaiseException('Screen has not been created. Unable to get screen width.');
      exit;
    end;

    if (width < 1) or (height < 1) then
    begin
      RaiseException('Screen Width and Height must be greater then 0 when resizing a Graphical Window');
      exit; 
    end;

    if (width = ScreenWidth()) and (height = ScreenHeight()) then exit;

    oldScr := _screen;
    _screen := SDL_SetVideoMode(width, height, 32, oldScr^.flags);
    _SetupScreen();
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'ChangeScreenSize');
    {$ENDIF}
  end;

  function ScreenWidth(): Longint;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'ScreenWidth');
    {$ENDIF}
    if (_screen = nil) then
    begin
      RaiseException('Screen has not been created. Unable to get screen width.');
      exit;
    end;
    
    result := _screen^.w;
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'ScreenWidth');
    {$ENDIF}
  end;

  function ScreenHeight(): Longint;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'ScreenHeight');
    {$ENDIF}
    if (_screen = nil) then
    begin
      RaiseException('Screen has not been created. Unable to get screen height.');
      exit;
    end;
    result := _screen^.h;
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'ScreenHeight');
    {$ENDIF}
  end;

  procedure TakeScreenShot(basename: String);
  var
    path: String;
    filename: String;
    i: Longint;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'TakeScreenShot');
    {$ENDIF}
    
    path := IncludeTrailingPathDelimiter(GetUserDir()) + 'Desktop' + PathDelim;
    if not DirectoryExists(path) then 
      path := IncludeTrailingPathDelimiter(GetUserDir());
    
    filename := basename + '.png';
    
    i := 1;
    
    while FileExists(path + filename) do
    begin
      filename := basename + IntToStr(i) + '.png';
      i := i + 1;
    end;
    
    //if SDL_SaveBMP(screen^.surface, PChar(path + filename)) = -1 then
    if not png_save_surface(path + filename, screen^.surface) then
    begin
      RaiseException('Failed to save ' + basename + ': ' + SDL_GetError());
      exit;
    end;
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'TakeScreenShot');
    {$ENDIF}
  end;

  procedure RefreshScreen(); overload;
  var
    nowTime: Longword;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'RefreshScreen');
    {$ENDIF}
    //Draw then delay
    sdlManager.DrawCollectedText(screen^.surface);
    SDL_BlitSurface(screen^.surface, nil, _screen, nil);
    SDL_Flip(_screen);

    nowTime := GetTicks();
    _UpdateFPSData(nowTime - _lastUpdateTime); // delta
    _lastUpdateTime := nowTime;
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'RefreshScreen');
    {$ENDIF}
  end;

  procedure RefreshScreen(TargetFPS: Longword); overload;
  var
    nowTime: Longword;
    delta, delayTime: Longword;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'RefreshScreen');
    {$ENDIF}
    sdlManager.DrawCollectedText(screen^.surface);
    SDL_BlitSurface(screen^.surface, nil, _screen, nil);
    SDL_Flip(_screen);
    
    nowTime := GetTicks();
    delta := nowTime - _lastUpdateTime;
    
    //dont sleep if 1ms remaining...
    while (delta + 1) * TargetFPS < 1000 do
    begin
      delayTime := (1000 div TargetFPS) - delta;
      Delay(delayTime);
      nowTime := GetTicks();
      delta := nowTime - _lastUpdateTime;
    end;  

    _UpdateFPSData(delta);
    _lastUpdateTime := nowTime;
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'RefreshScreen');
    {$ENDIF}
  end;
  
  
  
//----------------------------------------------------------------------------
// Colour
//----------------------------------------------------------------------------
  function  ColorToString(c: Color): string;
  var
    r,g,b,a : byte;
  begin
    colorComponents(c,r,g,b,a);
    result:=IntToStr(r)+','+IntToStr(g)+','+IntToStr(b)+','+IntToStr(a);
  end;

  procedure ColorComponents(c: Color; out r, g, b, a: byte);
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'ColorComponents');
    {$ENDIF}
    if baseSurface = nil then
    begin
      RaiseWarning('Estimating color components as Windows has not been opened.');
      a := (c and $FF000000) shr 24;
      r := (c and $00FF0000) shr 16;
      g := (c and $0000FF00) shr 8;
      b := (c and $000000FF);
      exit;
    end;
    
    SDL_GetRGBA(c, baseSurface^.Format, @r, @g, @b, @a);
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'ColorComponents');
    {$ENDIF}
  end;

  function RedOf(c: Color): byte;
  var
    r,g,b,a: Byte;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'RedOf');
    {$ENDIF}
    ColorComponents(c, r, g, b, a);
    result := r;
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'RedOf');
    {$ENDIF}
  end;

  function GreenOf(c: Color): byte;
  var
    r,g,b,a: Byte;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'GreenOf');
    {$ENDIF}
    ColorComponents(c, r, g, b, a);
    result := g;
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'GreenOf');
    {$ENDIF}
  end;

  function BlueOf(c: Color): byte;
  var
    r,g,b,a: Byte;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'BlueOf');
    {$ENDIF}
    ColorComponents(c, r, g, b, a);
    result := b;
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'BlueOf');
    {$ENDIF}
  end;

  function TransparencyOf(c: Color): byte;
  var
    r,g,b,a: Byte;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'TransparencyOf');
    {$ENDIF}
    ColorComponents(c, r, g, b, a);
    result := a;
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'TransparencyOf');
    {$ENDIF}
  end;
  
  procedure HSBValuesOf(c: Color; out h, s, b: Single);
  var
    red, green, blue, alpha: byte;
    rf, gf, bf: Single;
    minRGB, maxRGB, delta: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'HSBValuesOf');
    {$ENDIF}
     H := 0.0 ;
     
     ColorComponents(c, red, green, blue, alpha);
     
     rf := red / 255;
     gf := green / 255;
     bf := blue / 255;
     
     minRGB := Min(Min(rf, gf), bf);
     maxRGB := Max(Max(rf, gf), bf);
     delta := (maxRGB - minRGB);
     
     b := maxRGB;
     if (maxRGB <> 0.0) then s := delta / maxRGB
     else s := 0.0;
      
     if (s <> 0.0) then
     begin
       if rf = maxRGB then h := (gf - bf) / Delta
       else
         if gf = maxRGB then h := 2.0 + (bf - rf) / Delta
         else
           if bf = maxRGB then h := 4.0 + (rf - gf) / Delta
     end
     else h := -1.0;
     h := h * 60 ;
     if h < 0.0 then h := h + 360.0;
       
     h := h / 360.0;
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'HSBValuesOf');
    {$ENDIF}
  end;
  
  function HueOf(c: Color) : Single;
  var
    s, b: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'HueOf');
    {$ENDIF}
    HSBValuesOf(c, result, s, b);
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'HueOf');
    {$ENDIF}
  end;
  
  function SaturationOf(c: Color) : Single;
  var
    h, b: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'SaturationOf');
    {$ENDIF}
    HSBValuesOf(c, h, result, b);
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'SaturationOf');
    {$ENDIF}
  end;
  
  function BrightnessOf(c: Color) : Single;
  var
    h, s: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'BrightnessOf');
    {$ENDIF}
    HSBValuesOf(c, h, s, result);
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'BrightnessOf');
    {$ENDIF}
  end;
  
  function ColorFrom(bmp: Bitmap; apiColor: Color): Color; overload;
  var
    r,g,b,a: Byte;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'ColorFrom');
    {$ENDIF}
    if (bmp = nil) or (bmp^.surface = nil) then
    begin
      RaiseException('Unable to get color as bitmap not specified');
      exit;
    end;
    
    ColorComponents(apiColor, r, g, b, a);
    
    result := SDL_MapRGBA(bmp^.surface^.format, r, g, b, a);
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'ColorFrom');
    {$ENDIF}
  end;

  function RGBAColor(red, green, blue, alpha: Byte): Color; overload;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'RGBAColor');
    {$ENDIF}
    if (baseSurface = nil) or (baseSurface^.format = nil) then
    begin
      RaiseWarning('Estimating RGBAColor as the window is not open');
      result := (alpha shl 24) or (red shl 16) or (green shl 8) or (blue);
      exit;
    end;

    try
      result := SDL_MapRGBA(baseSurface^.format, red, green, blue, alpha);
    except
      RaiseException('Error occured while trying to get a color from RGBA components');
      exit;
    end;
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'RGBAColor');
    {$ENDIF}
  end;

  function RGBColor(red, green, blue: Byte): Color;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'RGBColor');
    {$ENDIF}
    result := RGBAColor(red, green, blue, 255);
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'RGBColor');
    {$ENDIF}
  end;

  function RGBFloatColor(r,g,b: Single): Color;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'RGBFloatColor');
    {$ENDIF}
    result := RGBColor(Round(r * 255), Round(g * 255), Round(b * 255));
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'RGBFloatColor');
    {$ENDIF}
  end;
  
  function RGBAFloatColor(r,g,b, a: Single): Color;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'RGBAFloatColor');
    {$ENDIF}
    result := RGBAColor(Round(r * 255), Round(g * 255), Round(b * 255), Round(a * 255));
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'RGBAFloatColor');
    {$ENDIF}
  end;

  function HSBColor(hue, saturation, brightness: Single): Color;
  var
    domainOffset: Single;
    red, green, blue: Single;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'HSBColor');
    {$ENDIF}
    if brightness = 0 then
    begin
      result := ColorBlack;
      exit;
    end;

    if saturation = 0 then
    begin
      result := RGBFloatColor(brightness, brightness, brightness);
      exit;
    end;

    if hue < 1.0 / 6 then
    begin // red domain... green ascends
      domainOffset := hue;
      red   := brightness;
      blue  := brightness * (1.0 - saturation);
      green := blue + (brightness - blue) * domainOffset * 6;
    end
    else if hue < 2.0 / 6 then
    begin // yellow domain; red descends
      domainOffset := hue - 1.0 / 6;
      green := brightness;
      blue  := brightness * (1.0 - saturation);
      red   := green - (brightness - blue) * domainOffset * 6;
    end
    else if hue < 3.0 / 6 then
    begin // green domain; blue ascends
      domainOffset := hue - 2.0 / 6;
      green := brightness;
      red   := brightness * (1.0 - saturation);
      blue  := red + (brightness - red) * domainOffset * 6;
    end
    else if hue < 4.0 / 6 then
    begin // cyan domain; green descends
      domainOffset := hue - 3.0 / 6;
      blue  := brightness;
      red   := brightness * (1.0 - saturation);
      green := blue - (brightness - red) * domainOffset * 6;
    end
    else if hue < 5.0 / 6 then
    begin // blue domain; red ascends
      domainOffset := hue - 4.0 / 6;
      blue  := brightness;
      green := brightness * (1.0 - saturation);
      red   := green + (brightness - green) * domainOffset * 6;
    end
    else
    begin // magenta domain; blue descends
      domainOffset := hue - 5.0 / 6;
      red   := brightness;
      green := brightness * (1.0 - saturation);
      blue  := red - (brightness - green) * domainOffset * 6;
    end;

    result := RGBFloatColor(red, green, blue);
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'HSBColor');
    {$ENDIF}
  end;
  
  function RandomColor(): Color;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'RandomColor');
    {$ENDIF}
    result := RGBAFloatColor(Rnd(), Rnd(), Rnd(), Rnd());
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'RandomColor');
    {$ENDIF}
  end;
  
  function RandomRGBColor(alpha: Byte): Color;
  begin
    {$IFDEF TRACE}
      TraceEnter('sgGraphics', 'RandomRGBColor');
    {$ENDIF}
    result := RGBAColor(Byte(Rnd(256)), Byte(Rnd(256)), Byte(Rnd(256)), alpha);
    {$IFDEF TRACE}
      TraceExit('sgGraphics', 'RandomRGBColor');
    {$ENDIF}
  end;


//=============================================================================
  
  initialization
  begin
    InitialiseSwinGame();
  end;
end.
