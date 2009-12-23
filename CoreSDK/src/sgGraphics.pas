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
// - 2008-12-17: Andrew : Moved all integers to LongInt
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

/// @module Graphics
/// @static
unit sgGraphics;

//=============================================================================
interface
  uses sgTypes;
//=============================================================================
  
  //---------------------------------------------------------------------------
  // Circle drawing code
  //---------------------------------------------------------------------------
  
  /// Draw a circle onto a destination (filled or outline).
  /// @lib DrawOrFillPtCircleOnto
  /// @sn drawOnto:%s color:%s filled:%s circleX:%s y:%s radius:%s
  procedure DrawCircle(dest: Bitmap; clr: Color; filled: Boolean; xc, yc: Single; radius: LongInt); overload;
  
  /// Draw a circle onto a destination.
  /// 
  /// @lib DrawOrFillPtCircleOnto(dest, clr, False, xc, yc, radius)
  /// @uname DrawPtCircleOnto
  /// @sn drawOnto:%s color:%s circleX:%s y:%s radius:%s
  procedure DrawCircle(dest: Bitmap; clr: Color; xc, yc: Single; radius: LongInt); overload;
  
  /// Fill a circle onto a destination.
  /// 
  /// @lib DrawOrFillPtCircleOnto(dest, clr, True, xc, yc, radius)
  /// @uname FillPtCircleOnto
  /// @sn fillOnto:%s color:%s circleX:%s y:%s radius:%s
  procedure FillCircle(dest: Bitmap; clr: Color; xc, yc: Single; radius: LongInt); overload;
  
  /// Draw a circle onto a destination (filled or outline).
  /// 
  /// @lib DrawOrFillPtCircleAtPointOnto
  /// @sn drawOnto:%s color:%s filled:%s circle:%s radius:%s
  procedure DrawCircle(dest: Bitmap; clr: Color; filled: Boolean; const point: Point2D; radius: LongInt); overload;
  
  /// Draw a circle onto a bitmap.
  /// 
  /// @lib DrawOrFillPtCircleAtPointOnto(dest, clr, False, point, radius)
  /// @uname DrawCircleAtPointOnto
  /// @sn drawOnto:%s color:%s circle:%s radius:%s
  procedure DrawCircle(dest: Bitmap; clr: Color; const point: Point2D; radius: LongInt); overload;
  
  /// Fill a circle onto a destination bitmap.
  /// 
  /// @lib DrawOrFillPtCircleAtPointOnto(dest, clr, True, point, radius)
  /// @uname FillCircleAtPointOnto
  /// @sn fillOnto:%s color:%s circle:%s radius:%s
  procedure FillCircle(dest: Bitmap; clr: Color; const point: Point2D; radius: LongInt); overload;
  
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
  procedure DrawCircle(clr: Color; filled: Boolean; xc, yc: Single; radius: LongInt); overload;
  
  /// Draw a circle in the game.
  /// 
  /// @lib DrawOrFillPtCircle(clr, False, xc, yc, radius)
  /// @uname DrawPtCircle
  /// @sn draw:%s circleX:%s y:%s radius:%s
  procedure DrawCircle(clr: Color; xc, yc: Single; radius: LongInt); overload;
  
  /// Fill a circle in the game.
  /// 
  /// @lib DrawOrFillPtCircle(clr, True, xc, yc, radius)
  /// @uname FillPtCircle
  /// @sn fill:%s circleX:%s y:%s radius:%s
  procedure FillCircle(clr: Color; xc, yc: Single; radius: LongInt); overload;
  
  /// Draw a circle in the game (filled or outline).
  /// 
  /// @lib DrawOrFillPtCircleAtPoint
  /// @sn draw:%s filled:%s circle:%s radius:%s
  procedure DrawCircle(clr: Color; filled: Boolean; const position: Point2D; radius: LongInt); overload;
  
  /// Draw circle in the game.
  /// 
  /// @lib DrawOrFillPtCircleAtPoint(clr, False, position, radius)
  /// @uname DrawPtCircleAtPoint
  /// @sn draw:%s circle:%s radius:%s
  procedure DrawCircle(clr: Color; const position: Point2D; radius: LongInt); overload;
  
  /// Fill a circle in the game.
  /// 
  /// @lib DrawOrFillPtCircleAtPoint(clr, True, position, radius)
  /// @uname FillPtCircleAtPoint
  /// @sn fill:%s circle:%s radius:%s
  procedure FillCircle(clr: Color; const position: Point2D; radius: LongInt); overload;
  
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
  procedure DrawCircleOnScreen(clr: Color; filled: Boolean; xc, yc: Single; radius: LongInt); overload;
  
  /// Draw a circle on the screen.
  /// 
  /// @lib DrawOrFillPtCircleOnScreen(clr, False, xc, yc, radius)
  /// @uname DrawCirclePtOnScreen
  /// @sn draw:%s circleOnScreenX:%s y:%s radius:%s
  procedure DrawCircleOnScreen(clr: Color; xc, yc: Single; radius: LongInt); overload;
  
  /// Fill a circle on the screen.
  /// 
  /// @lib DrawOrFillPtCircleOnScreen(clr, True, xc, yc, radius)
  /// @uname FillCirclePtOnScreen
  /// @sn fill:%s circleOnScreenX:%s y:%s radius:%s
  procedure FillCircleOnScreen(clr: Color; xc, yc: Single; radius: LongInt); overload;
  
  /// Draw a circle onto the screen (filled or outline).
  /// 
  /// Draw a circle onto the screen
  /// @lib DrawOrFillCircleAtPointOnScreen
  /// @sn draw:%s filled:%s circleOnScreen:%s radius:%s
  procedure DrawCircleOnScreen(clr: Color; filled: Boolean; const position: Point2D; radius: LongInt); overload;
  
  /// Draw a circle onto the screen.
  /// 
  /// @lib DrawOrFillCircleAtPointOnScreen(clr, False, position, radius)
  /// @uname DrawCircleAtPointOnScreen
  /// @sn draw:%s circleOnScreen:%s radius:%s
  procedure DrawCircleOnScreen(clr: Color; const position: Point2D; radius: LongInt); overload;
  
  /// Fills a circle onto the screen.
  /// 
  /// @lib DrawOrFillCircleAtPointOnScreen(clr, True, position, radius)
  /// @uname FillCircleAtPointOnScreen
  /// @fill:%s circleOnScreen:%s radius:%s
  procedure FillCircleOnScreen(clr: Color; const position: Point2D; radius: LongInt); overload;
  
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
  /// @uname DrawOrFillShapeOnto
  /// @sn drawOnto:%s shape:%s filled:%s
  /// 
  /// @class Shape
  /// @overload Draw DrawOrFillOnto
  /// @self 2
  /// @ drawOnto:%s filled:%s
  procedure DrawShape(dest: Bitmap; s: Shape; filled: Boolean); overload;
  
  /// Draw the Shape s onto the destination bitmap.
  /// 
  /// @lib DrawShapeOnto
  /// @uname DrawShapeOnto
  /// @sn drawOnto:%s shape:%s
  /// 
  /// @class Shape
  /// @overload Draw DrawOnto
  /// @self 2
  /// @sn drawOnto:%s
  procedure DrawShape(dest: Bitmap; s: Shape); overload;
  
  /// Fill the Shape s onto the destination bitmap.
  ///
  /// @lib FillShapeOnto
  /// @uname FillShapeOnto
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
  /// @uname DrawOrFillShape
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
  /// @uname DrawShape
  /// @class Shape
  /// @method Draw
  procedure DrawShape(s: Shape); overload;
  
  /// Fill the Shape s.
  /// 
  /// @lib FillShape
  /// 
  /// @uname FillShape
  /// @class Shape
  /// @method Fill
  procedure FillShape(s: Shape); overload;
  
  /// Draw or fill the Shape s onto the screen using the
  /// Shape's location as screen coordinates.
  /// 
  /// @lib DrawOrFillShapeOnScreen
  /// @sn drawShapeOnScreen:%s filled:%s
  /// 
  /// @uname DrawOrFillShapeOnScreen
  /// @class Shape
  /// @overload DrawOnScreen DrawOrFillOnScreen
  /// @csn drawOnScreenFilled:%s
  procedure DrawShapeOnScreen(s: Shape; filled: Boolean); overload;
  
  /// Draw the Shape s onto the screen using the
  /// Shape's location as screen coordinates.
  ///
  /// @lib DrawShapeOnScreen
  ///
  /// @uname DrawShapeOnScreen
  /// @class Shape
  /// @method DrawOnScreen
  procedure DrawShapeOnScreen(s: Shape); overload;
  
  /// Fill the Shape s onto the screen using the
  /// Shape's location as screen coordinates.
  /// 
  /// @lib FillShapeOnScreen
  /// 
  /// @uname FillShapeOnScreen
  /// @class Shape
  /// @method FillOnScreen
  procedure FillShapeOnScreen(s: Shape); overload;
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This version
  /// Draws the first point of the shape as a pixel.
  ///
  /// @lib
  /// @sn drawOnto:%s pointShape:%s filled:%s
  ///
  /// @class Shape
  /// @method DrawAsPoint
  /// @self 2
  /// @csn drawPointOnto:%s filled:%s
  procedure DrawShapeAsPoint(dest: Bitmap; s:Shape; filled: Boolean); overload;
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This version
  /// draws the shape as a circle, centered on the first point with a radius defined
  /// by the distance to the second point.
  /// 
  /// @lib
  /// @sn drawOnto:%s circleShape:%s filled:%s
  /// 
  /// @class Shape
  /// @method DrawAsCircle
  /// @self 2
  /// @csn drawCircleOnto:%s filled:%s
  procedure DrawShapeAsCircle(dest: Bitmap; s:Shape; filled: Boolean); overload;
  
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
  /// @sn drawOnto:%s lineShape:%s filled:%s
  /// 
  /// @class Shape
  /// @method DrawAsLine
  /// @self 2
  /// @csn drawLineOnto:%s filled:%s
  procedure DrawShapeAsLine(dest: Bitmap; s:Shape; filled: Boolean); overload;
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This version
  /// draws the shape as a triangle based on the first three points of the shape.
  /// 
  /// @lib
  /// @sn drawOnto:%s triangleShape:%s filled:%s
  ///
  /// @class Shape
  /// @method DrawAsTriangle
  /// @self 2
  /// @csn drawTriangleOnto:%s filled:%s
  procedure DrawShapeAsTriangle(dest: Bitmap; s:Shape; filled: Boolean); overload;
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This version
  /// draws the points as a list of lines. A shape with 4 points has two lines in its
  /// list. If an odd numer of points are supplied then the extra point will be skipped.
  /// In this way a shape with 5 points also has 2 lines.
  /// 
  /// @lib
  /// @sn drawOnto:%s lineListShape:%s filled:%s
  /// 
  /// @class Shape
  /// @method DrawAsLineList
  /// @self 2
  /// @csn drawLineListOnto:%s filled:%s
  procedure DrawShapeAsLineList(dest: Bitmap; s: Shape; filled: Boolean);
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This version
  /// draws the shape as a line strip. A shape with three points has two lines, one 
  /// from pt[0] to pt[1] and a second from pt[1] to pt[2].
  /// 
  /// @lib
  /// @sn drawOnto:%s lineStripShape:%s filled:%s
  /// 
  /// @class Shape
  /// @method DrawAsLineStrip
  /// @self 2
  /// @csn drawLineStripOnto:%s filled:%s
  procedure DrawShapeAsLineStrip(dest: Bitmap; s: Shape; filled: Boolean);
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This draws
  /// as a polygon where each point is connected to its neighbour and the
  /// first point is reconnected to the last point.
  /// 
  /// @lib
  /// @sn drawOnto:%s polygonShape:%s filled:%s
  /// 
  /// @class Shape
  /// @method DrawAsPolygon
  /// @self 2
  /// @csn drawPolygonOnto:%s filled:%s
  procedure DrawShapeAsPolygon(dest: Bitmap; s: Shape; filled: Boolean);
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This version
  /// draws the shape as a tan of triangles where each triangle is made up of
  /// the first point and two neighbouring points from the shape.
  /// 
  /// @lib
  /// @sn drawOnto:%s triangleFanShape:%s filled:%s
  /// 
  /// @class Shape
  /// @method DrawAsTriangleFan
  /// @self 2
  /// @csn drawTriangleFonOnto:%s filled:%s
  procedure DrawShapeAsTriangleFan(dest: Bitmap; s: Shape; filled: Boolean);
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This version
  /// draws as a strip of triangles where each triangle is made up of the 
  /// three neighbouring points. In this way 4 points gives two triangles.
  /// 
  /// @lib
  /// @sn drawOnto:%s triangleStripShape:%s filled:%s
  /// 
  /// @class Shape
  /// @method DrawAsTriangleStrip
  /// @self 2
  /// @csn drawTriangleStripOnto:%s filled:%s
  procedure DrawShapeAsTriangleStrip(dest: Bitmap; s: Shape; filled: Boolean);
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This version
  /// draws as a triangle list, where each set of three points is drawn as an
  /// individual triangle and extra points are ignored. So 6, 7, and 8 points 
  /// all create 2 triangles (pt[0] + pt[1] + pt[2] and pt[3] + pt[4] + pt[5]). 
  /// 
  /// @lib
  /// @sn drawOnto:%s triangleListShape:%s filled:%s
  /// 
  /// @class Shape
  /// @method DrawAsTriangleList
  /// @self 2
  /// @csn drawTriangleListOnto:%s filled:%s
  procedure DrawShapeAsTriangleList(dest: Bitmap; s: Shape; filled: Boolean);
  
  
  //---------------------------------------------------------------------------
  // Screen clearing routines
  //---------------------------------------------------------------------------
  
  /// Clear the screen black.
  /// 
  /// @lib ClearScreenToBlack
  /// @uname ClearScreen
  /// @sn clearScreen
  procedure ClearScreen(); overload;
  
  /// Clear the screen to a specified color.
  /// 
  /// @lib
  /// @uname ClearScreenWithColor
  /// @sn clearScreen:%s
  procedure ClearScreen(toColor : Color); overload;
  
  
  //---------------------------------------------------------------------------
  // Pixel drawing
  //---------------------------------------------------------------------------
  
  /// Draw a pixel onto a destination.
  /// 
  /// @lib DrawPixelOnto
  /// @sn drawOnto:%s color:%s pixelX:%s y:%s
  procedure DrawPixel(dest: Bitmap; clr: Color; x, y: LongInt); overload;
  
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
  procedure DrawPixelOnScreen(clr: Color; x, y: LongInt); overload;
  
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
  procedure DrawRectangle(dest: Bitmap; clr : Color; filled : Boolean; xPos, yPos, width, height : LongInt); overload;
  
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
  procedure DrawRectangle(dest: Bitmap; clr : Color; xPos, yPos, width, height: LongInt); overload;
  
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
  /// @sn fillOnto:@s color:%s rectangleX:%s y:%s width:%s height:%s
  procedure FillRectangle(dest: Bitmap; clr : Color; xPos, yPos, width, height : LongInt); overload;
  
  /// Fill a rectangle onto a destination.
  /// 
  /// @lib DrawOrFillRectangleRectOnto(dest, clr, True, source)
  /// @uname FillRectangleRectOnto
  /// @sn drawOnto:%s color:%s rectangle:%s
  procedure FillRectangle(dest: Bitmap; clr : Color; const source: Rectangle); overload;
  
  /// Draw a rectangle in the game (filled or outline).
  ///
  /// @lib DrawOrFillRectangle
  /// @sn draw:%s filled:%s rectangleX:%s y:%s width:%s height:%s
  procedure DrawRectangle(clr: Color; filled: Boolean; xPos, yPos: Single; width, height: LongInt); overload;
  
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
  procedure DrawRectangle(clr: Color; xPos, yPos: Single; width, height: LongInt); overload;
  
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
  procedure FillRectangle(clr: Color; xPos, yPos: Single; width, height: LongInt); overload;
  
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
  procedure DrawRectangleOnScreen(clr : Color; filled : Boolean; xPos, yPos, width, height : LongInt); overload;
  
  /// Draw a rectangle on the screen.
  ///
  /// @lib DrawOrFillRectangleOnScreen(clr, False, xPos, yPos, width, height)
  /// @uname DrawRectangleOnScreen
  /// @sn draw:%s rectangleOnScreenX:%s y:%s width:%s height:%s
  procedure DrawRectangleOnScreen(clr : Color; xPos, yPos, width, height : LongInt); overload;
  
  /// Fill a rectangle on the screen.
  ///
  /// @lib DrawOrFillRectangleOnScreen(clr, True, xPos, yPos, width, height)
  /// @uname FillRectangleOnScreen
  /// @sn fill:%s rectangleOnScreenX:%s y:%s width:%s height:%s
  procedure FillRectangleOnScreen(clr : Color; xPos, yPos, width, height : LongInt); overload;
  
  /// Draw a rectangle on the screen (fill or outline).
  ///
  /// @lib DrawOrFillRectangleRectOnScreen
  /// @sn draw:%s filled:%s rectangle:%s
  procedure DrawRectangleOnScreen(clr : Color; filled : Boolean; const source : Rectangle); overload;
  
  /// Draw a rectangle on the screen.
  ///
  /// @lib DrawOrFillRectangleRectOnScreen(clr, False, source)
  /// @uname DrawRectangleRectOnScreen
  /// @sn draw:%s rectangle:%s
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
  procedure DrawLine(dest: Bitmap; clr: Color; xPosStart, yPosStart, xPosEnd, yPosEnd: LongInt); overload;
  
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
  procedure DrawHorizontalLine(dest: Bitmap; clr: Color; y, x1, x2: LongInt); overload;
  
  /// Draw a vertical line onto a destination.
  ///
  /// @lib DrawVerticalLineOnto
  /// @sn drawOnto:%s color:%s verticalLineX:%s y1:%s y2:%s
  procedure DrawVerticalLine(dest: Bitmap; clr: Color; x, y1, y2: LongInt); overload;
  
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
  procedure DrawLineOnScreen(clr: Color; xPosStart, yPosStart, xPosEnd, yPosEnd: LongInt); overload;
  
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
  procedure DrawHorizontalLineOnScreen(clr: Color; y, x1, x2: LongInt);
  
  /// Draw a vertical line on the screen between y1 and y2.
  ///
  /// @lib
  /// @sn draw:%s verticalLineOnScreenX:%s y1:%s y2:%s
  procedure DrawVerticalLineOnScreen(clr: Color; x, y1, y2: LongInt);
  
  
  //---------------------------------------------------------------------------
  // Ellipse drawing
  //---------------------------------------------------------------------------
  
  /// Draw the ellipse onto the destination.
  /// 
  /// @lib DrawOrFillEllipseOnto
  /// @sn drawOnto:%s color:%s filled:%s ellipseX:%s y:%s width:%s height:%s
  procedure DrawEllipse(dest: Bitmap; clr: Color; filled: Boolean; xPos, yPos, width, height: LongInt); overload;
  
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
  procedure DrawEllipse(dest: Bitmap; clr: Color; xPos, yPos, width, height: LongInt); overload;
  
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
  procedure FillEllipse(dest: Bitmap; clr: Color; xPos, yPos, width, height: LongInt); overload;
  
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
  procedure DrawEllipse(clr: Color; filled: Boolean; xPos, yPos: Single; width, height: LongInt); overload;
  
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
  procedure DrawEllipse(clr: Color; xPos, yPos: Single; width, height: LongInt); overload;
  
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
  procedure FillEllipse(clr: Color; xPos, yPos: Single; width, height: LongInt); overload;
  
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
  procedure DrawEllipseOnScreen(clr: Color; filled: Boolean; xPos, yPos, width, height: LongInt); overload;
  
  /// Draw an ellipse on screen.
  ///
  /// @lib DrawOrFillEllipseOnScreen(clr, False, xPos, yPos, width, height)
  /// @uname DrawEllipseOnScreen
  /// @sn draw:%s ellipseOnScreenX:%s y:%s width:%s height:%s
  procedure DrawEllipseOnScreen(clr: Color; xPos, yPos, width, height: LongInt); overload;
  
  /// Fills an ellipse on the screen.
  ///
  /// @lib DrawOrFillEllipseOnScreen(clr, True, xPos, yPos, width, height)
  /// @uname FillEllipseOnScreen
  /// @sn fill:%s ellipseOnScreenX:%s y:%s width:%s height:%s
  procedure FillEllipseOnScreen(clr: Color; xPos, yPos, width, height: LongInt); overload;
  
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
  
  /// Set the clip rectangle of the screen.
  ///
  /// @lib
  /// @sn setClipX:%s y:%s width:%s height:%s
  procedure SetClip(x, y, w, h: LongInt); overload;
  
  /// Set the clip rectangle of the screen.
  ///
  /// @lib SetClipRect
  /// @sn setClip:%s
  procedure SetClip(const r: Rectangle); overload;
  
  /// Set the clipping rectangle of a bitmap.
  ///
  /// @lib SetClipForBitmap
  /// @sn bitmap:%s setClipRectX:%s y:%s width:%s height:%s
  ///
  /// @class Bitmap
  /// @method SetClip
  /// @csn setClipRectX:%s y:%s width:%s height:%s
  procedure SetClip(bmp: Bitmap; x, y, w, h: LongInt); overload;
  
  /// Set the clipping rectangle of a bitmap.
  /// 
  /// @lib SetClipRectForBitmap
  /// @sn bitmap:%s setClipRect:%s
  ///
  /// @class Bitmap
  /// @overload SetClip SetClipRect
  /// @csn setClip:%s
  procedure SetClip(bmp: Bitmap; const r: Rectangle); overload;
  
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
  function GetPixel(bmp: Bitmap; x, y: LongInt): Color;
  
  /// Returns the color of the pixel at the given x,y location.
  ///
  /// @lib
  /// @sn colorOnScreenAtX:%s y:%s
  function GetPixelFromScreen(x, y: LongInt): Color;
  

//=============================================================================
implementation
//=============================================================================

  uses Classes, SysUtils, // system
       SDL_gfx, SDL, SDL_Image, // sdl
       sgCamera, sgPhysics, sgShared, sgCore, sgGeometry, sgResources, sgImages;

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

  function GetPixel(bmp: Bitmap; x, y: LongInt): Color;
  begin
    if not Assigned(bmp) then begin RaiseException('No bitmap supplied'); exit; end;

    if (x < 0) or (x >= bmp^.width) or (y < 0) or (y >= bmp^.height) then
    begin
      result := 0;
      exit;
    end;

    result := GetPixel32(bmp^.surface, x, y);
  end;

  function GetPixelFromScreen(x, y: LongInt): Color;
  begin
    result := GetPixel(screen, x, y);
  end;

  procedure PutPixel(surface: PSDL_Surface; x, y: LongInt; color: Color);
  begin
    pixelColor(surface, x, y, ToGfxColor(color));
  end;
  
  /// Draws a pixel onto the screen.
  ///
  /// @param clr:     The color to draw the pixel
  /// @param x,y:         The x,y location to draw the pixel at
  ///
  /// Side Effects:
  /// - Sets one pixel on the screen
  procedure DrawPixelOnScreen(clr: Color; x, y: LongInt);
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
  procedure DrawRectangleOnScreen(clr : Color; filled : Boolean; xPos, yPos, width, height : LongInt); overload;
  begin
    DrawRectangle(screen, clr, filled, xPos, yPos, width, height);
  end;

  procedure DrawRectangle(clr : Color; filled : Boolean; xPos, yPos: Single; width, height : LongInt); overload;
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
  procedure DrawRectangleOnScreen(clr : Color; xPos, yPos, width, height : LongInt); overload;
  begin
    DrawRectangle(screen, clr, xPos, yPos, width, height);
  end;
  
  procedure DrawRectangle(clr: Color; xPos, yPos: Single; width, height : LongInt); overload;
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
  procedure FillRectangleOnScreen(clr : Color; xPos, yPos, width, height : LongInt); overload;
  begin
    FillRectangle(screen, clr, xPos, yPos, width, height);
  end;

  procedure FillRectangle(clr : Color; xPos, yPos: Single; width, height : LongInt); overload;
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
  procedure DrawLineOnScreen(clr: Color; xPosStart, yPosStart, xPosEnd, yPosEnd: LongInt); overload;
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
  
  procedure DrawHorizontalLineOnScreen(clr: Color; y, x1, x2: LongInt); overload;
  begin
    DrawHorizontalLine(screen, clr, y, x1, x2);
  end;

  procedure DrawHorizontalLine(clr: Color; y, x1, x2: Single); overload;
  begin
    DrawHorizontalLine(screen, clr, sgCamera.ToScreenY(y), sgCamera.ToScreenX(x1), sgCamera.ToScreenX(x2));
  end;

  procedure DrawVerticalLineOnScreen(clr: Color; x, y1, y2: LongInt); overload;
  begin
    DrawVerticalLine(screen, clr, x, y1, y2);
  end;

  procedure DrawVerticalLine(clr: Color; x, y1, y2: Single); overload;
  begin
    DrawVerticalLine(screen, clr, sgCamera.ToScreenX(x), sgCamera.ToScreenY(y1), sgCamera.ToScreenY(y2));
  end;
  
  //=============================================================================
  
  procedure DrawCircleOnScreen(clr: Color; filled: Boolean; xc, yc: Single; radius: LongInt); overload;
  begin
    DrawCircle(screen, clr, filled, xc, yc, radius);
  end;
  
  procedure DrawCircle(clr: Color; filled: Boolean; xc, yc: Single; radius: LongInt); overload;
  begin
    DrawCircle(screen, clr, filled, sgCamera.ToScreenX(xc), sgCamera.ToScreenY(yc), radius);
  end;
  
  procedure DrawCircleOnScreen(clr: Color; xc, yc: Single; radius: LongInt); overload;
  begin
    DrawCircle(screen, clr, xc, yc, radius);
  end;
  
  procedure DrawCircle(clr: Color; xc, yc: Single; radius: LongInt); overload;
  begin
    DrawCircle(screen, clr, sgCamera.ToScreenX(xc), sgCamera.ToScreenY(yc), radius);
  end;

  procedure FillCircleOnScreen(clr: Color; xc, yc: Single; radius: LongInt); overload;
  begin
    FillCircle(screen, clr, xc, yc, radius);
  end;

  procedure FillCircle(clr: Color; xc, yc: Single; radius: LongInt); overload;
  begin
    FillCircle(screen, clr, sgCamera.ToScreenX(xc), sgCamera.ToScreenY(yc), radius);
  end;
  
  procedure DrawCircle(dest: Bitmap; clr: Color; filled: Boolean; xc, yc: Single; radius: LongInt); overload;
  begin
    if filled then FillCircle(dest, clr, xc, yc, radius)
    else DrawCircle(dest, clr, xc, yc, radius);
  end;

  procedure DrawCircle(dest: Bitmap; clr: Color; xc, yc: Single; radius: LongInt); overload;
  begin
    aacircleColor(dest^.surface, Round(xc), Round(yc), Abs(radius), ToGfxColor(clr));
  end;

  procedure FillCircle(dest: Bitmap; clr: Color; xc, yc: Single; radius: LongInt);
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
  
  procedure DrawEllipseOnScreen(clr: Color; filled: Boolean; xPos, yPos, width, height: LongInt); overload;
  begin
    DrawEllipse(screen, clr, filled, xPos, yPos, width, height);
  end;

  procedure DrawEllipse(clr: Color; filled: Boolean; xPos, yPos: Single; width, height: LongInt); overload;
  begin
    DrawEllipse(screen, clr, filled, sgCamera.ToScreenX(xPos), sgCamera.ToScreenY(yPos), width, height);
  end;

  procedure DrawEllipseOnScreen(clr: Color; xPos, yPos, width, height: LongInt); overload;
  begin
    DrawEllipse(screen, clr, xPos, yPos, width, height);
  end;

  procedure DrawEllipse(clr: Color; xPos, yPos: Single; width, height: LongInt); overload;
  begin
    DrawEllipse(screen, clr, sgCamera.ToScreenX(xPos), sgCamera.ToScreenY(yPos), width, height);
  end;
  
  procedure FillEllipseOnScreen(clr: Color;  xPos, yPos, width, height: LongInt); overload;
  begin
    FillEllipse(screen, clr, xPos, yPos, width, height);
  end;
  
  procedure FillEllipse(clr: Color;  xPos, yPos: Single; width, height: LongInt); overload;
  begin
    FillEllipse(screen, clr, sgCamera.ToScreenX(xPos), sgCamera.ToScreenY(yPos), width, height);
  end;
  
  procedure DrawEllipse(dest: Bitmap; clr: Color; filled: Boolean; xPos, yPos, width, height: LongInt); overload;
  begin
    if filled then FillEllipse(dest, clr, xPos, yPos, width, height)
    else DrawEllipse(dest, clr, xPos, yPos, width, height);
  end;
  
  procedure DrawEllipse(dest: Bitmap; clr: Color;  xPos, yPos, width, height: LongInt); overload;
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

  procedure FillEllipse(dest: Bitmap; clr: Color; xPos, yPos, width, height: LongInt);
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
  
  procedure DrawRectangle(dest: Bitmap; clr: Color; filled : Boolean; xPos, yPos, width, height : LongInt); overload;
  begin
    if filled then FillRectangle(dest, clr, xPos, yPos, width, height)
    else DrawRectangle(dest, clr, xPos, yPos, width, height);
  end;

  procedure DrawRectangle(dest: Bitmap; clr : Color; xPos, yPos, width, height : LongInt); overload;
  begin
    if dest = nil then begin RaiseException('No destination bitmap supplied'); exit; end;
    rectangleColor(dest^.surface, xPos, yPos, xPos + width, yPos + height, ToGfxColor(clr));
  end;

  procedure FillRectangle(dest: Bitmap; clr : Color;  xPos, yPos, width, height : LongInt);
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
    boxColor(dest^.surface, rect.x, rect.y, rect.x + width, rect.y + height, ToGfxColor(clr));
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
  procedure DrawVerticalLine(dest: Bitmap; clr: Color; x, y1, y2: LongInt);
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
  procedure DrawHorizontalLine(dest: Bitmap; clr: Color; y, x1, x2: LongInt);
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
  procedure DrawLine(dest: Bitmap; clr: Color; xPosStart, yPosStart, xPosEnd, yPosEnd: LongInt);
  begin
    aalineColor(dest^.surface, xPosStart, yPosStart, xPosEnd, yPosEnd, ToGfxColor(clr));
  end;

  /// Draws a pixel onto the destination bitmap.
  ///
  /// @param dest:         The destination bitmap - not optimised!
  /// @param clr:     The color to draw the pixel
  /// @param x,y:         The x,y location to draw the pixel at
  ///
  /// Side Effects:
  /// - Sets one pixel on the destination bitmap
  procedure DrawPixel(dest: Bitmap; clr: Color; x, y: LongInt); overload;
  begin
    if dest = nil then begin RaiseException('The destination bitmap to draw a pixel is nil'); exit; end;
    
    if (x < 0) or (x >= dest^.surface^.w) or (y < 0) or (y >= dest^.surface^.h) then exit;
    
    //if SDL_MUSTLOCK(dest^.surface) then SDL_LockSurface(dest^.surface);
    
    PutPixel(dest^.surface, x, y, clr);
    
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

  procedure DrawCircle(dest: Bitmap; clr: Color; filled: Boolean; const point: Point2D; radius: LongInt); overload;
  begin
    DrawCircle(dest, clr, filled, Round(point.x), Round(point.y), radius);
  end;

  procedure DrawCircle(dest: Bitmap; clr: Color; const point: Point2D; radius: LongInt); overload;
  begin
    DrawCircle(dest, clr, Round(point.x), Round(point.y), radius);
  end;

  procedure FillCircle(dest: Bitmap; clr: Color; const point: Point2D; radius: LongInt); overload;
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

  procedure DrawCircle(clr: Color; filled: Boolean; const position: Point2D; radius: LongInt); overload;
  begin
    DrawCircle(clr, filled, Round(position.x), Round(position.y), radius);
  end;

  procedure DrawCircle(clr: Color; const position: Point2D; radius: LongInt); overload;
  begin
    DrawCircle(clr, Round(position.x), Round(position.y), radius);
  end;

  procedure FillCircle(clr: Color; const position: Point2D; radius: LongInt); overload;
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
  
  procedure DrawCircleOnScreen(clr: Color; filled: Boolean; const position: Point2D; radius: LongInt); overload;
  begin
    DrawCircleOnScreen(clr, filled, Round(position.x), Round(position.y), radius);
  end;

  procedure DrawCircleOnScreen(clr: Color; const position: Point2D; radius: LongInt); overload;
  begin
    DrawCircleOnScreen(clr, Round(position.x), Round(position.y), radius);
  end;

  procedure FillCircleOnScreen(clr: Color; const position: Point2D; radius: LongInt); overload;
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
    if bmp = nil then begin RaiseException('Cannot reset clip, bmp must not be nil'); exit; end;
    SDL_SetClipRect(bmp^.surface, nil);
  end;

  procedure ResetClip(); overload;
  begin
    ResetClip(screen);
  end;
  
  procedure SetClip(bmp: Bitmap; x, y, w, h: LongInt); overload;
  var
    rect: SDL_Rect;
  begin
    if bmp = nil then begin RaiseException('Cannot set clip, bmp must not be nil'); exit; end;
    rect := NewSDLRect(x, y, w, h);
    SDL_SetClipRect(bmp^.surface, @rect);
  end;
  
  procedure SetClip(bmp: Bitmap; const r: Rectangle); overload;
  begin
    SetClip(bmp, Round(r.x), Round(r.y), r.width, r.height);
  end;

  procedure SetClip(x, y, w, h: LongInt); overload;
  begin
    SetClip(screen, x, y, w, h);
  end;
  
  procedure SetClip(const r: Rectangle); overload;
  begin
    SetClip(screen, Round(r.x), Round(r.y), r.width, r.height);
  end;
  
  //=============================================================================
  
  procedure DrawLines(clr: Color; const lines: LinesArray); //TODO: overload;
  var
    i: LongInt;
  begin
    for i := 0 to High(lines) do
    begin
      DrawLine(clr, lines[i]);
    end;
  end;

  //=============================================================================
  
  procedure DrawShapeAsPoint(dest: Bitmap; s:Shape; filled: Boolean); overload;
  var
    pts: Point2DArray;
  begin
    pts := ShapePoints(s);
    if length(pts) = 0 then exit;
    
    DrawPixel(dest, s^.color, pts[0]);
  end;
  
  procedure DrawShapeAsCircle(dest: Bitmap; s:Shape; filled: Boolean); overload;
  var
    r: Single;
    pts: Point2DArray;
  begin
    pts := ShapePoints(s);
    if length(pts) < 2 then exit;
    
    r := PointPointDistance(pts[0], pts[1]);
    
    DrawCircle(dest, s^.color, filled, pts[0], Round(r));
  end;
  
  procedure DrawShapeAsEllipse(dest: Bitmap; s:Shape; filled: Boolean); overload;
  var
    pts: Point2DArray;
  begin
    pts := ShapePoints(s);
    if length(pts) < 2 then exit;
    
    DrawEllipse(dest, s^.color, filled, 
      Round(pts[0].x),
      Round(pts[0].y), 
      Round(pts[1].x) - Round(pts[0].x), 
      Round(pts[1].y) - Round(pts[0].y));
  end;
  
  procedure DrawShapeAsLine(dest: Bitmap; s:Shape; filled: Boolean); overload;
  var
    pts: Point2DArray;
  begin
    pts := ShapePoints(s);
    if length(pts) < 2 then exit;
    
    DrawLine(dest, s^.color, pts[0], pts[1]);
  end;
  
  procedure DrawShapeAsTriangle(dest: Bitmap; s:Shape; filled: Boolean); overload;
  var
    pts: Point2DArray;
  begin
    pts := ShapePoints(s);
    if length(pts) < 3 then exit;
    
    if filled then
      FillTriangle(dest, s^.color, pts[0].x, pts[0].y, pts[1].x, pts[1].y, pts[2].x, pts[2].y)
    else
      DrawTriangle(dest, s^.color, pts[0].x, pts[0].y, pts[1].x, pts[1].y, pts[2].x, pts[2].y);
  end;
  
  procedure DrawShapeAsLineList(dest: Bitmap; s: Shape; filled: Boolean);
  var
    i: LongInt;
    pts: Point2DArray;
  begin
    pts := ShapePoints(s);
    
    for i := 0 to Length(pts) div 2 - 1 do
    begin
      DrawLine(dest, s^.color, pts[i * 2], pts[i * 2 + 1]);
    end;
  end;
  
  procedure DrawShapeAsLineStrip(dest: Bitmap; s: Shape; filled: Boolean);
  var
    i: LongInt;
    pts: Point2DArray;
  begin
    pts := ShapePoints(s);
    
    for i := 0 to Length(pts) - 2 do
    begin
      DrawLine(dest, s^.color, pts[i], pts[i+ 1]);
    end;
  end;
  
  procedure DrawShapeAsPolygon(dest: Bitmap; s: Shape; filled: Boolean);
  var
    i, l: LongInt;
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
    
  end;
  
  procedure DrawTriangleFan(dest: Bitmap; s: Shape);
  var
    i: LongInt;
    pts: Point2DArray;
  begin
    pts := ShapePoints(s);
    
    for i := 0 to Length(pts) - 3 do
    begin
      DrawTriangle(dest, s^.color,
        pts[0].x,pts[0].y,
        pts[i + 1].x, pts[i + 1].y,
        pts[i + 2].x, pts[i + 2].y);
    end;
  end;
  
  procedure FillTriangleFan(dest: Bitmap; s: Shape);
  var
    i: LongInt;
    pts: Point2DArray;
  begin
    pts := ShapePoints(s);
    
    for i := 0 to Length(pts) - 3 do
    begin
      FillTriangle(dest, s^.color,
        pts[0].x,pts[0].y,
        pts[i + 1].x, pts[i + 1].y,
        pts[i + 2].x, pts[i + 2].y);
    end;
  end;
  
  procedure DrawShapeAsTriangleFan(dest: Bitmap; s: Shape; filled: Boolean);
  begin
    if filled then FillTriangleFan(dest, s) else DrawTriangleFan(dest, s);
  end;
  
  procedure DrawTriangleStrip(dest: Bitmap; s: Shape);
  var
    i: LongInt;
    pts: Point2DArray;
  begin
    pts := ShapePoints(s);
    
    for i := 0 to Length(pts) - 3 do
    begin
      DrawTriangle(dest, s^.color,
        pts[i].x,pts[i].y,
        pts[i + 1].x, pts[i + 1].y,
        pts[i + 2].x, pts[i + 2].y);
    end;
  end;
  
  procedure FillTriangleStrip(dest: Bitmap; s: Shape);
  var
    i: LongInt;
    pts: Point2DArray;
  begin
    pts := ShapePoints(s);
    
    for i := 0 to Length(pts) - 3 do
    begin
      FillTriangle(dest, s^.color,
        pts[i].x, pts[i].y,
        pts[i + 1].x, pts[i + 1].y,
        pts[i + 2].x, pts[i + 2].y);
    end;
  end;
  
  procedure DrawShapeAsTriangleStrip(dest: Bitmap; s: Shape; filled: Boolean);
  begin
    if filled then FillTriangleStrip(dest, s) else DrawTriangleStrip(dest, s);
  end;
  
  procedure DrawTriangleList(dest: Bitmap; s: Shape);
  var
    i: LongInt;
    pts: Point2DArray;
  begin
    pts := ShapePoints(s);
    
    for i := 0 to Length(pts) div 3 - 1 do
    begin
      DrawTriangle(dest, s^.color,
        pts[i * 3].x, pts[i * 3].y,
        pts[i * 3 + 1].x, pts[i * 3 + 1].y,
        pts[i * 3 + 2].x, pts[i * 3 + 2].y);
    end;
  end;
  
  procedure FillTriangleList(dest: Bitmap; s: Shape);
  var
    i: LongInt;
    pts: Point2DArray;
  begin
    pts := ShapePoints(s);
    
    for i := 0 to Length(pts) div 3 - 1 do
    begin
      FillTriangle(dest, s^.color,
        pts[i * 3].x, pts[i * 3].y,
        pts[i * 3 + 1].x, pts[i * 3 + 1].y,
        pts[i * 3 + 2].x, pts[i * 3 + 2].y);
    end;
  end;
  
  procedure DrawShapeAsTriangleList(dest: Bitmap; s: Shape; filled: Boolean);
  begin
    if filled then FillTriangleList(dest, s) else DrawTriangleList(dest, s);
  end;
  
  procedure DrawShape(dest: Bitmap; s: Shape; filled: Boolean); overload;
  var
    i: LongInt;
  begin
    s^.prototype^.drawWith(dest, s, filled);
    
    for i := 0 to High(s^.subShapes) do
    begin
      DrawShape(s^.subShapes[i], filled);
    end;
  end;
  
  procedure DrawShape(dest: Bitmap; s: Shape); overload;
  begin
    DrawShape(dest, s, false);
  end;
  
  procedure FillShape(dest: Bitmap; s: Shape); overload;
  begin
    DrawShape(dest, s, true);
  end;
  
  procedure DrawShape(s: Shape; filled: Boolean); overload;
  begin
    DrawShape(screen, s, filled);
  end;
  
  procedure DrawShape(s: Shape); overload;
  begin
    DrawShape(screen, s, false);
  end;
  
  procedure FillShape(s: Shape); overload;
  begin
    DrawShape(screen, s, true);
  end;
  
  procedure DrawShapeOnScreen(s: Shape; filled: Boolean); overload;
  var
    i: LongInt;
  begin
    s^.pt.x += CameraX(); s^.pt.y += CameraY();
    s^.prototype^.drawWith(screen, s, filled);
    s^.pt.x -= CameraX(); s^.pt.y -= CameraY();
    
    for i := 0 to High(s^.subShapes) do
    begin
      DrawShapeOnScreen(s^.subShapes[i], filled);
    end;
  end;

  procedure DrawShapeOnScreen(s: Shape); overload;
  begin
    DrawShapeOnScreen(s, false);
  end;
  
  procedure FillShapeOnScreen(s: Shape); overload;
  begin
    DrawShapeOnScreen(s, true);
  end;
  
//=============================================================================
  
  initialization
  begin
    InitialiseSwinGame();
  end;
end.