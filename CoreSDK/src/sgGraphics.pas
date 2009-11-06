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
//=============================================================================

  uses sgTypes;

  //---------------------------------------------------------------------------
  // Bitmap drawing routines
  //---------------------------------------------------------------------------

  /// @lib
  /// @class Bitmap
  /// @overload ClearSurface ClearSurfaceToColor
  procedure ClearSurface(dest: Bitmap; toColor: Color); overload;

  /// @lib ClearSurfaceToBlack
  /// @class Bitmap
  /// @method ClearSurface
  procedure ClearSurface(dest: Bitmap); overload;

  /// @lib DrawBitmapOnto
  /// @class Bitmap
  /// @method DrawOnto
  /// @self 2
  procedure DrawBitmap(dest: Bitmap; src: Bitmap; x, y : LongInt); overload;

  /// @lib DrawBitmapAtPointOnto
  /// @class Bitmap
  /// @overload DrawOnto DrawAtPointOnto
  /// @self 2
  procedure DrawBitmap(dest: Bitmap; src: Bitmap; const position : Point2D); overload;

  /// @lib DrawBitmapPartOnto
  /// @class Bitmap
  /// @method DrawPartOnto
  /// @self 2
  procedure DrawBitmapPart(dest: Bitmap; src: Bitmap; srcX, srcY, srcW, srcH, x, y : LongInt); overload;

  /// @lib DrawBitmapPartFromRectOnto
  /// @class Bitmap
  /// @overload DrawPartOnto DrawPartFromRectOnto
  procedure DrawBitmapPart(dest: Bitmap; src: Bitmap; const source: Rectangle; x, y : LongInt); overload;

  /// @lib DrawBitmapPartFromRectAtPointOnto
  /// @class Bitmap
  /// @overload DrawPartOnto DrawPartFromRectAtPointOnto
  procedure DrawBitmapPart(dest: Bitmap; src: Bitmap; const source: Rectangle; const position: Point2D); overload;

  /// @lib DrawPixelOnto
  procedure DrawPixel(dest: Bitmap; clr: Color; x, y: LongInt); overload;
  /// @lib DrawPixelAtPointOnto
  procedure DrawPixel(dest: Bitmap; clr: Color; const position: Point2D); overload;

  /// @lib DrawOrFillRectangleOnto
  procedure DrawRectangle(dest: Bitmap; clr : Color; filled : Boolean; xPos, yPos, width, height : LongInt); overload;
  /// @lib DrawOrFillRectangleRectOnto
  procedure DrawRectangle(dest: Bitmap; clr : Color; filled : Boolean; const source: Rectangle); overload;
  /// @lib DrawOrFillRectangleOnto(dest, clr, False, xPos, yPos, width, height)
  /// @uname DrawRectangleOnto
  procedure DrawRectangle(dest: Bitmap; clr : Color; xPos, yPos, width, height: LongInt); overload;
  /// @lib DrawOrFillRectangleRectOnto(dest, clr, False, source)
  /// @uname DrawRectangleRectOnto
  procedure DrawRectangle(dest: Bitmap; clr : Color; const source: Rectangle); overload;
  /// @lib DrawOrFillRectangleOnto(dest, clr, True, xPos, yPos, width, height)
  /// @uname FillRectangleOnto
  procedure FillRectangle(dest: Bitmap; clr : Color; xPos, yPos, width, height : LongInt); overload;
  /// @lib DrawOrFillRectangleRectOnto(dest, clr, True, source)
  /// @uname FillRectangleRectOnto
  procedure FillRectangle(dest: Bitmap; clr : Color; const source: Rectangle); overload;

  /// @lib DrawLineOnto
  procedure DrawLine(dest: Bitmap; clr: Color; xPosStart, yPosStart, xPosEnd, yPosEnd: LongInt); overload;
  /// @lib DrawLineSegmentOnto
  procedure DrawLine(dest: Bitmap; clr: Color; const line: LineSegment); overload;
  /// @lib DrawLinePtsOnto
  procedure DrawLine(dest: Bitmap; clr: Color; const startPt, endPt: Point2D); overload;

  /// @lib DrawHorizontalLineOnto
  procedure DrawHorizontalLine(dest: Bitmap; clr: Color; y, x1, x2: LongInt); overload;
  /// @lib DrawVerticalLineOnto
  procedure DrawVerticalLine(dest: Bitmap; clr: Color; x, y1, y2: LongInt); overload;



  //---------------------------------------------------------------------------
  // Circle drawing code
  //---------------------------------------------------------------------------

  /// @lib DrawOrFillPtCircleOnto
  procedure DrawCircle(dest: Bitmap; clr: Color; filled: Boolean; xc, yc: Single; radius: LongInt); overload;
  /// @lib DrawOrFillPtCircleOnto(dest, clr, False, xc, yc, radius)
  /// @uname DrawPtCircleOnto
  procedure DrawCircle(dest: Bitmap; clr: Color; xc, yc: Single; radius: LongInt); overload;
  /// @lib DrawOrFillPtCircleOnto(dest, clr, True, xc, yc, radius)
  /// @uname FillPtCircleOnto
  procedure FillCircle(dest: Bitmap; clr: Color; xc, yc: Single; radius: LongInt); overload;

  /// @lib DrawOrFillPtCircleAtPointOnto
  procedure DrawCircle(dest: Bitmap; clr: Color; filled: Boolean; const point: Point2D; radius: LongInt); overload;
  /// @lib DrawOrFillPtCircleAtPointOnto(dest, clr, False, point, radius)
  /// @uname DrawCircleAtPointOnto
  procedure DrawCircle(dest: Bitmap; clr: Color; const point: Point2D; radius: LongInt); overload;
  /// @lib DrawOrFillPtCircleAtPointOnto(dest, clr, True, point, radius)
  /// @uname FillCircleAtPointOnto
  procedure FillCircle(dest: Bitmap; clr: Color; const point: Point2D; radius: LongInt); overload;

  /// @lib DrawOrFillCircleOnto
  procedure DrawCircle(dest: Bitmap; clr: Color; filled: Boolean; const c: Circle); overload;
  /// @lib DrawOrFillCircleOnto(dest, clr, False, c)
  /// @uname DrawCircleOnto
  procedure DrawCircle(dest: Bitmap; clr: Color; const c: Circle); overload;
  /// @lib DrawOrFillCircleOnto(dest, clr, True, c)
  /// @uname FillCircleOnto
  procedure FillCircle(dest: Bitmap; clr: Color; const c: Circle); overload;

  /// @lib DrawOrFillPtCircle
  procedure DrawCircle(clr: Color; filled: Boolean; xc, yc: Single; radius: LongInt); overload;
  /// @lib DrawOrFillPtCircle(clr, False, xc, yc, radius)
  /// @uname DrawPtCircle
  procedure DrawCircle(clr: Color; xc, yc: Single; radius: LongInt); overload;
  /// @lib DrawOrFillPtCircle(clr, True, xc, yc, radius)
  /// @uname FillPtCircle
  procedure FillCircle(clr: Color; xc, yc: Single; radius: LongInt); overload;

  /// @lib DrawOrFillPtCircleAtPoint
  procedure DrawCircle(clr: Color; filled: Boolean; const position: Point2D; radius: LongInt); overload;
  /// @lib DrawOrFillPtCircleAtPoint(clr, False, position, radius)
  /// @uname DrawPtCircleAtPoint
  procedure DrawCircle(clr: Color; const position: Point2D; radius: LongInt); overload;
  /// @lib DrawOrFillPtCircleAtPoint(clr, True, position, radius)
  /// @uname FillPtCircleAtPoint
  procedure FillCircle(clr: Color; const position: Point2D; radius: LongInt); overload;

  /// @lib DrawOrFillCircle
  procedure DrawCircle(clr: Color; filled: Boolean; const c: Circle); overload;
  /// @lib DrawOrFillCircle(clr, False, c)
  /// @uname DrawCircle
  procedure DrawCircle(clr: Color; const c: Circle); overload;
  /// @lib DrawOrFillCircle(clr, True, c)
  /// @uname FillCircle
  procedure FillCircle(clr: Color; const c: Circle); overload;

  /// @lib DrawOrFillPtCircleOnScreen
  procedure DrawCircleOnScreen(clr: Color; filled: Boolean; xc, yc: Single; radius: LongInt); overload;
  /// @lib DrawOrFillPtCircleOnScreen(clr, False, xc, yc, radius)
  /// @uname DrawCirclePtOnScreen
  procedure DrawCircleOnScreen(clr: Color; xc, yc: Single; radius: LongInt); overload;
  /// @lib DrawOrFillPtCircleOnScreen(clr, True, xc, yc, radius)
  /// @uname FillCirclePtOnScreen
  procedure FillCircleOnScreen(clr: Color; xc, yc: Single; radius: LongInt); overload;

  /// @lib DrawOrFillCircleAtPointOnScreen
  procedure DrawCircleOnScreen(clr: Color; filled: Boolean; const position: Point2D; radius: LongInt); overload;
  /// @lib DrawOrFillCircleAtPointOnScreen(clr, False, position, radius)
  /// @uname DrawCircleAtPointOnScreen
  procedure DrawCircleOnScreen(clr: Color; const position: Point2D; radius: LongInt); overload;
  /// @lib DrawOrFillCircleAtPointOnScreen(clr, True, position, radius)
  /// @uname FillCircleAtPointOnScreen
  procedure FillCircleOnScreen(clr: Color; const position: Point2D; radius: LongInt); overload;

  /// @lib DrawOrFillCircleOnScreen
  procedure DrawCircleOnScreen(clr: Color; filled: Boolean; const c: Circle); overload;
  /// @lib DrawOrFillCircleOnScreen(clr, False, c)
  /// @uname DrawCircleOnScreen
  procedure DrawCircleOnScreen(clr: Color; const c: Circle); overload;
  /// @lib DrawOrFillCircleOnScreen(clr, True, c)
  /// @uname FillCircleOnScreen
  procedure FillCircleOnScreen(clr: Color; const c: Circle); overload;



  //---------------------------------------------------------------------------
  // Ellipse drawing code
  //---------------------------------------------------------------------------

  /// @lib DrawOrFillEllipseOnto
  procedure DrawEllipse(dest: Bitmap; clr: Color; filled: Boolean; xPos, yPos, width, height: LongInt); overload;
  /// @lib DrawOrFillEllipseInRectOnto
  procedure DrawEllipse(dest: Bitmap; clr: Color; filled: Boolean; const source: Rectangle); overload;
  /// @lib DrawOrFillEllipseOnto(dest, clr, False, xPos, yPos, width, height)
  /// @uname DrawEllipseOnto
  procedure DrawEllipse(dest: Bitmap; clr: Color; xPos, yPos, width, height: LongInt); overload;
  /// @lib DrawOrFillEllipseInRectOnto(dest, clr, False, source)
  /// @uname DrawEllipseInRectOnto
  procedure DrawEllipse(dest: Bitmap; clr: Color; const source: Rectangle); overload;
  /// @lib DrawOrFillEllipseOnto(dest, clr, True, xPos, yPos, width, height)
  /// @uname FillEllipseOnto
  procedure FillEllipse(dest: Bitmap; clr: Color; xPos, yPos, width, height: LongInt); overload;
  /// @lib DrawOrFillEllipseInRectOnto(dest, clr, True, source)
  /// @uname FillEllipseInRectOnto
  procedure FillEllipse(dest: Bitmap; clr: Color; const source: Rectangle); overload;
  
  
  
  //---------------------------------------------------------------------------
  // Triangle drawing code
  //---------------------------------------------------------------------------
  
  /// @lib DrawOrFillTriangleOnto
  procedure DrawTriangle(dest: Bitmap; clr: Color; filled: Boolean; const tri: Triangle); overload;
  /// @lib DrawOrFillTriangleOnto(dest, clr, False, tri)
  /// @uname DrawTriangleOnto
  procedure DrawTriangle(dest: Bitmap; clr: Color; const tri: Triangle); overload;
  /// @lib DrawOrFillTriangleOnto(dest, clr, True, tri)
  /// @uname FillTriangleOnto
  procedure FillTriangle(dest: Bitmap; clr: Color; const tri: Triangle); overload;
  /// @lib DrawTriangleFromPointsOnto
  procedure DrawTriangle(dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  /// @lib FillTriangleFromPointsOnto
  procedure FillTriangle(dest: Bitmap; clr: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  
  
  
  //---------------------------------------------------------------------------
  // Shape drawing code
  //---------------------------------------------------------------------------
  
  /// Draw the Shape s onto the destination bitmap. The filled boolean indicates
  /// if the Shape should be filled.
  ///
  /// @lib DrawOrFillShapeOnto
  /// @uname DrawOrFillShapeOnto
  /// @class Shape
  /// @overload Draw DrawOrFillOnto
  /// @self 2
  procedure DrawShape(dest: Bitmap; s: Shape; filled: Boolean); overload;
  
  /// Draw the Shape s onto the destination bitmap.
  ///
  /// @lib DrawShapeOnto
  /// @uname DrawShapeOnto
  /// @class Shape
  /// @overload Draw DrawOnto
  /// @self 2
  procedure DrawShape(dest: Bitmap; s: Shape); overload;
  
  /// Fill the Shape s onto the destination bitmap.
  ///
  /// @lib FillShapeOnto
  /// @uname FillShapeOnto
  /// @class Shape
  /// @overload Fill FillOnto
  /// @self 2
  procedure FillShape(dest: Bitmap; s: Shape); overload;
  
  /// Draw or fill the Shape s onto the screen at the 
  /// shapes game coordinates.
  ///
  /// @lib DrawOrFillShape
  /// @uname DrawOrFillShape
  /// @class Shape
  /// @overload Draw DrawOrFill
  procedure DrawShape(s: Shape; filled: Boolean); overload;
  
  /// Draw the Shape s onto the screen at the 
  /// shapes game coordinates.
  ///
  /// @lib DrawShape
  /// @uname DrawShape
  /// @class Shape
  /// @method Draw
  procedure DrawShape(s: Shape); overload;
  
  /// Fill the Shape s.
  ///
  /// @lib FillShape
  /// @uname FillShape
  /// @class Shape
  /// @method Fill
  procedure FillShape(s: Shape); overload;
  
  /// Draw or fill the Shape s onto the screen using the
  /// Shape's location as screen coordinates.
  ///
  /// @lib DrawOrFillShapeOnScreen
  /// @uname DrawOrFillShapeOnScreen
  /// @class Shape
  /// @overload DrawOnScreen DrawOrFillOnScreen
  procedure DrawShapeOnScreen(s: Shape; filled: Boolean); overload;
  
  /// Draw the Shape s onto the screen using the
  /// Shape's location as screen coordinates.
  ///
  /// @lib DrawShapeOnScreen
  /// @uname DrawShapeOnScreen
  /// @class Shape
  /// @method DrawOnScreen
  procedure DrawShapeOnScreen(s: Shape); overload;
  
  /// Fill the Shape s onto the screen using the
  /// Shape's location as screen coordinates.
  ///
  /// @lib FillShapeOnScreen
  /// @uname FillShapeOnScreen
  /// @class Shape
  /// @method FillOnScreen
  procedure FillShapeOnScreen(s: Shape); overload;
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This version
  /// Draws the first point of the shape as a pixel.
  ///
  /// @lib
  /// @class Shape
  /// @method DrawAsPoint
  /// @self 2
  procedure DrawShapeAsPoint(dest: Bitmap; s:Shape; filled: Boolean); overload;
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This version
  /// draws the shape as a circle, centered on the first point with a radius defined
  /// by the distance to the second point.
  ///
  /// @lib
  /// @class Shape
  /// @method DrawAsCircle
  /// @self 2
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
  /// @class Shape
  /// @method DrawAsLine
  /// @self 2
  procedure DrawShapeAsLine(dest: Bitmap; s:Shape; filled: Boolean); overload;
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This version
  /// draws the shape as a triangle based on the first three points of the shape.
  ///
  /// @lib
  /// @class Shape
  /// @method DrawAsTriangle
  /// @self 2
  procedure DrawShapeAsTriangle(dest: Bitmap; s:Shape; filled: Boolean); overload;
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This version
  /// draws the points as a list of lines. A shape with 4 points has two lines in its
  /// list. If an odd numer of points are supplied then the extra point will be skipped.
  /// In this way a shape with 5 points also has 2 lines.
  ///
  /// @lib
  /// @class Shape
  /// @method DrawAsLineList
  /// @self 2
  procedure DrawShapeAsLineList(dest: Bitmap; s: Shape; filled: Boolean);
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This version
  /// draws the shape as a line strip. A shape with three points has two lines, one 
  /// from pt[0] to pt[1] and a second from pt[1] to pt[2].
  ///
  /// @lib
  /// @class Shape
  /// @method DrawAsLineStrip
  /// @self 2
  procedure DrawShapeAsLineStrip(dest: Bitmap; s: Shape; filled: Boolean);
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This draws
  /// as a polygon where each point is connected to its neighbour and the
  /// first point is reconnected to the last point.
  ///
  /// @lib
  /// @class Shape
  /// @method DrawAsPolygon
  /// @self 2
  procedure DrawShapeAsPolygon(dest: Bitmap; s: Shape; filled: Boolean);
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This version
  /// draws the shape as a tan of triangles where each triangle is made up of
  /// the first point and two neighbouring points from the shape.
  ///
  /// @lib
  /// @class Shape
  /// @method DrawAsTriangleFan
  /// @self 2
  procedure DrawShapeAsTriangleFan(dest: Bitmap; s: Shape; filled: Boolean);
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This version
  /// draws as a strip of triangles where each triangle is made up of the 
  /// three neighbouring points. In this way 4 points gives two triangles.
  ///
  /// @lib
  /// @class Shape
  /// @method DrawAsTriangleStrip
  /// @self 2
  procedure DrawShapeAsTriangleStrip(dest: Bitmap; s: Shape; filled: Boolean);
  
  /// Draw the passed in shape to the specified bitmap. If filled the shape
  /// is drawn with a fill rather than drawn as a series of lines. This version
  /// draws as a triangle list, where each set of three points is drawn as an
  /// individual triangle and extra points are ignored. So 6, 7, and 8 points 
  /// all create 2 triangles (pt[0] + pt[1] + pt[2] and pt[3] + pt[4] + pt[5]). 
  ///
  /// @lib
  /// @class Shape
  /// @method DrawAsTriangleList
  /// @self 2
  procedure DrawShapeAsTriangleList(dest: Bitmap; s: Shape; filled: Boolean);
  
  //---------------------------------------------------------------------------
  // Screen drawing routines
  //---------------------------------------------------------------------------
  // These routines are used to to the View Area on the screen.

  /// @lib ClearScreenToBlack
  /// @uname ClearScreen
  procedure ClearScreen(); overload;
  /// @lib
  /// @uname ClearScreenWithColor
  procedure ClearScreen(toColor : Color); overload;

  /// @lib
  ///
  /// @class Bitmap
  /// @method Draw
  procedure DrawBitmap(src : Bitmap; x, y : Single); overload;
  /// @lib DrawBitmapAtPoint
  ///
  /// @class Bitmap
  /// @overload Draw DrawAtPoint
  procedure DrawBitmap(src : Bitmap; const position : Point2D); overload;

  /// @lib
  ///
  /// @class Bitmap
  /// @method DrawPart
  procedure DrawBitmapPart(src : Bitmap; srcX, srcY, srcW, srcH: LongInt; x, y : Single); overload;
  
  /// @lib DrawBitmapPartFromRect
  ///
  /// @class Bitmap
  /// @overload DrawPart DrawPartFromRect
  procedure DrawBitmapPart(src : Bitmap; const source : Rectangle; x, y : Single); overload;
  
  /// @lib DrawBitmapPartFromRectAtPoint
  ///
  /// @class Bitmap
  /// @overload DrawPart DrawPartFromRectAtPoint
  procedure DrawBitmapPart(src : Bitmap; const source : Rectangle; const position : Point2D); overload;
  
  /// @lib
  procedure DrawPixel(clr: Color; x, y: Single); overload;
  
  /// @lib DrawPixelAtPoint
  procedure DrawPixel(clr: Color; const position: Point2D); overload;

  /// @lib DrawOrFillRectangle
  procedure DrawRectangle(clr: Color; filled: Boolean; xPos, yPos: Single; width, height: LongInt); overload;
  /// @lib DrawOrFillRectangleRect
  procedure DrawRectangle(clr: Color; filled: Boolean; const source: Rectangle); overload;
  /// @lib DrawOrFillRectangle(clr, False, xPos, yPos, width, height)
  /// @uname DrawRectangle
  procedure DrawRectangle(clr: Color; xPos, yPos: Single; width, height: LongInt); overload;
  /// @lib DrawOrFillRectangleRect(clr, False, source)
  /// @uname DrawRectangleRect
  procedure DrawRectangle(clr: Color; const source: Rectangle); overload;
  /// @lib DrawOrFillRectangle(clr, True, xPos, yPos, width, height)
  /// @uname FillRectangle
  procedure FillRectangle(clr: Color; xPos, yPos: Single; width, height: LongInt); overload;
  /// @lib DrawOrFillRectangleRect(clr, True, source)
  /// @uname FillRectangleRect
  procedure FillRectangle(clr: Color; const source: Rectangle); overload;

  //TODO: Other overloads!
  /// @lib DrawLineSegments
  procedure DrawLines(clr: Color; const lines: LinesArray); //overload;


  /// @lib
  procedure DrawLine(clr: Color; xPosStart, yPosStart, xPosEnd, yPosEnd: Single); overload;
  /// @lib DrawLineSegment
  procedure DrawLine(clr: Color; const line: LineSegment); overload;
  /// @lib DrawLinePts
  procedure DrawLine(clr: Color; const startPt, endPt: Point2D); overload;

  /// @lib DrawOrFillTriangle
  procedure DrawTriangle(clr: Color; filled: Boolean; const tri: Triangle); overload;
  /// @lib DrawOrFillTriangle(clr, False, tri)
  /// @uname DrawTriangle
  procedure DrawTriangle(clr: Color; const tri: Triangle); overload;
  /// @lib DrawOrFillTriangle(clr, True, tri)
  /// @uname FillTriangle
  procedure FillTriangle(clr: Color; const tri: Triangle); overload;
  /// @lib DrawTriangleFromPoints
  procedure DrawTriangle(clr: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  /// @lib FillTriangleFromPoints
  procedure FillTriangle(clr: Color; x1, y1, x2, y2, x3, y3: Single); overload;

  /// @lib
  procedure DrawHorizontalLine(clr: Color; y, x1, x2: Single); overload;
  /// @lib
  procedure DrawVerticalLine(clr: Color; x, y1, y2: Single); overload;




  /// @lib DrawOrFillEllipse
  procedure DrawEllipse(clr: Color; filled: Boolean; xPos, yPos: Single; width, height: LongInt); overload;
  /// @lib DrawOrFillEllipseInRect
  procedure DrawEllipse(clr: Color; filled: Boolean; const source: Rectangle); overload;
  /// @lib DrawOrFillEllipse(clr, False, xPos, yPos, width, height)
  /// @uname DrawEllipse
  procedure DrawEllipse(clr: Color; xPos, yPos: Single; width, height: LongInt); overload;
  /// @lib DrawOrFillEllipseInRect(clr, False, source)
  /// @uname DrawEllipseInRect
  procedure DrawEllipse(clr: Color; const source: Rectangle); overload;
  /// @lib DrawOrFillEllipse(clr, True, xPos, yPos, width, height)
  /// @uname FillEllipse
  procedure FillEllipse(clr: Color; xPos, yPos: Single; width, height: LongInt); overload;
  /// @lib DrawOrFillEllipseInRect(clr, True, source)
  /// @uname FillEllipseInRect
  procedure FillEllipse(clr: Color; const source: Rectangle); overload;

  //---------------------------------------------------------------------------
  // Draws elements directly onto the screen, ignoring the visible window
  // setting.
  //---------------------------------------------------------------------------
  //
  // These routines are used to move the visual window.
  //

  /// @lib
  /// @class Bitmap
  /// @method DrawPartOnScreen
  procedure DrawBitmapPartOnScreen(src : Bitmap; srcX, srcY, srcW, srcH, x, y : LongInt); overload;
  /// @lib DrawBitmapPartFromRectOnScreen
  /// @class Bitmap
  /// @overload DrawPartOnScreen DrawPartFromRectOnScreen
  procedure DrawBitmapPartOnScreen(src : Bitmap; const source: Rectangle; x, y : LongInt); overload;
  /// @lib DrawBitmapPartFromRectAtPointOnScreen
  /// @class Bitmap
  /// @overload DrawPartOnScreen DrawPartOnFromRectAtPointScreen
  procedure DrawBitmapPartOnScreen(src : Bitmap; const source: Rectangle; const position: Point2D); overload;

  /// @lib
  /// @class Bitmap
  /// @method DrawOnScreen
  procedure DrawBitmapOnScreen(src : Bitmap; x, y : LongInt); overload;
  /// @lib DrawBitmapAtPointOnScreen
  /// @class Bitmap
  /// @overload DrawOnScreen DrawAtPointOnSreen
  procedure DrawBitmapOnScreen(src : Bitmap; const position : Point2D); overload;

  /// @lib
  procedure DrawPixelOnScreen(clr: Color; x, y: LongInt); overload;
  /// @lib DrawPixelAtPointOnScreen
  procedure DrawPixelOnScreen(clr: Color; const position: Point2D); overload;

  /// @lib DrawOrFillRectangleOnScreen
  procedure DrawRectangleOnScreen(clr : Color; filled : Boolean; xPos, yPos, width, height : LongInt); overload;
  /// @lib DrawOrFillRectangleOnScreen(clr, False, xPos, yPos, width, height)
  /// @uname DrawRectangleOnScreen
  procedure DrawRectangleOnScreen(clr : Color; xPos, yPos, width, height : LongInt); overload;
  /// @lib DrawOrFillRectangleOnScreen(clr, True, xPos, yPos, width, height)
  /// @uname FillRectangleOnScreen
  procedure FillRectangleOnScreen(clr : Color; xPos, yPos, width, height : LongInt); overload;
  /// @lib DrawOrFillRectangleRectOnScreen
  procedure DrawRectangleOnScreen(clr : Color; filled : Boolean; const source : Rectangle); overload;
  /// @lib DrawOrFillRectangleRectOnScreen(clr, False, source)
  /// @uname DrawRectangleRectOnScreen
  procedure DrawRectangleOnScreen(clr : Color; const source : Rectangle); overload;
  /// @lib DrawOrFillRectangleRectOnScreen(clr, True, source)
  /// @uname FillRectangleRectOnScreen
  procedure FillRectangleOnScreen(clr : Color; const source : Rectangle); overload;

  /// @lib
  procedure DrawLineOnScreen(clr: Color; xPosStart, yPosStart, xPosEnd, yPosEnd: LongInt); overload;
  /// @lib DrawLineSegmentOnScreen
  procedure DrawLineOnScreen(clr: Color; const line: LineSegment); overload;
  /// @lib DrawLinePtsOnScreen
  procedure DrawLineOnScreen(clr: Color; const startPt, endPt: Point2D); overload;

  /// @lib
  procedure DrawHorizontalLineOnScreen(clr: Color; y, x1, x2: LongInt);
  /// @lib
  procedure DrawVerticalLineOnScreen(clr: Color; x, y1, y2: LongInt);

  /// @lib DrawOrFillEllipseOnScreen
  procedure DrawEllipseOnScreen(clr: Color; filled: Boolean; xPos, yPos, width, height: LongInt); overload;
  /// @lib DrawOrFillEllipseOnScreen(clr, False, xPos, yPos, width, height)
  /// @uname DrawEllipseOnScreen
  procedure DrawEllipseOnScreen(clr: Color; xPos, yPos, width, height: LongInt); overload;
  /// @lib DrawOrFillEllipseOnScreen(clr, True, xPos, yPos, width, height)
  /// @uname FillEllipseOnScreen
  procedure FillEllipseOnScreen(clr: Color; xPos, yPos, width, height: LongInt); overload;
  /// @lib DrawOrFillEllipseInRectOnScreen
  procedure DrawEllipseOnScreen(clr: Color; filled: Boolean; const source: Rectangle); overload;
  /// @lib DrawOrFillEllipseInRectOnScreen(clr, False, source)
  /// @uname DrawEllipseInRectOnScreen
  procedure DrawEllipseOnScreen(clr: Color; const source: Rectangle); overload;
  /// @lib DrawOrFillEllipseInRectOnScreen(clr, True, source)
  /// @uname FillEllipseInRectOnScreen
  procedure FillEllipseOnScreen(clr: Color; const source: Rectangle); overload;

  /// @lib DrawOrFillTriangleOnScreen
  procedure DrawTriangleOnScreen(clr: Color; filled: Boolean; const tri: Triangle); overload;
  /// @lib DrawOrFillTriangleOnScreen(clr, False, tri)
  /// @uname DrawTriangleOnScreen
  procedure DrawTriangleOnScreen(clr: Color; const tri: Triangle); overload;
  /// @lib DrawOrFillTriangleOnScreen(clr, True, tri)
  /// @uname FillTriangleOnScreen
  procedure FillTriangleOnScreen(clr: Color; const tri: Triangle); overload;
  /// @lib DrawTriangleFromPointsOnScreen
  procedure DrawTriangleOnScreen(clr: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  /// @lib FillTriangleFromPointsOnScreen
  procedure FillTriangleOnScreen(clr: Color; x1, y1, x2, y2, x3, y3: Single); overload;

  //---------------------------------------------------------------------------
  // Clipping
  //---------------------------------------------------------------------------

  /// @lib
  procedure SetClip(x, y, w, h: LongInt); overload;
  /// @lib SetClipRect
  procedure SetClip(const r: Rectangle); overload;

  /// @lib SetClipForBitmap
  /// @class Bitmap
  /// @method SetClip
  procedure SetClip(bmp: Bitmap; x, y, w, h: LongInt); overload;

  /// @lib SetClipRectForBitmap
  /// @class Bitmap
  /// @overload SetClip SetClipRect
  procedure SetClip(bmp: Bitmap; const r: Rectangle); overload;

  /// @lib
  procedure ResetClip(); overload;
  /// @lib ResetClipForBitmap
  procedure ResetClip(bmp: Bitmap); overload;
  
  /// @lib
  /// @class Bitmap
  /// @method GetPixel
  function GetPixel(bmp: Bitmap; x, y: LongInt): Color;
  
  /// @lib
  function GetPixelFromScreen(x, y: LongInt): Color;
  

//=============================================================================
implementation
//=============================================================================

  uses Classes, SysUtils, // system
       SDL_gfx, SDL, SDL_Image, // sdl
       sgCamera, sgPhysics, sgShared, sgCore, sgGeometry, sgResources;

  /// Clears the surface of the bitmap to the passed in color.
  ///
  /// @param dest:     The bitmap to clear
  /// @param toColor: The colour to clear the bitmap to
  ///
  /// Side Effects:
  /// - dest's surface is set to the toColor
  procedure ClearSurface(dest: Bitmap; toColor: Color); overload;
  begin
    if dest = nil then
    begin
      RaiseException('Cannot clear, destination bitmap not supplied (nil)');
      exit;
    end;
    SDL_FillRect(dest^.surface, @dest^.surface^.clip_rect, toColor);
  end;

  /// Clears the surface of the bitmap to Black.
  ///
  /// @param dest:     The bitmap to clear
  ///
  /// Side Effects:
  /// - dest's surface is set to black
  procedure ClearSurface(dest: Bitmap); overload;
  begin
    ClearSurface(dest, ColorBlack);
  end;

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

  /// Draws one bitmap (src) onto another bitmap (dest).
  ///
  /// @param dest:         The destination bitmap - not optimised!
  /// @param src: The bitmap to be drawn onto the destination
  /// @param x,y:         The x,y location to draw the bitmap to
  ///
  /// Side Effects:
  /// - Draws the src at the x,y location in the destination.
  procedure DrawBitmap(dest: Bitmap; src: Bitmap; x, y : LongInt); overload;
  var
    offset: SDL_Rect;
  begin
    if (dest = nil) or (src = nil) then begin RaiseException('No bitmap supplied'); exit; end;
    
    offset := NewSDLRect(x, y, 0, 0);
    SDL_BlitSurface(src^.surface, nil, dest^.surface, @offset);
  end;

  /// Draws part of a bitmap (src) onto another bitmap (dest).
  ///
  /// @param dest:     The destination bitmap - not optimised!
  /// @param src: The bitmap to be drawn onto the destination
  /// @param srcX, srcY:   The x,y offset to the area to copy in src
  /// @param srcW, srcH:   The width and height of the area to copy
  /// @param x,y:      The x,y location to draw the bitmap part to
  ///
  /// Side Effects:
  /// - Draws part of the src at the x,y location in the destination.
  procedure DrawBitmapPart(dest: Bitmap; src: Bitmap; srcX, srcY, srcW, srcH, x, y : LongInt); overload;
  var
    offset, source: SDL_Rect;
  begin
    if (dest = nil) or (src = nil) then begin RaiseException('No bitmap supplied'); exit; end;
    if (srcW < 0) or (srcH < 0) then begin RaiseException('Width and Height must be >= 0'); exit; end;
    
    offset := NewSDLRect(x, y, 0, 0);
    source := NewSDLRect(srcX, srcY, srcW, srcH);

    SDL_BlitSurface(src^.surface, @source, dest^.surface, @offset);
  end;

  procedure DrawBitmapPart(dest: Bitmap; src: Bitmap; const source: Rectangle; x, y : LongInt); overload;
  begin
    DrawBitmapPart(dest, src, Round(source.x), Round(source.y), source.width, source.height, x, y);
  end;

  /// Draws part of a bitmap (src) onto the screen.
  ///
  /// @param src: The bitmap to be drawn onto the screen
  /// @param srcX, srcY:  The x,y offset to the area to copy in src
  /// @param srcW, srcH:  The width and height of the area to copy
  /// @param x,y:       The x,y location to draw the bitmap part to
  ///
  /// Side Effects:
  /// - Draws part of the src at the x,y location on the screen.
  /// - Effected by visible window
  procedure DrawBitmapPartOnScreen(src : Bitmap; srcX, srcY, srcW, srcH, x, y : LongInt); overload;
  begin
    DrawBitmapPart(screen, src, srcX, srcY, srcW, srcH, x, y);
  end;

  procedure DrawBitmapPart(src : Bitmap; srcX, srcY, srcW, srcH: LongInt; x, y : Single); overload;
  begin
    DrawBitmapPart(screen, src, srcX, srcY, srcW, srcH, sgCamera.ToScreenX(x), sgCamera.ToScreenY(y));
  end;

  /// Draws one bitmap (src) onto the screen.
  ///
  /// @param src:  The bitmap to be drawn onto the screen
  /// @param x,y:       The x,y location to draw the bitmap to
  ///
  /// Side Effects:
  /// - Draws the src at the x,y location on the screen.
  procedure DrawBitmapOnScreen(src : Bitmap; x, y : LongInt); overload;
  begin
    DrawBitmap(screen, src, x, y);
  end;

  procedure DrawBitmap(src : Bitmap; x, y : Single); overload;
  begin
    DrawBitmap(screen, src, sgCamera.ToScreenX(x), sgCamera.ToScreenY(y));
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
  
  
  
  
  procedure DrawBitmap(dest: Bitmap; src: Bitmap; const position: Point2D); overload;
  begin
    DrawBitmap(dest, src, Round(position.x), Round(position.y));
  end;

  procedure DrawBitmapPart(dest: Bitmap; src: Bitmap; const source: Rectangle; const position: Point2D); overload;
  begin
    DrawBitmapPart(dest, src, source, Round(position.x), Round(position.y));
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

  procedure DrawBitmap(src : Bitmap; const position : Point2D); overload;
  begin
    DrawBitmap(src, Round(position.x), Round(position.y));
  end;

  procedure DrawBitmapPart(src : Bitmap; const source : Rectangle; x, y : Single); overload;
  begin
    DrawBitmapPart(src, Round(source.x), Round(source.y), source.width, source.height, x, y);
  end;

  procedure DrawBitmapPart(src : Bitmap; const source : Rectangle; const position : Point2D); overload;
  begin
    DrawBitmapPart(src, source, Round(position.x), Round(position.y));
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

  procedure DrawBitmapPartOnScreen(src : Bitmap; const source: Rectangle; x, y : LongInt); overload;
  begin
    DrawBitmapPartOnScreen(src, Round(source.x), Round(source.y), source.width, source.height, x, y);
  end;

  procedure DrawBitmapPartOnScreen(src : Bitmap; const source: Rectangle; const position: Point2D); overload;
  begin
    DrawBitmapPartOnScreen(src, source, Round(position.x), Round(position.y));
  end;

  procedure DrawBitmapOnScreen(src : Bitmap; const position : Point2D); overload;
  begin
    DrawBitmapOnScreen(src, Round(position.x), Round(position.y))
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