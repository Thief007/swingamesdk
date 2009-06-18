//---------------------------------------------------/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
//          SGSDK_Graphics.pas
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// The Graphics unit is responsible for all of the drawing
// of anything to the screen or other surfaces. The
// ...OnScreen routines draw directly onto the screen
// ignoring the camera settings. The standard draw routines
// draw to the screen using the camera settings. Finally the
// overloaded drawing methods with a destination Bitmap will
// draw onto the supplied bitmap.
//
// Change History:
//
// Version 3.0:
// - 2009-06-05: Andrew: Using sg_Shared
//
// Version 2.0:
// - 2008-12-17: Andrew: Moved all integers to LongInt
// - 2008-12-10: Andrew: Moved primitive drawing to SDL_gfx
//             Added rotation and zoom to Sprite + Sprite Drawing
//             Added RotateZoomBitmap
//             Added MakeOpaque and MakeTransparent to allow multiple blending
//             Added extra triangle drawing code
// - 2008-12-09: Andrew: Started transition to SDL_gfx
//
// Version 1.1:
// - 2008-04-08: Stephen: Added DrawTriangle()
// - 2008-04-02: Andrew: Fixed issues related to freeing images
//             Fixed transparent pixels for non 32bit images
// - 2008-03-09: Andrew: Fixed DrawSprite with Offset
// - 2008-02-16: Andrew: Added GetPixel and GetPixelFromScreen
// - 2008-01-31: Andrew: Fixed Line Drawing Issue
// - 2008-01-30: Andrew: Fixed DrawRectangle
// - 2008-01-25: Andrew: Fixed compiler hints for pointer use
// - 2008-01-24: Andrew: Added Clipping
// - 2008-01-24: James: Version 1.1 overloads
// - 2008-01-21: Aki: 40 overloads added for Point2D and 
// - 2008-01-17: Aki + Andrew: Refactor
//   Rectangle support
//  
// Version 1.0:
// - Various

/// @module sgGraphics
/// @static
unit SGSDK_Graphics;

interface
  uses SDL, SGSDK_Core, SDL_Image, SGSDK_Shapes;
  
  type
    /// @class BitmapArray
    /// @array_wrapper
    /// @field data: BitmapPtr
    BitmapArray = array of Bitmap;
    
    /// @class BitmapPtr
    /// @pointer_wrapper
    /// @field pointer: ^Bitmap
    BitmapPtr = ^Bitmap;
    
    /// @class LongIntArray
    /// @array_wrapper
    /// @field data: LongIntPtr
    LongIntArray = array of LongInt;
    
    /// @class LongIntPtr
    /// @pointer_wrapper
    /// @field pointer: ^LongInt
    LongIntPtr = ^LongInt;
  
  /// @lib
  function CreateBitmap(width, height: LongInt): Bitmap;
  
  /// @lib LoadBitmapWithTransparentColor
  function LoadBitmap(pathToBitmap: String; transparent: Boolean; transparentColor: Color): Bitmap; overload;
  
  /// @lib
  /// @class Bitmap
  /// @constructor  
  function LoadBitmap(pathToBitmap : String): Bitmap; overload;
  
  /// @lib LoadBitmapWithTransparentColor(pathToBitmap, True, transparentColor)
  /// @class Bitmap
  /// @constructor
  function LoadTransparentBitmap(pathToBitmap : String; transparentColor : Color): Bitmap; overload;
  
  /// @lib
  /// @class Bitmap
  /// @method OptimiseBitmap
  procedure OptimiseBitmap(surface: Bitmap);
  
  /// @lib
  /// @class Bitmap
  /// @dispose
  procedure FreeBitmap(var bitmapToFree : Bitmap);
  
  //*****
  //
  // Bitmap drawing routines
  //
  //*****
  //
  // These routines are used to draw to a bitmap.
  
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
  procedure DrawPixel(dest: Bitmap; theColor: Color; x, y: LongInt); overload;
  /// @lib DrawPixelAtPointOnto
  procedure DrawPixel(dest: Bitmap; theColor: Color; const position: Point2D); overload;
  
  /// @lib DrawOrFillRectangleOnto
  procedure DrawRectangle(dest: Bitmap; theColor : Color; filled : Boolean; xPos, yPos, width, height : LongInt); overload;
  /// @lib DrawOrFillRectangleRectOnto
  procedure DrawRectangle(dest: Bitmap; theColor : Color; filled : Boolean; const source: Rectangle); overload;
  /// @lib DrawOrFillRectangleOnto(dest, theColor, False, xPos, yPos, width, height)
  /// @uname DrawRectangleOnto
  procedure DrawRectangle(dest: Bitmap; theColor : Color; xPos, yPos, width, height: LongInt); overload;
  /// @lib DrawOrFillRectangleRectOnto(dest, theColor, False, source)
  /// @uname DrawRectangleRectOnto
  procedure DrawRectangle(dest: Bitmap; theColor : Color; const source: Rectangle); overload;
  /// @lib DrawOrFillRectangleOnto(dest, theColor, True, xPos, yPos, width, height)
  /// @uname FillRectangleOnto
  procedure FillRectangle(dest: Bitmap; theColor : Color; xPos, yPos, width, height : LongInt); overload;
  /// @lib DrawOrFillRectangleRectOnto(dest, theColor, True, source)
  /// @uname FillRectangleRectOnto
  procedure FillRectangle(dest: Bitmap; theColor : Color; const source: Rectangle); overload;
  
  /// @lib DrawLineOnto
  procedure DrawLine(dest: Bitmap; theColor: Color; xPosStart, yPosStart, xPosEnd, yPosEnd: LongInt); overload;
  /// @lib DrawLineSegmentOnto
  procedure DrawLine(dest: Bitmap; theColor: Color; const line: LineSegment); overload;
  
  /// @lib DrawHorizontalLineOnto
  procedure DrawHorizontalLine(dest: Bitmap; theColor: Color; y, x1, x2: LongInt); overload;
  /// @lib DrawVerticalLineOnto
  procedure DrawVerticalLine(dest: Bitmap; theColor: Color; x, y1, y2: LongInt); overload;
  
  /// @lib DrawOrFillCircleOnto
  procedure DrawCircle(dest: Bitmap; theColor: Color; filled: Boolean; xc, yc, radius: LongInt); overload;
  /// @lib DrawOrFillCircleAtPointOnto
  procedure DrawCircle(dest: Bitmap; theColor: Color; filled: Boolean; const point: Point2D; radius: LongInt); overload;
  /// @lib DrawOrFillCircleOnto(dest, theColor, False, xc, yc, radius)
  /// @uname DrawCircleOnto
  procedure DrawCircle(dest: Bitmap; theColor: Color; xc, yc, radius: LongInt); overload;
  /// @lib DrawOrFillCircleAtPointOnto(dest, theColor, False, point, radius)
  /// @uname DrawCircleAtPointOnto
  procedure DrawCircle(dest: Bitmap; theColor: Color; const point: Point2D; radius: LongInt); overload;
  /// @lib DrawOrFillCircleOnto(dest, theColor, True, xc, yc, radius)
  /// @uname FillCircleOnto
  procedure FillCircle(dest: Bitmap; theColor: Color; xc, yc, radius: LongInt); overload;
  /// @lib DrawOrFillCircleAtPointOnto(dest, theColor, True, point, radius)
  /// @uname FillCircleAtPointOnto
  procedure FillCircle(dest: Bitmap; theColor: Color; const point: Point2D; radius: LongInt); overload;
  
  /// @lib DrawOrFillEllipseOnto
  procedure DrawEllipse(dest: Bitmap; theColor: Color; filled: Boolean; xPos, yPos, width, height: LongInt); overload;
  /// @lib DrawOrFillEllipseInRectOnto
  procedure DrawEllipse(dest: Bitmap; theColor: Color; filled: Boolean; const source: Rectangle); overload;
  /// @lib DrawOrFillEllipseOnto(dest, theColor, False, xPos, yPos, width, height)
  /// @uname DrawEllipseOnto
  procedure DrawEllipse(dest: Bitmap; theColor: Color; xPos, yPos, width, height: LongInt); overload;
  /// @lib DrawOrFillEllipseInRectOnto(dest, theColor, False, source)
  /// @uname DrawEllipseInRectOnto
  procedure DrawEllipse(dest: Bitmap; theColor: Color; const source: Rectangle); overload;
  /// @lib DrawOrFillEllipseOnto(dest, theColor, True, xPos, yPos, width, height)
  /// @uname FillEllipseOnto
  procedure FillEllipse(dest: Bitmap; theColor: Color; xPos, yPos, width, height: LongInt); overload;
  /// @lib DrawOrFillEllipseInRectOnto(dest, theColor, True, source)
  /// @uname FillEllipseInRectOnto
  procedure FillEllipse(dest: Bitmap; theColor: Color; const source: Rectangle); overload;
  
  /// @lib DrawOrFillTriangleOnto
  procedure DrawTriangle(dest: Bitmap; theColor: Color; filled: Boolean; const tri: Triangle); overload;
  /// @lib DrawOrFillTriangleOnto(dest, theColor, False, tri)
  /// @uname DrawTriangleOnto
  procedure DrawTriangle(dest: Bitmap; theColor: Color; const tri: Triangle); overload;
  /// @lib DrawOrFillTriangleOnto(dest, theColor, True, tri)
  /// @uname FillTriangleOnto
  procedure FillTriangle(dest: Bitmap; theColor: Color; const tri: Triangle); overload;
  /// @lib DrawTriangleFromPointsOnto
  procedure DrawTriangle(dest: Bitmap; theColor: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  /// @lib FillTriangleFromPointsOnto
  procedure FillTriangle(dest: Bitmap; theColor: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  
  //*****
  //
  // Screen drawing routines
  //
  //*****
  //
  // These routines are used to to the View Area on the screen.
  //
  
  /// @lib ClearScreenToBlack
  /// @uname ClearScreen
  procedure ClearScreen(); overload;
  /// @lib
  /// @uname ClearScreenWithColor
  procedure ClearScreen(toColor : Color); overload;
  
  /// @lib
  procedure DrawBitmap(src : Bitmap; x, y : Single); overload;
  /// @lib DrawBitmapAtPoint
  procedure DrawBitmap(src : Bitmap; const position : Point2D); overload;
  
  /// @lib
  procedure DrawBitmapPart(src : Bitmap; srcX, srcY, srcW, srcH: LongInt; x, y : Single); overload;
  /// @lib DrawBitmapPartFromRect
  procedure DrawBitmapPart(src : Bitmap; const source : Rectangle; x, y : Single); overload;
  /// @lib DrawBitmapPartFromRectAtPoint
  procedure DrawBitmapPart(src : Bitmap; const source : Rectangle; const position : Point2D); overload;
  
  /// @lib
  procedure DrawPixel(theColor: Color; x, y: Single); overload;
  /// @lib DrawPixelAtPoint
  procedure DrawPixel(theColor: Color; const position: Point2D); overload;
  
  /// @lib DrawOrFillRectangle
  procedure DrawRectangle(theColor: Color; filled: Boolean; xPos, yPos: Single; width, height: LongInt); overload;
  /// @lib DrawOrFillRectangleRect
  procedure DrawRectangle(theColor: Color; filled: Boolean; const source: Rectangle); overload;
  /// @lib DrawOrFillRectangle(theColor, False, xPos, yPos, width, height)
  /// @uname DrawRectangle
  procedure DrawRectangle(theColor: Color; xPos, yPos: Single; width, height: LongInt); overload;
  /// @lib DrawOrFillRectangleRect(theColor, False, source)
  /// @uname DrawRectangleRect
  procedure DrawRectangle(theColor: Color; const source: Rectangle); overload;
  /// @lib DrawOrFillRectangle(theColor, True, xPos, yPos, width, height)
  /// @uname FillRectangle
  procedure FillRectangle(theColor: Color; xPos, yPos: Single; width, height: LongInt); overload;
  /// @lib DrawOrFillRectangleRect(theColor, True, source)
  /// @uname FillRectangleRect
  procedure FillRectangle(theColor: Color; const source: Rectangle); overload;
  
  /// @lib
  procedure DrawLine(theColor: Color; xPosStart, yPosStart, xPosEnd, yPosEnd: Single); overload;
  /// @lib DrawLineSegment
  procedure DrawLine(theColor: Color; const line: LineSegment); overload;
  
  /// @lib DrawOrFillTriangle
  procedure DrawTriangle(theColor: Color; filled: Boolean; const tri: Triangle); overload;
  /// @lib DrawOrFillTriangle(theColor, False, tri)
  /// @uname DrawTriangle
  procedure DrawTriangle(theColor: Color; const tri: Triangle); overload;
  /// @lib DrawOrFillTriangle(theColor, True, tri)
  /// @uname FillTriangle
  procedure FillTriangle(theColor: Color; const tri: Triangle); overload;
  /// @lib DrawTriangleFromPoints
  procedure DrawTriangle(theColor: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  /// @lib FillTriangleFromPoints
  procedure FillTriangle(theColor: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  
  /// @lib
  procedure DrawHorizontalLine(theColor: Color; y, x1, x2: Single); overload;
  /// @lib
  procedure DrawVerticalLine(theColor: Color; x, y1, y2: Single); overload;
  
  /// @lib DrawOrFillCircle
  procedure DrawCircle(theColor: Color; filled: Boolean; xc, yc: Single; radius: LongInt); overload;
  /// @lib DrawOrFillCircleAtPoint
  procedure DrawCircle(theColor: Color; filled: Boolean; const position: Point2D; radius: LongInt); overload;
  /// @lib DrawOrFillCircle(theColor, False, xc, yc, radius)
  /// @uname DrawCircle
  procedure DrawCircle(theColor: Color; xc, yc: Single; radius: LongInt); overload;
  /// @lib DrawOrFillCircleAtPoint(theColor, False, position, radius)
  /// @uname DrawCircleAtPoint
  procedure DrawCircle(theColor: Color; const position: Point2D; radius: LongInt); overload;
  /// @lib DrawOrFillCircle(theColor, True, xc, yc, radius)
  /// @uname FillCircle
  procedure FillCircle(theColor: Color; xc, yc: Single; radius: LongInt); overload;
  /// @lib DrawOrFillCircleAtPoint(theColor, True, position, radius)
  /// @uname FillCircleAtPoint
  procedure FillCircle(theColor: Color; const position: Point2D; radius: LongInt); overload;
  
  /// @lib DrawOrFillEllipse
  procedure DrawEllipse(theColor: Color; filled: Boolean; xPos, yPos: Single; width, height: LongInt); overload;
  /// @lib DrawOrFillEllipseInRect
  procedure DrawEllipse(theColor: Color; filled: Boolean; const source: Rectangle); overload;
  /// @lib DrawOrFillEllipse(theColor, False, xPos, yPos, width, height)
  /// @uname DrawEllipse
  procedure DrawEllipse(theColor: Color; xPos, yPos: Single; width, height: LongInt); overload;
  /// @lib DrawOrFillEllipseInRect(theColor, False, source)
  /// @uname DrawEllipseInRect
  procedure DrawEllipse(theColor: Color; const source: Rectangle); overload;
  /// @lib DrawOrFillEllipse(theColor, True, xPos, yPos, width, height)
  /// @uname FillEllipse
  procedure FillEllipse(theColor: Color; xPos, yPos: Single; width, height: LongInt); overload;
  /// @lib DrawOrFillEllipseInRect(theColor, True, source)
  /// @uname FillEllipseInRect
  procedure FillEllipse(theColor: Color; const source: Rectangle); overload;

  //*****
  //
  // Sprite routines
  //
  //*****
  //
  // These routines are used to work with Sprites within your game.
  //
  
  /// @lib CreateAnimatedCellSpriteWithEndingAction
  /// @class Sprite
  /// @constructor
  function CreateSprite(image : Bitmap; isMulti : Boolean; const framesPerCell : LongIntArray; endingAction : SpriteEndingAction; width, height : LongInt): Sprite; overload;
  /// @lib CreateAnimatedCellSprite
  /// @class Sprite
  /// @constructor
  function CreateSprite(image : Bitmap; isMulti : Boolean; const framesPerCell : LongIntArray; width, height : LongInt): Sprite; overload;
  /// @lib CreateAnimatedCellSpriteWithSetFramesPerCell
  /// @class Sprite
  /// @constructor
  function CreateSprite(image : Bitmap; framesPerCell, frames, width, height: LongInt): Sprite; overload;
  /// @lib CreateBasicSprite
  /// @class Sprite
  /// @constructor
  function CreateSprite(image : Bitmap): Sprite; overload;
  /// @lib CreateAnimatedArraySpriteWithEndingAction
  /// @class Sprite
  /// @constructor
  function CreateSprite(const bitmaps : BitmapArray; const framesPerCell : LongIntArray; endingAction : SpriteEndingAction): Sprite; overload;
  /// @lib CreateAnimatedArraySprite
  /// @class Sprite
  /// @constructor
  function CreateSprite(const bitmaps : BitmapArray; const framesPerCell : LongIntArray): Sprite; overload;
  /// @lib CreateAnimatedArraySpriteWithFramesPerCell
  /// @class Sprite
  /// @constructor
  function CreateSprite(const bitmaps : BitmapArray; framesPerCell, frames : LongInt): Sprite; overload;
  
  /// @lib
  /// @class Sprite
  /// @dispose
  procedure FreeSprite(var spriteToFree : Sprite);
  
  /// @lib
  /// @class Sprite
  /// @method AddBitmap
  function AddBitmapToSprite(spriteToAddTo: Sprite; bitmapToAdd: Bitmap): LongInt;
  
  /// @lib
  /// @class Sprite
  /// @getter Height
  function CurrentHeight(sprite: Sprite): LongInt;
  /// @lib
  /// @class Sprite
  /// @getter Width
  function CurrentWidth(sprite: Sprite): LongInt;
  
  /// @lib
  /// @class Sprite
  /// @method ReplayAnimation
  procedure ReplayAnimation(theSprite : Sprite);
  
  /// @lib UpdateSpritePct(spriteToUpdate, 1.0)
  /// @uname UpdateSprite
  /// @class Sprite
  /// @method Update
  procedure UpdateSprite(spriteToUpdate: Sprite); overload;
  /// @lib UpdateSpritePct
  /// @class Sprite
  /// @overload Update UpdatePct
  procedure UpdateSprite(spriteToUpdate: Sprite; pct: Single); overload;
  
  /// @lib UpdateSpriteAnimationPct(spriteToUpdate, 1.0)
  /// @uname UpdateSpriteAnimation
  /// @class Sprite
  /// @method UpdateAnimation
  procedure UpdateSpriteAnimation(spriteToUpdate: Sprite); overload;
  /// @lib UpdateSpriteAnimationPct
  /// @class Sprite
  /// @overload UpdateAnimation UpdateAnimationPct
  procedure UpdateSpriteAnimation(spriteToUpdate: Sprite; pct: Single); overload;
  
  /// @lib DrawSpriteOffsetXY
  /// @class Sprite
  /// @overload Draw DrawOffsetXY
  procedure DrawSprite(spriteToDraw : Sprite; xOffset, yOffset: LongInt); overload;
  /// @lib DrawSpriteOffsetPoint
  /// @class Sprite
  /// @overload Draw DrawOffsetPoint
  procedure DrawSprite(spriteToDraw : Sprite; const position: Point2D); overload;
  /// @lib DrawSpriteOffsetXY(spriteToDraw, 0, 0)
  /// @uname DrawSprite
  /// @class Sprite
  /// @method Draw
  procedure DrawSprite(spriteToDraw : Sprite); overload;
    
  /// @lib MoveSprite(spriteToMove, 1.0)
  /// @uname MoveSprite
  /// @class Sprite
  /// @method Move
  procedure MoveSprite(spriteToMove: Sprite); overload;
  /// @lib MoveSprite
  /// @uname MoveSpritePct
  /// @class Sprite
  /// @overload Move MovePct
  procedure MoveSprite(spriteToMove: Sprite; pct: Single); overload;
  /// @lib MoveSpriteVecPct(spriteToMove, movementVector, 1.0)
  /// @uname MoveSpriteVec
  /// @class Sprite
  /// @overload Move MoveVec
  procedure MoveSprite(spriteToMove : Sprite; const movementVector: Vector); overload;
  /// @lib MoveSpriteVecPct
  /// @class Sprite
  ///@overload Move MoveVecPct
  procedure MoveSprite(spriteToMove : Sprite; const movementVector: Vector; pct: Single); overload;
  
  /// @lib
  /// @class Sprite
  /// @method MoveTo
  procedure MoveSpriteTo(spriteToMove : Sprite; x,y : LongInt);
  
  /// @lib
  /// @class Sprite
  /// @method IsOffscreen
  function IsSpriteOffscreen(theSprite : Sprite): Boolean;

  //*****
  //
  // Draws elements directly onto the screen, ignoring the visible window
  // setting.
  //
  //*****
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
  procedure DrawPixelOnScreen(theColor: Color; x, y: LongInt); overload;
  /// @lib DrawPixelAtPointOnScreen
  procedure DrawPixelOnScreen(theColor: Color; const position: Point2D); overload;
  
  /// @lib DrawOrFillRectangleOnScreen
  procedure DrawRectangleOnScreen(theColor : Color; filled : Boolean; xPos, yPos, width, height : LongInt); overload;
  /// @lib DrawOrFillRectangleOnScreen(theColor, False, xPos, yPos, width, height)
  /// @uname DrawRectangleOnScreen
  procedure DrawRectangleOnScreen(theColor : Color; xPos, yPos, width, height : LongInt); overload;
  /// @lib DrawOrFillRectangleOnScreen(theColor, True, xPos, yPos, width, height)
  /// @uname FillRectangleOnScreen
  procedure FillRectangleOnScreen(theColor : Color; xPos, yPos, width, height : LongInt); overload;
  /// @lib DrawOrFillRectangleRectOnScreen
  procedure DrawRectangleOnScreen(theColor : Color; filled : Boolean; const source : Rectangle); overload;
  /// @lib DrawOrFillRectangleRectOnScreen(theColor, False, source)
  /// @uname DrawRectangleRectOnScreen
  procedure DrawRectangleOnScreen(theColor : Color; const source : Rectangle); overload;
  /// @lib DrawOrFillRectangleRectOnScreen(theColor, True, source)
  /// @uname FillRectangleRectOnScreen
  procedure FillRectangleOnScreen(theColor : Color; const source : Rectangle); overload;
  
  /// @lib
  procedure DrawLineOnScreen(theColor: Color; xPosStart, yPosStart, xPosEnd, yPosEnd: LongInt); overload;
  /// @lib DrawLineSegmentOnScreen
  procedure DrawLineOnScreen(theColor: Color; const line: LineSegment); overload;
  
  /// @lib
  procedure DrawHorizontalLineOnScreen(theColor: Color; y, x1, x2: LongInt);
  /// @lib
  procedure DrawVerticalLineOnScreen(theColor: Color; x, y1, y2: LongInt);
  
  /// @lib DrawOrFillCircleOnScreen
  procedure DrawCircleOnScreen(theColor: Color; filled: Boolean; xc, yc, radius: LongInt); overload;
  /// @lib DrawOrFillCircleOnScreen(theColor, False, xc, yc, radius)
  /// @uname DrawCircleOnScreen
  procedure DrawCircleOnScreen(theColor: Color; xc, yc, radius: LongInt); overload;
  /// @lib DrawOrFillCircleOnScreen(theColor, True, xc, yc, radius)
  /// @uname FillCircleOnScreen
  procedure FillCircleOnScreen(theColor: Color; xc, yc, radius: LongInt); overload;
  /// @lib DrawOrFillCircleAtPointOnScreen
  procedure DrawCircleOnScreen(theColor: Color; filled: Boolean; const position: Point2D; radius: LongInt); overload;
  /// @lib DrawOrFillCircleAtPointOnScreen(theColor, False, position, radius)
  /// @uname DrawCircleAtPointOnScreen
  procedure DrawCircleOnScreen(theColor: Color; const position: Point2D; radius: LongInt); overload;
  /// @lib DrawOrFillCircleAtPointOnScreen(theColor, True, position, radius)
  /// @uname FillCircleAtPointOnScreen
  procedure FillCircleOnScreen(theColor: Color; const position: Point2D; radius: LongInt); overload;
  
  /// @lib DrawOrFillEllipseOnScreen
  procedure DrawEllipseOnScreen(theColor: Color; filled: Boolean; xPos, yPos, width, height: LongInt); overload;
  /// @lib DrawOrFillEllipseOnScreen(theColor, False, xPos, yPos, width, height)
  /// @uname DrawEllipseOnScreen
  procedure DrawEllipseOnScreen(theColor: Color; xPos, yPos, width, height: LongInt); overload;
  /// @lib DrawOrFillEllipseOnScreen(theColor, True, xPos, yPos, width, height)
  /// @uname FillEllipseOnScreen
  procedure FillEllipseOnScreen(theColor: Color; xPos, yPos, width, height: LongInt); overload;
  /// @lib DrawOrFillEllipseInRectOnScreen
  procedure DrawEllipseOnScreen(theColor: Color; filled: Boolean; const source: Rectangle); overload;
  /// @lib DrawOrFillEllipseInRectOnScreen(theColor, False, source)
  /// @uname DrawEllipseInRectOnScreen
  procedure DrawEllipseOnScreen(theColor: Color; const source: Rectangle); overload;
  /// @lib DrawOrFillEllipseInRectOnScreen(theColor, True, source)
  /// @uname FillEllipseInRectOnScreen
  procedure FillEllipseOnScreen(theColor: Color; const source: Rectangle); overload;
  
  /// @lib DrawOrFillTriangleOnScreen
  procedure DrawTriangleOnScreen(theColor: Color; filled: Boolean; const tri: Triangle); overload;
  /// @lib DrawOrFillTriangleOnScreen(theColor, False, tri)
  /// @uname DrawTriangleOnScreen
  procedure DrawTriangleOnScreen(theColor: Color; const tri: Triangle); overload;
  /// @lib DrawOrFillTriangleOnScreen(theColor, True, tri)
  /// @uname FillTriangleOnScreen  
  procedure FillTriangleOnScreen(theColor: Color; const tri: Triangle); overload;
  /// @lib DrawTriangleFromPointsOnScreen
  procedure DrawTriangleOnScreen(theColor: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  /// @lib FillTriangleFromPointsOnScreen
  procedure FillTriangleOnScreen(theColor: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  
  //
  // Clipping
  //
  
  
  /// @lib
  procedure SetClip(x, y, w, h: LongInt); overload;
  /// @lib SetClipRect
  procedure SetClip(r: Rectangle); overload;
  
  /// @lib SetClipForBitmap
  /// @class Bitmap
  /// @method SetClip
  procedure SetClip(bmp: Bitmap; x, y, w, h: LongInt); overload;
    
  /// @lib SetClipRectForBitmap
  /// @class Bitmap
  /// @overload SetClip SetClipRect
  procedure SetClip(bmp: Bitmap; r: Rectangle); overload;
  
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

  //
  // Alpha blendings adjusting code
  //
  
  /// @lib
  /// @class Bitmap
  /// @method MakeOpaque
  procedure MakeOpaque(bmp: Bitmap);
  /// @lib
  /// @class Bitmap
  /// @method MakeTransparent
  procedure MakeTransparent(bmp: Bitmap);
    
  //
  // Rotate and Zoom
  //
  
  /// @lib
  /// @class Bitmap
  /// @method RotateScaleBitmap
  function RotateScaleBitmap(src: Bitmap; degRot, scale: Single): Bitmap;
  
  /// @lib
  /// @class Bitmap
  /// @method SetupForCollisions
  procedure SetupBitmapForCollisions(src: Bitmap);

implementation
  uses Classes, SysUtils, SGSDK_Camera, SGSDK_Physics, SDL_gfx, sg_Shared;
  
  /// Clears the surface of the bitmap to the passed in color.
  ///
  /// @param dest:     The bitmap to clear
  /// @param toColor: The colour to clear the bitmap to
  ///
  /// Side Effects:
  /// - dest's surface is set to the toColor
  procedure ClearSurface(dest: Bitmap; toColor: Color); overload;
  begin
    if dest = nil then raise Exception.Create('Cannot clear, destination bitmap not supplied (nil)');
      
    SDL_FillRect(dest.surface, @dest.surface.clip_rect, toColor);
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
    ClearSurface(scr, toColor);
  end;

  /// Clears the screen to Black.
  ///
  /// Side Effects:
  /// - screen's surface is set to black
  procedure ClearScreen(); overload;
  begin
    ClearScreen(ColorBlack);
  end;

  function GetPixel32(surface: PSDL_Surface; x, y: LongInt): Color;
  var
    pixel, pixels: PUint32;
    offset: Uint32;
  {$IFDEF FPC}
    pixelAddress: PUint32;
  {$ELSE}
    pixelAddress: UInt32;
  {$ENDIF}
  begin
    //Convert the pixels to 32 bit
    pixels := surface.pixels;

    //Get the requested pixel
    offset := (( y * surface.w ) + x) * surface.format.BytesPerPixel;

    {$IFDEF FPC}
      pixelAddress := pixels + (offset div 4);
      pixel := PUint32(pixelAddress);
    {$ELSE}
      pixelAddress := UInt32(pixels) + offset;
      pixel := Ptr(pixelAddress);
    {$ENDIF}

    {$IF SDL_BYTEORDER = SDL_BIG_ENDIAN }
    case surface.format.BytesPerPixel of
      1: result := pixel^ and $000000ff;
      2: result := pixel^ and $0000ffff;
      3: result := pixel^ and $00ffffff;
      4: result := pixel^;
    else
      raise Exception.Create('Unsuported bit format...');
    end;
    {$ELSE}
    case surface.format.BytesPerPixel of
      1: result := pixel^ and $ff000000;
      2: result := pixel^ and $ffff0000;
      3: result := pixel^ and $ffffff00;
      4: result := pixel^;
    else
      raise Exception.Create('Unsuported bit format...')
    end;
    {$IFEND}
  end;
  
  function GetPixel(bmp: Bitmap; x, y: LongInt): Color;
  begin
    if not Assigned(bmp) then raise Exception.Create('No bitmap supplied');
    
    if (x < 0) or (x >= bmp.width) or (y < 0) or (y >= bmp.height) then
    begin
      result := 0;
      exit;
    end;
    
    result := GetPixel32(bmp.surface, x, y);
  end;
  
  function GetPixelFromScreen(x, y: LongInt): Color;
  begin
    result := GetPixel(scr, x, y);
  end;  

  // Sets the non-transparent pixels in a Bitmap. This is then used for
  // collision detection, allowing the original surface to be optimised.
  //
  // @param toSet  A pointer to the Bitmap being set
  // @param surface The surface with pixel data for this Bitmap
  procedure SetNonTransparentPixels(toSet: Bitmap; surface: PSDL_Surface; transparentColor: Color);
  var
    r, c: LongInt;
  begin
    SetLength(toSet.nonTransparentPixels, toSet.width, toSet.height);

    for c := 0 to toSet.width - 1 do
    begin
      for r := 0 to toSet.height - 1 do
      begin
        toSet.nonTransparentPixels[c, r] :=
          (GetPixel32(surface, c, r) <> transparentColor);
      end;
    end;
  end;

  procedure SetNonAlphaPixels(toSet: Bitmap; surface: PSDL_Surface);
  var
    r, c: LongInt;
    hasAlpha: Boolean;
  begin
    SetLength(toSet.nonTransparentPixels, toSet.width, toSet.height);
    hasAlpha := surface.format.BytesPerPixel = 4;

    for c := 0 to toSet.width - 1 do
    begin
      for r := 0 to toSet.height - 1 do
      begin
        toSet.nonTransparentPixels[c, r] := (not hasAlpha) or ((GetPixel32(surface, c, r) and SDL_Swap32($000000FF)) > 0);
      end;
    end;
  end;

  /// Loads a bitmap from a given path, with the indicated transparent color.
  /// This loads both transparent and non-transparent bitmaps.
  ///
  /// @param pathToBitmap:     the path to the bitmap to be loaded
  /// @param transparent:      Indicates if transparency should be set
  /// @param transparentColor: the color that will be transparent
  /// @returns: A bitmap from the loaded file.
  function LoadBitmap(pathToBitmap: String; transparent: Boolean; transparentColor: Color): Bitmap; overload;
  var
    loadedImage: PSDL_Surface;
    correctedTransColor: Color;
  begin
    if not FileExists(pathToBitmap) then raise Exception.Create('Unable to locate bitmap ' + pathToBitmap);
    
    loadedImage := IMG_Load(pchar(pathToBitmap));
    
    if loadedImage <> nil then
    begin
      new(result);
      if not transparent then result.surface := SDL_DisplayFormatAlpha(loadedImage)
      else result.surface := SDL_DisplayFormat(loadedImage);
      //result.surface := loadedImage;
      
      //WriteLn('Loaded ', pathToBitmap);
      //WriteLn('  at ', HexStr(result.surface));

      result.width := result.surface.w;
      result.height := result.surface.h;

      if transparent then
      begin
        correctedTransColor := GetColor(result, transparentColor);
        SDL_SetColorKey(result.surface, SDL_RLEACCEL or SDL_SRCCOLORKEY, correctedTransColor);
        SetNonTransparentPixels(result, loadedImage, correctedTransColor);
      end
      else
      begin
        SetNonAlphaPixels(result, loadedImage);
      end;

      if loadedImage <> result.surface then SDL_FreeSurface(loadedImage);
    end
    else
    begin
      raise Exception.Create('Error loading image: ' + pathToBitmap + ': ' + SDL_GetError());
    end;
  end;

  /// Loads a bitmap from file into a Bitmap variable. This can then be drawn to
  /// the screen. Bitmaps can be of bmp, jpeg, gif, png, etc. Images may also
  /// contain alpha values, which will be drawn correctly by the API. All
  /// bitmaps must be freed using the FreeBitmap once you are finished with
  /// them.
  ///
  /// @param pathToBitmap:   The path to the bitmap file to open.
  /// @returns: A bitmap from the loaded file
  function LoadBitmap(pathToBitmap : String): Bitmap; overload;
  begin
    result := LoadBitmap(pathToBitmap, false, ColorBlack);
  end;

  /// Loads a bitmap with a transparent color key. The transparent color is then
  /// setup as the color key to ensure the image is drawn correctly. Alpha
  /// values of Images loaded in this way will be ignored. All bitmaps must be
  /// freed using the FreeBitmap once you are finished with them.
  ///
  /// @param pathToBitmap:     the path to the bitmap to be loaded
  /// @param transparentColor: the color that will be transparent
  /// @returns: A bitmap from the loaded file.
  function LoadTransparentBitmap(pathToBitmap : String; transparentColor : Color): Bitmap; overload;
  begin
    result := LoadBitmap(pathToBitmap, true, transparentColor);
  end;

  /// Frees a loaded bitmap. Use this when you will no longer be drawing the
  /// bitmap, and when the program exits.
  ///
  /// Side Effects:
  /// - the bitmap is freeed and can no longer be drawn
  procedure FreeBitmap(var bitmapToFree : Bitmap);
  begin
    if Assigned(bitmapToFree) then
    begin
      if Assigned(bitmapToFree^.surface) then
      begin
        //WriteLn('Free Bitmap - ', HexStr(bitmapToFree^.surface));
        SDL_FreeSurface(bitmapToFree^.surface);
      end;
      bitmapToFree^.surface := nil;

      //SetLength(bitmapToFree^.nonTransparentPixels, 0, 0);

      Dispose(bitmapToFree);
      bitmapToFree := nil;
    end;
  end;
  
  /// Creates a sprites, and sets its first bitmap.
  ///
  /// @param startBitmap:   The sprites first bitmap (index 0)
  /// @param isMulti:     True if the bitmap specified is a multi bitmap
  /// @param framesPerCell: Array of LongInt that defines the frames per cell
  /// @param endingAction:  This sprite's ending action (Loop, ReverseLoop or Stop)
  /// @param width, height: Width and height of this sprite
  /// @returns:       A new sprite with this bitmap as its first bitmap
  function CreateSprite(image : Bitmap; isMulti : Boolean; const framesPerCell : LongIntArray; 
    endingAction : SpriteEndingAction; width, height : LongInt): Sprite; overload;
  var
    i : LongInt;
  begin
    if image = nil then raise Exception.Create('No image specified to create a sprite');
    if isMulti and (Length(framesPerCell) = 0) then raise Exception.Create('No frames per cell defined'); 
    if (width < 1) or (height < 1) then raise Exception.Create('Sprite Width and Height must be greater then 0');
    
    New(result);
    SetLength(result.bitmaps, 1);
    
    if isMulti then
    begin
      result.spriteKind := AnimMultiSprite;
      
      result.cols := image.width div width;
      result.row := image.height div height;
      
      SetLength(result.framesPerCell, Length(framesPerCell));
      for i := 0 to High(framesPerCell) do
      begin
        if framesPerCell[i] < 0 then 
          raise Exception.Create('Frames per cell must be larger than 0');
        
        result.framesPerCell[i] := framesPerCell[i];
      end;
    end
    else
    begin
      result.spriteKind := StaticSprite;
    end;

    result.x              := 0;
    result.y              := 0;
    // result.xPos            := @result.x;
    // result.yPos            := @result.y;
    result.currentFrame   := 0;
    result.usePixelCollision  := true;
    result.hasEnded       := false;
    result.bitmaps[0]     := image;
    result.frameCount     := 0;
    result.endingAction   := endingAction;
    result.width          := width;
    result.height         := height;
    result.reverse        := false;
    result.movement       := CreateVector(0,0);
    result.rotation       := 0;
    result.zoom           := 1;
    result.bufferedRotation := 0;
    result.bufferedZoom   := 1;
    result.bufferBmp      := nil;
  end;
  
  /// Creates a sprites, and sets its first bitmap.
  ///
  /// @param image:     The sprites first bitmap (index 0)
  /// @param isMulti:     True if the bitmap specified is a multi bitmap
  /// @param framesPerCell: Array of LongInt that defines the frames per cell
  /// @param width, height: Width and height of this sprite
  /// @returns:       A new sprite
  function CreateSprite(image : Bitmap; isMulti : Boolean; const framesPerCell : LongIntArray; 
    width, height : LongInt): Sprite; overload;
  begin
    result := CreateSprite(image, isMulti, framesPerCell, Loop, width, height);
  end;
  
  /// Creates a sprites, and sets its first bitmap.
  ///
  /// @param image:   The sprites first bitmap (index 0)
  /// @param framesPerCell: Number of frames per cell
  /// @param frames:      Number of frames this sprite contains
  /// @param width, height: Width and height of this sprite
  /// @returns:       A new sprite
  function CreateSprite(image: Bitmap; framesPerCell, frames, width, height: LongInt): Sprite; overload;
  var
    tempIntegers: LongIntArray;
    i: LongInt;
  begin
    if framesPerCell <= 0 then raise Exception.Create('Frames per cell must be larger than 0');
    
    SetLength(tempIntegers, frames);
    for i := 0 to High(tempIntegers) do
    begin
      tempIntegers[i] := framesPerCell;
    end;
    result := CreateSprite(image, true, tempIntegers, width, height);
  end;

  /// Creates a sprites, and sets its first bitmap.
  ///
  /// @param image:     The sprites first bitmap (index 0)
  /// @returns:       A new sprite with this bitmap as its first bitmap
  function CreateSprite(image : Bitmap): Sprite; overload;
  var
    empty : LongIntArray;
  begin
    SetLength(empty, 0);
    result := CreateSprite(image, false, empty, image.width, image.height);
  end;
  
  /// Creates a sprites ans set bitmaps.
  ///
  /// @param bitmaps:     The array of bitmaps
  /// @param framesPerCell: Array of Integer that defines the frames per cell
  /// @param endingAction:  Ending action of this sprite when it finishes animating
  /// @returns:       A new sprite with this bitmap as its first bitmap
  function CreateSprite(const bitmaps: BitmapArray; const framesPerCell: LongIntArray; endingAction: SpriteEndingAction): Sprite; overload;
  var
    i : LongInt;
  begin
    if Length(bitmaps) = 0 then raise Exception.Create('No images specified to create a sprite');
    if Length(framesPerCell) = 0 then raise Exception.Create('No frames per cell defined');
    
    New(result);
    result.x          := 0;
    result.y          := 0;
    // result.xPos          := @result.x;
    // result.yPos          := @result.y;
    result.currentFrame     := 0;
    result.usePixelCollision  := true;
    result.hasEnded       := false;
    result.movement       := CreateVector(0,0);
    result.rotation       := 0;
    result.zoom           := 1;
    result.bufferedRotation := 0;
    result.bufferedZoom   := 1;
    result.bufferBmp      := nil; 
    result.endingAction     := endingAction;
    result.width        := bitmaps[0].width;
    result.height       := bitmaps[0].height;
    result.reverse        := false;
    result.spriteKind     := AnimArraySprite;
    
    SetLength(result.bitmaps, Length(bitmaps));
    for i := 0 to High(bitmaps) do
    begin
      result.bitmaps[i] := bitmaps[i];
    end;

    SetLength(result.framesPerCell, Length(framesPerCell));
    for i := 0 to High(framesPerCell) do
    begin
      if framesPerCell[i] <= 0 then 
        raise Exception.Create('Frames per cell must be larger than 0');

      result.framesPerCell[i] := framesPerCell[i];
    end;
  end;
  
  /// Creates a sprites ans set bitmaps.
  ///
  /// @param bitmaps:     The array of bitmaps
  /// @param framesPerCell: Array of Integer that defines the frames per cell
  /// @returns:       A new sprite with this bitmap as its first bitmap
  function CreateSprite(const bitmaps : BitmapArray; const framesPerCell : LongIntArray): Sprite; overload;
  begin
    result := CreateSprite(bitmaps, framesPerCell, Loop);
  end;
  
  /// Creates a sprites, and sets its first bitmap.
  ///
  /// @param bitmaps:     The array of bitmaps
  /// @param framesPerCell: Number of frames per cell
  /// @param frames:      Number of frames this sprite contains
  /// @returns:       A new sprite
  function CreateSprite(const bitmaps: BitmapArray; framesPerCell, frames: LongInt): Sprite; overload;
  var
    tempIntegers: LongIntArray;
    i: LongInt;
  begin
    if framesPerCell <= 0 then raise Exception.Create('Frames per cell must be larger than 0');

    SetLength(tempIntegers, frames);
    for i := 0 to High(tempIntegers) do
    begin
      tempIntegers[i] := framesPerCell;
    end;

    result := CreateSprite(bitmaps, tempIntegers);
  end;
  
  procedure UpdateSpriteBuffers(sprt: Sprite);
  var
    dest: Bitmap; //temporary surface
    srcX, srcY: LongInt; //for image parts
  begin
    if (sprt.rotation = sprt.bufferedRotation) and (sprt.zoom = sprt.bufferedZoom) then exit;
    if (sprt.bufferBmp <> nil) then FreeBitmap(sprt.bufferBmp);
    if (sprt.rotation = 0) and (sprt.zoom = 1) then exit; //no need to transform

    //Draw non-transformed bitmap onto temp surface
    dest := CreateBitmap(sprt.width, sprt.height);
    
    if sprt.spriteKind <> AnimMultiSprite then
      DrawBitmap(dest, sprt.bitmaps[sprt.currentFrame], 0, 0)
    else
    begin
      with sprt^ do
      begin
        srcX := (currentFrame mod cols) * width;
        srcY := (currentFrame - (currentFrame mod cols)) div cols * height;
      end;
      
      MakeOpaque(sprt.bitmaps[0]);
      DrawBitmapPart(dest, sprt.bitmaps[0], srcX, srcY, sprt.width, sprt.height, 0, 0);
      MakeTransparent(sprt.bitmaps[0]);
    end;
    
    sprt.bufferBmp := RotateScaleBitmap(dest, sprt.rotation, sprt.zoom);

    FreeBitmap(dest);
  end;
  
  /// Frees a sprite, this does not free the sprite's bitmaps, which allows
  /// bitmaps to be shared between sprites. All created sprites need to be
  /// freed.
  ///
  /// @param spriteToFree:     the sprite to free
  ///
  /// Side Effects:
  /// - The sprites details are cleaned up.
  procedure FreeSprite(var spriteToFree : Sprite);
  begin
    if Assigned(spriteToFree) then
    begin
      //Free bitmaps
      SetLength(spriteToFree.bitmaps, 0);

      //Free buffered rotation image
      if spriteToFree.bufferBmp <> nil then FreeBitmap(spriteToFree.bufferBmp);
      spriteToFree.bufferBmp := nil;

      //Dispose sprite
      Dispose(spriteToFree);
      spriteToFree := nil;
    end;
  end;

  /// Sprites may contain multiple images. These images can be used for things
  /// line animation, facing, etc. This routine adds a bitmap to a sprite,
  /// returning the index of the added bitmap.
  ///
  /// @param spriteToAddTo:   the sprite to add the bitmap to
  /// @param bitmapToAdd:     the bitmap to add to the sprite
  /// @returns :               the index of the added bitmap
  ///
  /// Side Effects:
  /// - The bitmaps is added to the bitmaps within the sprite.
  function AddBitmapToSprite(spriteToAddTo : Sprite; bitmapToAdd : Bitmap): LongInt;
  begin
    if bitmapToAdd = nil then raise Exception.Create('Cannot add non-existing bitmap to Sprite');
    if spriteToAddTo = nil then raise Exception.Create('No sprite to add to');
    if spriteToAddTo.spriteKind = AnimMultiSprite then raise Exception.Create('Cannot add bitmap to an animated multi-sprite');
          
    //Resize the array
    SetLength(spriteToAddTo.bitmaps, Length(spriteToAddTo.bitmaps) + 1);

    //Add the values to the array
    spriteToAddTo.bitmaps[High(spriteToAddTo.bitmaps)] := bitmapToAdd;

    result := High(spriteToAddTo.bitmaps);
  end;

  /// Returns the current width of the sprite.
  ///
  /// @param sprite:     The sprite to get the width of
  /// @returns           The width of the sprite's current frame
  function CurrentWidth(sprite: Sprite): LongInt;
  begin
    if sprite = nil then raise Exception.Create('No sprite supplied');

    result := sprite.width;
  end;
  
  /// Returns the current height of the sprite.
  ///
  /// @param sprite:     The sprite to get the height of
  /// @returns           The height of the sprite's current frame
  function CurrentHeight(sprite: Sprite): LongInt;
  begin
    if sprite = nil then raise Exception.Create('No sprite supplied');

    result := sprite.height;
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
    if (dest = nil) or (src = nil) then raise Exception.Create('No bitmap supplied');
    
    offset := NewSDLRect(x, y, 0, 0);
    SDL_BlitSurface(src.surface, nil, dest.surface, @offset);
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
    if (dest = nil) or (src = nil) then raise Exception.Create('No bitmap supplied');
    if (srcW < 0) or (srcH < 0) then raise Exception.Create('Width and Height must be >= 0');
    
    offset := NewSDLRect(x, y, 0, 0);
    source := NewSDLRect(srcX, srcY, srcW, srcH);

    SDL_BlitSurface(src.surface, @source, dest.surface, @offset);
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
    DrawBitmapPart(scr, src, srcX, srcY, srcW, srcH, x, y);
  end;

  procedure DrawBitmapPart(src : Bitmap; srcX, srcY, srcW, srcH: LongInt; x, y : Single); overload;
  begin
    DrawBitmapPart(scr, src, srcX, srcY, srcW, srcH, SGSDK_Camera.ToScreenX(x), SGSDK_Camera.ToScreenY(y));
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
    DrawBitmap(scr, src, x, y);
  end;

  procedure DrawBitmap(src : Bitmap; x, y : Single); overload;
  begin
    DrawBitmap(scr, src, SGSDK_Camera.ToScreenX(x), SGSDK_Camera.ToScreenY(y));
  end;
  
  procedure ReplayAnimation(theSprite : Sprite);
  begin
    if theSprite = nil then raise Exception.Create('No sprite supplied');

    theSprite.currentFrame := 0;
    theSprite.hasEnded := false;
    theSprite.reverse := false;
  end;
  
  procedure CycleFrame(spriteToUpdate: Sprite);
    procedure EndAnimation(frame: LongInt);
    begin
      spriteToUpdate.currentFrame := frame;
      spriteToUpdate.hasEnded := true;
    end;
    procedure SetAnimation(frame: LongInt; reverse: Boolean);
    begin
      spriteToUpdate.currentFrame := frame;
      spriteToUpdate.reverse := reverse;
    end;
  begin
    if (spriteToUpdate.currentFrame > High(spriteToUpdate.framesPerCell)) then
    begin
      if (spriteToUpdate.endingAction = ReverseLoop) or (spriteToUpdate.endingAction = ReverseOnce) then
        SetAnimation(spriteToUpdate.currentFrame - 1, true)
      else if spriteToUpdate.endingAction = Loop then spriteToUpdate.currentFrame := 0
      else if spriteToUpdate.endingAction = Stop then EndAnimation(High(spriteToUpdate.framesPerCell));
    end
    else if (spriteToUpdate.currentFrame < Low(spriteToUpdate.framesPerCell)) then
    begin
      if spriteToUpdate.endingAction = ReverseOnce then EndAnimation(0)
      else SetAnimation(1, false);
    end;    
  end;
  
  procedure MoveToNextFrame(spriteToUpdate: Sprite);
  var
    i, sum: LongInt;
    frameChange: LongInt;
  begin
    //Check that they are all + or 0, at at least one is +
    sum := 0;
    for i := Low(spriteToUpdate.framesPerCell) to High(spriteToUpdate.framesPerCell) do
    begin
      if spriteToUpdate.framesPerCell[i] < 0 then raise Exception.Create('Frames per cell must be 0 or positive');
      sum := sum + spriteToUpdate.framesPerCell[i];
    end;

    if sum = 0 then raise Exception.Create('Frames per cell cannot all be zero');
    
    //Reset the frame count.
    spriteToUpdate.frameCount := 0;
    
    if spriteToUpdate.reverse then frameChange := -1
    else frameChange := +1;
      
    spriteToUpdate.currentFrame := spriteToUpdate.currentFrame + frameChange;
    
    if (spriteToUpdate.currentFrame > High(spriteToUpdate.framesPerCell)) or
       (spriteToUpdate.currentFrame < Low(spriteToUpdate.framesPerCell)) then
    begin
      CycleFrame(spriteToUpdate);
    end;
  end;

  procedure UpdateSpriteAnimation(spriteToUpdate: Sprite); overload;
  begin
    UpdateSpriteAnimation(spriteToUpdate, 1.0);
  end;
  
  /// Update the frame position
  ///
  /// @param spriteToUpdate:  The sprite to be processed
  /// @param pct: Percentage to update
  ///
  /// Side Effects:
  /// - Process the frame position depending on the sprite's setting
  procedure UpdateSpriteAnimation(spriteToUpdate: Sprite; pct: Single); overload;
  begin
    if spriteToUpdate = nil then raise Exception.Create('No sprite supplied');
    if spriteToUpdate.hasEnded then exit;
    if spriteToUpdate.spriteKind = StaticSprite then exit;
        
    spriteToUpdate.frameCount := spriteToUpdate.frameCount + pct;
    
    // If we are at the end of the current frame... need to move to the next frame
    if spriteToUpdate.frameCount >= spriteToUpdate.framesPerCell[spriteToUpdate.currentFrame] then
    begin
      MoveToNextFrame(spriteToUpdate);
      
      while spriteToUpdate.framesPerCell[spriteToUpdate.currentFrame] = 0 do
      begin
        MoveToNextFrame(spriteToUpdate);
      end;

      if spriteToUpdate.spriteKind = AnimArraySprite then
      begin
        spriteToUpdate.width := spriteToUpdate.bitmaps[spriteToUpdate.currentFrame].width;
        spriteToUpdate.height := spriteToUpdate.bitmaps[spriteToUpdate.currentFrame].height;
      end;
    end;
  end;

  procedure UpdateSprite(spriteToUpdate: Sprite); overload;
  begin
    UpdateSprite(spriteToUpdate, 1.0);
  end;
  
  procedure UpdateSprite(spriteToUpdate: Sprite; pct: Single); overload;
  begin
    MoveSprite(spriteToUpdate, pct);
    UpdateSpriteAnimation(spriteToUpdate, pct);
  end;
  
  /// Draws a sprite to the screen, without using a view port.
  ///
  /// @param spriteToDraw:     The sprite to be drawn
  ///
  /// Side Effects:
  /// - The sprite is drawn to the screen, if within screen area
  procedure DrawSprite(spriteToDraw: Sprite); overload;
  begin
    DrawSprite(spriteToDraw, 0, 0);
  end;
  
  procedure DrawSprite(spriteToDraw : Sprite; const position: Point2D); overload;
  begin
    DrawSprite(spriteToDraw, Round(position.x), Round(position.y));
  end;
  
  procedure DrawSprite(spriteToDraw: Sprite; xOffset, yOffset: LongInt); overload;
  var
    srcX, srcY: LongInt;
  begin
    if not Assigned(spriteToDraw) then raise Exception.Create('No sprite supplied');

    if (spriteToDraw.rotation <> 0) or (spriteToDraw.zoom <> 1) then
    begin
      UpdateSpriteBuffers(spriteToDraw);
      DrawBitmap(spriteToDraw.bufferBmp, spriteToDraw.x + xOffset, spriteToDraw.y + yOffset);
    end
    else
    begin
      if spriteToDraw.spriteKind <> AnimMultiSprite then
      begin
        DrawBitmap(spriteToDraw.bitmaps[spriteToDraw.currentFrame], spriteToDraw.x + xOffset, spriteToDraw.y + yOffset);
      end
      else
      begin
        with spriteToDraw^ do
        begin
          srcX := (currentFrame mod cols) * width;
          srcY := (currentFrame - (currentFrame mod cols)) div cols * height;
        end;
      
        DrawBitmapPart(spriteToDraw.bitmaps[0], srcX, srcY, 
                     spriteToDraw.width, spriteToDraw.height,
                     spriteToDraw.x + xOffset, spriteToDraw.y + yOffset);
      end;
    end;
  end;

    /// Determines if a sprite is off the screen.
  ///
  /// @param theSprite:     The sprite to check the position of
  /// @returns          True if the sprite is off the screen
  function IsSpriteOffscreen(theSprite : Sprite): Boolean;
  begin
    if theSprite = nil then raise Exception.Create('No sprite supplied');
    
    if SGSDK_Camera.ToScreenX(theSprite.x) >= ScreenWidth() then result := true
    else if SGSDK_Camera.ToScreenX(theSprite.x) + CurrentWidth(theSprite) < 0 then result := true
    else if SGSDK_Camera.ToScreenY(theSprite.y) >= ScreenHeight() then result := true
    else if SGSDK_Camera.ToScreenY(theSprite.y) + CurrentHeight(theSprite) < 0 then result := true
    else result := false;
  end;

  procedure MoveSprite(spriteToMove : Sprite; const movementVector : Vector); overload;
  begin
    MoveSprite(spriteToMove, movementVector, 1.0);
  end;

  /// Moves a sprite based on information in a movement vector.
  ///
  /// @param spriteToMove:     The sprite to move
  /// @param movementVector:   The vector containing the movement details
  procedure MoveSprite(spriteToMove : Sprite; const movementVector : Vector; pct: Single); overload;
  var
    mvmt: Vector;
    trans: Matrix2D;
  begin
    if not Assigned(spriteToMove) then raise Exception.Create('No sprite supplied');
    
    if spriteToMove.rotation <> 0 then
    begin
      trans := RotationMatrix(-spriteToMove.rotation);
      mvmt := MatrixMultiply(trans, movementVector);
    end
    else  mvmt := movementVector;
    
    spriteToMove.x := spriteToMove.x + (pct * mvmt.x);
    spriteToMove.y := spriteToMove.y + (pct * mvmt.y);
  end;

  /// Moves a sprite to a given x,y location.
  ///
  /// @param spriteToMove:     the sprite being moved
  /// @param x, y:             the new location of the sprite
  ///
  /// Side Effects:
  /// - Moves the sprite, changing its x and y
  procedure MoveSpriteTo(spriteToMove : Sprite; x,y : LongInt);
  begin
    if spriteToMove = nil then raise Exception.Create('No sprite supplied');
    
    spriteToMove.x := x;
    spriteToMove.y := y;
  end;

  procedure MoveSprite(spriteToMove: Sprite); overload;
  begin
    MoveSprite(spriteToMove, 1.0);
  end;  
  
  procedure MoveSprite(spriteToMove: Sprite; pct: Single); overload;
  begin
    MoveSprite(spriteToMove, spriteToMove.movement, pct);
  end;
  
  /// Creates a bitmap in memory that can be drawn onto. The bitmap is initially
  /// transparent and can be used as the target for various drawing operations.
  /// Once you have drawn the desired image onto the bitmap you can call
  /// OptimiseBitmap to optimise the surface.
  ///
  ///  @param width, height:  The width and height of the surface
  ///  @returns:              A new bitmap
  function CreateBitmap(width, height: LongInt): Bitmap;
  begin
    if (width < 1) or (height < 1) then
      raise Exception.Create('Bitmap width and height must be greater then 0');
      if (baseSurface = nil) or (baseSurface.format = nil) then
          raise Exception.Create('Unable to CreateBitmap as the window is not open');
    
    New(result);

    with baseSurface.format^ do
    begin
      result.surface := SDL_CreateRGBSurface(SDL_SRCALPHA, width, height, 32,
                       RMask, GMask, BMask, AMask);
    end;
    
    if result.surface = nil then
    begin
      Dispose(result);
      raise Exception.Create('Failed to create a bitmap: ' + SDL_GetError());
    end;
    
    result.width := width;
    result.height := height;
    SDL_SetAlpha(result.surface, SDL_SRCALPHA, 0);
    SDL_FillRect(result.surface, nil, ColorTransparent);
  end;
  
  procedure MakeOpaque(bmp: Bitmap);
  begin
    SDL_SetAlpha(bmp.surface, 0, 255);
  end;

  procedure MakeTransparent(bmp: Bitmap);
  begin
    SDL_SetAlpha(bmp.surface, SDL_SRCALPHA, 0);
  end;

  /// Created bitmaps can be optimised for faster drawing to the screen. This
  /// optimisation should be called only once after all drawing to the bitmap
  /// is complete. Optimisation should not be used if the bitmap is to be
  /// drawn to in the future. All loaded bitmaps are optimised during loading.
  ///
  /// @param surface: The bitmap to be optimised
  ///
  /// Side Effects:
  /// - Bitmap is optimised, and should not be used to draw onto in the future
  procedure OptimiseBitmap(surface: Bitmap);
  var
    oldSurface: PSDL_Surface;
  begin
    if surface = nil then raise Exception.Create('No bitmap supplied');
    
    oldSurface := surface.surface;
    SetNonAlphaPixels(surface, oldSurface);
    surface.surface := SDL_DisplayFormatAlpha(oldSurface);
    SDL_FreeSurface(oldSurface);
  end;
  
  procedure PutPixel(surface: PSDL_Surface; x, y: LongInt; color: Color);
  begin
    pixelColor(surface, x, y, ToGfxColor(color));
  end;
  
  /// Draws a pixel onto the screen.
  ///
  /// @param theColor:     The color to draw the pixel
  /// @param x,y:         The x,y location to draw the pixel at
  ///
  /// Side Effects:
  /// - Sets one pixel on the screen
  procedure DrawPixelOnScreen(theColor: Color; x, y: LongInt);
  begin
    DrawPixel(scr, theColor, x, y);
  end;

  procedure DrawPixel(theColor: Color; x, y: Single); overload;
  begin
    DrawPixelOnScreen(theColor, SGSDK_Camera.ToScreenX(x), SGSDK_Camera.ToScreenY(y));
  end;

  /// Draws a rectangle on the screen.
  ///
  /// @param theColor:     The color to draw the rectangle
  /// @param filled:       True to draw a filled rectangle, false for outline
  /// @param xPos,yPos:   The x,y location to draw the rectangle at
  /// @param width,height: The width and height of the rectangle
  ///
  /// Side Effects:
  /// - Draws a rectangle in the dest bitmap
  procedure DrawRectangleOnScreen(theColor : Color; filled : Boolean; xPos, yPos, width, height : LongInt); overload;
  begin
    DrawRectangle(scr, theColor, filled, xPos, yPos, width, height);
  end;

  procedure DrawRectangle(theColor : Color; filled : Boolean; xPos, yPos: Single; width, height : LongInt); overload;
  begin
    DrawRectangle(scr, theColor, filled, SGSDK_Camera.ToScreenX(xPos), SGSDK_Camera.ToScreenY(yPos), width, height);
  end;

  /// Draws the outline of a rectangle on the screen.
  ///
  /// @param theColor:     The color to draw the rectangle
  /// @param xPos,yPos:   The x,y location to draw the rectangle at
  /// @param width,height: The width and height of the rectangle
  ///
  /// Side Effects:
  /// - Draws a rectangle on the screen
  procedure DrawRectangleOnScreen(theColor : Color; xPos, yPos, width, height : LongInt); overload;
  begin
    DrawRectangle(scr, theColor, xPos, yPos, width, height);
  end;
  
  procedure DrawRectangle(theColor: Color; xPos, yPos: Single; width, height : LongInt); overload;
  begin
    DrawRectangle(scr, theColor, SGSDK_Camera.ToScreenX(xPos), SGSDK_Camera.ToScreenY(yPos), width, height);
  end;

  /// Draws a filled rectangle on the screen.
  ///
  /// @param theColor:     The color to draw the rectangle
  /// @param xPos,yPos:   The x,y location to draw the rectangle at
  /// @param width,height: The width and height of the rectangle
  ///
  /// Side Effects:
  /// - Draws a rectangle on the screen
  procedure FillRectangleOnScreen(theColor : Color; xPos, yPos, width, height : LongInt); overload;
  begin
    FillRectangle(scr, theColor, xPos, yPos, width, height);
  end;

  procedure FillRectangle(theColor : Color; xPos, yPos: Single; width, height : LongInt); overload;
  begin
    FillRectangle(scr, theColor, SGSDK_Camera.ToScreenX(xPos), SGSDK_Camera.ToScreenY(yPos), width, height);
  end;

  /// Draws a line on the screen.
  ///
  /// @param theColor:     The color to draw the line
  /// @param xPosStart,yPosStart: The x,y location to start the line at
  /// @param xPosEnd, yPosEnd:    The x,y location to end the line at
  ///
  /// Side Effects:
  /// - Draws a line in the screen
  procedure DrawLineOnScreen(theColor: Color; xPosStart, yPosStart, xPosEnd, yPosEnd: LongInt); overload;
  begin
    DrawLine(scr, theColor, xPosStart, yPosStart, xPosEnd, yPosEnd);
  end;
  
  procedure DrawLine(theColor: Color; xPosStart, yPosStart, xPosEnd, yPosEnd: Single); overload;
  begin
    DrawLine(scr, theColor, SGSDK_Camera.ToScreenX(xPosStart), SGSDK_Camera.ToScreenY(yPosStart), SGSDK_Camera.ToScreenX(xPosEnd), SGSDK_Camera.ToScreenY(yPosEnd));
  end;
  
  procedure DrawLine(theColor: Color; const line: LineSegment); overload;
  begin
    DrawLine(theColor, line.startPoint.x, line.startPoint.y, line.endPoint.x, line.endPoint.y);
  end;
  
  procedure DrawLine(dest: Bitmap; theColor: Color; const line: LineSegment); overload;
  begin
    DrawLine(dest, theColor, Round(line.startPoint.x), Round(line.startPoint.y), Round(line.endPoint.x), Round(line.endPoint.y));
  end;

  procedure DrawTriangle(theColor: Color; filled: Boolean; const tri: Triangle); overload;
  begin
    if filled then FillTriangle(theColor, tri)
    else DrawTriangle(theColor, tri);
  end;

  procedure DrawTriangle(dest: Bitmap; theColor: Color; filled: Boolean; const tri: Triangle); overload;
  begin
    if filled then FillTriangle(dest, theColor, tri)
    else DrawTriangle(dest, theColor, tri);
  end;
  
  procedure DrawTriangle(dest: Bitmap; theColor: Color; const tri: Triangle); overload;
  begin
    DrawTriangle(dest, theColor, tri[0].x, tri[0].y, tri[1].x, tri[1].y, tri[2].x, tri[2].y);
  end;

  procedure DrawTriangleOnScreen(theColor: Color; filled: Boolean; const tri: Triangle); overload;
  begin
    if filled then FillTriangleOnScreen(theColor, tri) 
    else DrawTriangleOnScreen(theColor, tri);
  end;

  procedure DrawTriangleOnScreen(theColor: Color; const tri: Triangle); overload;
  begin
    DrawTriangle(scr, theColor, tri[0].x, tri[0].y, tri[1].x, tri[1].y, tri[2].x, tri[2].y);
  end;

  procedure DrawTriangle(theColor: Color; const tri: Triangle); overload;
  begin
    DrawTriangle(theColor, tri[0].x, tri[0].y, tri[1].x, tri[1].y, tri[2].x, tri[2].y);
  end;
  
  procedure DrawTriangle(theColor: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  begin
    DrawTriangle(scr, theColor, SGSDK_Camera.ToScreenX(x1), SGSDK_Camera.ToScreenY(y1), SGSDK_Camera.ToScreenX(x2), SGSDK_Camera.ToScreenY(y2), SGSDK_Camera.ToScreenX(x3), SGSDK_Camera.ToScreenY(y3));
  end;
  
  procedure DrawTriangleOnScreen(theColor: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  begin
    DrawTriangle(scr, theColor, x1, y1, x2, y2, x3, y3);
  end;

  procedure DrawTriangle(dest: Bitmap; theColor: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  begin
    aatrigonColor(dest.surface, Round(x1), Round(y1), Round(x2), Round(y2), Round(x3), Round(y3), ToGfxColor(theColor));
  end;

  procedure FillTriangle(dest: Bitmap; theColor: Color; const tri: Triangle); overload;
  begin
    FillTriangle(dest, theColor, tri[0].x, tri[0].y, tri[1].x, tri[1].y, tri[2].x, tri[2].y);
  end;
  
  procedure FillTriangle(theColor: Color; const tri: Triangle); overload;
  begin
    FillTriangle(theColor, tri[0].x, tri[0].y, tri[1].x, tri[1].y, tri[2].x, tri[2].y);
  end;

  procedure FillTriangle(theColor: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  begin
    FillTriangle(scr, theColor, SGSDK_Camera.ToScreenX(x1), SGSDK_Camera.ToScreenY(y1), SGSDK_Camera.ToScreenX(x2), SGSDK_Camera.ToScreenY(y2), SGSDK_Camera.ToScreenX(x3), SGSDK_Camera.ToScreenY(y3));
  end;
  
  procedure FillTriangleOnScreen(theColor: Color; const tri: Triangle); overload;
  begin
    FillTriangle(scr, theColor, tri[0].x, tri[0].y, tri[1].x, tri[1].y, tri[2].x, tri[2].y);
  end;
  
  procedure FillTriangleOnScreen(theColor: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  begin
    FillTriangle(scr, theColor, x1, y1, x2, y2, x3, y3);
  end;

  procedure FillTriangle(dest: Bitmap; theColor: Color; x1, y1, x2, y2, x3, y3: Single); overload;
  begin
    filledTrigonColor(dest.surface, Round(x1), Round(y1), Round(x2), Round(y2), Round(x3), Trunc(y3), ToGfxColor(theColor));
  end;

  
  /// Draws a horizontal line on the screen.
  ///
  /// @param theColor:     The color to draw the line
  /// @param y:           The y location of the line
  /// @param x1, x2:       The starting and ending x value of the line
  ///
  /// Side Effects:
  /// - Draws a line on the screen
  procedure DrawHorizontalLineOnScreen(theColor: Color; y, x1, x2: LongInt); overload;
  begin
    DrawHorizontalLine(scr, theColor, y, x1, x2);
  end;

  procedure DrawHorizontalLine(theColor: Color; y, x1, x2: Single); overload;
  begin
    DrawHorizontalLine(scr, theColor, SGSDK_Camera.ToScreenY(y), SGSDK_Camera.ToScreenX(x1), SGSDK_Camera.ToScreenX(x2));
  end;

  /// Draws a vertical line on the screen.
  ///
  /// @param theColor:     The color to draw the line
  /// @param x:           The x location of the line
  /// @param y1, y2:       The starting and ending y value of the line
  ///
  /// Side Effects:
  /// - Draws a line on the screen
  procedure DrawVerticalLineOnScreen(theColor: Color; x, y1, y2: LongInt); overload;
  begin
    DrawVerticalLine(scr, theColor, x, y1, y2);
  end;

  procedure DrawVerticalLine(theColor: Color; x, y1, y2: Single); overload;
  begin
    DrawVerticalLine(scr, theColor, SGSDK_Camera.ToScreenX(x), SGSDK_Camera.ToScreenY(y1), SGSDK_Camera.ToScreenY(y2));
  end;

  /// Draws a circle centered on a given x, y location.
  ///
  /// @param theColor:     The color to draw the circle
  /// @param filled:       True to draw a filled circle, false for outline
  /// @param xc,yc:       The x,y location of the center of the circle
  /// @param radius:       The radius of the circle
  ///
  /// Side Effects:
  /// - Draws a Circle on the screen
  procedure DrawCircleOnScreen(theColor: Color; filled: Boolean; xc, yc, radius: LongInt); overload;
  begin
    DrawCircle(scr, theColor, filled, xc, yc, radius);
  end;

  procedure DrawCircle(theColor: Color; filled: Boolean; xc, yc: Single; radius: LongInt); overload;
  begin
    DrawCircle(scr, theColor, filled, SGSDK_Camera.ToScreenX(xc), SGSDK_Camera.ToScreenY(yc), radius);
  end;


  /// Draws a circle outline centered on a given x, y location.
  ///
  /// @param theColor:     The color to draw the circle
  /// @param xc,yc:       The x,y location of the center of the circle
  /// @param radius:       The radius of the circle
  ///
  /// Side Effects:
  /// - Draws a Circle on the screen
  procedure DrawCircleOnScreen(theColor: Color; xc, yc, radius: LongInt); overload;
  begin
    DrawCircle(scr, theColor, xc, yc, radius);
  end;

  procedure DrawCircle(theColor: Color; xc, yc: Single; radius: LongInt); overload;
  begin
    DrawCircle(scr, theColor, SGSDK_Camera.ToScreenX(xc), SGSDK_Camera.ToScreenY(yc), radius);
  end;

  /// Draws a filled circle centered on a given x, y location.
  ///
  /// @param theColor:     The color to draw the circle
  /// @param xc,yc:       The x,y location of the center of the circle
  /// @param radius:       The radius of the circle
  ///
  /// Side Effects:
  /// - Draws a Circle on the screen
  procedure FillCircleOnScreen(theColor: Color; xc, yc, radius: LongInt); overload;
  begin
    FillCircle(scr, theColor, xc, yc, radius);
  end;

  procedure FillCircle(theColor: Color; xc, yc: Single; radius: LongInt); overload;
  begin
    FillCircle(scr, theColor, SGSDK_Camera.ToScreenX(xc), SGSDK_Camera.ToScreenY(yc), radius);
  end;

  /// Draws a ellipse within a given rectangle on the screen.
  ///
  /// @param theColor:     The color to draw the ellipse
  /// @param filled:       True to draw a filled ellipse, false for outline
  /// @param xPos,yPos:   The x,y location of the top left of the ellipse
  /// @param width,height: The width and height of the ellipse
  ///
  /// Side Effects:
  /// - Draws a ellipse on the screen
  procedure DrawEllipseOnScreen(theColor: Color; filled: Boolean; xPos, yPos, width, height: LongInt); overload;
  begin
    DrawEllipse(scr, theColor, filled, xPos, yPos, width, height);
  end;

  procedure DrawEllipse(theColor: Color; filled: Boolean; xPos, yPos: Single; width, height: LongInt); overload;
  begin
    DrawEllipse(scr, theColor, filled, SGSDK_Camera.ToScreenX(xPos), SGSDK_Camera.ToScreenY(yPos), width, height);
  end;

  /// Draws a ellipse outline within a given rectangle on the screen.
  ///
  /// @param theColor:     The color to draw the ellipse
  /// @param xPos,yPos:   The x,y location of the top left of the ellipse
  /// @param width,height: The width and height of the ellipse
  ///
  /// Side Effects:
  /// - Draws a ellipse on the screen
  procedure DrawEllipseOnScreen(theColor: Color; xPos, yPos, width, height: LongInt); overload;
  begin
    DrawEllipse(scr, theColor, xPos, yPos, width, height);
  end;

  procedure DrawEllipse(theColor: Color; xPos, yPos: Single; width, height: LongInt); overload;
  begin
    DrawEllipse(scr, theColor, SGSDK_Camera.ToScreenX(xPos), SGSDK_Camera.ToScreenY(yPos), width, height);
  end;


  /// Draws a filled ellipse within a given rectangle on the screen.
  ///
  /// @param theColor:     The color to draw the ellipse
  /// @param xPos,yPos:   The x,y location of the top left of the ellipse
  /// @param width,height: The width and height of the ellipse
  ///
  /// Side Effects:
  /// - Draws a ellipse in the screen
  procedure FillEllipseOnScreen(theColor: Color;  xPos, yPos, width, height: LongInt); overload;
  begin
    FillEllipse(scr, theColor, xPos, yPos, width, height);
  end;

  procedure FillEllipse(theColor: Color;  xPos, yPos: Single; width, height: LongInt); overload;
  begin
    FillEllipse(scr, theColor, SGSDK_Camera.ToScreenX(xPos), SGSDK_Camera.ToScreenY(yPos), width, height);
  end;


  /// Draws a rectangle on the destination bitmap.
  ///
  /// @param dest:         The destination bitmap - not optimised!
  /// @param theColor:     The color to draw the rectangle
  /// @param filled:       True to draw a filled rectangle, false for outline
  /// @param xPos,yPos:   The x,y location to draw the rectangle at
  /// @param width,height: The width and height of the rectangle
  ///
  /// Side Effects:
  /// - Draws a rectangle in the dest bitmap
  procedure DrawRectangle(dest: Bitmap; theColor: Color; filled : Boolean; xPos, yPos, width, height : LongInt); overload;
  begin
    if filled then FillRectangle(dest, theColor, xPos, yPos, width, height)
    else DrawRectangle(dest, theColor, xPos, yPos, width, height);
  end;

  /// Draws the outline of a rectangle on the destination bitmap.
  ///
  /// @param dest:         The destination bitmap - not optimised!
  /// @param theColor:     The color to draw the rectangle
  /// @param xPos,yPos:   The x,y location to draw the rectangle at
  /// @param width,height: The width and height of the rectangle
  ///
  /// Side Effects:
  /// - Draws a rectangle in the dest bitmap
  procedure DrawRectangle(dest: Bitmap; theColor : Color; xPos, yPos, width, height : LongInt); overload;
  begin
    if dest = nil then raise Exception.Create('No destination bitmap supplied');
    rectangleColor(dest.surface, xPos, yPos, xPos + width, yPos + height, ToGfxColor(theColor));
  end;

  /// Draws a filled rectangle on the destination bitmap.
  ///
  /// @param dest:         The destination bitmap - not optimised!
  /// @param theColor:     The color to draw the rectangle
  /// @param xPos,yPos:   The x,y location to draw the rectangle at
  /// @param width,height: The width and height of the rectangle
  ///
  /// Side Effects:
  /// - Draws a rectangle in the dest bitmap
  procedure FillRectangle(dest: Bitmap; theColor : Color;  xPos, yPos, width, height : LongInt);
  var
    rect: SDL_Rect;
  begin
    if dest = nil then raise Exception.Create('No destination bitmap supplied');
    
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
    
    //SDL_FillRect(dest.surface, @rect, theColor);
    boxColor(dest.surface, rect.x, rect.y, rect.x + width, rect.y + height, ToGfxColor(theColor));
  end;

  /// Draws a ellipse within a given rectangle on the dest bitmap.
  ///
  /// @param dest:         The destination bitmap - not optimised!
  /// @param theColor:     The color to draw the ellipse
  /// @param filled:       True to draw a filled ellipse, false for outline
  /// @param xPos,yPos:   The x,y location of the top left of the ellipse
  /// @param width,height: The width and height of the ellipse
  ///
  /// Side Effects:
  /// - Draws a ellipse in the dest bitmap
  procedure DrawEllipse(dest: Bitmap; theColor: Color; filled: Boolean; xPos, yPos, width, height: LongInt); overload;
  begin
    if filled then FillEllipse(dest, theColor, xPos, yPos, width, height)
    else DrawEllipse(dest, theColor, xPos, yPos, width, height);
  end;

  /// Draws a ellipse outline within a given rectangle on the dest bitmap.
  ///
  /// @param dest:         The destination bitmap - not optimised!
  /// @param theColor:     The color to draw the ellipse
  /// @param xPos,yPos:   The x,y location of the top left of the ellipse
  /// @param width,height: The width and height of the ellipse
  ///
  /// Side Effects:
  /// - Draws a ellipse in the dest bitmap
  procedure DrawEllipse(dest: Bitmap; theColor: Color;  xPos, yPos, width, height: LongInt); overload;
  var
    halfWidth, halfHeight: Sint16;
  begin
    halfWidth := width div 2;
    halfHeight := height div 2;
    
    aaellipseColor(dest.surface, xPos + halfWidth, yPos + halfHeight, halfWidth, halfHeight, ToGfxColor(theColor));
  end;

  /// Draws a filled ellipse within a given rectangle on the dest bitmap.
  ///
  /// @param dest:         The destination bitmap - not optimised!
  /// @param theColor:     The color to draw the ellipse
  /// @param xPos,yPos:   The x,y location of the top left of the ellipse
  /// @param width,height: The width and height of the ellipse
  ///
  /// Side Effects:
  /// - Draws a ellipse in the dest bitmap
  procedure FillEllipse(dest: Bitmap; theColor: Color; xPos, yPos, width, height: LongInt);
  var
    halfWidth, halfHeight: Sint16;
  begin
    halfWidth := width div 2;
    halfHeight := height div 2;

    filledEllipseColor(dest.surface, xPos + halfWidth, yPos + halfHeight, halfWidth, halfHeight, ToGfxColor(theColor));
  end;
  
  /// Draws a vertical line on the destination bitmap.
  ///
  /// @param dest:         The destination bitmap - not optimised!
  /// @param theColor:     The color to draw the line
  /// @param x:           The x location of the line
  /// @param y1, y2:       The starting and ending y value of the line
  ///
  /// Side Effects:
  /// - Draws a line in the dest bitmap
  procedure DrawVerticalLine(dest: Bitmap; theColor: Color; x, y1, y2: LongInt);
  begin
    if dest = nil then raise Exception.Create('The destination bitmap to draw a vertical line is nil');
    vlineColor(dest.surface, x, y1, y2, ToGfxColor(theColor));
  end;

  /// Draws a horizontal line on the destination bitmap.
  ///
  /// @param dest:         The destination bitmap - not optimised!
  /// @param theColor:     The color to draw the line
  /// @param y:           The y location of the line
  /// @param x1, x2:       The starting and ending x value of the line
  ///
  /// Side Effects:
  /// - Draws a line in the dest bitmap
  procedure DrawHorizontalLine(dest: Bitmap; theColor: Color; y, x1, x2: LongInt);
  begin
    if dest = nil then raise Exception.Create('The destination bitmap to draw a vertical line is nil');
      
    hlineColor(dest.surface, x1, x2, y, ToGfxColor(theColor));
  end;

  /// Draws a line on the destination bitmap.
  ///
  /// @param dest:         The destination bitmap - not optimised!
  /// @param theColor:     The color to draw the line
  /// @param xPosStart,yPosStart: The x,y location to start the line at
  /// @param xPosEnd, yPosEnd:    The x,y location to end the line at
  ///
  /// Side Effects:
  /// - Draws a line in the dest bitmap
  procedure DrawLine(dest: Bitmap; theColor: Color; xPosStart, yPosStart, xPosEnd, yPosEnd: LongInt);
  begin
    aalineColor(dest.surface, xPosStart, yPosStart, xPosEnd, yPosEnd, ToGfxColor(theColor));
  end;

  /// Draws a pixel onto the destination bitmap.
  ///
  /// @param dest:         The destination bitmap - not optimised!
  /// @param theColor:     The color to draw the pixel
  /// @param x,y:         The x,y location to draw the pixel at
  ///
  /// Side Effects:
  /// - Sets one pixel on the destination bitmap
  procedure DrawPixel(dest: Bitmap; theColor: Color; x, y: LongInt); overload;
  begin
    if dest = nil then raise Exception.Create('The destination bitmap to draw a pixel is nil');
    
    if (x < 0) or (x >= dest.surface.w) or (y < 0) or (y >= dest.surface.h) then exit;
    
    //if SDL_MUSTLOCK(dest.surface) then SDL_LockSurface(dest.surface);
    
    PutPixel(dest.surface, x, y, theColor);
    
    //if SDL_MUSTLOCK(dest.surface) then SDL_UnlockSurface(dest.surface);
  end;
  
  /// Draws a circle centered on a given x, y location.
  ///
  /// @param dest:         The destination bitmap - not optimised!
  /// @param theColor:     The color to draw the circle
  /// @param filled:       True to draw a filled circle, false for outline
  /// @param xc,yc:       The x,y location of the center of the circle
  /// @param radius:       The radius of the circle
  ///
  /// Side Effects:
  /// - Draws a Circle in the dest bitmap
  procedure DrawCircle(dest: Bitmap; theColor: Color; filled: Boolean; xc, yc, radius: LongInt); overload;
  begin
    if filled then FillCircle(dest, theColor, xc, yc, radius)
    else DrawCircle(dest, theColor, xc, yc, radius);
  end;

  /// Draws a circle outline centered on a given x, y location.
  ///
  /// @param dest:         The destination bitmap - not optimised!
  /// @param theColor:     The color to draw the circle
  /// @param xc,yc:       The x,y location of the center of the circle
  /// @param radius:       The radius of the circle
  ///
  /// Side Effects:
  /// - Draws a Circle in the dest bitmap
  procedure DrawCircle(dest: Bitmap; theColor: Color; xc, yc, radius: LongInt); overload;
  begin
    aacircleColor(dest.surface, xc, yc, radius, ToGfxColor(theColor));
  end;

  /// Draws a filled circle centered on a given x, y location.
  ///
  /// @param dest:         The destination bitmap - not optimised!
  /// @param theColor:     The color to draw the circle
  /// @param xc,yc:       The x,y location of the center of the circle
  /// @param radius:       The radius of the circle
  ///
  /// Side Effects:
  /// - Draws a Circle in the dest bitmap
  procedure FillCircle(dest: Bitmap; theColor: Color; xc, yc, radius: LongInt);
  begin
    filledCircleColor(dest.surface, xc, yc, radius, ToGfxColor(theColor));
  end;
  
  
  
  //**********
  // Overloaded draw methods...
  //**********
  
  
  procedure DrawBitmap(dest: Bitmap; src: Bitmap; const position: Point2D); overload;
  begin
    DrawBitmap(dest, src, Round(position.x), Round(position.y));
  end;

  procedure DrawBitmapPart(dest: Bitmap; src: Bitmap; const source: Rectangle; const position: Point2D); overload;
  begin
    DrawBitmapPart(dest, src, source, Round(position.x), Round(position.y));
  end;

  procedure DrawPixel(dest: Bitmap; theColor: Color; const position : Point2D); overload;
  begin
    DrawPixel(dest, theColor, Round(position.x), Round(position.y));
  end;

  procedure DrawRectangle(dest: Bitmap; theColor : Color; filled : Boolean; const source: Rectangle); overload;
  begin
    DrawRectangle(dest, theColor, filled, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure DrawRectangle(dest: Bitmap; theColor : Color; const source: Rectangle); overload;
  begin
    DrawRectangle(dest, theColor, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure FillRectangle(dest: Bitmap; theColor : Color; const source: Rectangle); overload;
  begin
    FillRectangle(dest, theColor, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure DrawCircle(dest: Bitmap; theColor: Color; filled: Boolean; const point: Point2D; radius: LongInt); overload;
  begin
    DrawCircle(dest, theColor, filled, Round(point.x), Round(point.y), radius);
  end;

  procedure DrawCircle(dest: Bitmap; theColor: Color; const point: Point2D; radius: LongInt); overload;
  begin
    DrawCircle(dest, theColor, Round(point.x), Round(point.y), radius);
  end;

  procedure FillCircle(dest: Bitmap; theColor: Color; const point: Point2D; radius: LongInt); overload;
  begin
    FillCircle(dest, theColor, Round(point.x), Round(point.y), radius);
  end;

  procedure DrawEllipse(dest: Bitmap; theColor: Color; filled: Boolean; const source: Rectangle); overload;
  begin
    DrawEllipse(dest, theColor, filled, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure DrawEllipse(dest: Bitmap; theColor: Color; const source: Rectangle); overload;
  begin
    DrawEllipse(dest, theColor, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure FillEllipse(dest: Bitmap; theColor: Color; const source: Rectangle); overload;
  begin
    FillEllipse(dest, theColor, Round(source.x), Round(source.y), source.width, source.height);
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

  procedure DrawPixel(theColor: Color; const position: Point2D); overload;
  begin
    DrawPixel(theColor, Round(position.x), Round(position.y));
  end;

  procedure DrawRectangle(theColor : Color; filled : Boolean; const source : Rectangle); overload;
  begin
    DrawRectangle(theColor, filled, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure DrawRectangle(theColor : Color; const source : Rectangle); overload;
  begin
    DrawRectangle(theColor, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure FillRectangle(theColor : Color; const source : Rectangle); overload;
  begin
    FillRectangle(theColor, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure DrawCircle(theColor: Color; filled: Boolean; const position: Point2D; radius: LongInt); overload;
  begin
    DrawCircle(theColor, filled, Round(position.x), Round(position.y), radius);
  end;

  procedure DrawCircle(theColor: Color; const position: Point2D; radius: LongInt); overload;
  begin
    DrawCircle(theColor, Round(position.x), Round(position.y), radius);
  end;

  procedure FillCircle(theColor: Color; const position: Point2D; radius: LongInt); overload;
  begin
    FillCircle(theColor, position.x, position.y, radius);
  end;

  procedure DrawEllipse(theColor: Color; filled: Boolean; const source: Rectangle); overload;
  begin
    DrawEllipse(theColor, filled, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure DrawEllipse(theColor: Color; const source: Rectangle); overload;
  begin
    DrawEllipse(theColor, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure FillEllipse(theColor: Color; const source: Rectangle); overload;
  begin
    FillEllipse(theColor, Round(source.x), Round(source.y), source.width, source.height);
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

  procedure DrawPixelOnScreen(theColor: Color; const position: Point2D); overload;
  begin
    DrawPixelOnScreen(theColor, Round(position.x), Round(position.y));
  end;

  procedure DrawRectangleOnScreen(theColor : Color; filled : Boolean; const source : Rectangle); overload;
  begin
    DrawRectangleOnScreen(theColor, filled, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure DrawRectangleOnScreen(theColor : Color; const source : Rectangle); overload;
  begin
    DrawRectangleOnScreen(theColor, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure FillRectangleOnScreen(theColor : Color; const source : Rectangle); overload;
  begin
    FillRectangleOnScreen(theColor, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure DrawLineOnScreen(theColor: Color; const line: LineSegment); overload;
  begin
    DrawLineOnScreen(theColor, Round(line.startPoint.x), Round(line.startPoint.y), Round(line.endPoint.x), Round(line.endPoint.y));
  end;

  procedure DrawCircleOnScreen(theColor: Color; filled: Boolean; const position: Point2D; radius: LongInt); overload;
  begin
    DrawCircleOnScreen(theColor, filled, Round(position.x), Round(position.y), radius);
  end;

  procedure DrawCircleOnScreen(theColor: Color; const position: Point2D; radius: LongInt); overload;
  begin
    DrawCircleOnScreen(theColor, Round(position.x), Round(position.y), radius);
  end;

  procedure FillCircleOnScreen(theColor: Color; const position: Point2D; radius: LongInt); overload;
  begin
    FillCircleOnScreen(theColor, Round(position.x), Round(position.y), radius);
  end;

  procedure DrawEllipseOnScreen(theColor: Color; filled: Boolean; const source: Rectangle); overload;
  begin
    DrawEllipseOnScreen(theColor, filled, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure DrawEllipseOnScreen(theColor: Color; const source: Rectangle); overload;
  begin
    DrawEllipseOnScreen(theColor, Round(source.x), Round(source.y), source.width, source.height);
  end;

  procedure FillEllipseOnScreen(theColor: Color; const source: Rectangle); overload;
  begin
    FillEllipseOnScreen(theColor, Round(source.x), Round(source.y), source.width, source.height);
  end;
  
  procedure ResetClip(bmp: Bitmap); overload;
  begin
    if bmp = nil then raise Exception.Create('Cannot reset clip, bmp must not be nil');
    SDL_SetClipRect(bmp.surface, nil);
  end;

  procedure ResetClip(); overload;
  begin
    ResetClip(scr);
  end;
  
  procedure SetClip(bmp: Bitmap; x, y, w, h: LongInt); overload;
  var
    rect: SDL_Rect;
  begin
    if bmp = nil then raise Exception.Create('Cannot set clip, bmp must not be nil');
    rect := NewSDLRect(x, y, w, h);
    SDL_SetClipRect(bmp.surface, @rect);
  end;
  
  procedure SetClip(bmp: Bitmap; r: Rectangle); overload;
  begin
    SetClip(bmp, Round(r.x), Round(r.y), r.width, r.height);
  end;

  procedure SetClip(x, y, w, h: LongInt); overload;
  begin
    SetClip(scr, x, y, w, h);
  end;
  
  procedure SetClip(r: Rectangle); overload;
  begin
    SetClip(scr, Round(r.x), Round(r.y), r.width, r.height);
  end;
  
  function RotateScaleBitmap(src: Bitmap; degRot, scale: Single): Bitmap;
  begin
    New(result);
    result.surface := rotozoomSurface(src.surface, degRot, scale, 1);
    result.width := result.surface.w;
    result.height := result.surface.h;
  end;
  
  procedure SetupBitmapForCollisions(src: Bitmap);
  begin
    if Length(src.nonTransparentPixels) <> 0 then exit;
      
    SetNonAlphaPixels(src, src.surface);
    OptimiseBitmap(src);
  end;
end.