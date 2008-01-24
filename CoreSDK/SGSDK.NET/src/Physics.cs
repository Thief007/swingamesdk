//-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
// 					Physics
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// The Physics unit contains the code that is responsible
// for performing collisions and vector maths.
//
// Change History:
//
// Version 1.1:
// - 2008-01-23: Andrew: Fixed exceptions
//    Added changes for 1.1 compatibility
//    Refactored some methods, to limit routines exposed by DLL
//    Added extra comments, and fixed code layout and line endings.
//    
// Version 1.0:
// - Various

using System;
using System.Runtime.InteropServices;
using System.Drawing;

namespace SwinGame
{
    /// <summary>
    /// Enumeration: CollisionDetectionRanges
    ///	This is used to indicate the kind of collision being checked with the
    ///	Sprite collision routines.  
    /// </summary>
    public enum CollisionDetectionRange
    {
        /// <summary>
        /// Collision Range is Equal
        /// </summary>
        CollisionRangeEquals = 0,
        /// <summary>
        /// Collision Range is Greater
        /// </summary>
        CollisionRangeGreaterThan = 1,
        /// <summary>
        /// Collision Range is Less
        /// </summary>
        CollisionRangeLessThan = 2
    }

    /// <summary>
    /// This record is used to represent transformations that can be
    /// used to apply these changes to vectors.
    /// </summary>
    //[StructLayout(LayoutKind.Sequential)]
    public class Matrix2D : IDisposable
    {
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "FreeMatrix2D")]
        private static extern void FreeMaxtrix2D(ref IntPtr maxtrix2d);
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetMatrix2DElement")]
        private static extern Single GetMaxtrix2DElement(IntPtr maxtrix2d, int r, int c);
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SetMatrix2DElement")]
        private static extern void SetMaxtrix2DElement(IntPtr maxtrix2d, int r, int c, Single val); 

        //private IntPtr Pointer;
        private SwinGamePointer Pointer;
        private bool disposed = false;

        /// <summary>
        /// Gets an element from the Matrix2D
        /// </summary>
        /// <param name="r">Row</param>
        /// <param name="c">Column</param>
        /// <returns>Element</returns>
        public float this[int r, int c]
        {
            get
            {
                return GetMaxtrix2DElement(this.Pointer, r, c);
            }
            set 
            {
                SetMaxtrix2DElement(this.Pointer, r, c, value);
            }
        }

        /// <summary>
        /// Disposes the 2D Matrix
        /// </summary>
        public void Dispose()
        {
            Dispose(true);
        }

        private void Dispose(bool disposing)
        {
            if (!this.disposed)
            {
                //System.Diagnostics.Debug.WriteLine("Disposing - " + Pointer);
                if (disposing)
                {
                    GC.SuppressFinalize(this);
                }
                
                Pointer.Free();

                disposed = true;
            }
        }

        internal Matrix2D(IntPtr ptr)
        {
            if (ptr == IntPtr.Zero) 
                throw new SwinGameException("Error Creating Matrix2D");
            Pointer = new SwinGamePointer(ptr, FreeMaxtrix2D);
        }

        /// <summary>
        /// Garbage Collection Disposal
        /// </summary>
        ~Matrix2D()
        {
            Dispose(false);
        }

        /// <summary>
        /// A Matrix2D can be treated as an integer pointer for the purpose
        /// of interacting with the SwinGame DLL.
        /// </summary>
        /// <param name="m">The matrix to get the pointer of</param>
        /// <returns>an IntPtr pointing to the memory where the Matrix resides</returns>
        public static implicit operator IntPtr (Matrix2D m)
        {
            return m.Pointer;
        }
    }

    /// <summary>
    /// SGSDK.NET's Physics Class contains the code to perform various mathematical
    /// functions and collisions.
    /// </summary>
    public class Physics
    {
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "HasSpriteCollidedX")]
        private static extern bool DLL_HasSpriteCollidedX(IntPtr theSprite, int x, CollisionDetectionRange  range);
        /// <summary>
        /// Determines if a sprite has collided with a given x position.
        /// </summary>
        /// <param name="theSprite">The sprite to check</param>
        /// <param name="x">The x location to check collision with</param>
        /// <param name="range">The kind of check to perform less, larger or equal.</param>
        /// <returns>True if the sprite is within the range requested</returns>
        public static bool HasSpriteCollidedX(Sprite theSprite, int x, CollisionDetectionRange range)
        {
            bool temp = DLL_HasSpriteCollidedX(theSprite.Pointer, x, range);
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "HasSpriteCollidedY")]
        private static extern bool DLL_HasSpriteCollidedY(IntPtr theSprite, int y ,CollisionDetectionRange range);
        /// <summary>
        /// Determines if a sprite has collided with a given y position.
        /// </summary>
        /// <param name="theSprite">The sprite to check</param>
        /// <param name="y">The y location to check collision with</param>
        /// <param name="range">The kind of check to perform less, larger or equal.</param>
        /// <returns>True if the sprite is within the range requested</returns>
        public static bool HasSpriteCollidedY(Sprite theSprite, int y, CollisionDetectionRange range)
        {
            bool temp;

            try
            {
                temp = DLL_HasSpriteCollidedY(theSprite.Pointer, y, range);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "HasSpriteCollidedWithRect")]
        private static extern bool DLL_HasSpriteCollidedWithRect(IntPtr theSprite, Single x, Single y, int width, int height);
        /// <summary>
        /// Determined if a sprite has collided with a given rectangle. The rectangles
        ///	coordinates are expressed in "world" coordinates.
        /// </summary>
        /// <param name="theSprite">The sprite to check</param>
        /// <param name="x">The x location of the rectangle</param>
        /// <param name="y">The y location of the rectangle</param>
        /// <param name="width">The width of the rectangle</param>
        /// <param name="height">The height of the rectangle</param>
        /// <returns>True if the sprite collides with the rectangle</returns>
        public static bool HasSpriteCollidedWithRect(Sprite theSprite, Single x, Single y, int width, int height)
        {
            bool temp;

            try
            {
                temp = DLL_HasSpriteCollidedWithRect(theSprite.Pointer, x, y, width, height);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }
        
        /// <summary>
        /// Determined if a sprite has collided with a given rectangle. The rectangles
        ///	coordinates must be expressed in "game" coordinates.
        /// </summary>
        /// <param name="theSprite">The sprite to check</param>
        /// <param name="rect">The rectangle to check collisions with</param>
        /// <returns>True if the sprite collides with the rectangle</returns>
        public static bool HasSpriteCollidedWithRect(Sprite theSprite, Rectangle rect)
        {
            return HasSpriteCollidedWithRect(theSprite, rect.X, rect.Y, rect.Width, rect.Height);
        }
        
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "HaveSpritesCollided")]
        private static extern bool DLL_HaveSpritesCollided(IntPtr sprite1, IntPtr sprite2);

        /// <summary>
        /// Determines if two sprites have collided. Sprites have collided when
        /// their images overlap. You may want to consider separating these if you
        /// are performing any "bounce" like activities. Separation is part of the
        /// collision code in many cases.
        /// </summary>
        /// <param name="sprite1">The first sprite to check.</param>
        /// <param name="sprite2">The second sprite to check.</param>
        /// <returns>True if the sprites have collided.</returns>
        public static bool HaveSpritesCollided(Sprite sprite1, Sprite sprite2)
        {
            bool temp;

            try
            {
                temp = DLL_HaveSpritesCollided(sprite1.Pointer, sprite2.Pointer);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "HasSpriteCollidedWithBitmap")]
        private static extern int DLL_HasSpriteCollidedWithBitmap(IntPtr sprt, IntPtr bitmap, float x, float y, int bounded);
        /// <summary>
        /// Determines if a sprite has collided with a bitmap using pixel level
        ///	collision detection with the bitmap.
        /// </summary>
        /// <param name="theSprite">The sprite to check for collision</param>
        /// <param name="theBitmap">The bitmap image to check for collision</param>
        /// <param name="x">The x location of the bitmap</param>
        /// <param name="y">The y location of the bitmap</param>
        /// <param name="bounded">Indicates if theBitmap should use bounded collision</param>
        /// <returns>True if the bitmap has collided with the sprite.</returns>
        public static bool HasSpriteCollidedWithBitmap(Sprite theSprite, Bitmap theBitmap, float x, float y, bool bounded)
        {
            bool temp;
            try
            {
                temp = DLL_HasSpriteCollidedWithBitmap(theSprite.Pointer, theBitmap.pointer, x, y, (bounded?-1:0)) == -1;
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        /// <summary>
        /// Determines if a sprite has collided with a bitmap. The x and y values
        ///	are expressed in "game" coordinates. This uses a bounded collision check
        /// rather than performing a pixel level check on the bitmap.
        /// </summary>
        /// <param name="theSprite">The sprite to check for collision</param>
        /// <param name="theBitmap">The bitmap image to check for collision</param>
        /// <param name="x">The x location of the bitmap</param>
        /// <param name="y">The y location of the bitmap</param>
        /// <returns> True if the bitmap has collided with the sprite.</returns>
        public static bool HasSpriteCollidedWithBitmap(Sprite theSprite, Bitmap theBitmap, float x, float y)
        {
            return HasSpriteCollidedWithBitmap(theSprite, theBitmap, x, y, true); 
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "HasSpriteCollidedWithBitmapPart")]
        private static extern int DLL_HasSpriteCollidedWithBitmapPart(IntPtr sprt, IntPtr bitmap, Point2D pt, SGSDKRectangle rect, int bounded);
        /// <summary>
        /// Determines if a sprte has collided with a bitmap. This version is used to check
        /// for collisions with a cell from the bitmap. The src rectangle defines the part of
        /// the bitmap you want to check collisions with. This is usefull for checking
        /// collisions between sprites and bitmaps you use to contain multiple cells
        /// for animation etc.
        /// </summary>
        /// <param name="sprt">the sprite to check collision of</param>
        /// <param name="bmp">the bitmap containing the cell you want to check</param>
        /// <param name="pt">the pt, in game coordinates, of the bitmap cell</param>
        /// <param name="src">the rectangle containing the portion of the bitmap to check</param>
        /// <param name="bounded">set to true to perform a bounded box check, false for per pixel checks</param>
        /// <returns>true if the sprite has collided with the bitmap cell</returns>
        public static bool HasSpriteCollidedWithBitmap(Sprite sprt, Bitmap bmp, Point2D pt, Rectangle src, bool bounded)
        {
            bool temp;
            try
            {
                temp = DLL_HasSpriteCollidedWithBitmapPart(sprt.Pointer, bmp.pointer, pt, Shapes.ToSGSDKRect(src), (bounded ? -1 : 0)) == -1;
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }
        /// <summary>
        /// Determines if a sprte has collided with a bitmap. This version is used to check
        /// for collisions with a cell from the bitmap. The src rectangle defines the part of
        /// the bitmap you want to check collisions with. This is usefull for checking
        /// collisions between sprites and bitmaps you use to contain multiple cells
        /// for animation etc.
        /// </summary>
        /// <param name="sprt">the sprite to check collision of</param>
        /// <param name="bmp">the bitmap containing the cell you want to check</param>
        /// <param name="pt">the pt, in game coordinates, of the bitmap cell</param>
        /// <param name="bounded">set to true to perform a bounded box check, false for per pixel checks</param>
        /// <returns>true if the sprite has collided with the bitmap cell</returns>
        public static bool HasSpriteCollidedWithBitmap(Sprite sprt, Bitmap bmp, Point2D pt, bool bounded)
        {
            return HasSpriteCollidedWithBitmap(sprt, bmp, pt, Shapes.CreateRectangle(bmp), bounded);
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "RectangleHasCollidedWithLine")]
        private static extern bool DLL_RectangleHasCollidedWithLine(SGSDKRectangle rect, LineSegment line);
        /// <summary>
        /// Returns true if the Rectangle has collided with the line specified
        /// </summary>
        /// <param name="rect">Rectangle</param>
        /// <param name="line">Line</param>
        /// <returns>True if the Rectangle has collided with the line</returns>
        public static bool RectangleHasCollidedWithLine(Rectangle rect, LineSegment line)
        {
            bool temp;

            try
            {
                temp = DLL_RectangleHasCollidedWithLine(Shapes.ToSGSDKRect(rect), line);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }
        /// <summary>
        /// Returns true if the Sprite's Rectangle has collided with the line specified
        /// </summary>
        /// <param name="sprite">Sprite</param>
        /// <param name="line">Line</param>
        /// <returns>True if the Sprite has collided with the line</returns>
        public static bool RectangleHasCollidedWithLine(Sprite sprite, LineSegment line)
        {
            
            return RectangleHasCollidedWithLine(Shapes.CreateRectangle(sprite), line);
        }

        
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "HaveBitmapsCollided")]
        private static extern int DLL_HaveBitmapsCollided(IntPtr image1, int x1, int y1, int bounded1, IntPtr image2, int  x2, int y2, int bounded2);
        /// <summary>
        /// Checks to see if two bitmaps have collided, this performs a bounded check
        ///	then, if required, it performs a per pixel check on the colliding region.
        /// </summary>
        /// <param name="image1">The bitmap to check for collision</param>
        /// <param name="x1">The x location of image 1</param>
        /// <param name="y1">The y location of image 1</param>
        /// <param name="bounded1">Indicates if image1 should use bounded collision</param>
        /// <param name="image2">The bitmap to check for collision</param>
        /// <param name="x2">The x location of image 2</param>
        /// <param name="y2">The y location of image 2</param>
        /// <param name="bounded2">Indicates if image2 should use bounded collision</param>
        /// <returns>True if the bitmaps collide.</returns>
        public static bool HaveBitmapsCollided(Bitmap image1, int x1, int y1, bool bounded1, Bitmap image2, int x2, int y2, bool bounded2)
        {
            bool temp;

            try
            {
                temp = DLL_HaveBitmapsCollided(image1.pointer, x1, y1, (bounded1?-1:0), image2.pointer, x2, y2, (bounded2?-1:0)) == -1;

            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }
        /// <summary>
        /// Checks to see if two bitmaps have collided, this performs a bounded check.
        /// </summary>
        /// <param name="image1">The bitmap to check for collision</param>
        /// <param name="x1">The x location of image 1</param>
        /// <param name="y1">The y location of image 1</param>
        /// <param name="image2">The bitmap to check for collision</param>
        /// <param name="x2">The x location of image 2</param>
        /// <param name="y2">The y location of image 2</param>
        /// <returns>True if the bitmaps collide.</returns>
        public static bool HaveBitmapsCollided(Bitmap image1, int x1, int y1, Bitmap image2, int x2, int y2)
        {
            return HaveBitmapsCollided(image1, x1, y1, true,image2, x2, y2, true);
        }

        /// <summary>
        /// Checks if two bitmaps has collided using a bounded box, not pixel level
        /// collisions.
        /// </summary>
        /// <param name="image1">image 1</param>
        /// <param name="pt1">the location of image 1</param>
        /// <param name="image2">image 2</param>
        /// <param name="pt2">the location of image 2</param>
        /// <returns>true if the bitmaps collide at the indicated location</returns>
        public static bool HaveBitmapsCollided(Bitmap image1, Point2D pt1, Bitmap image2, Point2D pt2)
        {
            return HaveBitmapsCollided(image1, (int)pt1.X, (int)pt1.Y, true, image2, (int)pt2.X, (int)pt2.Y, true);
        }

        /// <summary>
        /// Checks if two bitmaps has collided using a bounded box or pixel level
        /// collisions.
        /// </summary>
        /// <param name="image1">image 1</param>
        /// <param name="pt1">the location of image 1</param>
        /// <param name="bounded1">if true a bounded box is used for image 1</param>
        /// <param name="image2">image 2</param>
        /// <param name="pt2">the location of image 2</param>
        /// <param name="bounded2">if true a bounded box is used for image 2</param>
        /// <returns>true if the bitmaps collide at the indicated location</returns>
        public static bool HaveBitmapsCollided(Bitmap image1, Point2D pt1, bool bounded1, Bitmap image2, Point2D pt2, bool bounded2)
        {
            return HaveBitmapsCollided(image1, (int)pt1.X, (int)pt1.Y, bounded1, image2, (int)pt2.X, (int)pt2.Y, bounded2);
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "HaveBitmapPartsCollided")]
        private static extern int DLL_HaveBitmapPartsCollided(IntPtr image1, Point2D pt1, SGSDKRectangle src1, int bounded1, IntPtr image2, Point2D pt2, SGSDKRectangle src2, int bounded2);

        /// <summary>
        /// This performs a collision detection of two bitmap cells. This can be either
        /// bounded or pixel level.
        /// </summary>
        /// <param name="image1">image 1</param>
        /// <param name="pt1">the location of image 1</param>
        /// <param name="src1">the cell within image 1</param>
        /// <param name="bounded1">true if you want to use a bounded box for image 1, false for pixel level collisions.</param>
        /// <param name="image2">image 2</param>
        /// <param name="pt2">the location of image 2</param>
        /// <param name="src2">the cell within image 2</param>
        /// <param name="bounded2">true if you want to use a bounded box for image 2, false for pixel level collisions.</param>
        /// <returns>true if the cells collide at that location</returns>
        public static bool HaveBitmapsCollided(Bitmap image1, Point2D pt1, Rectangle src1, bool bounded1, Bitmap image2, Point2D pt2, Rectangle src2, bool bounded2)
        {
            bool temp;
            try
            {
                temp = DLL_HaveBitmapPartsCollided(image1.pointer, pt1, Shapes.ToSGSDKRect(src1), (bounded1 ? -1 : 0), image2.pointer, pt2, Shapes.ToSGSDKRect(src2),(bounded2 ? -1 : 0)) == -1;
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        /// <summary>
        /// This performs a collision detection of two bitmap cells using bounded box
        /// collisions.
        /// </summary>
        /// <param name="image1">image 1</param>
        /// <param name="pt1">the location of image 1</param>
        /// <param name="src1">the cell within image 1</param>
        /// <param name="image2">image 2</param>
        /// <param name="pt2">the location of image 2</param>
        /// <param name="src2">the cell within image 2</param>
        /// <returns>true if the cells collide at that location</returns>
        public static bool HaveBitmapsCollided(Bitmap image1, Point2D pt1, Rectangle src1, Bitmap image2, Point2D pt2, Rectangle src2)
        {
            return HaveBitmapsCollided(image1, pt1, src1, true, image2, pt2, src2, true);
        }
        
        /// <summary>
        /// Creates a new vector with values x and y, possibly with an inverted y. The
        ///	inversion of the y value provides a convienient option for handling
        ///	screen related vectors.
        /// </summary>
        /// <param name="x">Initial values for the vector</param>
        /// <param name="y">Initial values for the vector</param>
        /// <param name="invertY">Indicates if the y value should be inverted.</param>
        /// <returns>A new vector with values x and y</returns>
        public static Vector CreateVector(Single x, Single y, bool invertY)
        {
            Vector result = new Vector();
            result.X = x;
            result.Y = invertY?-y:y;
            result.W = 1;
            return result;
        }

        /// <summary>
        /// Creates a new vector with values x and y.
        /// </summary>
        /// <param name="x">Initial values for the vector</param>
        /// <param name="y">Initial values for the vector</param>
        /// <returns>A new vector with values x and y</returns>
        public static Vector CreateVector(Single x, Single y)
        {
            return CreateVector(x, y, false);  
        }
        /// <summary>
        /// Creates a new vector from the origin to the indicated point.
        /// </summary>
        /// <param name="p1">The point</param>
        /// <returns>A new vector from the origin (0,0) to pt (x,y)</returns>
        public static Vector PointToVector(Point2D p1)
        {
            return CreateVector(p1.X, p1.Y, false);
        }
        /// <summary>
        /// Gets the Vector from 2 Points
        /// </summary>
        /// <param name="p1">Point 1</param>
        /// <param name="p2">Point 2</param>
        /// <returns>Vector</returns>
        public static Vector VectorFromPoints(Point2D p1, Point2D p2)
        {
            return CreateVector(p2.X - p1.X, p2.Y - p1.Y, false);
        }
        /// <summary>
        /// VectorFromCenterSpriteToPoint creates and returns the vector from centre of the sprite to the point.
        /// </summary>
        /// <param name="sprt">Sprite</param>
        /// <param name="pnt">Point</param>
        /// <returns>Vector from Sprite to Point</returns>
        public static Vector VectorFromCenterSpriteToPoint(Sprite sprt, Point2D pnt)
        {
            return VectorFromPoints(Shapes.CenterPoint(sprt), pnt);
        }
        /// <summary>
        /// LineAsVector converts the specified line to a vector.
        /// </summary>
        /// <param name="line">Line to convert to Vector</param>
        /// <returns>Vector representation of the Line</returns>
        public static Vector LineAsVector(LineSegment line)
        {
            return VectorFromPoints(line.StartPoint, line.EndPoint);
        }

        /// <summary>
        /// Gets a Vector from a Angle
        /// </summary>
        /// <param name="angle">Angle</param>
        /// <param name="magnitude">Magnitude</param>
        /// <returns>Vector</returns>
        public static Vector GetVectorFromAngle(float angle, float magnitude)
        {
            return CreateVector(magnitude * Core.Cos(angle), magnitude * Core.Sin(angle));
        }
        /// <summary>
        /// VectorFromPointToRectangle creates and returns the vector from the point to the rectangle.
        /// </summary>
        /// <param name="x">X Coordinate of the point</param>
        /// <param name="y">Y Coordinate of the point</param>
        /// <param name="rectX">X Coordinate of the Rectangle</param>
        /// <param name="rectY">Y Coordinate of the Rectangle</param>
        /// <param name="rectWidth">Width of the Rectangle</param>
        /// <param name="rectHeight">Height of the Rectangle</param>
        /// <returns>Vector from Point to Rectangle</returns>
        public static Vector VectorFromPointToRectangle(float x, float y, float rectX, float rectY, int rectWidth, int rectHeight)
		{
            float px, py;

		    if (x < rectX) px = rectX;
		    else if (x > (rectX + rectWidth)) px = rectX + rectWidth;
		    else px = x;
			
		    if (y < rectY) py = rectY;
		    else if (y > (rectY + rectHeight)) py = rectY + rectHeight;
		    else py = y;
			
		    return CreateVector(px - x, py - y);
        }
        /// <summary>
        /// VectorFromPointToRectangle creates and returns the vector from the point to the rectangle.
        /// </summary>
        /// <param name="x">X Coordinate of the point to rectangle</param>
        /// <param name="y">Y Coordinate of the point to rectangle</param>
        /// <param name="rect">Rectangle to get the vector</param>
        /// <returns>Vector from point to rectangle</returns>
	    public static Vector VectorFromPointToRectangle(float x, float y, Rectangle rect)
        {
		    return VectorFromPointToRectangle(x, y, rect.X, rect.Y, rect.Width, rect.Height);
        }
	    /// <summary>
        /// VectorFromPointToRectangle creates and returns the vector from the point to the rectangle.
	    /// </summary>
	    /// <param name="pt">Point to get Vector</param>
	    /// <param name="rect">Rectangle to get Vector</param>
	    /// <returns>Vector from point to rectangle</returns>
	    public static Vector VectorFromPointToRectangle(Point2D pt, Rectangle rect)
        {
		    return VectorFromPointToRectangle(pt.X, pt.Y, rect.X, rect.Y, rect.Width, rect.Height);
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "HasBitmapCollidedWithRect")]
        private static extern bool DLL_HasBitmapCollidedWithRect(IntPtr bitmap, int x, int y, int width, int height);
        /// <summary>
        /// Returns true if the Bitmap has collided with the specified Rectangle
        /// </summary>
        /// <param name="bitmap">Bitmap</param>
        /// <param name="x">X Position of the Bitmap</param>
        /// <param name="y">Y Position of the Bitmap</param>
        /// <param name="rectX">X Position of the Rectangle</param>
        /// <param name="rectY">Y Position of the Rectangle</param>
        /// <param name="width">Width of the Rectangle</param>
        /// <param name="height">Height of the Rectangle</param>
        /// <returns>True if the Bitmap Collides with the Rectangle</returns>
        public static bool HasBitmapCollidedWithRect(Bitmap bitmap, int x, int y, int rectX, int rectY, int width, int height)
        {
            bool temp;

            try
            {
                temp = DLL_HasBitmapCollidedWithRect(bitmap.pointer, x, y, width, height);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }
        /// <summary>
        /// Returns true if the Bitmap has collided with the specified Rectangle
        /// </summary>
        /// <param name="bitmap">Bitmap</param>
        /// <param name="x">X Position of the Bitmap</param>
        /// <param name="y">Y Position of the Bitmap</param>
        /// <param name="rectangle">Rectangle</param>
        public static bool HasBitmapCollidedWithRect(Bitmap bitmap, int x, int y, Rectangle rectangle)
        {
            return HasBitmapCollidedWithRect(bitmap, x, y, rectangle.X, rectangle.Y, rectangle.Width, rectangle.Height);
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "IsSpriteOnScreenAt")]
        private static extern bool DLL_IsSpriteOnScreenAt(IntPtr sprite, int x, int y);
        /// <summary>
        /// Checks if the Sprite is on Screen at the given Coordinates
        /// </summary>
        /// <param name="sprite">Sprite</param>
        /// <param name="x">X Coordinate</param>
        /// <param name="y">Y Coordinate</param>
        /// <returns>True if the sprite is on screen at the coordinates</returns>
        public static bool IsSpriteOnScreenAt(Sprite sprite, int x, int y)
        {
            bool temp;
            try
            {
                temp = DLL_IsSpriteOnScreenAt(sprite.Pointer, x, y);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }
        /// <summary>
        /// Checks if the Sprite is on Screen at the given Coordinates
        /// </summary>
        /// <param name="sprite">Sprite</param>
        /// <param name="point">Coordinates</param>
        /// <returns>True if the Sprite is on Screen at the Coordinates</returns>
        public static bool IsSpriteOnScreenAt(Sprite sprite, Point2D point)
        {
            return IsSpriteOnScreenAt(sprite, (int)point.X, (int)point.Y);
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CircleHasCollidedWithLine")]
        private static extern bool DLL_CircleHasCollidedWithLine(IntPtr sprite, Point2D point);
        /// <summary>
        /// Checks if the Sprite has Collided with the Line
        /// </summary>
        /// <param name="sprite">Sprite</param>
        /// <param name="point">Point</param>
        /// <returns>True if sprite collides with the line</returns>
        public static bool CircleHasCollidedWithLine(Sprite sprite, Point2D point)
        {
            bool temp;
            try
            {
                temp = DLL_CircleHasCollidedWithLine(sprite.Pointer, point);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "VectorOutOfCircleFromPoint")]
        private static extern Vector DLL_VectorOutOfCircleFromPoint(Point2D pnt, Point2D center, float radius, Vector movement);
        /// <summary>
        /// VectorOutOfCircleFromPoint calculates and returns the vector required to push the point out of the circle. 
        /// You will need to specify the vector of the point when calling this routine.
        /// </summary>
        /// <param name="pnt">Point to calculate against</param>
        /// <param name="center">Center of the Circle</param>
        /// <param name="radius">Radius of the Cricle</param>
        /// <param name="movement">Movement of the point</param>
        /// <returns>Vector required to push the point out of the Circle</returns>
        public static Vector VectorOutOfCircleFromPoint(Point2D pnt, Point2D center, float radius, Vector movement)
        {
            Vector temp;
            try
            {
                temp = DLL_VectorOutOfCircleFromPoint(pnt, center, radius, movement);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "VectorOutOfCircleFromCircle")]
        private static extern Vector DLL_VectorOutOfCircleFromCircle(Point2D pnt, float radius1, Point2D center, float radius2, Vector movement);
        /// <summary>
        /// VectorOutOfCircleFromCircle calculates and returns the vector required to push the circle out of the another circle. 
        /// You will need to specify the vector of the moving circle when calling this routine.
        /// </summary>
        /// <param name="pnt">Center of the first Circle</param>
        /// <param name="radius1">Radius of the first Circle</param>
        /// <param name="center">Center of the second Circle</param>
        /// <param name="radius2">Radius of the second Circle</param>
        /// <param name="movement">Movement of the first Circle</param>
        /// <returns>Vector required to move the Circle outside of the second Circle</returns>
        public static Vector VectorOutOfCircleFromCircle(Point2D pnt, float radius1, Point2D center, float radius2, Vector movement)
        {
            Vector temp;
            try
            {
                temp = DLL_VectorOutOfCircleFromCircle(pnt, radius1, center, radius2, movement);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        /// <summary>
        /// Adds the Vector v1 and the Vector v2.
        /// </summary>
        /// <param name="v1">The vectors to work with</param>
        /// <param name="v2">The vectors to work with</param>
        /// <returns>v1 + v2</returns>
        public static Vector AddVectors(Vector v1, Vector v2)
        {
            Vector result = new Vector();
            result.X = v1.X + v2.X;
            result.Y = v1.Y + v2.Y;
            return result;
        }

        /// <summary>
        /// Subtracts the Vector v1 and the Vector v2.
        /// </summary>
        /// <param name="v1">The vectors to work with</param>
        /// <param name="v2">The vectors to work with</param>
        /// <returns>v1 - v2</returns>
        public static Vector SubtractVectors(Vector v1, Vector v2)
        {
            Vector result = new Vector();
            result.X = v1.X - v2.X;
            result.Y = v1.Y - v2.Y;
            return result;
        }

        /// <summary>
        /// Multiplies a Vector by a number
        /// </summary>
        /// <param name="v1">The vector to multiply</param>
        /// <param name="s1">The number to multiply the vector by</param>
        /// <returns>The multiplyed vector</returns>
        public static Vector MultiplyVector(Vector v1, Single s1)
        {
            v1.X *= s1;
            v1.Y *= s1;

            return v1;
        }

        /// <summary>
        /// The Angle between two vectors
        /// </summary>
        /// <param name="v1">The first Vector</param>
        /// <param name="v2">The Second Vector</param>
        /// <returns>The angle</returns>
        public static Single DotProduct(Vector v1, Vector v2)
        {
            return (v1.X * v2.X) + (v1.Y * v2.Y);
        }
        /// <summary>
        /// VectorNormal returns the normal of the specified vector.
        /// </summary>
        /// <param name="vect">Vector to get normal from</param>
        /// <returns>Normal of the Vector specified</returns>
        public static Vector VectorNormal(Vector vect)
        {	
		    float sqrY, sqrX;
		    sqrX = vect.X * vect.X;
		    sqrY = vect.Y * vect.Y;
		
            return CreateVector( -vect.Y / (float)Math.Sqrt(sqrY + sqrX), vect.X / (float)Math.Sqrt(sqrY + sqrX));
        }	
        /// <summary>
        /// LineNormal returns the normal of the specified line.
        /// </summary>
        /// <param name="line">Line to get the normal from</param>
        /// <returns>Normal of the Line specified</returns>
	    public static Vector LineNormal(LineSegment line)
	    {
		    return VectorNormal(LineAsVector(line));
	    }

        /// <summary>
        /// Inverts the vector v. Changes the direction of the vector's x and y.
        /// </summary>
        /// <param name="theVector">The vector to invert</param>
        /// <returns>A new inverted vector</returns>
        public static Vector InvertVector(Vector theVector)
        {
            Vector result = new Vector();
            result.X = -theVector.X;
            result.Y = -theVector.Y;
            return result;
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "LimitMagnitude")]
        private static extern Vector DLL_LimitMagnitude(Vector theVector, Single maxMagnitude);
        /// <summary>
        /// Limit the magnitude of a vector.
        /// </summary>
        /// <param name="theVector">The vector to limit</param>
        /// <param name="maxMagnitude">The maximum magnitude of the vector.</param>
        /// <returns>A new vector with the same direction as theVector,
        ///	with a maximum magnitude of maxMagnitude</returns>
        public static Vector LimitMagnitude(Vector theVector, Single maxMagnitude)
        {
            Vector temp;
            try
            {
                temp = DLL_LimitMagnitude(theVector, maxMagnitude);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetUnitVector")]
        private static extern Vector DLL_GetUnitVector(Vector theVector);
        /// <summary>
        /// Gets the unit vector of the passed in vector. The unit vector has a
        ///	magnitude of 1, resulting in a vector that indicates the direction of
        ///	the original vector.
        /// </summary>
        /// <param name="theVector">The vector to get the unit vector of</param>
        /// <returns>The unit vector from the passed in vector</returns>
        public static Vector GetUnitVector(Vector theVector)
        {
            try
            {
                Vector temp = DLL_GetUnitVector(theVector);
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }  
        }

        /// <summary>
        /// Indicates if the magnitude of the vector is 0.
        /// </summary>
        /// <param name="theVector">The vector to check</param>
        /// <returns>True if the vector has values 0, 0</returns>
        public static bool IsZeroVector(Vector theVector)
        {
            return theVector.X == 0 && theVector.Y == 0;
        }

        /// <summary>
        /// Returns the magnitude of a vector. The magnitude represents the length of
        ///	the vector.
        /// </summary>
        /// <param name="theVector">The vector to get the magnitude of</param>
        /// <returns>The magnitude of the vector</returns>
        public static Single Magnitude(Vector theVector)
        {
		    return (float)Math.Sqrt((theVector.X * theVector.X) + (theVector.Y * theVector.Y));
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CalculateAngle")]
        private static extern Single DLL_CalculateAngle(Single x1, Single y1, Single x2, Single y2);
        /// <summary>
        /// Gets the Angle between two points 
        /// </summary>
        /// <param name="x1">Position X 1</param>
        /// <param name="y1">Position Y 1</param>
        /// <param name="x2">Position X 2</param>
        /// <param name="y2">Position Y 2</param>
        /// <returns>Angle Between Points</returns>
        public static Single CalculateAngle(Single x1, Single y1, Single x2, Single y2)
        {
            try
            {
                Single temp = DLL_CalculateAngle(x1, y1, x2, y2);
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }  
        }
        /// <summary>
        /// Calculates the Angle between 2 Points
        /// </summary>
        /// <param name="p1">First Point to Calculate Angle</param>
        /// <param name="p2">Second Point to Calculate Angle</param>
        /// <returns>Angle Between the 2 Points</returns>
        public static float CalculateAngle(Point2D p1, Point2D p2)
        {
            return CalculateAngle(p1.X, p1.Y, p2.X, p2.Y);
        }

        /// <summary>
        /// Calculates the Angle between 2 Sprites
        /// </summary>
        /// <param name="sprite1">Sprite 1</param>
        /// <param name="sprite2">Sprite 2</param>
        /// <returns>Angle Between Sprites</returns>
        public static float CalculateAngle(Sprite sprite1, Sprite sprite2)
        {
            return CalculateAngle(Shapes.CenterPoint(sprite1), Shapes.CenterPoint(sprite2));
        }

        /// <summary>
        /// Calculates the Angle between 2 Vectors
        /// </summary>
        /// <param name="v1">Vector 1</param>
        /// <param name="v2">Vector 2</param>
        /// <returns>Angle between the 2 Vectors</returns>
        public static float CalculateAngleBetween(Vector v1, Vector v2)
        {
            float result, t1, t2;

            t1 = CalculateAngle(0, 0, v1.X, v1.Y);
		    t2 = CalculateAngle(0, 0, v2.X, v2.Y);
		

		    result = t2 - t1;

            if (result > 180) result = result - 360;
            else if (result <= -180) result = result + 360;

            return result;
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "TranslationMatrix")]
        private static extern IntPtr DLL_TranslationMatrix(Single dx, Single dy);
        /// <summary>
        /// TranslationMatric
        /// </summary>
        /// <param name="dx">Translation X</param>
        /// <param name="dy">Translation Y</param>
        /// <returns>TranslationMatric</returns>
        public static Matrix2D TranslationMatrix(Single dx, Single dy)
        {
            try
            {
                Matrix2D temp = new Matrix2D(DLL_TranslationMatrix(dx, dy));
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ScaleMatrix")]
        private static extern IntPtr DLL_ScaleMatrix(Single scale);
        /// <summary>
        /// ScaleMatrix acquires a new Matrix which can be used to scale Vectors and Matrices
        /// </summary>
        /// <param name="scale">Scale Factor</param>
        /// <returns>Scale Matrix</returns>
        public static Matrix2D ScaleMatrix(Single scale)
        {
            Matrix2D temp;

            try
            {
                temp = new Matrix2D(DLL_ScaleMatrix(scale));
            }
            catch (Exception e)
            {
                //if (Core.ExceptionOccured())
                throw new SwinGameException(e.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "RotationMatrix")]
        private static extern IntPtr DLL_RotationMatrix(Single deg);
        /// <summary>
        /// Rotation Matrix gets the Matrix that allows you to rotate Vectors and Matrices
        /// </summary>
        /// <param name="deg">Degrees</param>
        /// <returns>Rotation Matrix</returns>
        public static Matrix2D RotationMatrix(Single deg)
        {
            try
            {
                Matrix2D temp = new Matrix2D(DLL_RotationMatrix(deg));
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "MultiplyMatrix2D")]
        private static extern IntPtr DLL_Multiply(IntPtr m1, IntPtr m2);//const
        /// <summary>
        /// Multiplies two matrixs 
        /// </summary>
        /// <param name="m1">The first Matrix</param>
        /// <param name="m2">The second Matrix</param>
        /// <returns>The combined Matrixes</returns>
        public static Matrix2D Multiply(Matrix2D m1, Matrix2D m2)
        {
            Matrix2D temp;

            try
            {
                temp = new Matrix2D(DLL_Multiply(m1, m2));
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "MultiplyMatrix2DAndVector")]
        private static extern Vector DLL_Multiply(IntPtr m, Vector v);//const
        /// <summary>
        /// Multiplies 1 Vector and 1 Matrix2D
        /// </summary>
        /// <param name="m">The Matrix2D</param>
        /// <param name="v">The Vector</param>
        /// <returns>The resulting Matrix2D</returns>
        public static Vector Multiply(Matrix2D m, Vector v)
        {
            Vector temp;

            try
            {
                temp = DLL_Multiply(m, v);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }
        /// <summary>
        /// Calculates the Vector to get from the first Sprite to the second
        /// </summary>
        /// <param name="obj">Sprite to start the Vector from</param>
        /// <param name="dest">Sprite to end the Vector</param>
        /// <returns>Vector from Sprite 1 to Sprite 2</returns>
        public static Vector CalculateVectorFromTo(Sprite obj, Sprite dest)
        {
            return VectorFromPoints(Shapes.CenterPoint(obj), Shapes.CenterPoint(dest));
        }
        /// <summary>
        /// VectorIsWithinRect checks if the specified vector ends at the rectangle specified. 
        /// The routine assumes that the vector starts from 0, 0.
        /// </summary>
        /// <param name="v">Vector to check if inside Rectangle</param>
        /// <param name="x">X Coordinate of the Rectangle</param>
        /// <param name="y">Y Coordinate of the Rectangle</param>
        /// <param name="width">Width of the Rectangle</param>
        /// <param name="height">Height of the Rectangle</param>
        /// <returns>True if the Vector ends at the rectangle</returns>
        public static bool VectorIsWithinRect(Vector v, float x, float y, int width, int height)
        {
            if (v.X < x) return false;
            else if (v.X > x + width) return false;
            else if (v.Y < y) return false;
            else if (v.Y > y + height) return false;
            else return true;
        }
        /// <summary>
        /// VectorIsWithinRect checks if the specified vector ends at the rectangle specified. 
        /// The routine assumes that the vector starts from 0, 0.
        /// </summary>
        /// <param name="v">Vector to check if inside Rectangle</param>
        /// <param name="rectangle">Rectangle</param>
        /// <returns>True if the Vector ends at the rectangle</returns>
        public static bool VectorIsWithinRect(Vector v, Rectangle rectangle)
        {
            return VectorIsWithinRect(v, rectangle.X, rectangle.Y, rectangle.Width, rectangle.Height);
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "VectorOutOfRectFromPoint")]
        private static extern Vector DLL_VectorOutOfRectFromPoint(Point2D pnt, SGSDKRectangle rect, Vector movement);
        /// <summary>
        /// Gets the Vector out of a Rectangle from a Point
        /// </summary>
        /// <param name="pnt">Point</param>
        /// <param name="rect">Rectangle</param>
        /// <param name="movement">Movement of the Point</param>
        /// <returns>The Vector out of the Rectangle</returns>
        public static Vector VectorOutOfRectFromPoint(Point2D pnt, Rectangle rect, Vector movement)
        {
            Vector temp;
            try
            {
                temp = DLL_VectorOutOfRectFromPoint(pnt, Shapes.ToSGSDKRect(rect), movement);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }

            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "VectorOutOfRectFromRect")]
        private static extern Vector DLL_VectorOutOfRectFromRect(SGSDKRectangle srcRect, SGSDKRectangle targetRect, Vector movement);
        /// <summary>
        /// Gets the Vector out of a Rectangle from a Rectangle
        /// </summary>
        /// <param name="srcRect">The Rectangle to get the Vector to move</param>
        /// <param name="targetRect">The Rectangle to get out of</param>
        /// <param name="movement">Movement of the first Rectangle</param>
        /// <returns>The Vector out of the Rectangle</returns>
        public static Vector VectorOutOfRectFromRect(Rectangle srcRect, Rectangle targetRect, Vector movement)
        {
            Vector temp;
            try
            {
                temp = DLL_VectorOutOfRectFromRect(Shapes.ToSGSDKRect(srcRect), Shapes.ToSGSDKRect(targetRect), movement);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }

            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "VectorCollision")]
        private static extern void DLL_VectorCollision(IntPtr spr1, IntPtr spr2);
        /// <summary>
        /// Vector Collisions alters the vectors of two Sprites depending on
        /// the movement and mass of each sprute.
        /// </summary>
        /// <param name="sprite1">Sprite 1</param>
        /// <param name="sprite2">Sprite 2</param>
        public static void VectorCollision(Sprite sprite1, Sprite sprite2)
        {
            try
            {
                DLL_VectorCollision(sprite1.Pointer, sprite2.Pointer);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
  
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CircleCollisionWithLine")]
        private static extern void DLL_CircleCollisionWithLine(IntPtr spr1, LineSegment line);
        /// <summary>
        /// CircleCollisionWithLine calculates the after-effect from colliding with the line. 
        /// It will directly modify the value of the specified sprite's vector.
        /// </summary>
        /// <param name="sprt">Sprite</param>
        /// <param name="line">Line Segment</param>
        public static void CircleCollisionWithLine(Sprite sprt, LineSegment line)
        {
            try
            {
                DLL_CircleCollisionWithLine(sprt.Pointer, line);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }

            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CircularCollision")]
        private static extern void DLL_CircularCollision(IntPtr spr1, IntPtr spr2);
        /// <summary>
        /// CircularCollision is used to calculate an effect from a collision from two circular sprites. 
        /// It takes their masses and their movement vectors in consideration. 
        /// The routine will process the energy transfer as well.
        /// </summary>
        /// <param name="sprt1">Sprite 1</param>
        /// <param name="sprt2">Sprite 2</param>
        public static void CircularCollision(Sprite sprt1, Sprite sprt2)
        {
            try
            {
                DLL_CircularCollision(sprt1.Pointer, sprt2.Pointer);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }

            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
        }
    }
}
