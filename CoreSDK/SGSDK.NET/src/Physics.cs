using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Drawing;
using System.IO;

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
        private static extern void FreeMaxtrix2D(IntPtr maxtrix2d);
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetMatrix2DElement")]
        private static extern Single GetMaxtrix2DElement(IntPtr maxtrix2d, int r, int c);
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SetMatrix2DElement")]
        private static extern void SetMaxtrix2DElement(IntPtr maxtrix2d, int r, int c, Single val); 

        internal IntPtr Pointer;
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
            GC.SuppressFinalize(this);
        }

        private void Dispose(bool disposing)
        {
            if (!this.disposed)
            {
                if (disposing)
                {
                }
                FreeMaxtrix2D(Pointer);
                Pointer = IntPtr.Zero;

                disposed = true;
            }
        }

        /// <summary>
        /// Garbage Collection Disposal
        /// </summary>
        ~Matrix2D()
        {
            Dispose(false);
        }

        /*

        public static Matrix2D IdentityMatrix
        {
            get
            {
                Matrix2D result;
                result._Matrix2D = new float[3, 3];
                result[0, 0] = 1;
                result[1, 1] = 1;

                return result;
            }
        }
         */
    }

 


    /// <summary>
    /// SGSDK.NET's Physics Class
    /// </summary>
    public class Physics
    {
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ExceptionOccured")]
        private static extern bool ExceptionOccured();
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetExceptionMessage")]
        private static extern String GetExceptionMessage();

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
            try
            {
                bool temp = DLL_HasSpriteCollidedX(theSprite.Pointer, x, range);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
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
            try
            {
                bool temp = DLL_HasSpriteCollidedY(theSprite.Pointer, y, range);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
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
            try
            {
                bool temp = DLL_HasSpriteCollidedWithRect(theSprite.Pointer, x, y, width, height);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }
        /// <summary>
        /// Determined if a sprite has collided with a given rectangle. The rectangles
        ///	coordinates are expressed in "world" coordinates.
        /// </summary>
        /// <param name="theSprite">The sprite to check</param>
        /// <param name="x">The x location of the rectangle</param>
        /// <param name="y">The y location of the rectangle</param>
        /// <param name="width">The width of the rectangle</param>
        /// <param name="height">The height of the rectangle</param>
        /// <param name="vwPrtX">The x offset of the screen's portal</param>
        /// <param name="vwPrtY">The y offset of the screen's portal</param>
        /// <returns>True if the sprite collides with the rectangle</returns>
        public static bool HasSpriteCollidedWithRect(Sprite theSprite, Single x, Single y, int width, int height, int vwPrtX, int vwPrtY)
        {
            try
            {
                bool temp = DLL_HasSpriteCollidedWithRect(theSprite.Pointer, x + vwPrtX, y + vwPrtY, width, height);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "HaveSpritesCollided")]
        private static extern bool DLL_HaveSpritesCollided(IntPtr sprite1, IntPtr sprite2);
        /// <summary>
        /// Determines if two sprites have collided.
        /// </summary>
        /// <param name="sprite1">The first sprite to check.</param>
        /// <param name="sprite2">The second sprite to check.</param>
        /// <returns>True if the sprites have collided.</returns>
        public static bool HaveSpritesCollided(Sprite sprite1, Sprite sprite2)
        {
            try
            {
                bool temp = DLL_HaveSpritesCollided(sprite1.Pointer, sprite2.Pointer);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "HasSpriteCollidedWithBitmap")]
        private static extern bool DLL_HasSpriteCollidedWithBitmap();
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CollisionWithinBitmapImages")]
        private static extern bool DLL_CollisionWithinBitmapImages(IntPtr image1, int x1, int y1, bool bounded1, IntPtr image2, int x2, int y2, bool bounded2);
        /// <summary>
        ///  Performs a collision detection within two bitmaps at the given x, y
        ///	locations. The bounded values indicate if each bitmap should use per
        ///	pixel collision detection or a bounded collision detection. This version
        ///	uses pixel based checking at all times.
        ///
        ///	When both bitmaps are using bounded collision the routine checks to see
        ///	if the bitmap rectangles intersect. If one is bounded and the other is
        ///	pixel based the routine checks to see if a non-transparent pixel in the
        ///	pixel based image intersects with the bounds of the bounded image. If
        ///	both are pixel based, the routine checks to see if two non-transparent
        ///	pixels collide.
        ///
        ///	Note: Bitmaps do not need to actually be drawn on the screen.
        /// </summary>
        /// <param name="image1">The bitmap image to check for collision</param>
        /// <param name="x1">The x location of image 1</param>
        /// <param name="y1">The y location of image 1</param>
        /// <param name="bounded1">Indicates if image1 should use bounded collision</param>
        /// <param name="image2">The bitmap image to check for collision</param>
        /// <param name="x2">The x location of image 2</param>
        /// <param name="y2">The y location of image 2</param>
        /// <param name="bounded2">Indicates if image2 should use bounded collision</param>
        /// <returns>True if the bitmaps collide.</returns>
        public static bool CollisionWithinBitmapImages(Bitmap image1, int x1, int y1, bool bounded1, Bitmap image2, int x2, int y2, bool bounded2)
        {
            try
            {
                bool temp = DLL_CollisionWithinBitmapImages(image1.pointer, x1, y1, bounded1, image2.pointer, x2, y2, bounded2);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }
        /// <summary>
        /// Performs a collision detection within two bitmaps at the given x, y
        ///	locations using per pixel collision detection. This checks to see if
        ///	two non-transparent pixels collide.
        /// </summary>
        /// <param name="image1">The bitmap image to check for collision</param>
        /// <param name="x1">The x location of image 1</param>
        /// <param name="y1">The y location of image 1</param>
        /// <param name="image2">The bitmap image to check for collision</param>
        /// <param name="x2">The x location of image 2</param>
        /// <param name="y2">The y location of image 2</param>
        /// <returns>True if the bitmaps collide.</returns>
        public static bool CollisionWithinBitmapImages(Bitmap image1, int x1, int y1, Bitmap image2, int x2, int y2)
        {
            try
            {
                bool temp = DLL_CollisionWithinBitmapImages(image1.pointer, x1, y1, false, image2.pointer, x2, y2, false);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }
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
        public static bool HasSpriteCollidedWithBitmap(Sprite theSprite, Bitmap theBitmap, int x, int y, bool bounded)
        {
            try
            {
                bool temp = CollisionWithinBitmapImages(theSprite[theSprite.CurrentFrame], (int)theSprite.xPos, (int)theSprite.yPos, !theSprite.UsePixelCollision, theBitmap, x, y, bounded);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }
        /// <summary>
        /// Determines if a sprite has collided with a bitmap. The x and y values
        ///	are expressed in "screen" coordinates, with vwPrtX and vwPrtY storing
        ///	the offset from world to screen coordinates.
        /// </summary>
        /// <param name="theSprite">The sprite to check for collision</param>
        /// <param name="theBitmap">The bitmap image to check for collision</param>
        /// <param name="x">The x location of the bitmap</param>
        /// <param name="y">The y location of the bitmap</param>
        /// <param name="bounded">Indicates if bitmap should use bounded collision</param>
        /// <param name="vwPrtX">The x offset of the screen's portal</param>
        /// <param name="vwPrtY">The y offset of the screen's portal</param>
        /// <returns> True if the bitmap has collided with the sprite.</returns>
        public static bool HasSpriteCollidedWithBitmap(Sprite theSprite, Bitmap theBitmap, int x, int y, bool bounded, int vwPrtX, int vwPrtY)
        {
            try
            {
                bool temp = CollisionWithinBitmapImages(theSprite[theSprite.CurrentFrame], (int)theSprite.xPos, (int)theSprite.yPos, !theSprite.UsePixelCollision, theBitmap, x + vwPrtX, y + vwPrtY, bounded);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }
        /// <summary>
        /// Determines if a sprite has collided with a bitmap. The x and y values
        ///	are expressed in "world" coordinates.
        /// </summary>
        /// <param name="theSprite">The sprite to check for collision</param>
        /// <param name="theBitmap">The bitmap image to check for collision</param>
        /// <param name="x">The x location of the bitmap</param>
        /// <param name="y">The y location of the bitmap</param>
        /// <returns> True if the bitmap has collided with the sprite.</returns>
        public static bool HasSpriteCollidedWithBitmap(Sprite theSprite, Bitmap theBitmap, int x, int y)
        {
            try
            {
                bool temp = CollisionWithinBitmapImages(theSprite[theSprite.CurrentFrame], (int)theSprite.xPos, (int)theSprite.yPos, !theSprite.UsePixelCollision, theBitmap, x, y, true);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }
        /// <summary>
        /// Determines if a sprite has collided with a bitmap. The x and y values
        ///	are expressed in "screen" coordinates, with vwPrtX and vwPrtY storing
        ///	the offset from world to screen coordinates.
        /// </summary>
        /// <param name="theSprite">The sprite to check for collision</param>
        /// <param name="theBitmap">The bitmap image to check for collision</param>
        /// <param name="x">The x location of the bitmap</param>
        /// <param name="y">The y location of the bitmap</param>
        /// <param name="vwPrtX">The x offset of the screen's portal</param>
        /// <param name="vwPrtY">The y offset of the screen's portal</param>
        /// <returns> True if the bitmap has collided with the sprite.</returns>
        public static bool HasSpriteCollidedWithBitmap(Sprite theSprite, Bitmap theBitmap, int x, int y, int vwPrtX, int vwPrtY)
        {
            try
            {
                bool temp = CollisionWithinBitmapImages(theSprite[theSprite.CurrentFrame], (int)theSprite.xPos, (int)theSprite.yPos, !theSprite.UsePixelCollision, theBitmap, x + vwPrtX, y + vwPrtY, true);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }


        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "HaveBitmapsCollided")]
        private static extern bool DLL_HaveBitmapsCollided(IntPtr image1, int x1, int y1, bool bounded1, IntPtr image2, int  x2, int y2, bool bounded2);
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
            try
            {
                bool temp = DLL_HaveBitmapsCollided(image1.pointer, x1, y1, bounded1, image2.pointer, x2, y2, bounded2);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }
        /// <summary>
        /// Checks to see if two bitmaps have collided, this performs a bounded check
        ///	then, if required, it performs a per pixel check on the colliding region.
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
            try
            {
                bool temp = DLL_HaveBitmapsCollided(image1.pointer, x1, y1, false, image2.pointer, x2, y2, false);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", EntryPoint = "CreateVector")]
        private static extern Vector DLL_CreateVector(Single x, Single y, bool invertY);
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
            try
            {
                Vector temp = DLL_CreateVector(x, y, invertY);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        /// <summary>
        /// Creates a new vector with values x and y.
        /// </summary>
        /// <param name="x">Initial values for the vector</param>
        /// <param name="y">Initial values for the vector</param>
        /// <returns>A new vector with values x and y</returns>
        public static Vector CreateVector(Single x, Single y)
        {
            try
            {
                Vector temp = DLL_CreateVector(x, y, false);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "AddVectors")]
        private static extern Vector DLL_AddVectors(Vector v1, Vector v2);
        /// <summary>
        /// Adds the Vector v1 and the Vector v2.
        /// </summary>
        /// <param name="v1">The vectors to work with</param>
        /// <param name="v2">The vectors to work with</param>
        /// <returns>v1 + v2</returns>
        public static Vector AddVectors(Vector v1, Vector v2)
        {
            try
            {
                Vector temp = DLL_AddVectors(v1, v2);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SubtractVectors")]
        private static extern Vector DLL_SubtractVectors(Vector v1, Vector v2);
        /// <summary>
        /// Subtracts the Vector v1 and the Vector v2.
        /// </summary>
        /// <param name="v1">The vectors to work with</param>
        /// <param name="v2">The vectors to work with</param>
        /// <returns>v1 - v2</returns>
        public static Vector SubtractVectors(Vector v1, Vector v2)
        {
            try
            {
                Vector temp = DLL_SubtractVectors(v1, v2);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "InvertVector")]
        private static extern Vector DLL_InvertVector(Vector theVector);
        /// <summary>
        /// Inverts the vector v. Changes the direction of the vector's x and y.
        /// </summary>
        /// <param name="theVector">The vector to invert</param>
        /// <returns>A new inverted vector</returns>
        public static Vector InvertVector(Vector theVector)
        {
            try
            {
                Vector temp = DLL_InvertVector(theVector);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ChopVector")]
        private static extern Vector DLL_ChopVector(Vector theVector, int minX, int maxX, int minY, int maxY);
        /// <summary>
        /// Limits the vector within the range min-max of X-Y. AVOID use... use
        ///  LimitMagnitude
        /// </summary>
        /// <param name="theVector">The vector to limit</param>
        /// <param name="minX">The min range of the vector's x</param>
        /// <param name="maxX">The max range of the vector's x</param>
        /// <param name="minY">The min range of the vector's y</param>
        /// <param name="maxY">The max range of the vector's y</param>
        /// <returns>A new vector limited within that range</returns>
        public static Vector ChopVector(Vector theVector, int minX, int maxX, int minY, int maxY)
        {
            try
            {
                Vector temp = DLL_ChopVector(theVector, minX, maxX, minY, maxY);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "LimitVector")]
        private static extern Vector DLL_LimitVector(Vector theVector, Single maxMagnitude);
        /// <summary>
        /// Limit the magnitude of a vector.
        /// </summary>
        /// <param name="theVector">The vector to limit</param>
        /// <param name="maxMagnitude">The maximum magnitude of the vector.</param>
        /// <returns>A new vector with the same direction as theVector,
        ///	with a maximum magnitude of maxMagnitude</returns>
        public static Vector LimitVector(Vector theVector, Single maxMagnitude)
        {
            try
            {
                Vector temp = DLL_LimitVector(theVector, maxMagnitude);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
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
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "IsZeroVector")]
        private static extern Vector DLL_IsZeroVector(Vector theVector);
        /// <summary>
        /// Indicates if the magnitude of the vector is 0.
        /// </summary>
        /// <param name="theVector">The vector to check</param>
        /// <returns>True if the vector has values 0, 0</returns>
        public static Vector IsZeroVector(Vector theVector)
        {
            try
            {
                Vector temp = DLL_IsZeroVector(theVector);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "Magnitude")]
        private static extern Single DLL_Magnitude(Vector theVector);
        /// <summary>
        /// Returns the magnitude of a vector. The magnitude represents the length of
        ///	the vector.
        /// </summary>
        /// <param name="theVector">The vector to get the magnitude of</param>
        /// <returns>The magnitude of the vector</returns>
        public static Single Magnitude(Vector theVector)
        {
            try
            {
                Single temp = DLL_Magnitude(theVector);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DotProduct")]
        private static extern Single DLL_DotProduct(Vector v1, Vector v2);
        /// <summary>
        /// The Angle between two vectors
        /// </summary>
        /// <param name="v1">The first Vector</param>
        /// <param name="v2">The Second Vector</param>
        /// <returns>The angle</returns>
        public static Single DotProduct(Vector v1, Vector v2)
        {
            try
            {
                Single temp = DLL_DotProduct(v1, v2);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "MultiplyVector")]
        private static extern Vector DLL_MultiplyVector(Vector v1, Single s1);
        /// <summary>
        /// Multiplies a Vector by a number
        /// </summary>
        /// <param name="v1">The vector to multiply</param>
        /// <param name="s1">The number to multiply the vector by</param>
        /// <returns>The multiplyed vector</returns>
        public static Vector MultiplyVector(Vector v1, Single s1)
        {
            try
            {
                Vector temp = DLL_MultiplyVector(v1, s1);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CalculateAngleNumber")]
        private static extern Single DLL_CalculateAngleNumber(Single x1, Single y1, Single x2, Single y2);
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
                Single temp = DLL_CalculateAngleNumber(x1, y1, x2, y2);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CalculateAngleSprite")]
        private static extern Single DLL_CalculateAngleSprite(IntPtr sprite1, IntPtr sprite2);
        /// <summary>
        /// Calculates the Angle between 2 Sprites
        /// </summary>
        /// <param name="sprite1">Sprite 1</param>
        /// <param name="sprite2">Sprite 2</param>
        /// <returns>Angle Between Sprites</returns>
        public static Single CalculateAngle(Sprite sprite1, Sprite sprite2)
        {
            try
            {
                Single temp = DLL_CalculateAngleSprite(sprite1.Pointer, sprite2.Pointer);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "TranslationMatrix")]
        private static extern IntPtr DLL_TranslationMatric(Single dx, Single dy);
        /// <summary>
        /// TranslationMatric
        /// </summary>
        /// <param name="dx">Translation X</param>
        /// <param name="dy">Translation Y</param>
        /// <returns>TranslationMatric</returns>
        public static Matrix2D TranslationMatric(Single dx, Single dy)
        {
            try
            {
                Matrix2D temp = new Matrix2D();
                temp.Pointer = DLL_TranslationMatric(dx, dy);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ScaleMatrix")]
        private static extern IntPtr DLL_ScaleMatrix(Single scale);
        /// <summary>
        /// ScaleMatrix
        /// </summary>
        /// <param name="scale">Scale Factor</param>
        /// <returns>Scale Matrix</returns>
        public static Matrix2D ScaleMatrix(Single scale)
        {
            try
            {
                Matrix2D temp = new Matrix2D();
                temp.Pointer = DLL_ScaleMatrix(scale);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "RotationMatrix")]
        private static extern IntPtr DLL_RotationMatrix(Single deg);
        /// <summary>
        /// Rotation Matrix
        /// </summary>
        /// <param name="deg">Degrees</param>
        /// <returns>Rotation Matrix</returns>
        public static Matrix2D RotationMatrix(Single deg)
        {
            try
            {
                Matrix2D temp = new Matrix2D();
                temp.Pointer = DLL_RotationMatrix(deg);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
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
            try
            {
                Matrix2D temp = new Matrix2D();
                temp.Pointer = DLL_Multiply(m1.Pointer, m2.Pointer);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
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
            try
            {
                Vector temp = DLL_Multiply(m.Pointer, v);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
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
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetVectorFromAngle")]
        private static extern Vector DLL_GetVectorFromAngle(float angle, float magnitude);
        /// <summary>
        /// Gets a Vector from a Angle
        /// </summary>
        /// <param name="angle">Angle</param>
        /// <param name="magnitude">Magnitude</param>
        /// <returns>Vector</returns>
        public static Vector GetVectorFromAngle(float angle, float magnitude)
        {
            try
            {
                Vector temp = DLL_GetVectorFromAngle(angle, magnitude);
                if (ExceptionOccured())
                {
                    throw new SwinGameException(GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (ExceptionOccured())
                throw new SwinGameException(GetExceptionMessage());
            }  
        }
    }
}
