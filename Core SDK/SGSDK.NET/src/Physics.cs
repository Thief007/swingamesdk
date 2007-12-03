using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
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
        CollisionRangeEquals = 0,
        CollisionRangeGreaterThan = 1,
        CollisionRangeLessThan = 2
    }

    /// <summary>
    /// This record is used to represent transformations that can be
    /// used to apply these changes to vectors.
    /// </summary>
    public struct Matrix2D
    {
        internal float[,] _Matrix2D;

        public float this[int r, int c]
        {
            get
            {
                if (_Matrix2D == null) _Matrix2D = new float[3, 3];
                return _Matrix2D[r,c];
            }
            set 
            {
                if (_Matrix2D == null) _Matrix2D = new float[3, 3];
                _Matrix2D[r,c] = value;
            }
        }

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
    }

    public class Physics
    {
        [DllImport("lib/SGSDk.dll", EntryPoint="HasSpriteCollidedX")]
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
            return DLL_HasSpriteCollidedX(theSprite.Pointer, x, range);
        }

        [DllImport("SGSDK.dll", EntryPoint="HasSpriteCollidedY")]
        private static extern bool DLL_HasSpriteCollidedY(IntPtr theSprite, int y ,CollisionDetectionRange range);
        /// <summary>
        /// Determines if a sprite has collided with a given y position.
        /// </summary>
        /// <param name="theSprite">The sprite to check</param>
        /// <param name="x">The y location to check collision with</param>
        /// <param name="range">The kind of check to perform less, larger or equal.</param>
        /// <returns>True if the sprite is within the range requested</returns>
        public static bool HasSpriteCollidedY(Sprite theSprite, int y, CollisionDetectionRange range)
        {
            return DLL_HasSpriteCollidedY(theSprite.Pointer, y, range);
        }

        [DllImport("SGSDK.dll", EntryPoint = "HasSpriteCollidedWithRect")]
        private static extern bool DLL_HasSpriteCollidedWithRect(IntPtr theSprite, double x, double y, int width, int height);
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
        public static bool HasSpriteCollidedWithRect(Sprite theSprite, double x, double y, int width, int height)
        {
            return DLL_HasSpriteCollidedWithRect(theSprite.Pointer, x, y, width, height);
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
        public static bool HasSpriteCollidedWithRect(Sprite theSprite, double x, double y, int width, int height, int vwPrtX, int vwPrtY)
        {
            return DLL_HasSpriteCollidedWithRect(theSprite.Pointer, x + vwPrtX, y + vwPrtY, width, height);
        }

        [DllImport("SGSDK.dll", EntryPoint = "HaveSpritesCollided")]
        private static extern bool DLL_HaveSpritesCollided(IntPtr sprite1, IntPtr sprite2);
        /// <summary>
        /// Determines if two sprites have collided.
        /// </summary>
        /// <param name="sprite1">The first sprite to check.</param>
        /// <param name="sprite2">The second sprite to check.</param>
        /// <returns>True if the sprites have collided.</returns>
        public static bool HaveSpritesCollided(Sprite sprite1, Sprite sprite2)
        {
            return DLL_HaveSpritesCollided(sprite1.Pointer, sprite2.Pointer);
        }

        [DllImport("SGSDK.dll", EntryPoint = "HasSpriteCollidedWithBitmap")]
        private static extern bool DLL_HasSpriteCollidedWithBitmap();///to do!!
                                                                    ///
        [DllImport("SGSDK.dll", EntryPoint = "CollisionWithinBitmapImages")]
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
            return DLL_CollisionWithinBitmapImages(image1.pointer, x1, y1, bounded1, image2.pointer, x2, y2, bounded2);
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
            return DLL_CollisionWithinBitmapImages(image1.pointer, x1, y1, false, image2.pointer, x2, y2, false);
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
            return CollisionWithinBitmapImages(theSprite[theSprite.CurrentFrame], (int)theSprite.X, (int)theSprite.Y,	 !theSprite.UsePixelCollision, theBitmap,	x, y, bounded);
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
        /// <returns> rue if the bitmap has collided with the sprite.</returns>
        public static bool HasSpriteCollidedWithBitmap(Sprite theSprite, Bitmap theBitmap, int x, int y, bool bounded, int vwPrtX, int vwPrtY)
        {
            return CollisionWithinBitmapImages(theSprite[theSprite.CurrentFrame], (int)theSprite.X, (int)theSprite.Y, !theSprite.UsePixelCollision, theBitmap, x + vwPrtX, y + vwPrtY, bounded);
        }
        /// <summary>
        /// Determines if a sprite has collided with a bitmap. The x and y values
        ///	are expressed in "world" coordinates.
        /// </summary>
        /// <param name="theSprite">The sprite to check for collision</param>
        /// <param name="theBitmap">The bitmap image to check for collision</param>
        /// <param name="x">The x location of the bitmap</param>
        /// <param name="y">The y location of the bitmap</param>
        /// <returns> rue if the bitmap has collided with the sprite.</returns>
        public static bool HasSpriteCollidedWithBitmap(Sprite theSprite, Bitmap theBitmap, int x, int y)
        {
            return CollisionWithinBitmapImages(theSprite[theSprite.CurrentFrame], (int)theSprite.X, (int)theSprite.Y, !theSprite.UsePixelCollision, theBitmap, x , y , true);
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
        /// <returns> rue if the bitmap has collided with the sprite.</returns>
        public static bool HasSpriteCollidedWithBitmap(Sprite theSprite, Bitmap theBitmap, int x, int y, int vwPrtX, int vwPrtY)
        {
            return CollisionWithinBitmapImages(theSprite[theSprite.CurrentFrame], (int)theSprite.X, (int)theSprite.Y, !theSprite.UsePixelCollision, theBitmap, x + vwPrtX, y + vwPrtY, true);
        }


        [DllImport("SGSDK.dll", EntryPoint = "HaveBitmapsCollided")]
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
            return DLL_HaveBitmapsCollided(image1.pointer, x1, y1, bounded1, image2.pointer, x2, y2, bounded2);
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
            return DLL_HaveBitmapsCollided(image1.pointer, x1, y1, false, image2.pointer, x2, y2, false);
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
        [DllImport("SGSDK.dll", EntryPoint = "CreateVector")]
        public static extern Vector CreateVector(double x, double y, bool invertY);
        /// <summary>
        /// Creates a new vector with values x and y.
        /// </summary>
        /// <param name="x">Initial values for the vector</param>
        /// <param name="y">Initial values for the vector</param>
        /// <returns>A new vector with values x and y</returns>
        public static Vector CreateVector(double x, double y)
        {
            return CreateVector(x, y, false);
        }

        /// <summary>
        /// Adds the Vector v1 and the Vector v2.
        /// </summary>
        /// <param name="v1">The vectors to work with</param>
        /// <param name="v2">The vectors to work with</param>
        /// <returns>v1 + v2</returns>
        [DllImport("SGSDK.dll", EntryPoint = "AddVectors")]
        public static extern Vector AddVectors(Vector v1, Vector v2);

        /// <summary>
        /// Subtracts the Vector v1 and the Vector v2.
        /// </summary>
        /// <param name="v1">The vectors to work with</param>
        /// <param name="v2">The vectors to work with</param>
        /// <returns>v1 - v2</returns>
        [DllImport("SGSDK.dll", EntryPoint = "SubtractVectors")]
        public static extern Vector SubtractVectors(Vector v1, Vector v2);

        /// <summary>
        /// Inverts the vector v. Changes the direction of the vector's x and y.
        /// </summary>
        /// <param name="theVector">The vector to invert</param>
        /// <returns>A new inverted vector</returns>
        [DllImport("SGSDK.dll", EntryPoint = "InvertVector")]
        public static extern Vector InvertVector(Vector theVector);

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
        [DllImport("SGSDK.dll", EntryPoint = "ChopVector")]
        public static extern Vector ChopVector(Vector theVector, int minX, int maxX, int minY, int maxY);

        /// <summary>
        /// Limit the magnitude of a vector.
        /// </summary>
        /// <param name="theVector">The vector to limit</param>
        /// <param name="maxMagnitude">The maximum magnitude of the vector.</param>
        /// <returns>A new vector with the same direction as theVector,
        ///	with a maximum magnitude of maxMagnitude</returns>
        [DllImport("SGSDK.dll", EntryPoint = "LimitVector")]
        public static extern Vector LimitVector(Vector theVector, double maxMagnitude);

        /// <summary>
        /// Gets the unit vector of the passed in vector. The unit vector has a
        ///	magnitude of 1, resulting in a vector that indicates the direction of
        ///	the original vector.
        /// </summary>
        /// <param name="theVector">The vector to get the unit vector of</param>
        /// <returns>The unit vector from the passed in vector</returns>
        [DllImport("SGSDK.dll", EntryPoint = "GetUnitVector")]
        public static extern Vector GetUnitVector(Vector theVector);

        /// <summary>
        /// Indicates if the magnitude of the vector is 0.
        /// </summary>
        /// <param name="theVector">The vector to check</param>
        /// <returns>True if the vector has values 0, 0</returns>
        [DllImport("SGSDK.dll", EntryPoint = "IsZeroVector")]
        public static extern Vector IsZeroVector(Vector theVector);

        /// <summary>
        /// Returns the magnitude of a vector. The magnitude represents the length of
        ///	the vector.
        /// </summary>
        /// <param name="theVector">The vector to get the magnitude of</param>
        /// <returns>The magnitude of the vector</returns>
        [DllImport("SGSDK.dll", EntryPoint = "GetVectorMagnitude")]
        public static extern double GetVectorMagnitude(Vector theVector);

        /// <summary>
        /// The Angle between two vectors
        /// </summary>
        /// <param name="v1">The first Vector</param>
        /// <param name="v2">The Second Vector</param>
        /// <returns>The angle</returns>
        [DllImport("SGSDK.dll", EntryPoint = "DotProduct")]
        public static extern double DotProduct(Vector v1, Vector v2);

        /// <summary>
        /// Multiplies a Vector by a number
        /// </summary>
        /// <param name="v1">The vector to multiply</param>
        /// <param name="s1">The number to multiply the vector by</param>
        /// <returns>The multiplyed vector</returns>
        [DllImport("SGSDK.dll", EntryPoint = "MultiplyVector")]
        public static extern Vector MultiplyVector(Vector v1, double s1);

        /// <summary>
        /// Gets the Angle between two points 
        /// </summary>
        /// <param name="x1"></param>
        /// <param name="y1"></param>
        /// <param name="x2"></param>
        /// <param name="y2"></param>
        /// <returns></returns>
        [DllImport("SGSDK.dll", EntryPoint = "CalculateAngleNumber")]
        public static extern double CalculateAngle(double x1, double y1, double x2, double y2);

        [DllImport("SGSDK.dll", EntryPoint = "CalculateAngleSprite")]
        private static extern double CalculateAngle(IntPtr sprite1, IntPtr sprite2);
        /// <summary>
        /// 
        /// </summary>
        /// <param name="sprite1"></param>
        /// <param name="sprite2"></param>
        /// <returns></returns>
        public static double CalculateAngle(Sprite sprite1, Sprite sprite2)
        {
            return CalculateAngle(sprite1.Pointer, sprite2.Pointer);
        }

        [DllImport("SGSDK.dll", EntryPoint = "TranslationMatrix")]
        public static extern Matrix2D TranslationMatric(double dx, double dy);

        [DllImport("SGSDK.dll", EntryPoint = "ScaleMatrix")]
        public static extern Matrix2D ScaleMatrix(double scale);

        [DllImport("SGSDK.dll", EntryPoint = "RotationMatrix")]
        public static extern Matrix2D RotationMatrix(double deg);

        /// <summary>
        /// Multiplies two matrixs 
        /// </summary>
        /// <param name="m1">The first Matrix</param>
        /// <param name="m2">The second Matrix</param>
        /// <returns>The combined Matrixes</returns>
        [DllImport("SGSDK.dll", EntryPoint = "MultiplyMatrix2D")]
        public static extern Matrix2D Multiply(ref Matrix2D m1, ref Matrix2D m2);//const

        /// <summary>
        /// Multiplies 1 Vector and 1 Matrix2D
        /// </summary>
        /// <param name="m">The Matrix2D</param>
        /// <param name="v">The Vector</param>
        /// <returns>The resulting Matrix2D</returns>
        [DllImport("SGSDK.dll", EntryPoint = "MultiplyMatrix2DAndVector")]
        public static extern Matrix2D Multiply(ref Matrix2D m, ref Vector v);//const

  

    }
}
