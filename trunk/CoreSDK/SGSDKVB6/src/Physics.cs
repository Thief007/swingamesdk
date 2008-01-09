using System;
using System.Collections.Generic;
using System.Text;
using SwinGame;
using System.Runtime.InteropServices;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
namespace SwinGameVB
{


    /// <summary>
    /// Enumeration: CollisionDetectionRanges
    ///	This is used to indicate the kind of collision being checked with the
    ///	Sprite collision routines.  
    /// </summary>
    [Guid("D8B00F7C-D51C-4548-9108-E53D3DE1E9CD")]
    [ComVisible(true)]
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
    //[StructLayout(LayoutKind.Sequential)]
    [ClassInterface(ClassInterfaceType.None)]
    [Guid("87C6F043-9E0A-4579-878D-EBD72F98554A")]
    [ComVisible(true)]
    public class Matrix2D : IMatrix2D, IDisposable
    {
        private SwinGame.Matrix2D marrix2D;
        internal SwinGame.Matrix2D result
        {
            get
            {
                return marrix2D;
            }
            set
            {
                marrix2D = value;
            }
        }

        public float GetMaxtrix2DElement(int r, int c)
        {
            return marrix2D[r, c];
            
        }

        public void SetMaxtrix2DElement(float value, int r, int c)
        {
            marrix2D[r, c] = value;
            
        }

        public void Dispose()
        {
            marrix2D.Dispose();
            //Dispose(true);
            //GC.SuppressFinalize(this);
        }
        /*
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

        ~Matrix2D()
        {
            Dispose(false);
        }

        /*

        public Matrix2D IdentityMatrix
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
    [Guid("354E0F76-E669-4f54-9D80-E204FF559F15")]
    [ComVisible(true)]
    public interface IMatrix2D 
    {
        float GetMaxtrix2DElement(int r, int c);
        void SetMaxtrix2DElement(float value, int r, int c);
        void Dispose();
    }


    /// <summary>
    /// SGSDK.NET's Physics Class
    /// </summary>
    [ClassInterface(ClassInterfaceType.None)]
    [Guid("2542698C-64CF-4767-B993-D9017D721444")]
    [ComVisible(true)]
    public class Physics : IPhysics
    {
        /// <summary>
        /// Determines if a sprite has collided with a given x position.
        /// </summary>
        /// <param name="theSprite">The sprite to check</param>
        /// <param name="x">The x location to check collision with</param>
        /// <param name="range">The kind of check to perform less, larger or equal.</param>
        /// <returns>True if the sprite is within the range requested</returns>
        public bool HasSpriteCollidedX(Sprite theSprite, int x, CollisionDetectionRange range)
        {
            return SwinGame.Physics.HasSpriteCollidedX(theSprite.result, x, (SwinGame.CollisionDetectionRange)range);
        }

        /// <summary>
        /// Determines if a sprite has collided with a given y position.
        /// </summary>
        /// <param name="theSprite">The sprite to check</param>
        /// <param name="x">The y location to check collision with</param>
        /// <param name="range">The kind of check to perform less, larger or equal.</param>
        /// <returns>True if the sprite is within the range requested</returns>
        public bool HasSpriteCollidedY(Sprite theSprite, int y, CollisionDetectionRange range)
        {
            return SwinGame.Physics.HasSpriteCollidedY(theSprite.result, y, (SwinGame.CollisionDetectionRange)range);
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
        /// <returns>True if the sprite collides with the rectangle</returns>
        public bool HasSpriteCollidedWithRect(Sprite theSprite, Single x, Single y, int width, int height)
        {
            return SwinGame.Physics.HasSpriteCollidedWithRect(theSprite.result, x, y, width, height);
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
        public bool HasSpriteCollidedWithRect_ViewPort(Sprite theSprite, Single x, Single y, int width, int height, int vwPrtX, int vwPrtY)
        {
            return SwinGame.Physics.HasSpriteCollidedWithRect(theSprite.result, x + vwPrtX, y + vwPrtY, width, height);
        }
        
        /// <summary>
        /// Determines if two sprites have collided.
        /// </summary>
        /// <param name="sprite1">The first sprite to check.</param>
        /// <param name="sprite2">The second sprite to check.</param>
        /// <returns>True if the sprites have collided.</returns>
        public bool HaveSpritesCollided(Sprite sprite1, Sprite sprite2)
        {
            return SwinGame.Physics.HaveSpritesCollided(sprite1.result, sprite2.result);
        }

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
        public bool CollisionWithinBitmapImages(Bitmap image1, int x1, int y1, bool bounded1, Bitmap image2, int x2, int y2, bool bounded2)
        {
            return SwinGame.Physics.CollisionWithinBitmapImages(image1.result, x1, y1, bounded1, image2.result, x2, y2, bounded2);
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
        public bool CollisionWithinBitmapImages_NoBound(Bitmap image1, int x1, int y1, Bitmap image2, int x2, int y2)
        {
            return SwinGame.Physics.CollisionWithinBitmapImages(image1.result, x1, y1, false, image2.result, x2, y2, false);
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
        public bool HasSpriteCollidedWithBitmap(Sprite theSprite, Bitmap theBitmap, int x, int y, bool bounded)
        {
            return SwinGame.Physics.HasSpriteCollidedWithBitmap( theSprite.result,  theBitmap.result,  x,  y,  bounded);
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
        public bool HasSpriteCollidedWithBitmap_ViewPort(Sprite theSprite, Bitmap theBitmap, int x, int y, bool bounded, int vwPrtX, int vwPrtY)
        {
            return SwinGame.Physics.HasSpriteCollidedWithBitmap(theSprite.result, theBitmap.result, x, y, bounded, vwPrtX, vwPrtY);
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
        public bool HasSpriteCollidedWithBitmap_Bound(Sprite theSprite, Bitmap theBitmap, int x, int y)
        {
            return SwinGame.Physics.HasSpriteCollidedWithBitmap(theSprite.result, theBitmap.result, x, y);
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
        public bool HasSpriteCollidedWithBitmap_ViewPort_Bound(Sprite theSprite, Bitmap theBitmap, int x, int y, int vwPrtX, int vwPrtY)
        {
            return SwinGame.Physics.HasSpriteCollidedWithBitmap(theSprite.result, theBitmap.result, x, y, vwPrtX, vwPrtY);
        }
        
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
        public bool HaveBitmapsCollided(Bitmap image1, int x1, int y1, bool bounded1, Bitmap image2, int x2, int y2, bool bounded2)
        {
            return SwinGame.Physics.HaveBitmapsCollided(image1.result, x1, y1, bounded1, image2.result, x2, y2, bounded2);
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
        public bool HaveBitmapsCollided_NoBound(Bitmap image1, int x1, int y1, Bitmap image2, int x2, int y2)
        {
            return SwinGame.Physics.HaveBitmapsCollided(image1.result, x1, y1, false, image2.result, x2, y2, false);
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
        public Vector CreateVector(Single x, Single y, bool invertY)
        {
            Vector vector = new Vector();
            vector.result=SwinGame.Physics.CreateVector(x, y, invertY);
            return vector;
        }
        /// <summary>
        /// Creates a new vector with values x and y.
        /// </summary>
        /// <param name="x">Initial values for the vector</param>
        /// <param name="y">Initial values for the vector</param>
        /// <returns>A new vector with values x and y</returns>
        public Vector CreateVector_NoInvert(Single x, Single y)
        {
            Vector vector = new Vector();
            vector.result = SwinGame.Physics.CreateVector(x, y);
            return vector;
        }

        /// <summary>
        /// Adds the Vector v1 and the Vector v2.
        /// </summary>
        /// <param name="v1">The vectors to work with</param>
        /// <param name="v2">The vectors to work with</param>
        /// <returns>v1 + v2</returns>
        public Vector AddVectors(Vector v1, Vector v2)
        {
            Vector vector = new Vector();
            vector.setW (v1.getW() + v2.getW());
            vector.setX (v1.getX() + v2.getX());
            vector.setY (v1.getY() + v2.getY());
            //vector.result = SwinGame.Physics.AddVectors(v1.result, v2.result);
            return vector;
        }

        /// <summary>
        /// Subtracts the Vector v1 and the Vector v2.
        /// </summary>
        /// <param name="v1">The vectors to work with</param>
        /// <param name="v2">The vectors to work with</param>
        /// <returns>v1 - v2</returns>
        public Vector SubtractVectors(Vector v1, Vector v2)
        {
            Vector vector = new Vector();
            vector.result = SwinGame.Physics.SubtractVectors(v1.result, v2.result);
            return vector;
        }

        /// <summary>
        /// Inverts the vector v. Changes the direction of the vector's x and y.
        /// </summary>
        /// <param name="theVector">The vector to invert</param>
        /// <returns>A new inverted vector</returns>
        public Vector InvertVector(Vector theVector)
        {
            Vector vector = new Vector();
            vector.result = SwinGame.Physics.InvertVector(theVector.result);
            return vector;
        }

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
        public Vector ChopVector(Vector theVector, int minX, int maxX, int minY, int maxY)
        {
            Vector vector = new Vector();
            vector.result = SwinGame.Physics.ChopVector(theVector.result, maxX, maxX, minY, maxY);
            return vector;
        }

        /// <summary>
        /// Limit the magnitude of a vector.
        /// </summary>
        /// <param name="theVector">The vector to limit</param>
        /// <param name="maxMagnitude">The maximum magnitude of the vector.</param>
        /// <returns>A new vector with the same direction as theVector,
        ///	with a maximum magnitude of maxMagnitude</returns>
        public Vector LimitVector(Vector theVector, Single maxMagnitude)
        {
            Vector vector = new Vector();
            vector.result = SwinGame.Physics.LimitVector(theVector.result, maxMagnitude);
            return vector;
        }

        /// <summary>
        /// Gets the unit vector of the passed in vector. The unit vector has a
        ///	magnitude of 1, resulting in a vector that indicates the direction of
        ///	the original vector.
        /// </summary>
        /// <param name="theVector">The vector to get the unit vector of</param>
        /// <returns>The unit vector from the passed in vector</returns>
        public Vector GetUnitVector(Vector theVector)
        {
            Vector vector = new Vector();
            vector.result = SwinGame.Physics.GetUnitVector(theVector.result);
            return vector;
        }

        /// <summary>
        /// Indicates if the magnitude of the vector is 0.
        /// </summary>
        /// <param name="theVector">The vector to check</param>
        /// <returns>True if the vector has values 0, 0</returns>
        public Vector IsZeroVector(Vector theVector)
        {
            Vector vector = new Vector();
            vector.result = SwinGame.Physics.IsZeroVector(theVector.result);
            return vector;
        }

        /// <summary>
        /// Returns the magnitude of a vector. The magnitude represents the length of
        ///	the vector.
        /// </summary>
        /// <param name="theVector">The vector to get the magnitude of</param>
        /// <returns>The magnitude of the vector</returns>
        public Single Magnitude(Vector theVector)
        {
            return SwinGame.Physics.Magnitude(theVector.result);
        }

        /// <summary>
        /// The Angle between two vectors
        /// </summary>
        /// <param name="v1">The first Vector</param>
        /// <param name="v2">The Second Vector</param>
        /// <returns>The angle</returns>
        public Single DotProduct(Vector v1, Vector v2)
        {
            return SwinGame.Physics.DotProduct(v1.result, v2.result);
        }

        /// <summary>
        /// Multiplies a Vector by a number
        /// </summary>
        /// <param name="v1">The vector to multiply</param>
        /// <param name="s1">The number to multiply the vector by</param>
        /// <returns>The multiplyed vector</returns>
        public Vector MultiplyVector(Vector v1, Single s1)
        {
            Vector vector = new Vector();
            vector.result = SwinGame.Physics.MultiplyVector(v1.result, s1);
            return vector;
        }

        /// <summary>
        /// Gets the Angle between two points 
        /// </summary>
        /// <param name="x1"></param>
        /// <param name="y1"></param>
        /// <param name="x2"></param>
        /// <param name="y2"></param>
        /// <returns></returns>
        public Single CalculateAngle_Number(Single x1, Single y1, Single x2, Single y2)
        {
            return SwinGame.Physics.CalculateAngle(x1, y1, x2, y2);
        }

        public Single CalculateAngle(Sprite sprite1, Sprite sprite2)
        {
            return SwinGame.Physics.CalculateAngle(sprite1.result, sprite2.result);
        }

        public Matrix2D TranslationMatric(Single dx, Single dy)
        {
            Matrix2D temp = new Matrix2D();
            temp.result = SwinGame.Physics.TranslationMatric(dx, dy);
            return temp;
        }

        public Matrix2D ScaleMatrix(Single scale)
        {
            Matrix2D temp = new Matrix2D();
            temp.result = SwinGame.Physics.ScaleMatrix(scale);
            return temp;
        }

        public Matrix2D RotationMatrix(Single deg)
        {
            Matrix2D temp = new Matrix2D();
            temp.result = SwinGame.Physics.RotationMatrix(deg);
            return temp;
        }

        /// <summary>
        /// Multiplies two matrixs 
        /// </summary>
        /// <param name="m1">The first Matrix</param>
        /// <param name="m2">The second Matrix</param>
        /// <returns>The combined Matrixes</returns>
        public Matrix2D Multiply(Matrix2D m1, Matrix2D m2)
        {
            Matrix2D temp = new Matrix2D();
            temp.result = SwinGame.Physics.Multiply(m1.result, m2.result);
            return temp;
        }

        /// <summary>
        /// Multiplies 1 Vector and 1 Matrix2D
        /// </summary>
        /// <param name="m">The Matrix2D</param>
        /// <param name="v">The Vector</param>
        /// <returns>The resulting Matrix2D</returns>
        public Vector Multiply_Vector(Matrix2D m, Vector v)
        {
            Vector vector = new Vector();
            vector.result = SwinGame.Physics.Multiply(m.result, v.result);
            return vector;
        }

        /// <summary>
        /// Vector Collisions alters the vectors of two Physics Entities depending on
        /// the movement and mass of the 2 entities.
        /// </summary>
        /// <param name="p1">Physics Data Object 1</param>
        /// <param name="p2">Physics Data Object 2</param>

        public void VectorCollision(Sprite sprite1, Sprite sprite2)
        {
            SwinGame.Physics.VectorCollision(sprite1.result, sprite2.result);
        }


        public Vector GetVectorFromAngle(float angle, float magnitude)
        {
            Vector vector = new Vector();
            vector.result = SwinGame.Physics.GetVectorFromAngle(angle, magnitude);
            return vector;
        }
        public void temp()
        {
        }
    }
    [Guid("F6B70E5B-3A0E-48e5-8F6B-7033A40D758C")]
    [ComVisible(true)]
    public interface IPhysics
    {
        void temp();
        bool HasSpriteCollidedX(Sprite theSprite, int x, CollisionDetectionRange range);
        bool HasSpriteCollidedY(Sprite theSprite, int y, CollisionDetectionRange range);
        bool HasSpriteCollidedWithRect(Sprite theSprite, Single x, Single y, int width, int height);
        bool HasSpriteCollidedWithRect_ViewPort(Sprite theSprite, Single x, Single y, int width, int height, int vwPrtX, int vwPrtY);
        bool HaveSpritesCollided(Sprite sprite1, Sprite sprite2);
        bool CollisionWithinBitmapImages(Bitmap image1, int x1, int y1, bool bounded1, Bitmap image2, int x2, int y2, bool bounded2);
        bool CollisionWithinBitmapImages_NoBound(Bitmap image1, int x1, int y1, Bitmap image2, int x2, int y2);
        bool HasSpriteCollidedWithBitmap(Sprite theSprite, Bitmap theBitmap, int x, int y, bool bounded);
        bool HasSpriteCollidedWithBitmap_ViewPort(Sprite theSprite, Bitmap theBitmap, int x, int y, bool bounded, int vwPrtX, int vwPrtY);
        bool HasSpriteCollidedWithBitmap_Bound(Sprite theSprite, Bitmap theBitmap, int x, int y);
        bool HasSpriteCollidedWithBitmap_ViewPort_Bound(Sprite theSprite, Bitmap theBitmap, int x, int y, int vwPrtX, int vwPrtY);
        bool HaveBitmapsCollided(Bitmap image1, int x1, int y1, bool bounded1, Bitmap image2, int x2, int y2, bool bounded2);
        bool HaveBitmapsCollided_NoBound(Bitmap image1, int x1, int y1, Bitmap image2, int x2, int y2);
        Vector CreateVector(Single x, Single y, bool invertY);
        Vector CreateVector_NoInvert(Single x, Single y);
        Vector AddVectors(Vector v1, Vector v2);
        Vector SubtractVectors(Vector v1, Vector v2);
        Vector InvertVector(Vector theVector);
        Vector ChopVector(Vector theVector, int minX, int maxX, int minY, int maxY);
        Vector LimitVector(Vector theVector, Single maxMagnitude);
        Vector GetUnitVector(Vector theVector);
        Vector IsZeroVector(Vector theVector);
        Single Magnitude(Vector theVector);
        Single DotProduct(Vector v1, Vector v2);
        Vector MultiplyVector(Vector v1, Single s1);
        Single CalculateAngle_Number(Single x1, Single y1, Single x2, Single y2);
        Single CalculateAngle(Sprite sprite1, Sprite sprite2);
        Matrix2D TranslationMatric(Single dx, Single dy);
        Matrix2D ScaleMatrix(Single scale);
        Matrix2D RotationMatrix(Single deg);
        Matrix2D Multiply(Matrix2D m1, Matrix2D m2);
        Vector Multiply_Vector(Matrix2D m, Vector v);
        void VectorCollision(Sprite sprite1, Sprite sprite2);
        Vector GetVectorFromAngle(float angle, Single magnitude);
    }

}
