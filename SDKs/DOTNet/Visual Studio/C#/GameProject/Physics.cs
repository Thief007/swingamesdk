using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

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
        [DllImport("SGSDK.dll", EntryPoint="HasSpriteCollidedX")]
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
        public static extern bool HasSpriteCollidedWithRect(Sprite theSprite, double x, double y, int width, int height)
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
        /// <param name="vwPrtX">The x position in the view point window</param>
        /// <param name="vwPrtY">The y position in the view point window</param>
        /// <returns>True if the sprite collides with the rectangle</returns>
        public static extern bool HasSpriteCollidedWithRect(Sprite theSprite, double x, double y, int width, int height, int vwPrtX, int vwPrtY)
        {
            return HasSpriteCollidedWithRect(theSprite.Pointer, x + vwPrtX, y + vwPrtY, width, height);
        }

        [DllImport("SGSDK.dll", EntryPoint = "HaveSpritesCollided")]
        private static extern bool DLL_HaveSpritesCollided(IntPtr sprite1, IntPtr sprite2);
        /// <summary>
        /// Determines if two sprites have collided.
        /// </summary>
        /// <param name="sprite1">The first sprite to check.</param>
        /// <param name="sprite2">The second sprite to check.</param>
        /// <returns>True if the sprites have collided.</returns>
        public static extern bool HaveSpritesCollided(Sprite sprite1, Sprite sprite2)
        {
            return DLL_HaveSpritesCollided(sprite1.Pointer, sprite2.Pointer);
        }

    }
}
