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
        
        public static bool HasSpriteCollidedX(Sprite theSprite, int x, CollisionDetectionRange range)
        {
            return DLL_HasSpriteCollidedX(theSprite, x, range);
        }

        [DllImport("SGSDK.dll", EntryPoint="HasSpriteCollidedY")]
        private static extern bool DLL_HasSpriteCollidedY(IntPtr theSprite, int y ,CollisionDetectionRange range);
        
        public static bool HasSpriteCollidedY(Sprite theSprite, int y, CollisionDetectionRange range)
        {
            return DLL_HasSpriteCollidedY(theSprite, y, range);
        }
    }
}
