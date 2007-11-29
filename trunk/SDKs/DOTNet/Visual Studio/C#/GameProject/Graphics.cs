using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Drawing;

namespace SwinGame
{
    public struct Sprite
    {
        [DllImport("SGSDK.dll")]
        private static extern Bitmap GetSpriteBitmap(IntPtr pointer, int id);

        [DllImport("SGSDK.dll")]
        private static extern float GetSpriteX(IntPtr pointer);

        [DllImport("SGSDK.dll")]
        private static extern void SetSpriteX(IntPtr pointer, float X);

        [DllImport("SGSDK.dll")]
        private static extern float GetSpriteY(IntPtr pointer);

        [DllImport("SGSDK.dll")]
        private static extern void SetSpriteY(IntPtr pointer, float Y);

        [DllImport("SGSDK.dll")]
        private static extern float GetSpriteCurrentFrame(IntPtr pointer);

        [DllImport("SGSDK.dll")]
        private static extern void SetSpriteCurrentFrame(IntPtr pointer, int frame);

        [DllImport("SGSDK.dll")]
        private static extern float GetSpriteUsePixelCollision(IntPtr pointer);

        [DllImport("SGSDK.dll")]
        private static extern void SetSpriteUsePixelCollusion(IntPtr pointer, bool pixelcollision);

        internal IntPtr Pointer;

        public Bitmap this[int idx]
        {
            get
            {
                return GetSpriteBitmap(Pointer, idx);
            }
        }

        public float X
        {
            get
            {
                return GetSpriteX(Pointer);
            }
            set
            {
                SetSpriteX(Pointer, value);
            }
        }

        public float Y
        {
            get
            {
                return GetSpriteY(Pointer);
            }
            set
            {
                SetSpriteY(Pointer, value);
            }
        }

        public int CurrentFrame
        {
            get
            {
                return GetSpriteCurrentFrame(Pointer);
            }
            set
            {
                SetSpriteCurrentFrame(Pointer, value);
            }
        }
        
        public bool UsePixelCollision
        {
            get
            {
                return GetSpriteUsePixelCollision(Pointer);
            }
            set
            {
                SetSpriteUsePixelCollision(Pointer, value);
            }
        }
    }

    public class Graphics
    {
        [DllImport("SGSDK.dll")]
        public static extern Bitmap CreateBitmap(int width, int height);

        [DllImport("SGSDK.dll")]
        public static extern void OptimiseBitmap(Bitmap surface);

        [DllImport("SGSDK.dll")]
        public static extern Bitmap LoadTransparentBitmap(string pathToBitmap, Color transparentColor);

        //	function LoadTransparentBitmap(pathToBitmap : String;
		//						transparentColor : Colour): Bitmap; cdecl; export;
    }
}
