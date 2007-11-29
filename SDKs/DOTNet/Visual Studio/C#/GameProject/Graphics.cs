using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Drawing;

namespace SwinGame
{
    /// <summary>
    /// This contains number of bitmaps and its position.
    /// </summary>
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
        private static extern int GetSpriteCurrentFrame(IntPtr pointer);

        [DllImport("SGSDK.dll")]
        private static extern void SetSpriteCurrentFrame(IntPtr pointer, int frame);

        [DllImport("SGSDK.dll")]
        private static extern bool GetSpriteUsePixelCollision(IntPtr pointer);

        [DllImport("SGSDK.dll")]
        private static extern void SetSpriteUsePixelCollision(IntPtr pointer, bool pixelcollision);

        internal IntPtr Pointer;

        /// <summary>
        /// Array of bitmaps this sprite contains
        /// </summary>
        /// <param name="idx">Index number</param>
        /// <returns>Bitmap of the specified frame</returns>
        public Bitmap this[int idx]
        {
            get
            {
                return GetSpriteBitmap(Pointer, idx);
            }
        }

        /// <summary>
        /// X position of this sprite
        /// </summary>
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

        /// <summary>
        /// Y position of this sprite
        /// </summary>
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

        /// <summary>
        /// Current animation frame of this sprite
        /// </summary>
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
        
        /// <summary>
        /// True if this sprite use pixel collision
        /// </summary>
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
        /// <summary>
        /// Create a bitmap
        /// </summary>
        /// <param name="width">Width of a bitmap</param>
        /// <param name="height">Height of a bitmap</param>
        /// <returns>New bitmap</returns>
        [DllImport("SGSDK.dll")]
        public static extern Bitmap CreateBitmap(int width, int height);

        /// <summary>
        /// Optimise the specified bitmap
        /// </summary>
        /// <param name="surface">Bitmap to optimise</param>
        [DllImport("SGSDK.dll")]
        public static extern void OptimiseBitmap(Bitmap surface);

        /// <summary>
        /// Load the specified image file
        /// </summary>
        /// <param name="pathToBitmap">Path to the image file</param>
        /// <returns>New bitmap</returns>
        public static Bitmap LoadBitmap(String pathToBitmap)
        {
            Bitmap result;
            result.pointer = DLL_LoadBitmapWithTransparentColor(pathToBitmap, false, Color.Black);
            return result;
        }

        [DllImport("SGSDK.dll", EntryPoint = "LoadBitmapWithTransparentColor")]
        private static extern IntPtr DLL_LoadBitmapWithTransparentColor(String pathToBitmap, Boolean transparent, Color transparentColor);

        /// <summary>
        /// Load the specified image file with a transparent color
        /// </summary>
        /// <param name="pathToBitmap">Path to the image file</param>
        /// <param name="transparent">True if this image has transparent pixels</param>
        /// <param name="transparentColor">Color of the transparent pixels</param>
        /// <returns>New bitmap</returns>
        public static Bitmap LoadBitmap(String pathToBitmap, Boolean transparent, Color transparentColor)
        {
            Bitmap result;
            result.pointer = DLL_LoadBitmapWithTransparentColor(pathToBitmap, transparent, transparentColor);
            return result;
        }

        [DllImport("SGSDK.dll", EntryPoint = "LoadTransparentBitmap")]
        private static extern IntPtr DLL_LoadTransparentBitmap(string pathToBitmap, Color transparentColor);

        /// <summary>
        /// Load an image with transparency
        /// </summary>
        /// <param name="pathToBitmap">Path to the image file</param>
        /// <param name="transparentColor">Color of the transparent pixels</param>
        /// <returns>New bitmap</returns>
        public static Bitmap LoadTransparentBitmap(string pathToBitmap, Color transparentColor)
        {
            Bitmap result;
            result.pointer = DLL_LoadTransparentBitmap(pathToBitmap, transparentColor);
            return result;
        }

        [DllImport("SGSDK.dll", EntryPoint = "FreeBitmap")]
        private static extern void DLL_FreeBitmap(ref IntPtr bitmapToFree);

        /// <summary>
        /// Free the specified bitmap
        /// </summary>
        /// <param name="bitmapToFree">Bitmap to free</param>
        public static void FreeBitmap(ref Bitmap bitmapToFree)
        {
            DLL_FreeBitmap(ref bitmapToFree.pointer);
        }

        [DllImport("SGSDK.dll", EntryPoint = "GetBitmapWidth")]
        private static extern int DLL_GetBitmapWidth(IntPtr targetbitmap);

        /// <summary>
        /// Get the specified bitmap's width
        /// </summary>
        /// <param name="targetbitmap">Target bitmap</param>
        /// <returns>Width of the bitmap</returns>
        public static int GetBitmapWidth(Bitmap targetbitmap)
        {
            return DLL_GetBitmapWidth(targetbitmap.pointer);
        }

        [DllImport("SGSDK.dll", EntryPoint = "GetBitmapHeight")]
        private static extern int DLL_GetBitmapHeight(IntPtr targetbitmap);

        /// <summary>
        /// Get the specified bitmap's height
        /// </summary>
        /// <param name="targetbitmap">Target bitmap</param>
        /// <returns>Height of the bitmap</returns>
        public static int GetBitmapHeight(Bitmap targetbitmap)
        {
            return DLL_GetBitmapHeight(targetbitmap.pointer);
        }

        [DllImport("SGSDK.dll", EntryPoint = "ClearSurfaceWithColor")]
        private static extern void DLL_ClearSurfaceWithColor(IntPtr dest, Color toColour);

        /// <summary>
        /// Clear the bitmap with the specified color
        /// </summary>
        /// <param name="dest">Bitmap to clear</param>
        /// <param name="toColour">The color used to clear</param>
        public static void ClearSurface(Bitmap dest, Color toColour)
        {
            DLL_ClearSurfaceWithColor(dest.pointer, toColour);
        }

        /// <summary>
        /// Clear the bitmap
        /// </summary>
        /// <param name="dest">Bitmap to clear</param>
        public static void ClearSurface(Bitmap dest)
        {
            DLL_ClearSurfaceWithColor(dest.pointer, Color.Black);
        }

        [DllImport("SGSDK.dll", EntryPoint = "DrawBitmapWithDestination")]
        private static extern void DLL_DrawBitmapWithDestination(IntPtr dest, IntPtr bitmapToDraw, int x, int y);

        /// <summary>
        /// Draw bitmap to the specified bitmap
        /// </summary>
        /// <param name="dest">Bitmap to draw on</param>
        /// <param name="bitmapToDraw">Bitmap to draw</param>
        /// <param name="x">X coordinate</param>
        /// <param name="y">Y coordinate</param>
        public static void DrawBitmap(Bitmap dest, Bitmap bitmapToDraw, int x, int y)
        {
            DLL_DrawBitmapWithDestination(dest.pointer, bitmapToDraw.pointer, x, y);
        }

        [DllImport("SGSDK.dll", EntryPoint = "DrawBitmapPartWithDestination")]
        private static extern void DLL_DrawBitmapPartWithDestination(IntPtr dest, IntPtr bitmapToDraw, int srcX, int srcY, int srcW, int srcH, int x, int y);

        /// <summary>
        /// Draws part of a bitmap (bitmapToDraw) onto another bitmap (dest)
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="bitmapToDraw">The bitmap to be drawn onto the destination</param>
        /// <param name="srcX">The x offset to the area to copy in bitmapToDraw</param>
        /// <param name="srcY">The y offset to the area to copy in bitmapToDraw</param>
        /// <param name="srcW">The width of the area to copy</param>
        /// <param name="srcH">The height of the area to copy</param>
        /// <param name="x">The x location to draw the bitmap part to</param>
        /// <param name="y">The y location to draw the bitmap part to</param>
        public static void DrawBitmapPart(Bitmap dest, Bitmap bitmapToDraw, int srcX, int srcY, int srcW, int srcH, int x, int y)
        {
            DLL_DrawBitmapPartWithDestination(dest.pointer, bitmapToDraw.pointer, srcX, srcY, srcW, srcH, x, y);
        }
    }
}
