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

        [DllImport("SGSDK.dll", EntryPoint = "LoadBitmapWithTransparentColor", CallingConvention = CallingConvention.Cdecl)]
        private static extern IntPtr DLL_LoadBitmapWithTransparentColor(String pathToBitmap, bool transparent, uint transparentColor);
        /// <summary>
        /// Load the specified image file
        /// </summary>
        /// <param name="pathToBitmap">Path to the image file</param>
        /// <returns>New bitmap</returns>
        public static Bitmap LoadBitmap(String pathToBitmap)
        {
            Bitmap result;
            int color = Color.Black.ToArgb();
            result.pointer = DLL_LoadBitmapWithTransparentColor(pathToBitmap, false, (uint)color);
            return result;
        }

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
            int color = transparentColor.ToArgb();
            result.pointer = DLL_LoadBitmapWithTransparentColor(pathToBitmap, transparent, (uint)color);
            return result;
        }

        [DllImport("SGSDK.dll", EntryPoint = "LoadTransparentBitmap")]
        private static extern IntPtr DLL_LoadTransparentBitmap(string pathToBitmap, uint transparentColor);

        /// <summary>
        /// Load an image with transparency
        /// </summary>
        /// <param name="pathToBitmap">Path to the image file</param>
        /// <param name="transparentColor">Color of the transparent pixels</param>
        /// <returns>New bitmap</returns>
        public static Bitmap LoadTransparentBitmap(string pathToBitmap, Color transparentColor)
        {
            Bitmap result;
            int color = transparentColor.ToArgb();
            result.pointer = DLL_LoadTransparentBitmap(pathToBitmap, (uint)color);
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
        private static extern void DLL_ClearSurfaceWithColor(IntPtr dest, uint toColour);

        /// <summary>
        /// Clear the bitmap with the specified color
        /// </summary>
        /// <param name="dest">Bitmap to clear</param>
        /// <param name="toColour">The color used to clear</param>
        public static void ClearSurface(Bitmap dest, Color toColour)
        {
            int color = toColour.ToArgb();
            DLL_ClearSurfaceWithColor(dest.pointer, (uint)color);
        }

        /// <summary>
        /// Clear the bitmap
        /// </summary>
        /// <param name="dest">Bitmap to clear</param>
        public static void ClearSurface(Bitmap dest)
        {
            int color = Color.Black.ToArgb();
            DLL_ClearSurfaceWithColor(dest.pointer, (uint)color);
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

        [DllImport("SGSDK.dll", EntryPoint = "DrawPixelWithDestination")]
        private static extern void DLL_DrawPixelWithDestination(IntPtr dest, uint theColour, int x, int y);

        /// <summary>
        /// Draws a pixel onto the destination bitmap
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the pixel</param>
        /// <param name="x">The x location to draw the pixel at</param>
        /// <param name="y">The y location to draw the pixel at</param>
        public static void DrawPixel(Bitmap dest, Color theColour, int x, int y)
        {
            int color = theColour.ToArgb();
            DLL_DrawPixelWithDestination(dest.pointer, (uint)color, x, y);
        }

        [DllImport("SGSDK.dll", EntryPoint = "DrawRectangleWithDestination")]
        private static extern void DLL_DrawRectangleWithDestination(IntPtr dest, uint theColour, bool filled, int xPos, int yPos, int width, int height);

        /// <summary>
        /// Draws a rectangle on the destination bitmap
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the rectangle</param>
        /// <param name="filled">True to draw a filled rectangle, false for outline</param>
        /// <param name="xPos">The x location to draw the rectangle at</param>
        /// <param name="yPos">The y location to draw the rectangle at</param>
        /// <param name="width">The width of the rectangle</param>
        /// <param name="height">The height of the rectangle</param>
        public static void DrawRectangle(Bitmap dest, Color theColour, bool filled, int xPos, int yPos, int width, int height)
        {
            int color = theColour.ToArgb();
            DLL_DrawRectangleWithDestination(dest.pointer, (uint)color, filled, xPos, yPos, width, height);
        }

        [DllImport("SGSDK.dll", EntryPoint = "FillRectangleWithDestination")]
        private static extern void DLL_FillRectangleWithDestination(IntPtr dest, uint theColour, int xPos, int yPos, int width, int height);

        /// <summary>
        /// Draws a filled rectangle on the destination bitmap
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the rectangle</param>
        /// <param name="xPos">The x location to draw the rectangle at</param>
        /// <param name="yPos">The y location to draw the rectangle at</param>
        /// <param name="width">The width of the rectangle</param>
        /// <param name="height">The height of the rectangle</param>
        public static void FillRectangle(Bitmap dest, Color theColour, int xPos, int yPos, int width, int height)
        {
            int color = theColour.ToArgb();
            DLL_FillRectangleWithDestination(dest.pointer, (uint)color, xPos, yPos, width, height);
        }

        /// <summary>
        /// Draws the outline of a rectangle on the destination bitmap
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the rectangle</param>
        /// <param name="xPos">The x location to draw the rectangle at</param>
        /// <param name="yPos">The y location to draw the rectangle at</param>
        /// <param name="width">The width of the rectangle</param>
        /// <param name="height">The height of the rectangle</param>
        public static void DrawRectangle(Bitmap dest, Color theColour, int xPos, int yPos, int width, int height)
        {
            int color = theColour.ToArgb();
            DLL_DrawRectangleWithDestination(dest.pointer, (uint)color, false, xPos, yPos, width, height);
        }

        [DllImport("SGSDK.dll", EntryPoint = "DrawLineWithDestination")]
        private static extern void DLL_DrawLineWithDestination(IntPtr dest, uint theColour, int xPosStart, int yPosStart, int xPosEnd, int yPosEnd);

        /// <summary>
        /// Draws a line on the destination bitmap
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the line</param>
        /// <param name="xPosStart">The x location to start the line at</param>
        /// <param name="yPosStart">The y location to start the line at</param>
        /// <param name="xPosEnd">The x location to end the line at</param>
        /// <param name="yPosEnd">The y location to end the line at</param>
        public static void DrawLine(Bitmap dest, Color theColour, int xPosStart, int yPosStart, int xPosEnd, int yPosEnd)
        {
            int color = theColour.ToArgb();
            DLL_DrawLineWithDestination(dest.pointer, (uint)color, xPosStart, yPosStart, xPosEnd, yPosEnd);
        }

        [DllImport("SGSDK.dll", EntryPoint = "DrawHorizontalLineWithDestination")]
        private static extern void DLL_DrawHorizontalLineWithDestination(IntPtr dest, uint theColour, int y, int x1, int x2);

        /// <summary>
        /// Draws a horizontal line on the destination bitmap
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the line</param>
        /// <param name="y">The y location of the line</param>
        /// <param name="x1">The starting x value of the line</param>
        /// <param name="x2">The starting y value of the line</param>
        public static void DrawHorizontalLine(Bitmap dest, Color theColour, int y, int x1, int x2)
        {
            int color = theColour.ToArgb();
            DLL_DrawHorizontalLineWithDestination(dest.pointer, (uint)color, y, x1, x2);
        }

        [DllImport("SGSDK.dll", EntryPoint = "DrawVerticalLineWithDestination")]
        private static extern void DLL_DrawVerticalLineWithDestination(IntPtr dest, uint theColour, int x, int y1, int y2);

        /// <summary>
        /// Draws a vertical line on the destination bitmap
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the line</param>
        /// <param name="x">The x location of the line</param>
        /// <param name="y1">The starting y value of the line</param>
        /// <param name="y2">The ending y value of the line</param>
        public static void DrawVerticalLine(Bitmap dest, Color theColour, int x, int y1, int y2)
        {
            int color = theColour.ToArgb();
            DLL_DrawVerticalLineWithDestination(dest.pointer, (uint)color, x, y1, y2);
        }

        [DllImport("SGSDK.dll", EntryPoint = "DrawCircleWithDestination")]
        private static extern void DLL_DrawCircleWithDestination(IntPtr dest, uint theColour, bool filled, int xc, int yc, int radius);

        /// <summary>
        /// Draws a circle centered on a given x, y location
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the circle</param>
        /// <param name="filled">True to draw a filled circle, false for outline</param>
        /// <param name="xc">The x location of the center of the circle</param>
        /// <param name="yc">The y location of the center of the circle</param>
        /// <param name="radius">The radius of the circle</param>
        public static void DrawCircle(Bitmap dest, Color theColour, bool filled, int xc, int yc, int radius)
        {
            int color = theColour.ToArgb();
            DLL_DrawCircleWithDestination(dest.pointer, (uint)color, filled, xc, yc, radius);
        }

        /// <summary>
        /// Draws a circle outline centered on a given x, y location
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the circle</param>
        /// <param name="xc">The x location of the center of the circle</param>
        /// <param name="yc">The y location of the center of the circle</param>
        /// <param name="radius">The radius of the circle</param>
        public static void DrawCircle(Bitmap dest, Color theColour, int xc, int yc, int radius)
        {
            int color = theColour.ToArgb();
            DLL_DrawCircleWithDestination(dest.pointer, (uint)color, false, xc, yc, radius);
        }

        /// <summary>
        /// Draws a filled circle centered on a given x, y location
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the circle</param>
        /// <param name="xc">The x location of the center of the circle</param>
        /// <param name="yc">The y location of the center of the circle</param>
        /// <param name="radius">The radius of the circle</param>
        public static void FillCircle(Bitmap dest, Color theColour, int xc, int yc, int radius)
        {
            int color = theColour.ToArgb();
            DLL_DrawCircleWithDestination(dest.pointer, (uint)color, true, xc, yc, radius);
        }

        [DllImport("SGSDK.dll", EntryPoint = "DrawEllipseWithDestination")]
        private static extern void DLL_DrawEllipseWithDestination(IntPtr dest, uint theColour, bool filled, int xPos, int yPos, int width, int height);

        /// <summary>
        /// Draws a ellipse within a given rectangle on the dest bitmap
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the ellipse</param>
        /// <param name="filled">True to draw a filled ellipse, false for outline</param>
        /// <param name="xPos">The x location of the top left of the ellipse</param>
        /// <param name="yPos">The y location of the top left of the ellipse</param>
        /// <param name="width">The width of the ellipse</param>
        /// <param name="height">The height of the ellipse</param>
        public static void DrawEllipse(Bitmap dest, Color theColour, bool filled, int xPos, int yPos, int width, int height)
        {
            int color = theColour.ToArgb();
            DLL_DrawEllipseWithDestination(dest.pointer, (uint)color, filled, xPos, yPos, width, height);
        }

        /// <summary>
        /// Draws a ellipse outline within a given rectangle on the dest bitmap
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the ellipse</param>
        /// <param name="xPos">The x location of the top left of the ellipse</param>
        /// <param name="yPos">The y location of the top left of the ellipse</param>
        /// <param name="width">The width of the ellipse</param>
        /// <param name="height">The height of the ellipse</param>
        public static void DrawEllipse(Bitmap dest, Color theColour, int xPos, int yPos, int width, int height)
        {
            int color = theColour.ToArgb();
            DLL_DrawEllipseWithDestination(dest.pointer, (uint)color, false, xPos, yPos, width, height);
        }

        /// <summary>
        /// Draws a filled ellipse within a given rectangle on the dest bitmap
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the ellipse</param>
        /// <param name="xPos">The x location of the top left of the ellipse</param>
        /// <param name="yPos">The y location of the top left of the ellipse</param>
        /// <param name="width">The width of the ellipse</param>
        /// <param name="height">The height of the ellipse</param>
        public static void FillEllipse(Bitmap dest, Color theColour, int xPos, int yPos, int width, int height)
        {
            int color = theColour.ToArgb();
            DLL_DrawEllipseWithDestination(dest.pointer, (uint)color, true, xPos, yPos, width, height);
        }

        [DllImport("SGSDK.dll", EntryPoint = "ClearScreen")]
        private static extern void DLL_ClearScreen(uint toColour);

        /// <summary>
        /// Clears the surface of the screen to the passed in color
        /// </summary>
        /// <param name="toColour">The colour to clear the bitmap to</param>
        public static void ClearScreen(Color toColour)
        {
            int color = toColour.ToArgb();
            DLL_ClearScreen((uint)color);
        }

        /// <summary>
        /// Clears the screen to Black
        /// </summary>
        public static void ClearScreen()
        {
            int color = Color.Black.ToArgb();
            DLL_ClearScreen((uint)color);
        }

        [DllImport("SGSDK.dll", EntryPoint = "DrawBitmap")]
        private static extern void DLL_DrawBitmap(IntPtr bitmapToDraw, int x, int y);

        /// <summary>
        /// Draws one bitmap (bitmapToDraw) onto the screen
        /// </summary>
        /// <param name="bitmapToDraw">The bitmap to be drawn onto the screen</param>
        /// <param name="x">The x location to draw the bitmap to</param>
        /// <param name="y">The y location to draw the bitmap to</param>
        public static void DrawBitmap(Bitmap bitmapToDraw, int x, int y)
        {
            DLL_DrawBitmap(bitmapToDraw.pointer, x, y);
        }
    }
}
