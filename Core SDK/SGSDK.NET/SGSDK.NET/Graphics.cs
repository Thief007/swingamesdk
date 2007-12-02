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

        [DllImport("SGSDK.dll", EntryPoint = "DrawBitmapPart")]
        private static extern void DLL_DrawBitmapPart(IntPtr bitmapToDraw, int srcX, int srcY, int srcW, int srcH, int x, int y);

        /// <summary>
        /// Draws part of a bitmap (bitmapToDraw) onto the screen
        /// </summary>
        /// <param name="bitmapToDraw">The bitmap to be drawn onto the screen</param>
        /// <param name="srcX">The x offset to the area to copy in bitmapToDraw</param>
        /// <param name="srcY">The y offset to the area to copy in bitmapToDraw</param>
        /// <param name="srcW">The width of the area to copy</param>
        /// <param name="srcH">The height of the area to copy</param>
        /// <param name="x">The x location to draw the bitmap part to</param>
        /// <param name="y">The y location to draw the bitmap part to</param>
        public static void DrawBitmapPart(Bitmap bitmapToDraw, int srcX, int srcY, int srcW, int srcH, int x, int y)
        {
            DLL_DrawBitmapPart(bitmapToDraw.pointer, srcX, srcY, srcW, srcH, x, y);
        }

        [DllImport("SGSDK.dll", EntryPoint = "DrawPixel")]
        private static extern void DLL_DrawPixel(uint theColour, int x, int y);

        /// <summary>
        /// Draws a pixel onto the screen
        /// </summary>
        /// <param name="theColour">The color to draw the pixel</param>
        /// <param name="x">The x location to draw the pixel at</param>
        /// <param name="y">The y location to draw the pixel at</param>
        public static void DrawPixel(Color theColour, int x, int y)
        {
            int color = theColour.ToArgb();
            DLL_DrawPixel((uint)color, x, y);
        }

        [DllImport("SGSDK.dll", EntryPoint = "DrawRectangle")]
        private static extern void DLL_DrawRectangle(uint theColour, bool filled, int xPos, int yPos, int width, int height);

        /// <summary>
        /// Draws a rectangle on the screen
        /// </summary>
        /// <param name="theColour">The color to draw the rectangle</param>
        /// <param name="filled">True to draw a filled rectangle, false for outline</param>
        /// <param name="xPos">The x location to draw the rectangle at</param>
        /// <param name="yPos">The y location to draw the rectangle at</param>
        /// <param name="width">The width of the rectangle</param>
        /// <param name="height">The height of the rectangle</param>
        public static void DrawRectangle(Color theColour, bool filled, int xPos, int yPos, int width, int height)
        {
            int color = theColour.ToArgb();
            DLL_DrawRectangle((uint)color, filled, xPos, yPos, width, height);
        }

        /// <summary>
        /// Draws the outline of a rectangle on the screen
        /// </summary>
        /// <param name="theColour">The color to draw the rectangle</param>
        /// <param name="xPos">The x location to draw the rectangle at</param>
        /// <param name="yPos">The y location to draw the rectangle at</param>
        /// <param name="width">The width of the rectangle</param>
        /// <param name="height">The height of the rectangle</param>
        public static void DrawRectangle(Color theColour, int xPos, int yPos, int width, int height)
        {
            int color = theColour.ToArgb();
            DLL_DrawRectangle((uint)color, false, xPos, yPos, width, height);
        }

        /// <summary>
        /// Draws a filled rectangle on the screen
        /// </summary>
        /// <param name="theColour">The color to draw the rectangle</param>
        /// <param name="xPos">The x location to draw the rectangle at</param>
        /// <param name="yPos">The y location to draw the rectangle at</param>
        /// <param name="width">The width of the rectangle</param>
        /// <param name="height">The height of the rectangle</param>
        public static void FillRectangle(Color theColour, int xPos, int yPos, int width, int height)
        {
            int color = theColour.ToArgb();
            DLL_DrawRectangle((uint)color, true, xPos, yPos, width, height);
        }

        [DllImport("SGSDK.dll", EntryPoint = "DrawLine")]
        private static extern void DLL_DrawLine(uint theColour, int xPosStart, int yPosStart, int xPosEnd, int yPosEnd);

        /// <summary>
        /// Draws a line on the screen
        /// </summary>
        /// <param name="theColour">The color to draw the line</param>
        /// <param name="xPosStart">The x location to start the line at</param>
        /// <param name="yPosStart">The y location to start the line at</param>
        /// <param name="xPosEnd">The x location to end the line at</param>
        /// <param name="yPosEnd">The y location to end the line at</param>
        public static void DrawLine(Color theColour, int xPosStart, int yPosStart, int xPosEnd, int yPosEnd)
        {
            int color = theColour.ToArgb();
            DLL_DrawLine((uint)color, xPosStart, yPosStart, xPosEnd, yPosEnd);
        }

        [DllImport("SGSDK.dll", EntryPoint = "DrawHorizontalLine")]
        private static extern void DLL_DrawHorizontalLine(uint theColor, int y, int x1, int x2);

        /// <summary>
        /// Draws a horizontal line on the screen
        /// </summary>
        /// <param name="theColor">The color to draw the line</param>
        /// <param name="y">The y location of the line</param>
        /// <param name="x1">The starting x value of the line</param>
        /// <param name="x2">The ending x value of the line</param>
        public static void DrawHorizontalLine(Color theColor, int y, int x1, int x2)
        {
            int color = theColor.ToArgb();
            DLL_DrawHorizontalLine((uint)color, y, x1, x2);
        }

        [DllImport("SGSDK.dll", EntryPoint = "DrawVerticalLine")]
        private static extern void DLL_DrawVerticalLine(uint theColor, int x, int y1, int y2);

        /// <summary>
        /// Draws a vertical line on the screen
        /// </summary>
        /// <param name="theColor">The color to draw the line</param>
        /// <param name="x">The color to draw the line</param>
        /// <param name="y1">The starting y value of the line</param>
        /// <param name="y2">The ending y value of the line</param>
        public static void DrawVerticalLine(Color theColor, int x, int y1, int y2)
        {
            int color = theColor.ToArgb();
            DLL_DrawVerticalLine((uint)color, x, y1, y2);
        }

        [DllImport("SGSDK.dll", EntryPoint = "DrawCircle")]
        private static extern void DLL_DrawCircle(uint theColor, bool filled, int xc, int yc, int radius);

        /// <summary>
        /// Draws a circle centered on a given x, y location
        /// </summary>
        /// <param name="theColor">The color to draw the circle</param>
        /// <param name="filled">True to draw a filled circle, false for outline</param>
        /// <param name="xc">The x location of the center of the circle</param>
        /// <param name="yc">The y location of the center of the circle</param>
        /// <param name="radius">The radius of the circle</param>
        public static void DrawCircle(Color theColor, bool filled, int xc, int yc, int radius)
        {
            int color = theColor.ToArgb();
            DLL_DrawCircle((uint)color, filled, xc, yc, radius);
        }

        /// <summary>
        /// Draws a circle outline centered on a given x, y location
        /// </summary>
        /// <param name="theColour">The color to draw the circle</param>
        /// <param name="xc">The x location of the center of the circle</param>
        /// <param name="yc">The y location of the center of the circle</param>
        /// <param name="radius">The radius of the circle</param>
        public static void DrawCircle(Color theColour, int xc, int yc, int radius)
        {
            int color = theColour.ToArgb();
            DLL_DrawCircle((uint)color, false, xc, yc, radius);
        }

        /// <summary>
        /// Draws a filled circle centered on a given x, y location
        /// </summary>
        /// <param name="theColour">The color to draw the circle</param>
        /// <param name="xc">The x location of the center of the circle</param>
        /// <param name="yc">The y location of the center of the circle</param>
        /// <param name="radius">The radius of the circle</param>
        public static void FillCircle(Color theColour, int xc, int yc, int radius)
        {
            int color = theColour.ToArgb();
            DLL_DrawCircle((uint)color, true, xc, yc, radius);
        }

        [DllImport("SGSDK.dll", EntryPoint = "DrawEllipse")]
        private static extern void DLL_DrawEllipse(uint theColor, bool filled, int xPos, int yPos, int width, int height);

        /// <summary>
        /// Draws a ellipse within a given rectangle on the screen
        /// </summary>
        /// <param name="theColor">The color to draw the ellipse</param>
        /// <param name="filled">True to draw a filled ellipse, false for outline</param>
        /// <param name="xPos">The x location of the top left of the ellipse</param>
        /// <param name="yPos">The y location of the top left of the ellipse</param>
        /// <param name="width">The width of the ellipse</param>
        /// <param name="height">The height of the ellipse</param>
        public static void DrawEllipse(Color theColor, bool filled, int xPos, int yPos, int width, int height)
        {
            int color = theColor.ToArgb();
            DLL_DrawEllipse((uint)color, filled, xPos, yPos, width, height);
        }

        /// <summary>
        /// Draws a ellipse outline within a given rectangle on the screen
        /// </summary>
        /// <param name="theColor">The color to draw the ellipse</param>
        /// <param name="xPos">The x,y location of the top left of the ellipse</param>
        /// <param name="yPos">The y location of the top left of the ellipse</param>
        /// <param name="width">The width and height of the ellipse</param>
        /// <param name="height">The height of the ellipse</param>
        public static void DrawEllipse(Color theColor, int xPos, int yPos, int width, int height)
        {
            int color = theColor.ToArgb();
            DLL_DrawEllipse((uint)color, false, xPos, yPos, width, height);
        }

        /// <summary>
        /// Draws a filled ellipse within a given rectangle on the screen
        /// </summary>
        /// <param name="theColor">The color to draw the ellipse</param>
        /// <param name="xPos">The x location of the top left of the ellipse</param>
        /// <param name="yPos">The y location of the top left of the ellipse</param>
        /// <param name="width">The width of the ellipse</param>
        /// <param name="height">The height of the ellipse</param>
        public static void FillEllipse(Color theColor, int xPos, int yPos, int width, int height)
        {
            int color = theColor.ToArgb();
            DLL_DrawEllipse((uint)color, true, xPos, yPos, width, height);
        }

        [DllImport("SGSDK.dll", EntryPoint = "CreateSprite")]
        private static extern IntPtr DLL_CreateSprite(IntPtr startBitmap);

        /// <summary>
        /// Creates a sprites, and sets its firat bitmap
        /// </summary>
        /// <param name="startBitmap">The sprites first bitmap (index 0)</param>
        /// <returns>A new sprite with this bitmap as its first bitmap</returns>
        public static Sprite CreateSprite(Bitmap startBitmap)
        {
            Sprite result;
            result.Pointer = DLL_CreateSprite(startBitmap.pointer);
            return result;
        }

        [DllImport("SGSDK.dll", EntryPoint = "FreeSprite")]
        private static extern IntPtr DLL_FreeSprite(ref IntPtr spriteToFree);

        /// <summary>
        /// Frees a sprite, this does not free the sprite's bitmaps, which allows
        ///	bitmaps to be shared between sprites. All created sprites need to be
        ///	freed.
        /// </summary>
        /// <param name="spriteToFree">the sprite to free</param>
        public static void FreeSprite(ref Sprite spriteToFree)
        {
            DLL_FreeSprite(ref spriteToFree.Pointer);
        }

        [DllImport("SGSDK.dll", EntryPoint = "AddBitmapToSprite")]
        private static extern int DLL_AddBitmapToSprite(IntPtr spriteToAddTo, IntPtr bitmapToAdd);

        /// <summary>
        /// Sprites may contain multiple images. These images can be used for things
        ///	line animation, facing, etc. This routine adds a bitmap to a sprite,
        ///	returning the index of the added bitmap.
        /// </summary>
        /// <param name="spriteToAddTo">the sprite to add the bitmap to</param>
        /// <param name="bitmapToAdd">the bitmap to add to the sprite</param>
        /// <returns>the index of the added bitmap</returns>
        public static int AddBitmapToSprite(Sprite spriteToAddTo, Bitmap bitmapToAdd)
        {
            return DLL_AddBitmapToSprite(spriteToAddTo.Pointer, bitmapToAdd.pointer);
        }

        [DllImport("SGSDK.dll", EntryPoint = "CurrentHeight")]
        private static extern int DLL_CurrentHeight(IntPtr sprite);

        /// <summary>
        /// Returns the current height of the sprite
        /// </summary>
        /// <param name="sprite">The sprite to get the height of</param>
        /// <returns>The height of the sprite's current frame</returns>
        public static int CurrentHeight(Sprite sprite)
        {
            return DLL_CurrentHeight(sprite.Pointer);
        }

        [DllImport("SGSDK.dll", EntryPoint = "CurrentWidth")]
        private static extern int DLL_CurrentWidth(IntPtr sprite);
        
        /// <summary>
        /// Returns the current width of the sprite
        /// </summary>
        /// <param name="sprite">The sprite to get the width of</param>
        /// <returns>The width of the sprite's current frame</returns>
        public static int CurrentWidth(Sprite sprite)
        {
            return DLL_CurrentWidth(sprite.Pointer);
        }

        [DllImport("SGSDK.dll", EntryPoint = "DrawSprite")]
        private static extern void DLL_DrawSprite(IntPtr spriteToDraw, int vwPrtX, int vwPrtY, int vwPrtWidth, int vwPrtHeight);

        /// <summary>
        /// Draws the sprite to the screen within a given view port
        /// </summary>
        /// <param name="spriteToDraw">The sprite to be drawn</param>
        /// <param name="vwPrtX">The x of the current view port (i.e. screen)</param>
        /// <param name="vwPrtY">The y of the current view port (i.e. screen)</param>
        /// <param name="vwPrtWidth">The width of the view port</param>
        /// <param name="vwPrtHeight">The height of the view port</param>
        public static void DrawSprite(Sprite spriteToDraw, int vwPrtX, int vwPrtY, int vwPrtWidth, int vwPrtHeight)
        {
            DLL_DrawSprite(spriteToDraw.Pointer, vwPrtX, vwPrtY, vwPrtWidth, vwPrtHeight);
        }

        /// <summary>
        /// Draws a sprite to the screen, without using a view port
        /// </summary>
        /// <param name="spriteToDraw">The sprite to be drawn</param>
        public static void DrawSprite(Sprite spriteToDraw)
        {
            DLL_DrawSprite(spriteToDraw.Pointer, 0, 0, 0, 0);
        }

        [DllImport("SGSDK.dll", EntryPoint = "MoveSprite")]
        private static extern void DLL_MoveSprite(IntPtr spriteToMove, Vector movementVector);

        /// <summary>
        /// Moves a sprite based on information in a movement vector
        /// </summary>
        /// <param name="spriteToMove">The sprite to move</param>
        /// <param name="movementVector">The vector containing the movement details</param>
        public static void MoveSprite(Sprite spriteToMove, Vector movementVector)
        {
            DLL_MoveSprite(spriteToMove.Pointer, movementVector);
        }

        [DllImport("SGSDK.dll", EntryPoint = "MoveSpriteTo")]
        private static extern void DLL_MoveSpriteTo(IntPtr spriteToMove, int x, int y);

        /// <summary>
        /// Moves a sprite to a given x,y location
        /// </summary>
        /// <param name="spriteToMove">the sprite being moved</param>
        /// <param name="x">the new location of the sprite</param>
        /// <param name="y">the new location of the sprite</param>
        public void MoveSpriteTo(Sprite spriteToMove, int x, int y)
        {
            DLL_MoveSpriteTo(spriteToMove.Pointer, x, y);
        }

        [DllImport("SGSDK.dll", EntryPoint = "IsSpriteOffscreen")]
        private static extern int DLL_IsSpriteOffscreen(IntPtr theSprite);

        /// <summary>
        /// Determines if a sprite is off the screen
        /// </summary>
        /// <param name="theSprite">The sprite to check the position of</param>
        /// <returns>True if the sprite is off the screen</returns>
        public static bool IsSpriteOffscreen(Sprite theSprite)
        {
            if (DLL_IsSpriteOffscreen(theSprite.Pointer) == -1)
            {
                return true;
            }
            else
            {
                return false;
            }
        }

        [DllImport("SGSDK.dll", EntryPoint = "IsSpriteOffscreenWithViewPort")]
        private static extern int DLL_IsSpriteOffscreenWithViewPort(IntPtr theSprite, int vwPrtX, int vwPrtY, int vwPrtWidth, int vwPrtHeight);

        /// <summary>
        /// Determines if a sprite is off the screen. The view port of the screen
        ///	is defined in the vwPrt... parameters
        /// </summary>
        /// <param name="theSprite">The sprite to check the position of</param>
        /// <param name="vwPrtX">The x of the current view port (i.e. screen)</param>
        /// <param name="vwPrtY">The y of the current view port (i.e. screen)</param>
        /// <param name="vwPrtWidth">The width of the view port</param>
        /// <param name="vwPrtHeight">The height of the view port</param>
        /// <returns>True if the sprite is off the screen</returns>
        public static bool IsSpriteOffscreen(Sprite theSprite, int vwPrtX, int vwPrtY, int vwPrtWidth, int vwPrtHeight)
        {
            if (DLL_IsSpriteOffscreenWithViewPort(theSprite.Pointer, vwPrtX, vwPrtY, vwPrtWidth, vwPrtHeight) == -1)
            {
                return true;
            }
            else
            {
                return false;
            }
        }
    }
}
