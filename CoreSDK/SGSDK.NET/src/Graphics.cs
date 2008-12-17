//-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
// 					Graphics
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// The Graphics unit is responsible for all of the drawing
// of anything to the screen or other surfaces. The
// ...OnScreen routines draw directly onto the screen
// ignoring the camera settings. The standard draw routines
// draw to the screen using the camera settings. Finally the
// overloaded drawing methods with a destination Bitmap will
// draw onto the supplied bitmap.
//
// Change History:
//
// Version 2.0:
// - 2008-12-09: Andrew: Changed Triangle to an array.
//
// Version 1.1:
// - 2008-04-02: Andrew: Fixed GetPixel and GetPixel from Screen
// - 2008-03-09: Andrew: Added DrawSprite with offsets
// - 2008-02-16: Andrew: Added GetPixel and GetPixelFromScreen
// - 2008-01-30: Andrew: Fixed String Marshalling and Free
// - 2008-01-30: James: Fixed DLL calls, fixed bitmap returning from sprite
// - 2008-01-29: Andrew: Removed ref from Free
// - 2008-01-24: Andrew: Moved DLL calls together (some), adding Clipping
// - 2008-01-24: Stephen: Added Comments
// - 2008-01-23: James : Fixed exceptions for functions
//               Changed comments
// - 2008-01-23: Andrew: Fixed exceptions
//               Added changes for 1.1 compatibility
//               Refactored some methods, to limit routines exposed by DLL
//               Added extra comments, and fixed code layout and line endings.
//               Using SwinGamePointer for safer management of pointers.
// Version 1.0:
// - Various

using System;
using System.Runtime.InteropServices;
using System.Drawing;

namespace SwinGame
{
    /// <summary>
    /// The Graphics Class enables most of the Drawing features of SGSDK
    /// </summary>
    public class Graphics
    {
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SetClip")]
        private static extern void DLL_SetClip(IntPtr bmp, int x, int y, int width, int height);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ResetClip")]
        private static extern void DLL_ResetClip(IntPtr bmp);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetPixel")]
        private static extern int DLL_GetPixel(IntPtr bmp, int x, int y);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetPixelFromScreen")]
        private static extern int DLL_GetPixelFromScreen(int x, int y);


        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        //private static extern int GetSpritehasEnded(IntPtr pointer);

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        //private static extern int GetSpriteReverse(IntPtr pointer);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CreateBitmap")]
        private static extern IntPtr DLL_CreateBitmap(int width, int height);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "OptimiseBitmap")]
        private static extern void DLL_OptimiseBitmap(IntPtr surface);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "LoadBitmapWithTransparentColor", CharSet = CharSet.Ansi)]
        private static extern IntPtr DLL_LoadBitmapWithTransparentColor([MarshalAs(UnmanagedType.LPStr)]String pathToBitmap, int transparent, uint transparentColor);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "LoadTransparentBitmap", CharSet = CharSet.Ansi)]
        private static extern IntPtr DLL_LoadTransparentBitmap([MarshalAs(UnmanagedType.LPStr)]string pathToBitmap, uint transparentColor);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetBitmapWidth")]
        private static extern int DLL_GetBitmapWidth(IntPtr targetbitmap);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetBitmapHeight")]
        private static extern int DLL_GetBitmapHeight(IntPtr targetbitmap);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ClearSurfaceWithColor")]
        private static extern void DLL_ClearSurfaceWithColor(IntPtr dest, uint toColour);

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawBitmapWithDestination")]
        private static extern void DLL_DrawBitmapWithDestination(IntPtr dest, IntPtr bitmapToDraw, int x, int y);

        /// <summary>
        /// Create a blank bitmap of the given size. This is useful for caching drawing, 
        /// for slower drawing operations.Most of the Drawing routines provide an 
        /// option to specify what bitmap to draw onto.
        /// </summary>
        /// <param name="width">Width of a bitmap</param>
        /// <param name="height">Height of a bitmap</param>
        /// <returns>New bitmap</returns>
        public static Bitmap CreateBitmap(int width, int height)
        {
            return new Bitmap(width, height);
        }

        /// <summary>
        /// Optimise the specified bitmap. This will make the bitmap faster to be drawn to the screen
        /// </summary>
        /// <param name="surface">Bitmap to optimise</param>
        public static void OptimiseBitmap(Bitmap surface)
        {
            surface.Optimise();
        }

        /// <summary>
        /// Load the specified image file. Use the GetPathToResource methods from Core to
        /// ensure that you load the file in a platform neutral way, enabling your game
        /// to run on Windows, Mac, and Linux.
        /// </summary>
        /// <param name="pathToBitmap">Path to the image file</param>
        /// <returns>New bitmap</returns>
        public static Bitmap LoadBitmap(string pathToBitmap)
        {
            return new Bitmap(pathToBitmap);
        }

        /// <summary>
        /// Load the specified image file with a transparent color. Use the GetPathToResource methods from Core to
        /// ensure that you load the file in a platform neutral way, enabling your game
        /// to run on Windows, Mac, and Linux.
        /// </summary>
        /// <param name="pathToBitmap">Path to the image file</param>
        /// <param name="transparent">True if this image has transparent pixels</param>
        /// <param name="transparentColor">Color of the transparent pixels</param>
        /// <returns>New bitmap</returns>
        public static Bitmap LoadBitmap(String pathToBitmap, bool transparent, Color transparentColor)
        {
            return new Bitmap(pathToBitmap, transparent, transparentColor);
        }

        /// <summary>
        /// Load an image with transparency. Use the GetPathToResource methods from Core to
        /// ensure that you load the file in a platform neutral way, enabling your game
        /// to run on Windows, Mac, and Linux.
        /// </summary>
        /// <param name="pathToBitmap">Path to the image file</param>
        /// <param name="transparentColor">Color of the transparent pixels</param>
        /// <returns>New bitmap</returns>
        public static Bitmap LoadTransparentBitmap(string pathToBitmap, Color transparentColor)
        {
            return new Bitmap(pathToBitmap, transparentColor);
        }

        /// <summary>
        /// Frees a Bitmap From Memory. You need to ensure that all bitmaps
        /// that you load are freed by the end of the game. This is usually done
        /// when the program exits.
        /// </summary>
        /// <param name="bitmapToFree">Bitmap to free</param>
        public static void FreeBitmap(Bitmap bitmapToFree)
        {
            bitmapToFree.Dispose();
        }

        /// <summary>
        /// Get the specified bitmap's width
        /// </summary>
        /// <param name="targetbitmap">Target bitmap</param>
        /// <returns>Width of the bitmap</returns>
        public static int GetBitmapWidth(Bitmap targetbitmap)
        {
            return targetbitmap.Width;
        }

        /// <summary>
        /// Get the specified bitmap's height
        /// </summary>
        /// <param name="targetbitmap">Target bitmap</param>
        /// <returns>Height of the bitmap</returns>
        public static int GetBitmapHeight(Bitmap targetbitmap)
        {
            return targetbitmap.Height;
        }

        /// <summary>
        /// Clear the bitmap with the specified color.
        /// </summary>
        /// <param name="dest">Bitmap to clear</param>
        /// <param name="toColour">The color used to clear</param>
        public static void ClearSurface(Bitmap dest, Color toColour)
        {
            dest.Clear(toColour);
        }

        /// <summary>
        /// Clear the bitmap to black
        /// </summary>
        /// <param name="dest">Bitmap to clear</param>
        public static void ClearSurface(Bitmap dest)
        {
            dest.Clear();
        }

        /// <summary>
        /// Draw bitmap to the specified bitmap. You will often use the bitmap from CreateBitmap to draw on.
        /// </summary>
        /// <param name="dest">Bitmap to draw on</param>
        /// <param name="bitmapToDraw">Bitmap to draw</param>
        /// <param name="x">X coordinate</param>
        /// <param name="y">Y coordinate</param>
        public static void DrawBitmap(Bitmap dest, Bitmap bitmapToDraw, int x, int y)
        {
            bitmapToDraw.DrawOnto(dest, x, y);
        }

        /// <summary>
        /// Draw bitmap to the specified bitmap. You will often use the bitmap from CreateBitmap to draw on.
        /// </summary>
        /// <param name="dest">Bitmap to draw on</param>
        /// <param name="bitmapToDraw">Bitmap to draw</param>
        /// <param name="position">Position to Draw to</param>
        public static void DrawBitmap(Bitmap dest, Bitmap bitmapToDraw, Point2D position)
        {
            DrawBitmap(dest, bitmapToDraw, (int)position.X, (int)position.Y);
        }

        /// <summary>
        /// Draw bitmap to the specified bitmap. You will often use the bitmap from CreateBitmap to draw on.
        /// </summary>
        /// <param name="bitmapToDraw">Bitmap to draw</param>
        /// <param name="position">Position to Draw to</param>
        public static void DrawBitmap(Bitmap bitmapToDraw, Point2D position)
        {
            DrawBitmap(bitmapToDraw, (int)position.X, (int)position.Y);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawBitmapPartWithDestination")]
        //private static extern void DLL_DrawBitmapPartWithDestination(IntPtr dest, IntPtr bitmapToDraw, int srcX, int srcY, int srcW, int srcH, int x, int y);

        /// <summary>
        /// Draws part of a bitmap (bitmapToDraw) onto another bitmap (dest), this is faster than drawing 
        /// the whole bitmap
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
            bitmapToDraw.DrawPartOnto(dest, srcX, srcY, srcW, srcH, x, y);
        }

        /// <summary>
        /// Draws part of a bitmap (bitmapToDraw) onto another bitmap (dest), this is faster than drawing 
        /// the whole bitmap
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="bitmapToDraw">The bitmap to be drawn onto the destination</param>
        /// <param name="source">The possition and size of the bitmapToDraw</param>
        /// <param name="x">The x location to draw the bitmap part to</param>
        /// <param name="y">The y location to draw the bitmap part to</param>
        public static void DrawBitmapPart(Bitmap dest, Bitmap bitmapToDraw, Rectangle source, int x, int y)
        {
            DrawBitmapPart(dest, bitmapToDraw, source.X, source.Y, source.Width, source.Height, x, y);
        }

        /// <summary>
        /// Draws part of a bitmap (bitmapToDraw) onto another bitmap (dest), this is faster than drawing 
        /// the whole bitmap
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="bitmapToDraw">The bitmap to be drawn onto the destination</param>
        /// <param name="source">The possition and size of the bitmapToDraw</param>
        /// <param name="position">The x,y location to draw the bitmap part to</param>
        public static void DrawBitmapPart(Bitmap dest, Bitmap bitmapToDraw, Rectangle source, Point2D position)
        {
            DrawBitmapPart(dest, bitmapToDraw, source.X, source.Y, source.Width, source.Height, (int)position.X, (int)position.Y);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawBitmap")]
        //private static extern void DLL_DrawBitmap(IntPtr bitmapToDraw, float x, float y);

        /// <summary>
        /// Draws one bitmap (bitmapToDraw) onto the screen, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="bitmapToDraw">The bitmap to be drawn onto the screen</param>
        /// <param name="x">The x location to draw the bitmap to</param>
        /// <param name="y">The y location to draw the bitmap to</param>
        public static void DrawBitmap(Bitmap bitmapToDraw, float x, float y)
        {
            bitmapToDraw.Draw(x, y);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawBitmapPart")]
        //private static extern void DLL_DrawBitmapPart(IntPtr bitmapToDraw, int srcX, int srcY, int srcW, int srcH, float x, float y);

        /// <summary>
        /// Draws part of a bitmap (bitmapToDraw) onto the screen, x and y are in game 
        /// coordinates not screen coordinates, this is faster than DrawBitmap
        /// if you don't want all of the bitmap to be shown
        /// </summary>
        /// <param name="bitmapToDraw">The bitmap to be drawn onto the screen</param>
        /// <param name="srcX">The x offset to the area to copy in bitmapToDraw</param>
        /// <param name="srcY">The y offset to the area to copy in bitmapToDraw</param>
        /// <param name="srcW">The width of the area to copy</param>
        /// <param name="srcH">The height of the area to copy</param>
        /// <param name="x">The x location to draw the bitmap part to</param>
        /// <param name="y">The y location to draw the bitmap part to</param>
        public static void DrawBitmapPart(Bitmap bitmapToDraw, int srcX, int srcY, int srcW, int srcH, float x, float y)
        {
            bitmapToDraw.DrawPart(srcX, srcY, srcW, srcH, x, y);
        }

        /// <summary>
        /// Draws part of a bitmap (bitmapToDraw) onto the screen, x and y are in game 
        /// coordinates not screen coordinates, this is faster than DrawBitmap
        /// if you don't want all of the bitmap to be shown
        /// </summary>
        /// <param name="bitmapToDraw">The bitmap to be drawn onto the screen</param>
        /// <param name="source">The possition and size of the bitmapToDraw</param>
        /// <param name="x">The x location to draw the bitmap part to</param>
        /// <param name="y">The y location to draw the bitmap part to</param>
        public static void DrawBitmapPart(Bitmap bitmapToDraw, Rectangle source, float x, float y)
        {
            DrawBitmapPart(bitmapToDraw, source.X, source.Y, source.Width, source.Height, x, y);
        }

        /// <summary>
        /// Draws part of a bitmap (bitmapToDraw) onto the screen, x and y are in game 
        /// coordinates not screen coordinates, this is faster than DrawBitmap
        /// if you don't want all of the bitmap to be shown
        /// </summary>
        /// <param name="bitmapToDraw">The bitmap to be drawn onto the screen</param>
        /// <param name="source">The possition and size of the bitmapToDraw</param>
        /// <param name="position">The x,y location to draw the bitmap part to</param>
        public static void DrawBitmapPart(Bitmap bitmapToDraw, Rectangle source, Point2D position)
        {
            DrawBitmapPart(bitmapToDraw, source.X, source.Y, source.Width, source.Height, position.X, position.Y);
        }

        /// <summary>
        /// Get the color of the pixel in the bitmap.
        /// </summary>
        /// <param name="from">the bitmap to get the color from</param>
        /// <param name="x">the x location of the pixel</param>
        /// <param name="y">the y location of the pixel</param>
        /// <returns>the color of the pixel in the bitmap at location x,y</returns>
        public static Color GetPixel(Bitmap from, int x, int y)
        {
            return from.GetPixel(x, y);
        }

        /// <summary>
        /// Get the color of a pixel on the screen.
        /// </summary>
        /// <param name="x">the x location of the pixel</param>
        /// <param name="y">the y location of the pixel</param>
        /// <returns>the color of the pixel at the x,y location</returns>
        public static Color GetPixelFromScreen(int x, int y)
        {
            return Color.FromArgb((int)SGSDK.GetPixelFromScreen(x, y));
        }


        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawPixelWithDestination")]
        //private static extern void DLL_DrawPixelWithDestination(IntPtr dest, uint theColour, int x, int y);
        
        /// <summary>
        /// Draws a pixel onto the destination bitmap, You will often use 
        /// the bitmap from CreateBitmap to draw on.
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the pixel</param>
        /// <param name="x">The x location to draw the pixel at</param>
        /// <param name="y">The y location to draw the pixel at</param>
        public static void DrawPixel(Bitmap dest, Color theColour, int x, int y)
        {
            int color = theColour.ToArgb();
            SGSDK.DrawPixelWithDestination(dest.pointer, (uint)color, x, y);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawRectangleWithDestination")]
        //private static extern void DLL_DrawRectangleWithDestination(IntPtr dest, uint theColour, int filled, int xPos, int yPos, int width, int height);

        /// <summary>
        /// Draws a rectangle on the destination bitmap, You will often use the 
        /// bitmap from CreateBitmap to draw on.
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
            SGSDK.DrawRectangleWithDestination(dest.pointer, (uint)color, (filled ? -1 : 0), xPos, yPos, width, height);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "FillRectangleWithDestination")]
        //private static extern void DLL_FillRectangleWithDestination(IntPtr dest, uint theColour, int xPos, int yPos, int width, int height);

        /// <summary>
        /// Draws a filled rectangle on the destination bitmap, You will often use 
        /// the bitmap from CreateBitmap to draw on.
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
            SGSDK.FillRectangleWithDestination(dest.pointer, (uint)color, xPos, yPos, width, height);
        }

        /// <summary>
        /// Draws the outline of a rectangle on the destination bitmap, You will often use 
        /// the bitmap from CreateBitmap to draw on.
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
            SGSDK.DrawRectangleWithDestination(dest.pointer, (uint)color, 0, xPos, yPos, width, height);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawLineWithDestination")]
        //private static extern void DLL_DrawLineWithDestination(IntPtr dest, uint theColour, int xPosStart, int yPosStart, int xPosEnd, int yPosEnd);

        /// <summary>
        /// Draws a line on the destination bitmap, You will often use the 
        /// bitmap from CreateBitmap to draw on.
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
            SGSDK.DrawLineWithDestination(dest.pointer, (uint)color, xPosStart, yPosStart, xPosEnd, yPosEnd);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawHorizontalLineWithDestination")]
        //private static extern void DLL_DrawHorizontalLineWithDestination(IntPtr dest, uint theColour, int y, int x1, int x2);

        /// <summary>
        /// Draws a horizontal line on the destination bitmap, You will often use the 
        /// bitmap from CreateBitmap to draw on.
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the line</param>
        /// <param name="y">The y location of the line</param>
        /// <param name="x1">The starting x value of the line</param>
        /// <param name="x2">The starting y value of the line</param>
        public static void DrawHorizontalLine(Bitmap dest, Color theColour, int y, int x1, int x2)
        {
            int color = theColour.ToArgb();
            SGSDK.DrawHorizontalLineWithDestination(dest.pointer, (uint)color, y, x1, x2);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawVerticalLineWithDestination")]
        //private static extern void DLL_DrawVerticalLineWithDestination(IntPtr dest, uint theColour, int x, int y1, int y2);

        /// <summary>
        /// Draws a vertical line on the destination bitmap, You will often use the 
        /// bitmap from CreateBitmap to draw on.
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the line</param>
        /// <param name="x">The x location of the line</param>
        /// <param name="y1">The starting y value of the line</param>
        /// <param name="y2">The ending y value of the line</param>
        public static void DrawVerticalLine(Bitmap dest, Color theColour, int x, int y1, int y2)
        {
            int color = theColour.ToArgb();
            SGSDK.DrawVerticalLineWithDestination(dest.pointer, (uint)color, x, y1, y2);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawCircleWithDestination")]
        //private static extern void DLL_DrawCircleWithDestination(IntPtr dest, uint theColour, int filled, int xc, int yc, int radius);

        /// <summary>
        /// Draws a circle centered on a given x, y locationon on the destination bitmap, 
        /// You will often use the bitmap from CreateBitmap to draw on.
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
            SGSDK.DrawCircleWithDestination(dest.pointer, (uint)color, (filled ? -1 : 0), xc, yc, radius);
        }

        /// <summary>
        /// Draws a circle centered on a given x, y locationon on the destination bitmap, 
        /// You will often use the bitmap from CreateBitmap to draw on.
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the circle</param>
        /// <param name="filled">True to draw a filled circle, false for outline</param>
        /// <param name="position">The x,y location of the center of the circle</param>
        /// <param name="radius">The radius of the circle</param>
        public static void DrawCircle(Bitmap dest, Color theColour, bool filled, Point2D position, int radius)
        {
            DrawCircle(dest, theColour, filled, (int)position.X, (int)position.Y, radius);
        }


        /// <summary>
        /// Draws a circle outline centered on a given x, y location on the destination bitmap, 
        /// You will often use the bitmap from CreateBitmap to draw on.
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the circle</param>
        /// <param name="xc">The x location of the center of the circle</param>
        /// <param name="yc">The y location of the center of the circle</param>
        /// <param name="radius">The radius of the circle</param>
        public static void DrawCircle(Bitmap dest, Color theColour, int xc, int yc, int radius)
        {
            int color = theColour.ToArgb();
            SGSDK.DrawCircleWithDestination(dest.pointer, (uint)color, 0, xc, yc, radius);
        }

        /// <summary>
        /// Draws a circle outline centered on a given x, y location on the destination bitmap, 
        /// You will often use the bitmap from CreateBitmap to draw on.
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the circle</param>
        /// <param name="position">The x,y location of the center of the circle</param>
        /// <param name="radius">The radius of the circle</param>
        public static void DrawCircle(Bitmap dest, Color theColour, Point2D position, int radius)
        {
            DrawCircle(dest, theColour, (int)position.X, (int)position.Y, radius);
        }

        /// <summary>
        /// Draws a filled circle centered on a given x, y location on the destination bitmap, 
        /// You will often use the bitmap from CreateBitmap to draw on.
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the circle</param>
        /// <param name="xc">The x location of the center of the circle</param>
        /// <param name="yc">The y location of the center of the circle</param>
        /// <param name="radius">The radius of the circle</param>
        public static void FillCircle(Bitmap dest, Color theColour, int xc, int yc, int radius)
        {
            int color = theColour.ToArgb();
            SGSDK.DrawCircleWithDestination(dest.pointer, (uint)color, -1, xc, yc, radius);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawEllipseWithDestination")]
        //private static extern void DLL_DrawEllipseWithDestination(IntPtr dest, uint theColour, int filled, int xPos, int yPos, int width, int height);

        /// <summary>
        /// Draws a ellipse within a given rectangle on the destination bitmap, 
        /// You will often use the bitmap from CreateBitmap to draw on.
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
            SGSDK.DrawEllipseWithDestination(dest.pointer, (uint)color, (filled ? -1 : 0), xPos, yPos, width, height);
        }

        /// <summary>
        /// Draws a ellipse within a given rectangle on the destination bitmap, 
        /// You will often use the bitmap from CreateBitmap to draw on.
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the ellipse</param>
        /// <param name="filled">True to draw a filled ellipse, false for outline</param>
        /// <param name="source">The locations and size to draw the ellipse to</param>
        public static void DrawEllipse(Bitmap dest, Color theColour, bool filled, Rectangle source)
        {
            DrawEllipse(dest, theColour, filled, source.X, source.Y, source.Width, source.Height);
        }

        /// <summary>
        /// Draws a ellipse outline within a given rectangle on the destination bitmap, 
        /// You will often use the bitmap from CreateBitmap to draw on.
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
            SGSDK.DrawEllipseWithDestination(dest.pointer, (uint)color, 0, xPos, yPos, width, height);
        }

        /// <summary>
        /// Draws a ellipse outline within a given rectangle on the destination bitmap, 
        /// You will often use the bitmap from CreateBitmap to draw on.
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the ellipse</param>
        /// <param name="source">The locations and size to draw the ellipse to</param>
        public static void DrawEllipse(Bitmap dest, Color theColour, Rectangle source)
        {
            DrawEllipse(dest, theColour, source.X, source.Y, source.Width, source.Height);
        }

        /// <summary>
        /// Draws a filled ellipse within a given rectangle on the destination bitmap, 
        /// You will often use the bitmap from CreateBitmap to draw on.
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
            SGSDK.DrawEllipseWithDestination(dest.pointer, (uint)color, -1, xPos, yPos, width, height);
        }

        /// <summary>
        /// Draws a filled ellipse within a given rectangle on the destination bitmap, 
        /// You will often use the bitmap from CreateBitmap to draw on.
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the ellipse</param>
        /// <param name="source">The locations and size to draw the ellipse to</param>
        public static void FillEllipse(Bitmap dest, Color theColour, Rectangle source)
        {
            FillEllipse(dest, theColour, source.X, source.Y, source.Width, source.Height);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ClearScreen")]
        //private static extern void DLL_ClearScreen(uint toColour);

        /// <summary>
        /// Clears the surface of the screen to the passed in color, it is usefull to call this
        /// every time you call RefreshScreen
        /// </summary>
        /// <param name="toColour">The colour to clear the bitmap to</param>
        public static void ClearScreen(Color toColour)
        {
            int color = toColour.ToArgb();
            SGSDK.ClearScreen((uint)color);
        }

        /// <summary>
        /// Clears the screen to Black, it is usefull to call this
        /// every time you call RefreshScreen
        /// </summary>
        public static void ClearScreen()
        {
            int color = Color.Black.ToArgb();
            SGSDK.ClearScreen((uint)color);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawPixel")]
        //private static extern void DLL_DrawPixel(uint theColour, float x, float y);

        /// <summary>
        /// Draws a pixel onto the screen at the give location, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="theColour">The color to draw the pixel</param>
        /// <param name="x">The x location to draw the pixel at</param>
        /// <param name="y">The y location to draw the pixel at</param>
        public static void DrawPixel(Color theColour, float x, float y)
        {
            int color = theColour.ToArgb();
            SGSDK.DrawPixel((uint)color, x, y);
        }

        /// <summary>
        /// Draws a pixel onto the screen at the given location
        /// </summary>
        /// <param name="dest">Destination Bitmap</param>
        /// <param name="theColor">Color of the Pixel</param>
        /// <param name="position">Position of Pixel</param>
        public static void DrawPixel(Bitmap dest, Color theColor, Point2D position)
        {
            DrawPixel(dest, theColor, (int)position.X, (int)position.Y);
        }

        /// <summary>
        /// Draws a pixel onto the screen at the given location
        /// </summary>
        /// <param name="theColor"></param>
        /// <param name="position"></param>
        public static void DrawPixel(Color theColor, Point2D position)
        {
            DrawPixel(theColor, (int)position.X, (int)position.Y);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawRectangle")]
        //private static extern void DLL_DrawRectangle(uint theColour, int filled, float xPos, float yPos, int width, int height);

        /// <summary>
        /// Draws a rectangle on the screen at the give location, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="theColour">The color to draw the rectangle</param>
        /// <param name="filled">True to draw a filled rectangle, false for outline</param>
        /// <param name="xPos">The x location to draw the rectangle at</param>
        /// <param name="yPos">The y location to draw the rectangle at</param>
        /// <param name="width">The width of the rectangle</param>
        /// <param name="height">The height of the rectangle</param>
        public static void DrawRectangle(Color theColour, bool filled, float xPos, float yPos, int width, int height)
        {
            int color = theColour.ToArgb();
            SGSDK.DrawRectangle((uint)color, (filled ? -1 : 0), xPos, yPos, width, height);
        }

        /// <summary>
        /// Draws the outline of a rectangle on the screen at the give location, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="theColour">The color to draw the rectangle</param>
        /// <param name="xPos">The x location to draw the rectangle at</param>
        /// <param name="yPos">The y location to draw the rectangle at</param>
        /// <param name="width">The width of the rectangle</param>
        /// <param name="height">The height of the rectangle</param>
        public static void DrawRectangle(Color theColour, float xPos, float yPos, int width, int height)
        {
            int color = theColour.ToArgb();
            SGSDK.DrawRectangle((uint)color, 0, xPos, yPos, width, height);
        }

        /// <summary>
        /// Draws a Rectangle on a destination bitmap with the specified Color
        /// </summary>
        /// <param name="dest">Destination Bitmap</param>
        /// <param name="theColor">Color of the Recangle</param>
        /// <param name="filled">Filled Rectangle</param>
        /// <param name="source">Rectangle</param>
        public static void DrawRectangle(Bitmap dest, Color theColor, bool filled, Rectangle source)
        {
            DrawRectangle(dest, theColor, filled, source.X, source.Y, source.Width, source.Height);
        }

        /// <summary>
        /// Draws a Rectangle on a destination bitmap with the specified Color
        /// </summary>
        /// <param name="dest">Destination Bitmap</param>
        /// <param name="theColor">Color of the Recangle</param>
        /// <param name="source">Rectangle</param>
        public static void DrawRectangle(Bitmap dest, Color theColor, Rectangle source)
        {
            DrawRectangle(dest, theColor, source.X, source.Y, source.Width, source.Height);
        }

        /// <summary>
        /// Draws a Rectangle on a destination bitmap with the specified Color
        /// </summary>
        /// <param name="theColor">Color of the Recangle</param>
        /// <param name="filled">Filled Rectangle</param>
        /// <param name="source">Rectangle</param>
        public static void DrawRectangle(Color theColor, bool filled, Rectangle source)
        {
            DrawRectangle(theColor, filled, source.X, source.Y, source.Width, source.Height);
        }

        /// <summary>
        /// Draws a Rectangle on a destination bitmap with the specified Color
        /// </summary>
        /// <param name="theColor">Color of the Recangle</param>
        /// <param name="source">Rectangle</param>
        public static void DrawRectangle(Color theColor, Rectangle source)
        {
            DrawRectangle(theColor, source.X, source.Y, source.Width, source.Height);
        }

        /// <summary>
        /// Draws a filled rectangle on the screen at the give location, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="theColour">The color to draw the rectangle</param>
        /// <param name="xPos">The x location to draw the rectangle at</param>
        /// <param name="yPos">The y location to draw the rectangle at</param>
        /// <param name="width">The width of the rectangle</param>
        /// <param name="height">The height of the rectangle</param>
        public static void FillRectangle(Color theColour, float xPos, float yPos, int width, int height)
        {
            int color = theColour.ToArgb();
            SGSDK.DrawRectangle((uint)color, -1, xPos, yPos, width, height);
        }

        /// <summary>
        /// Draws a filled rectangle on the screen at the give location, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="theColor">Color of the Rectangle</param>
        /// <param name="source">Rectangle</param>
        public static void FillRectangle(Color theColor, Rectangle source)
        {
            FillRectangle(theColor, source.X, source.Y, source.Width, source.Height);
        }

        /// <summary>
        /// Draws a filled rectangle on the screen at the give location, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="dest">Destination Bitmap</param>
        /// <param name="theColor">Color of the Rectangle</param>
        /// <param name="source">Rectangle</param>
        public static void FillRectangle(Bitmap dest, Color theColor, Rectangle source)
        {
            FillRectangle(dest, theColor, source.X, source.Y, source.Width, source.Height);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawLine")]
        //private static extern void DLL_DrawLine(uint theColour, float xPosStart, float yPosStart, float xPosEnd, float yPosEnd);

        /// <summary>
        /// Draws a line on the screen at the give location, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="theColour">The color to draw the line</param>
        /// <param name="xPosStart">The x location to start the line at</param>
        /// <param name="yPosStart">The y location to start the line at</param>
        /// <param name="xPosEnd">The x location to end the line at</param>
        /// <param name="yPosEnd">The y location to end the line at</param>
        public static void DrawLine(Color theColour, float xPosStart, float yPosStart, float xPosEnd, float yPosEnd)
        {
            int color = theColour.ToArgb();
            SGSDK.DrawLine((uint)color, xPosStart, yPosStart, xPosEnd, yPosEnd);
        }

        /// <summary>
        /// Draws a line on the screen at the given location, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="dest">Destination Bitmap</param>
        /// <param name="theColor">Color of the Line</param>
        /// <param name="line">Line</param>
        public static void DrawLine(Bitmap dest, Color theColor, LineSegment line)
        {
            DrawLine(dest, theColor, (int)line.StartPoint.X, (int)line.StartPoint.Y, (int)line.EndPoint.X, (int)line.EndPoint.Y);
        }

        /// <summary>
        /// Draws a line on the screen at the given location, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="theColor">Color of the Line</param>
        /// <param name="line">Line</param>
        public static void DrawLine(Color theColor, LineSegment line)
        {
            DrawLine(theColor, (int)line.StartPoint.X, (int)line.StartPoint.Y, (int)line.EndPoint.X, (int)line.EndPoint.Y);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawHorizontalLine")]
        //private static extern void DLL_DrawHorizontalLine(uint theColor, float y, float x1, float x2);

        /// <summary>
        /// Draws a horizontal line on the screen at the give location, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="theColor">The color to draw the line</param>
        /// <param name="y">The y location of the line</param>
        /// <param name="x1">The starting x value of the line</param>
        /// <param name="x2">The ending x value of the line</param>
        public static void DrawHorizontalLine(Color theColor, float y, float x1, float x2)
        {
            int color = theColor.ToArgb();
            SGSDK.DrawHorizontalLine((uint)color, y, x1, x2);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawVerticalLine")]
        //private static extern void DLL_DrawVerticalLine(uint theColor, float x, float y1, float y2);

        /// <summary>
        /// Draws a vertical line on the screen at the give location, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="theColor">The color to draw the line</param>
        /// <param name="x">The color to draw the line</param>
        /// <param name="y1">The starting y value of the line</param>
        /// <param name="y2">The ending y value of the line</param>
        public static void DrawVerticalLine(Color theColor, float x, float y1, float y2)
        {
            int color = theColor.ToArgb();
            SGSDK.DrawVerticalLine((uint)color, x, y1, y2);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawCircle")]
        //private static extern void DLL_DrawCircle(uint theColor, int filled, float xc, float yc, int radius);

        /// <summary>
        /// Draws a circle centered on a given x, y location, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="theColor">The color to draw the circle</param>
        /// <param name="filled">True to draw a filled circle, false for outline</param>
        /// <param name="xc">The x location of the center of the circle</param>
        /// <param name="yc">The y location of the center of the circle</param>
        /// <param name="radius">The radius of the circle</param>
        public static void DrawCircle(Color theColor, bool filled, float xc, float yc, int radius)
        {
            int color = theColor.ToArgb();
            SGSDK.DrawCircle((uint)color, (filled ? -1 : 0), xc, yc, radius);
        }

        /// <summary>
        /// Draws a circle centered on a given x, y location, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="theColor">The color to draw the circle</param>
        /// <param name="filled">True to draw a filled circle, false for outline</param>
        /// <param name="position">The x,y location of the center of the circle</param>
        /// <param name="radius">The radius of the circle</param>
        public static void DrawCircle(Color theColor, bool filled, Point2D position, int radius)
        {
            DrawCircle(theColor, filled, position.X, position.Y, radius);
        }

        /// <summary>
        /// Draws a circle outline centered on a given x, y location, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="theColour">The color to draw the circle</param>
        /// <param name="xc">The x location of the center of the circle</param>
        /// <param name="yc">The y location of the center of the circle</param>
        /// <param name="radius">The radius of the circle</param>
        public static void DrawCircle(Color theColour, float xc, float yc, int radius)
        {
            int color = theColour.ToArgb();
            SGSDK.DrawCircle((uint)color, 0, xc, yc, radius);
        }

        /// <summary>
        /// Draws a circle outline centered on a given x, y location, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="theColour">The color to draw the circle</param>
        /// <param name="position">The x,y location of the center of the circle</param>
        /// <param name="radius">The radius of the circle</param>
        public static void DrawCircle(Color theColour, Point2D position, int radius)
        {
            DrawCircle(theColour, position.X, position.Y, radius);
        }

        /// <summary>
        /// Draws a filled circle centered on a given x, y location, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="theColour">The color to draw the circle</param>
        /// <param name="xc">The x location of the center of the circle</param>
        /// <param name="yc">The y location of the center of the circle</param>
        /// <param name="radius">The radius of the circle</param>
        public static void FillCircle(Color theColour, float xc, float yc, int radius)
        {
            int color = theColour.ToArgb();
            SGSDK.DrawCircle((uint)color, -1, xc, yc, radius);
        }

        /// <summary>
        /// Draws a filled circle centered on a given x, y location, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="dest">Destination Bitmap</param>
        /// <param name="theColor">Color of the Circle</param>
        /// <param name="point">Position of the Circle</param>
        /// <param name="radius">Radius of the Circle</param>
        public static void FillCircle(Bitmap dest, Color theColor, Point2D point, int radius)
        {
            FillCircle(dest, theColor, (int)point.X, (int)point.Y, radius);
        }

        /// <summary>
        /// Draws a filled circle centered on a given x, y location, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="theColor">Color of the Circle</param>
        /// <param name="position">Position of the Circle</param>
        /// <param name="radius">Radius of the Circle</param>
        public static void FillCircle(Color theColor, Point2D position, int radius)
        {
            FillCircle(theColor, position.X, position.Y, radius);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawEllipse")]
        //private static extern void DLL_DrawEllipse(uint theColor, int filled, float xPos, float yPos, int width, int height);

        /// <summary>
        /// Draws a ellipse within a given rectangle on the screen, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="theColor">The color to draw the ellipse</param>
        /// <param name="filled">True to draw a filled ellipse, false for outline</param>
        /// <param name="xPos">The x location of the top left of the ellipse</param>
        /// <param name="yPos">The y location of the top left of the ellipse</param>
        /// <param name="width">The width of the ellipse</param>
        /// <param name="height">The height of the ellipse</param>
        public static void DrawEllipse(Color theColor, bool filled, float xPos, float yPos, int width, int height)
        {
            int color = theColor.ToArgb();
            SGSDK.DrawEllipse((uint)color, (filled ? -1 : 0), xPos, yPos, width, height);
        }

        /// <summary>
        /// Draws a ellipse within a given rectangle on the screen, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="theColor">The color to draw the ellipse</param>
        /// <param name="filled">True to draw a filled ellipse, false for outline</param>
        /// <param name="source">The location and size of the ellipse to draw</param>
        public static void DrawEllipse(Color theColor, bool filled, Rectangle source)
        {
            DrawEllipse(theColor, filled, source.X, source.Y, source.Width, source.Height);
        }

        /// <summary>
        /// Draws a ellipse outline within a given rectangle on the screen, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="theColor">The color to draw the ellipse</param>
        /// <param name="xPos">The x,y location of the top left of the ellipse</param>
        /// <param name="yPos">The y location of the top left of the ellipse</param>
        /// <param name="width">The width and height of the ellipse</param>
        /// <param name="height">The height of the ellipse</param>
        public static void DrawEllipse(Color theColor, float xPos, float yPos, int width, int height)
        {
            int color = theColor.ToArgb();
            SGSDK.DrawEllipse((uint)color, 0, xPos, yPos, width, height);
        }

        /// <summary>
        /// Draws a ellipse outline within a given rectangle on the screen, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="theColor">The color to draw the ellipse</param>
        /// <param name="source">The location and size of the ellipse to draw</param>
        public static void DrawEllipse(Color theColor, Rectangle source)
        {
            DrawEllipse(theColor, source.X, source.Y, source.Width, source.Height);
        }

        /// <summary>
        /// Draws a filled ellipse within a given rectangle on the screen, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="theColor">The color to draw the ellipse</param>
        /// <param name="source">The location and size of the ellipse to draw</param>
        public static void FillEllipse(Color theColor, Rectangle source)
        {
            FillEllipse(theColor, source.X, source.Y, source.Width, source.Height);
        }
        /// <summary>
        /// Draws a filled ellipse within a given rectangle on the screen, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="theColor">The color to draw the ellipse</param>
        /// <param name="xPos">The x location of the top left of the ellipse</param>
        /// <param name="yPos">The y location of the top left of the ellipse</param>
        /// <param name="width">The width of the ellipse</param>
        /// <param name="height">The height of the ellipse</param>
        public static void FillEllipse(Color theColor, float xPos, float yPos, int width, int height)
        {
            int color = theColor.ToArgb();
            SGSDK.DrawEllipse((uint)color, -1, xPos, yPos, width, height);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CreateSprite")]
        //private static extern IntPtr DLL_CreateSprite(IntPtr startBitmap);

        /// <summary>
        /// Creates a sprites, and sets its first bitmap, this is only good for non-animating sprites
        /// </summary>
        /// <param name="startBitmap">The sprites first bitmap (index 0)</param>
        /// <returns>A new sprite with this bitmap as its first bitmap</returns>
        public static Sprite CreateSprite(Bitmap startBitmap)
        {
            return new Sprite(startBitmap);
        }

        /// <summary>
        /// Frees a sprite, this does not free the sprite's bitmaps, which allows
        ///	bitmaps to be shared between sprites. All created sprites need to be
        ///	freed.
        /// </summary>
        /// <param name="spriteToFree">the sprite to free</param>
        public static void FreeSprite(Sprite spriteToFree)
        {
            spriteToFree.Dispose();
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "AddBitmapToSprite")]
        //private static extern int DLL_AddBitmapToSprite(IntPtr spriteToAddTo, IntPtr bitmapToAdd);

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
            return spriteToAddTo.AddBitmap(bitmapToAdd);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CurrentHeight")]
        //private static extern int DLL_CurrentHeight(IntPtr sprite);

        /// <summary>
        /// Returns the current height of the sprite
        /// </summary>
        /// <param name="sprite">The sprite to get the height of</param>
        /// <returns>The height of the sprite's current frame</returns>
        public static int CurrentHeight(Sprite sprite)
        {
            return sprite.Height;
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CurrentWidth")]
        //private static extern int DLL_CurrentWidth(IntPtr sprite);

        /// <summary>
        /// Returns the current width of the sprite
        /// </summary>
        /// <param name="sprite">The sprite to get the width of</param>
        /// <returns>The width of the sprite's current frame</returns>
        public static int CurrentWidth(Sprite sprite)
        {
            return sprite.Width;
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawSprite")]
        //private static extern void DLL_DrawSprite(IntPtr spriteToDraw, int offsetX, int offsetY);

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawSpriteOffset")]
        //private static extern void DLL_DrawSpriteOffset(IntPtr spriteToDraw, int xOffset, int yOffset);

        /// <summary>
        /// Draws a sprite to the game screen
        /// </summary>
        /// <param name="spriteToDraw">The sprite to be drawn</param>
        public static void DrawSprite(Sprite spriteToDraw)
        {
            DrawSprite(spriteToDraw, 0, 0);
        }

        public static void DrawSprite(Sprite spriteToDraw, int xOffset, int yOffset)
        {
            spriteToDraw.Draw(xOffset, yOffset);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "MoveSprite")]
        //private static extern void DLL_MoveSprite(IntPtr spriteToMove, Vector movementVector);

        /// <summary>
        /// Moves a sprite based on information in a movement vector
        /// </summary>
        /// <param name="spriteToMove">The sprite to move</param>
        /// <param name="movementVector">The vector containing the movement details</param>
        public static void MoveSprite(Sprite spriteToMove, Vector movementVector)
        {
            spriteToMove.Move(movementVector);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "MoveSpriteItself")]
        //private static extern void DLL_MoveSpriteItself(IntPtr spriteToMove);

        /// <summary>
        /// Moves a sprite based on the movement vector that is associated
        /// with the sprite.
        /// </summary>
        /// <param name="toMove">The sprite to move</param>
        public static void MoveSprite(Sprite toMove)
        {
            toMove.Move();
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "MoveSpriteTo")]
        //private static extern void DLL_MoveSpriteTo(IntPtr spriteToMove, int x, int y);

        /// <summary>
        /// Moves a sprite to a given x,y location, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="spriteToMove">the sprite being moved</param>
        /// <param name="x">the new location of the sprite</param>
        /// <param name="y">the new location of the sprite</param>
        public static void MoveSpriteTo(Sprite spriteToMove, int x, int y)
        {
            spriteToMove.MoveTo(x, y);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "IsSpriteOffscreen")]
        //private static extern int DLL_IsSpriteOffscreen(IntPtr theSprite);

        /// <summary>
        /// Determines if a sprite is off the screen
        /// </summary>
        /// <param name="theSprite">The sprite to check the position of</param>
        /// <returns>True if the sprite is off the screen</returns>
        public static bool IsSpriteOffscreen(Sprite theSprite)
        {
            return theSprite.IsOffscreen();
        }

        // Sprite Additions

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CreateSpriteMultiEnding")]
        //private static extern IntPtr DLL_CreateSpriteMultiEnding(IntPtr startBitmap, int isMulti, int length, [MarshalAs(UnmanagedType.LPArray)]int[] framesPerCell, SpriteEndingAction endingAction, int width, int height);

        /// <summary>
        /// Creates a new Sprite. This version allows you to chose if you want a multi cell bitmap 
        /// and pick the ending action
        /// </summary>
        /// <param name="startBitmap">Bitmap to add</param>
        /// <param name="isMulti">set to true if the bitmap is a tileset</param>
        /// <param name="framesPerCell">framesPerCell sets howmany times each frame is drawn</param>
        /// <param name="endingAction">sets the ending action</param>
        /// <param name="width">The width of the Sprite</param>
        /// <param name="height">The height of the Sprite</param>
        /// <returns>A Sprite</returns>
        public static Sprite CreateSprite(Bitmap startBitmap, bool isMulti, int[] framesPerCell, SpriteEndingAction endingAction, int width, int height)
        {
            return new Sprite(startBitmap, isMulti, framesPerCell, endingAction, width, height);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CreateSpriteMulti")]
        //private static extern IntPtr DLL_CreateSpriteMulti(IntPtr startBitmap, int isMulti, int length, [MarshalAs(UnmanagedType.LPArray)] int[] framesPerCell, int width, int height);

        /// <summary>
        /// Creates a new Sprite. This version allows you to chose if you want a multi cell bitmap
        /// </summary>
        /// <param name="startBitmap">Bitmap to add</param>
        /// <param name="isMulti">set to true if the bitmap is a tileset</param>
        /// <param name="framesPerCell">framesPerCell sets howmany times each frame is drawn</param>
        /// <param name="width">The width of the Sprite</param>
        /// <param name="height">The height of the Sprite</param>
        /// <returns>A Sprite</returns>
        public static Sprite CreateSprite(Bitmap startBitmap, bool isMulti, int[] framesPerCell, int width, int height)
        {
            return new Sprite(startBitmap, isMulti, framesPerCell, width, height);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CreateSpriteMultiFPC")]
        //private static extern IntPtr DLL_CreateSpriteMultiFPC(IntPtr startBitmap, int framesPerCell, int frames, int width, int height);

        /// <summary>
        /// Creates a Sprite. This version allows you to pick the frames per cell
        /// </summary>
        /// <param name="startBitmap">StartBitmap</param>
        /// <param name="framesPerCell">Delay each frame</param>
        /// <param name="frames">number of Frames</param>
        /// <param name="width">Width</param>
        /// <param name="height">Height</param>
        /// <returns>Sprite</returns>
        public static Sprite CreateSprite(Bitmap startBitmap, int framesPerCell, int frames, int width, int height)
        {
            return new Sprite(startBitmap, framesPerCell, frames, width, height);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CreateSpriteArrayEnding")]
        //private static extern IntPtr DLL_CreateSpriteArrayEnding(int bitmaplength, [MarshalAs(UnmanagedType.LPArray)] IntPtr[] startBitmap, int length, [MarshalAs(UnmanagedType.LPArray)] int[] framesPerCell, int endingAction);

        /// <summary>
        /// Creates a new Sprite. This version allows you to set how many times each frame is drawn
        /// </summary>
        /// <param name="startBitmap">Bitmap to add</param>
        /// <param name="framesPerCell">framesPerCell sets how many times each frame is drawn</param>
        /// <param name="endingAction">sets the ending action</param>
        /// <returns>A Sprite</returns>
        public static Sprite CreateSprite(Bitmap[] startBitmap, int[] framesPerCell, SpriteEndingAction endingAction)
        {
            return new Sprite(startBitmap, framesPerCell, endingAction);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CreateSpriteArray")]
        //private static extern IntPtr DLL_CreateSpriteArray(int bitmaplength, [MarshalAs(UnmanagedType.LPArray)] IntPtr[] startBitmap, int length, [MarshalAs(UnmanagedType.LPArray)] int[] framesPerCell);

        /// <summary>
        /// Creates a new Sprite with an array of bitmaps
        /// </summary>
        /// <param name="startBitmap">Bitmap to add</param>
        /// <param name="framesPerCell">framesPerCell sets howmany times each frame is drawn</param>
        /// <returns>A Sprite</returns>
        public static Sprite CreateSprite(Bitmap[] startBitmap, int[] framesPerCell)
        {
            return new Sprite(startBitmap, framesPerCell);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CreateSpriteArrayFPC")]
        //private static extern IntPtr DLL_CreateSpriteArrayFPC(int bitmaplength, [MarshalAs(UnmanagedType.LPArray)] IntPtr[] startBitmap, int framesPerCell, int frames);

        /// <summary>
        /// Creates a Sprite with an array of bitmaps and the frames per cell
        /// </summary>
        /// <param name="startBitmap">Start Bitmap</param>
        /// <param name="framesPerCell">Delay per Frame</param>
        /// <param name="frames">Number of Frames</param>
        /// <returns>Sprite</returns>
        public static Sprite CreateSprite(Bitmap[] startBitmap, int framesPerCell, int frames)
        {
            return new Sprite(startBitmap, framesPerCell, frames);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "UpdateSpriteAnimation")]
        //private static extern void DLL_UpdateSpriteAnimation(IntPtr sprite);

        /// <summary>
        /// Updates the Sprites Animation, this will move to the next frame in the sprite
        /// </summary>
        /// <param name="sprite">The Sprite</param>
        public static void UpdateSpriteAnimation(Sprite sprite)
        {
            sprite.UpdateAnimation();
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ReplayAnimation")]
        //private static extern void DLL_ReplayAnimation(IntPtr sprite);

        /// <summary>
        /// Replays a Sprite's Animation, if it has stopped
        /// </summary>
        /// <param name="sprite">The Sprite</param>
        public static void ReplayAnimation(Sprite sprite)
        {
            sprite.ReplayAnimation();
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "UpdateSprite")]
        private static extern void DLL_UpdateSprite(IntPtr sprite);
        /// <summary>
        /// Updates the Sprites Animation and Movement
        /// </summary>
        /// <param name="sprite">The Sprite</param>
        public static void UpdateSprite(Sprite sprite)
        {
            try
            {
                DLL_UpdateSprite(sprite.Pointer);
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

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawBitmapPartOnScreen")]
        private static extern void DLL_DrawBitmapPartOnScreen(IntPtr bitmapToDraw, int srcX, int srcY, int srcW, int srcH, int x, int y);
        /// <summary>
        /// Draws Part of a Bitmap On Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="bitmapToDraw">Bitmap to Draw</param>
        /// <param name="srcX">Starting X on Bitmap</param>
        /// <param name="srcY">Starting Y on Bitmap</param>
        /// <param name="srcW">Width</param>
        /// <param name="srcH">Height</param>
        /// <param name="x">Screen X Position</param>
        /// <param name="y">Screen Y Position</param>
        public static void DrawBitmapPartOnScreen(Bitmap bitmapToDraw, int srcX, int srcY, int srcW, int srcH, int x, int y)
        {
            try
            {
                DLL_DrawBitmapPartOnScreen(bitmapToDraw.pointer, srcX, srcY, srcW, srcH, x, y);
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

        /// <summary>
        /// Draws Part of a Bitmap On Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="bitmapToDraw">Bitmap to Draw</param>
        /// <param name="source">The size of the bitmap to draw</param>
        /// <param name="x">Screen X Position</param>
        /// <param name="y">Screen Y Position</param>
        public static void DrawBitmapPartOnScreen(Bitmap bitmapToDraw, Rectangle source, int x, int y)
        {
            DrawBitmapPartOnScreen(bitmapToDraw, source.X, source.Y, source.Width, source.Height, x, y);
        }

        /// <summary>
        /// Draws Part of a Bitmap On Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="bitmapToDraw">Bitmap to Draw</param>
        /// <param name="source">The size of the bitmap to draw</param>
        /// <param name="position">Screen X,Y Position</param>
        public static void DrawBitmapPartOnScreen(Bitmap bitmapToDraw, Rectangle source, Point2D position)
        {
            DrawBitmapPartOnScreen(bitmapToDraw, source.X, source.Y, source.Width, source.Height, (int)position.X, (int)position.Y);
        }


        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawBitmapOnScreen")]
        private static extern void DLL_DrawBitmapOnScreen(IntPtr bitmapToDraw, int x, int y);
        /// <summary>
        /// Draws a Bitmap, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="bitmapToDraw">Bitmap to draw</param>
        /// <param name="x">X Position</param>
        /// <param name="y">Y Position</param>
        public static void DrawBitmapOnScreen(Bitmap bitmapToDraw, int x, int y)
        {
            try
            {
                DLL_DrawBitmapOnScreen(bitmapToDraw.pointer, x, y);
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
        /// <summary>
        /// Draws a Bitmap, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="bitmapToDraw">The Bitmap to draw</param>
        /// <param name="position">The Position to draw the Bitmap</param>
        public static void DrawBitmapOnScreen(Bitmap bitmapToDraw, Point2D position)
        {
            DrawBitmapOnScreen(bitmapToDraw, (int)position.X, (int)position.Y);
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawPixelOnScreen")]
        private static extern void DLL_DrawPixelOnScreen(int theColor, int x, int y);
        /// <summary>
        /// Draw a Pixel on the Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="theColor">Color of Pixel</param>
        /// <param name="x">X Position</param>
        /// <param name="y">Y Position</param>
        public static void DrawPixelOnScreen(Color theColor, int x, int y)
        {
            try
            {
                int color = theColor.ToArgb();
                DLL_DrawPixelOnScreen(color, x, y);
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
        /// <summary>
        /// Draw a Pixel on the Screen, it will always draw to the Point
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="theColor">The Color of the Pixel</param>
        /// <param name="position">Position of the Pixel</param>
        public static void DrawPixelOnScreen(Color theColor, Point2D position)
        {
            DrawPixelOnScreen(theColor, (int)position.X, (int)position.Y);
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawRectangleOnScreen")]
        private static extern void DLL_DrawRectangleOnScreen(int theColor, int filled, int x, int y, int width, int height);
        /// <summary>
        /// Draw a Rectangle on Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="theColor">Color</param>
        /// <param name="x">X Position</param>
        /// <param name="y">Y Position</param>
        /// <param name="width">Width</param>
        /// <param name="height">Height</param>
        public static void DrawRectangleOnScreen(Color theColor, int x, int y, int width, int height)
        {
            try
            {
                int color = theColor.ToArgb();
                DLL_DrawRectangleOnScreen(color, 0, x, y, width, height);
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

        /// <summary>
        /// Draw a Rectangle on Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="theColor">The Color of the Rectangle</param>
        /// <param name="filled">Filled Rectangle</param>
        /// <param name="x">X Position</param>
        /// <param name="y">Y Position</param>
        /// <param name="width">Width</param>
        /// <param name="height">Height</param>
        public static void DrawRectangleOnScreen(Color theColor, bool filled, int x, int y, int width, int height)
        {
            if (filled)
            {
                FillRectangleOnScreen(theColor, x, y, width, height);
            }
            else
            {
                DrawRectangleOnScreen(theColor, x, y, width, height);
            }
        }

        /// <summary>
        /// Draw a Rectangle on Screen
        /// This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="theColor">The Color of the Rectangle</param>
        /// <param name="source">The Rectangle</param>
        public static void DrawRectangleOnScreen(Color theColor, Rectangle source)
        {
            DrawRectangleOnScreen(theColor, source.X, source.Y, source.Width, source.Height);
        }

        /// <summary>
        /// Draw a Rectangle on Screen
        /// This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="theColor">The Color of the Rectangle</param>
        /// <param name="filled">Filled Rectangle</param>
        /// <param name="source">The Rectangle</param>
        public static void DrawRectangleOnScreen(Color theColor, bool filled, Rectangle source)
        {
            DrawRectangleOnScreen(theColor, filled, source.X, source.Y, source.Width, source.Height);
        }

        /// <summary>
        /// Draws a Filled Rectangle on the Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="theColor">Color</param>
        /// <param name="x">X Position</param>
        /// <param name="y">Y Position</param>
        /// <param name="width">Width</param>
        /// <param name="height">Height</param>
        public static void FillRectangleOnScreen(Color theColor, int x, int y, int width, int height)
        {
            try
            {
                int color = theColor.ToArgb();
                DLL_DrawRectangleOnScreen(color, -1, x, y, width, height);
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
        /// <summary>
        /// Draws a Filled Rectangle on the Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="theColor">Color of the Rectangle</param>
        /// <param name="source">Rectangle</param>
        public static void FillRectangleOnScreen(Color theColor, Rectangle source)
        {
            FillRectangleOnScreen(theColor, source.X, source.Y, source.Width, source.Height);
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawLineOnScreen")]
        private static extern void DLL_DrawLineOnScreen(int theColor, int x, int y, int x2, int y2);
        /// <summary>
        /// Draws a Line on the Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="theColor">Color</param>
        /// <param name="xPosStart">X Start Coordinate</param>
        /// <param name="yPosStart">Y Start Coordinate</param>
        /// <param name="xPosEnd">X End Coordinate</param>
        /// <param name="yPosEnd">Y End Coordinate</param>
        public static void DrawLineOnScreen(Color theColor, int xPosStart, int yPosStart, int xPosEnd, int yPosEnd)
        {
            try
            {
                int color = theColor.ToArgb();
                DLL_DrawLineOnScreen(color, xPosStart, yPosStart, xPosEnd, yPosEnd);
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
        /// <summary>
        /// Draws a Line on the Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="theColor">Color of the Line</param>
        /// <param name="line">The Line</param>
        public static void DrawLineOnScreen(Color theColor, LineSegment line)
        {
            DrawLineOnScreen(theColor, (int)line.StartPoint.X, (int)line.StartPoint.Y, (int)line.EndPoint.X, (int)line.EndPoint.Y);
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawHorizontalLineOnScreen")]
        private static extern void DLL_DrawHorizontalLineOnScreen(int theColor, int y, int x1, int x2);
        /// <summary>
        /// Draw Horizontal Line on Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="theColor">Color</param>
        /// <param name="y">Y Position</param>
        /// <param name="x1">X Starting Coordinate</param>
        /// <param name="x2">X Ending Coordinate</param>
        public static void DrawHorizontalLineOnScreen(Color theColor, int y, int x1, int x2)
        {
            try
            {
                int color = theColor.ToArgb();
                DLL_DrawHorizontalLineOnScreen(color, y, x1, x2);
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

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawVerticalLineOnScreen")]
        private static extern void DLL_DrawVerticalLineOnScreen(int theColor, int x, int y1, int y2);
        /// <summary>
        /// Draws a Vertical Line on the Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="theColor">Color</param>
        /// <param name="x">X Position</param>
        /// <param name="y1">Y Starting Coordinate</param>
        /// <param name="y2">Y Ending Coordinate</param>
        public static void DrawVerticalLineOnScreen(Color theColor, int x, int y1, int y2)
        {
            try
            {
                int color = theColor.ToArgb();
                DLL_DrawVerticalLineOnScreen(color, x, y1, y2);
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

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawCircleOnScreen")]
        private static extern void DLL_DrawCircleOnScreen(int theColor, int filled, int xc, int yc, int radius);
        /// <summary>
        /// Draws a Circle on the Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="theColor">Color</param>
        /// <param name="xc">X Center Position</param>
        /// <param name="yc">Y Center Position</param>
        /// <param name="radius">Radius</param>
        public static void DrawCircleOnScreen(Color theColor, int xc, int yc, int radius)
        {
            try
            {
                int color = theColor.ToArgb();
                DLL_DrawCircleOnScreen(color, 0, xc, yc, radius);
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

        /// <summary>
        /// Draws a Circle on the Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="theColor">Color</param>
        /// <param name="position">The x, y location of the center of the circle</param>
        /// <param name="radius">Radius</param>
        public static void DrawCircleOnScreen(Color theColor, Point2D position, int radius)
        {
            DrawCircleOnScreen(theColor, (int)position.X, (int)position.Y, radius);
        }

        /// <summary>
        /// Draws a Circle on the Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="theColor">Color</param>
        /// <param name="filled">If true the circle will be filled</param>
        /// <param name="xc">X Center Position</param>
        /// <param name="yc">Y Center Position</param>
        /// <param name="radius">Radius</param>
        public static void DrawCircleOnScreen(Color theColor, Boolean filled, int xc, int yc, int radius)
        {
            if (filled)
            {
                FillCircleOnScreen(theColor, xc, yc, radius);
            }
            else
            {
                DrawCircleOnScreen(theColor, xc, yc, radius);
            }
        }


        /// <summary>
        /// Draws a Circle on the Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="theColor">Color</param>
        /// <param name="filled">If true the circle will be filled</param>
        /// <param name="position">The x, y location of the center of the circle</param>
        /// <param name="radius">Radius</param>
        public static void DrawCircleOnScreen(Color theColor, Boolean filled, Point2D position, int radius)
        {
            if (filled)
            {
                FillCircleOnScreen(theColor, (int)position.X, (int)position.Y, radius);
            }
            else
            {
                DrawCircleOnScreen(theColor, (int)position.X, (int)position.Y, radius);
            }
        }


        /// <summary>
        /// Draws a Filled Circle On Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="theColor">Color</param>
        /// <param name="xc">X Center Position</param>
        /// <param name="yc">Y Center Position</param>
        /// <param name="radius">Radius</param>
        public static void FillCircleOnScreen(Color theColor, int xc, int yc, int radius)
        {
            try
            {
                int color = theColor.ToArgb();
                DLL_DrawCircleOnScreen(color, -1, xc, yc, radius);
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
        /// <summary>
        /// Draws a Filled Circle On Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="theColor">Color of the Circle</param>
        /// <param name="position">Position of the Circle</param>
        /// <param name="radius">Radius of the Circle</param>
        public static void FillCircleOnScreen(Color theColor, Point2D position, int radius)
        {
            FillCircleOnScreen(theColor, (int)position.X, (int)position.Y, radius);
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawEllipseOnScreen")]
        private static extern void DLL_DrawEllipseOnScreen(int theColor, int filled, int x, int y, int width, int height);
        /// <summary>
        /// Draws an Ellipse on the Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="theColor">Color</param>
        /// <param name="x">X Position</param>
        /// <param name="y">Y Position</param>
        /// <param name="width">Width</param>
        /// <param name="height">Height</param>
        public static void DrawEllipseOnScreen(Color theColor, int x, int y, int width, int height)
        {
            try
            {
                int color = theColor.ToArgb();
                DLL_DrawEllipseOnScreen(color, 0, x, y, width, height);
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
        /// <summary>
        /// Draws an Ellipse on the Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="theColor">Ellipse Color</param>
        /// <param name="filled">Filled Ellipse</param>
        /// <param name="source">Rectangle</param>
        public static void DrawEllipseOnScreen(Color theColor, bool filled, Rectangle source)
        {
            DrawEllipseOnScreen(theColor, filled, (int)source.X, (int)source.Y, (int)source.Width, (int)source.Height);
        }
        /// <summary>
        /// Draws an Ellipse on the Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="theColor"></param>
        /// <param name="source"></param>
        public static void DrawEllipseOnScreen(Color theColor, Rectangle source)
        {
            DrawEllipseOnScreen(theColor, (int)source.X, (int)source.Y, (int)source.Width, (int)source.Height);
        }

        /// <summary>
        /// Draws an Ellipse on the Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="theColor">Color of the Ellipse</param>
        /// <param name="filled">Filled Ellipse</param>
        /// <param name="x">X Coordinate</param>
        /// <param name="y">Y Coordinate</param>
        /// <param name="width">Width</param>
        /// <param name="height">Height</param>
        public static void DrawEllipseOnScreen(Color theColor, bool filled, int x, int y, int width, int height)
        {
            if (filled)
            {
                FillEllipseOnScreen(theColor, x, y, width, height);
            }
            else
            {
                DrawEllipseOnScreen(theColor, x, y, width, height);
            }
        }

        /// <summary>
        /// Draws a Filled Ellipse On the Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="theColor">Color</param>
        /// <param name="x">X Position</param>
        /// <param name="y">Y Position</param>
        /// <param name="width">Width</param>
        /// <param name="height">Height</param>
        public static void FillEllipseOnScreen(Color theColor, int x, int y, int width, int height)
        {
            try
            {
                int color = theColor.ToArgb();
                DLL_DrawEllipseOnScreen(color, -1, x, y, width, height);
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

        /// <summary>
        /// Draws a Filled Ellipse On the Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="theColor">Color of the Ellipse</param>
        /// <param name="source">Ellipse</param>
        public static void FillEllipseOnScreen(Color theColor, Rectangle source)
        {
            FillEllipseOnScreen(theColor, source.X, source.Y, source.Width, source.Height);
        }

        /// <summary>
        /// Sets the clipping rectangle to the specified values. Any drawing outside of
        /// this area is ignored.
        /// </summary>
        /// <param name="x">X Position of clipping rectangle</param>
        /// <param name="y">Y Position of clipping rectangle</param>
        /// <param name="width">Width of clipping rectangle</param>
        /// <param name="height">Height of clipping rectangle</param>
        public static void SetClip(int x, int y, int width, int height)
        {
            try
            {
                DLL_SetClip(IntPtr.Zero, x, y, width, height);
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

        /// <summary>
        /// Sets the clipping rectangle to the specified values. Any drawing outside of
        /// this area is ignored.
        /// </summary>
        /// <param name="rect">the clipping rectangle</param>
        public static void SetClip(Rectangle rect)
        {
            SetClip(rect.X, rect.Y, rect.Width, rect.Height);
        }

        /// <summary>
        /// Sets the clipping rectangle to the specified values. Any drawing outside of
        /// this area is ignored.
        /// </summary>
        /// <param name="bmp">Bitmap to clip</param>
        /// <param name="rect">the clipping rectangle</param>
        public static void SetClip(Bitmap bmp, Rectangle rect)
        {
            SetClip(bmp, rect.X, rect.Y, rect.Width, rect.Height);
        }

        /// <summary>
        /// Sets the clipping rectangle to the specified values. Any drawing outside of
        /// this area is ignored.
        /// </summary>
        /// <param name="bmp">The bitmap to clip drawing operations on</param>
        /// <param name="x">X Position of clipping rectangle</param>
        /// <param name="y">Y Position of clipping rectangle</param>
        /// <param name="width">Width of clipping rectangle</param>
        /// <param name="height">Height of clipping rectangle</param>
        public static void SetClip(Bitmap bmp, int x, int y, int width, int height)
        {
            try
            {
                DLL_SetClip(bmp.pointer, x, y, width, height);
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

        /// <summary>
        /// Resets the clipping area to allow you to draw to the entire screen.
        /// </summary>
        public static void ResetClip()
        {
            try
            {
                DLL_ResetClip(IntPtr.Zero);
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

        /// <summary>
        /// Resets the clipping area to allow you to draw to the entire screen.
        /// </summary>
        /// <param name="bmp">The bitmap to clip drawing operations on</param>
        public static void ResetClip(Bitmap bmp)
        {
            try
            {
                DLL_ResetClip(bmp.pointer);
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

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawTriangle")]
        private static extern void DLL_DrawTriangle(int theColor, [MarshalAs(UnmanagedType.LPArray)]Point2D[] points);
        /// <summary>
        /// Draws the specified Triangle to the screen
        /// </summary>
        /// <param name="theColor">The color of the triangle</param>
        /// <param name="triangle">The triangle to be drawn</param>
        public static void DrawTriangle(Color theColor, Triangle triangle)
        {
            try
            {
                int color = theColor.ToArgb();
                DLL_DrawTriangle(color, triangle.ToArray());
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
