using System;
using System.Collections.Generic;
using System.Text;
using Color = System.Drawing.Color;
using Rectangle = System.Drawing.Rectangle;

namespace SwinGame
{
    /// <summary>
    /// Bitmap
    ///
    ///	The bitmap type is a pointer to a BitmapData. The BitmapData record
    ///	contains the data used by the SwinGame API to represent
    ///	bitmaps. You can create new bitmaps in memory for drawing operatings
    ///	using the CreateBitmap function. This can then be optimised for drawing
    ///	to the screen using the OptimiseBitmap routine. Also see the DrawBitmap
    ///	routines.
    /// </summary>
    public class Bitmap : IDisposable
    {
        internal readonly SwinGamePointer pointer;

        internal Bitmap(IntPtr devPtr, bool isCopy)
        {
            if(isCopy)
                pointer = new SwinGamePointer(devPtr, PtrKind.Copy);
            else
                pointer = new SwinGamePointer(devPtr, PtrKind.Image);
        }

        /// <summary>
        /// Create a blank bitmap of the given size. This is useful for caching drawing, 
        /// for slower drawing operations.Most of the Drawing routines provide an 
        /// option to specify what bitmap to draw onto.
        /// </summary>
        /// <param name="width">Width of a bitmap</param>
        /// <param name="height">Height of a bitmap</param>
        public Bitmap(int width, int height)
        {
            pointer = new SwinGamePointer(SGSDK.CreateBitmap(width, height), PtrKind.Image);
        }

        /// <summary>
        /// Load the specified image file. Use the GetPathToResource methods from Core to
        /// ensure that you load the file in a platform neutral way, enabling your game
        /// to run on Windows, Mac, and Linux.
        /// </summary>
        /// <param name="path">Path to the image file</param>
        public Bitmap(String path)
        {
            int color = Color.Black.ToArgb();
            pointer = new SwinGamePointer(SGSDK.LoadBitmapWithTransparentColor(path, 0, (uint)color), PtrKind.Image);
        }

        /// <summary>
        /// Load the specified image file with a transparent color. Use the GetPathToResource methods from Core to
        /// ensure that you load the file in a platform neutral way, enabling your game
        /// to run on Windows, Mac, and Linux.
        /// </summary>
        /// <param name="pathToBitmap">Path to the image file</param>
        /// <param name="transparent">True if this image has transparent pixels</param>
        /// <param name="transparentColor">Color of the transparent pixels</param>
        public Bitmap(String pathToBitmap, bool transparent, Color transparentColor)
        {
            int color = transparentColor.ToArgb();
            pointer = new SwinGamePointer(SGSDK.LoadBitmapWithTransparentColor(pathToBitmap, (transparent ? -1 : 0), (uint)color), PtrKind.Image);
        }

        /// <summary>
        /// Load an image with transparency. Use the GetPathToResource methods from Core to
        /// ensure that you load the file in a platform neutral way, enabling your game
        /// to run on Windows, Mac, and Linux.
        /// </summary>
        /// <param name="pathToBitmap">Path to the image file</param>
        /// <param name="transparentColor">Color of the transparent pixels</param>
        public Bitmap(string pathToBitmap, Color transparentColor)
        {
            int color = transparentColor.ToArgb();
            pointer = new SwinGamePointer(SGSDK.LoadTransparentBitmap(pathToBitmap, (uint)color), PtrKind.Image);
        }

        /// <summary>
        /// The Width of the Bitmap
        /// </summary>
        public int Width
        {
            get { return SGSDK.GetBitmapWidth(this); }
        }

        /// <summary>
        /// The Height of the Bitmap
        /// </summary>
        public int Height
        {
            get { return SGSDK.GetBitmapHeight(this); }
        }

        /// <summary>
        /// Optimise the bitmap for faster drawing.
        /// </summary>
        public void Optimise()
        {
            SGSDK.OptimiseBitmap(this);
        }

        /// <summary>
        /// Clears the bitmap to Black.
        /// </summary>
        public void Clear()
        {
            Clear(Color.Black);
        }

        /// <summary>
        /// Clear the bitmap with the specified color.
        /// </summary>
        /// <param name="toColour">The color used to clear</param>
        public void Clear(Color toColour)
        {
            int color = toColour.ToArgb();
            SGSDK.ClearSurfaceWithColor(this, (uint)color);
        }

        /// <summary>
        /// Draw bitmap to the specified bitmap. You will often use the bitmap from CreateBitmap to draw on.
        /// </summary>
        /// <param name="dest">Bitmap to draw on</param>
        /// <param name="x">X coordinate</param>
        /// <param name="y">Y coordinate</param>
        public void DrawOnto(Bitmap dest, int x, int y)
        {
            SGSDK.DrawBitmapWithDestination(dest, this, x, y);
        }

        /// <summary>
        /// Draw bitmap to the specified bitmap. You will often use the bitmap from CreateBitmap to draw on.
        /// </summary>
        /// <param name="dest">Bitmap to draw on</param>
        /// <param name="position">Position to Draw to</param>
        public void DrawOnto(Bitmap dest, Point2D position)
        {
            DrawOnto(dest, (int)position.X, (int)position.Y);
        }

        /// <summary>
        /// Draw the bitmap.
        /// </summary>
        /// <param name="position">Position to Draw to</param>
        public void Draw(Point2D position)
        {
            Draw((int)position.X, (int)position.Y);
        }

        /// <summary>
        /// Draws part of a bitmap (bitmapToDraw) onto another bitmap (dest), this is faster than drawing 
        /// the whole bitmap
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="srcX">The x offset to the area to copy in bitmapToDraw</param>
        /// <param name="srcY">The y offset to the area to copy in bitmapToDraw</param>
        /// <param name="srcW">The width of the area to copy</param>
        /// <param name="srcH">The height of the area to copy</param>
        /// <param name="x">The x location to draw the bitmap part to</param>
        /// <param name="y">The y location to draw the bitmap part to</param>
        public void DrawPartOnto(Bitmap dest, int srcX, int srcY, int srcW, int srcH, int x, int y)
        {
            SGSDK.DrawBitmapPartWithDestination(dest, this, srcX, srcY, srcW, srcH, x, y);
        }

        /// <summary>
        /// Draws part of the bitmap onto another bitmap (dest), this is faster than drawing 
        /// the whole bitmap
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="source">The possition and size of the bitmapToDraw</param>
        /// <param name="x">The x location to draw the bitmap part to</param>
        /// <param name="y">The y location to draw the bitmap part to</param>
        public void DrawPartOnto(Bitmap dest, Rectangle source, int x, int y)
        {
            DrawPartOnto(dest, source.X, source.Y, source.Width, source.Height, x, y);
        }

        /// <summary>
        /// Draws part of a bitmap onto another bitmap (dest).
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="source">The possition and size of the part of the bitmap to draw</param>
        /// <param name="position">The x,y location to draw the bitmap part to</param>
        public void DrawPartOnto(Bitmap dest, Rectangle source, Point2D position)
        {
            DrawPartOnto(dest, source.X, source.Y, source.Width, source.Height, (int)position.X, (int)position.Y);
        }

        /// <summary>
        /// Draws the bitmap onto the screen, x and y are in game 
        /// coordinates not screen coordinates
        /// </summary>
        /// <param name="x">The x location to draw the bitmap to</param>
        /// <param name="y">The y location to draw the bitmap to</param>
        public void Draw(float x, float y)
        {
            SGSDK.DrawBitmap(this, x, y);
        }

        /// <summary>
        /// Draws part of a bitmap onto the screen, x and y are in game 
        /// coordinates not screen coordinates.
        /// </summary>
        /// <param name="srcX">The x offset to the area to copy in bitmapToDraw</param>
        /// <param name="srcY">The y offset to the area to copy in bitmapToDraw</param>
        /// <param name="srcW">The width of the area to copy</param>
        /// <param name="srcH">The height of the area to copy</param>
        /// <param name="x">The x location to draw the bitmap part to</param>
        /// <param name="y">The y location to draw the bitmap part to</param>
        public void DrawPart(int srcX, int srcY, int srcW, int srcH, float x, float y)
        {
            SGSDK.DrawBitmapPart(this, srcX, srcY, srcW, srcH, x, y);
        }

        /// <summary>
        /// Draws part of a bitmap (bitmapToDraw) onto the screen, x and y are in game 
        /// coordinates not screen coordinates, this is faster than DrawBitmap
        /// if you don't want all of the bitmap to be shown
        /// </summary>
        /// <param name="source">The possition and size of the bitmapToDraw</param>
        /// <param name="x">The x location to draw the bitmap part to</param>
        /// <param name="y">The y location to draw the bitmap part to</param>
        public void DrawPart(Rectangle source, float x, float y)
        {
            DrawPart(source.X, source.Y, source.Width, source.Height, x, y);
        }

        /// <summary>
        /// Draws part of a bitmap (bitmapToDraw) onto the screen, x and y are in game 
        /// coordinates not screen coordinates, this is faster than DrawBitmap
        /// if you don't want all of the bitmap to be shown
        /// </summary>
        /// <param name="source">The possition and size of the bitmapToDraw</param>
        /// <param name="position">The x,y location to draw the bitmap part to</param>
        public void DrawPart(Rectangle source, Point2D position)
        {
            DrawPart(source.X, source.Y, source.Width, source.Height, position.X, position.Y);
        }

        /// <summary>
        /// Draws Part of a Bitmap On Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="srcX">Starting X on Bitmap</param>
        /// <param name="srcY">Starting Y on Bitmap</param>
        /// <param name="srcW">Width</param>
        /// <param name="srcH">Height</param>
        /// <param name="x">Screen X Position</param>
        /// <param name="y">Screen Y Position</param>
        public void DrawPartOnScreen(int srcX, int srcY, int srcW, int srcH, int x, int y)
        {
            SGSDK.DrawBitmapPartOnScreen(this, srcX, srcY, srcW, srcH, x, y);
        }

        /// <summary>
        /// Draws Part of a Bitmap On Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="source">The size of the bitmap to draw</param>
        /// <param name="x">Screen X Position</param>
        /// <param name="y">Screen Y Position</param>
        public void DrawPartOnScreen(Rectangle source, int x, int y)
        {
            DrawPartOnScreen(source.X, source.Y, source.Width, source.Height, x, y);
        }

        /// <summary>
        /// Draws Part of a Bitmap On Screen, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="source">The size of the bitmap to draw</param>
        /// <param name="position">Screen X,Y Position</param>
        public void DrawPartOnScreen(Rectangle source, Point2D position)
        {
            DrawPartOnScreen(source.X, source.Y, source.Width, source.Height, (int)position.X, (int)position.Y);
        }

        /// <summary>
        /// Draws a Bitmap, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="x">X Position</param>
        /// <param name="y">Y Position</param>
        public void DrawOnScreen(int x, int y)
        {
            SGSDK.DrawBitmapOnScreen(this, x, y);
        }

        /// <summary>
        /// Draws a Bitmap, it will always draw to x, y 
        /// regardless of the position of the camera. This is usefull for drawing 
        /// things like the user interface or overlays
        /// </summary>
        /// <param name="position">The Position to draw the Bitmap</param>
        public void DrawOnScreen(Point2D position)
        {
            DrawOnScreen((int)position.X, (int)position.Y);
        }

        /// <summary>
        /// Get the color of the pixel in the bitmap.
        /// </summary>
        /// <param name="x">the x location of the pixel</param>
        /// <param name="y">the y location of the pixel</param>
        /// <returns>the color of the pixel in the bitmap at location x,y</returns>
        public Color GetPixel(int x, int y)
        {
            return Color.FromArgb((int)SGSDK.GetPixel(this, x, y));
        }

        /// <summary>
        /// Cast the bitmap to its native pointer.
        /// </summary>
        /// <param name="bmp">the bitmap</param>
        /// <returns>its native pointer</returns>
        public static implicit operator IntPtr(Bitmap bmp)
        {
            return bmp.pointer.Pointer;
        }

        #region IDisposable Members

        /// <summary>
        /// Clean up the associated system resources.
        /// </summary>
        public void Dispose()
        {
            pointer.Free();
        }

        #endregion

        /// <summary>
        /// Make the bitmap opaque, for the purpose of drawing onto another bitmap. 
        /// This is required in cases where drawing a transparent bitmap onto a 
        /// fully transparent bitmap.
        /// </summary>
        /// <remarks>
        /// Make the bitmap to be drawn opaque when drawing onto a fully transparent
        /// bitmap.
        /// </remarks>
        public void MakeOpaque()
        {
            SGSDK.MakeOpaque(this);
        }

        /// <summary>
        /// Turns alpha blending back on for a bitmap. This reverses the
        /// MakeOpaque process.
        /// </summary>
        public void MakeTransparent()
        {
            SGSDK.MakeTransparent(this);
        }

        /// <summary>
        /// Rotate and zoom the passed in bitmap, returning a new
        /// bitmap at the given rotation.
        /// </summary>
        /// <param name="deg">the amount to rotate (in degrees)</param>
        /// <param name="scale">the amount to scale the bitmap</param>
        /// <returns>The new bitmap, rotated and zoomed</returns>
        public Bitmap RotateZoom(int deg, float scale)
        {
            return new Bitmap(SGSDK.RotateZoomBitmap(this, deg, scale), false);
        }

        /// <summary>
        /// Sets up a bitmap so that collisions will work at a pixel level.
        /// This does not need to be called on loaded bitmaps, only on
        /// dynamically created bitmaps, and bitmaps created using
        /// rotate and zoom.
        /// </summary>
        public void SetupForCollisions()
        {
            SGSDK.SetupBitmapForCollisions(this);
        }
    }
}
