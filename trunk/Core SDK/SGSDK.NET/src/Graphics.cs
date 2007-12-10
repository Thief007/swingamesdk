using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Drawing;
using System.IO;

namespace SwinGame
{

    /// <summary>
    /// A Sprite Collection is an array of Sprites
    /// </summary>
    public struct SpriteCollection
    {
        public IntPtr[] Sprites;
    }

    /// <summary>
    /// This contains number of bitmaps and its position.
    /// </summary>
    public struct Sprite
    {
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern Bitmap GetSpriteBitmap(IntPtr pointer, int id);

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern float GetSpriteX(IntPtr pointer);

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern void SetSpriteX(IntPtr pointer, float X);

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern float GetSpriteY(IntPtr pointer);

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern void SetSpriteY(IntPtr pointer, float Y);

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern int GetSpriteCurrentFrame(IntPtr pointer);

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern void SetSpriteCurrentFrame(IntPtr pointer, int frame);

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern bool GetSpriteUsePixelCollision(IntPtr pointer);

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern void SetSpriteUsePixelCollision(IntPtr pointer, bool pixelcollision);

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern float GetSpriteMass(IntPtr pointer);

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="GetSpriteMovement")]
        private static extern Vector DLL_GetSpriteMovement(IntPtr pointer);

        private static Vector GetSpriteMovement(IntPtr pointer)
        {
            if (pointer == IntPtr.Zero) 
                throw new SwinGameException("The Sprite has not been created. Ensure that the sprite is created before accessing the Vector");
            return DLL_GetSpriteMovement(pointer);
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern void SetSpriteMass(IntPtr pointer, Single mass);

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern void SetSpriteMovement(IntPtr pointer, Vector movement);

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
        public float xPos
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
        public float yPos
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

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern int GetSpriteKind(IntPtr pointer);

        /// <summary>
        /// Gets the Sprite Kind
        /// </summary>
        public SpriteKind SpriteKind
        {
            get
            {
                return (SpriteKind)GetSpriteKind(Pointer);
            }
        }

        /*
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern int[] GetSpriteFramesPerCell(IntPtr pointer);

        /// <summary>
        /// Gets the Frames per Cell
        /// </summary>
        public int[] FramesPerCell
        {
            get
            {
                return (int[])GetSpriteFramesPerCell(Pointer);
            }
        }
        */

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern int GetSpriteCols(IntPtr pointer);

        /// <summary>
        /// Gets the number of Columns
        /// </summary>
        public int Cols
        {
            get
            {
                return GetSpriteCols(Pointer);
            }
        }
	        
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern int GetSpriteRow(IntPtr pointer);

        /// <summary>
        /// Gets the number of Rows
        /// </summary>
        public int Rows
        {
            get
            {
                return GetSpriteRow(Pointer);
            }
        }
	
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern int GetSpriteFrameCount(IntPtr pointer);

        /// <summary>
        /// Gets the Frame Count
        /// </summary>
        public int FrameCount
        {
            get
            {
                return GetSpriteFrameCount(Pointer);
            }
        }

	    [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern int GetSpriteendingAction(IntPtr pointer);

        /// <summary>
        /// Gets the Ending Action
        /// </summary>
        public SpriteEndingAction EndingAction
        {
            get
            {
                return (SpriteEndingAction)GetSpriteendingAction(Pointer);
            }
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern int GetSpritehasEnded(IntPtr pointer);

        /// <summary>
        /// Gets whether the Sprite Animation has Ended
        /// </summary>
        public Boolean hasEnded
        {
            get
            {
                if (GetSpritehasEnded(Pointer) == -1)
            {
                return true;
            }
            else
            {
                return false;
            }
            }
        }
	
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern int GetSpriteReverse(IntPtr pointer);

        /// <summary>
        /// Gets whether the Sprite is reversed
        /// </summary>
        public Boolean Reverse
        {
            get
            {
                if (GetSpriteReverse(Pointer) == -1)
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }
        }

        public MovementClass Movement
        {
            get
            {
                return new MovementClass(this);
            }
        }

        public class MovementClass
        {
            private Sprite _Data;

            internal MovementClass(Sprite data)
            {
                _Data = data;
            }

            public float X
            {
                get
                {
                    return GetSpriteMovement(_Data.Pointer).x;
                }
                set
                {
                    Vector v = GetSpriteMovement(_Data.Pointer);
                    v.x = value;
                    SetSpriteMovement(_Data.Pointer, v);
                }
            }
            
            public float Y
            {
                get
                {
                    return GetSpriteMovement(_Data.Pointer).y;
                }
                set
                {
                    Vector v = GetSpriteMovement(_Data.Pointer);
                    v.y = value;
                    SetSpriteMovement(_Data.Pointer, v);
                }
            }

            public void SetTo(Vector v)
            {
                SetSpriteMovement(_Data.Pointer, v);
            }

            public static implicit operator Vector(MovementClass mc)
            {
                return GetSpriteMovement(mc._Data.Pointer);
            }
 
        }

        public float Mass
        {
            get
            {
                return GetSpriteMass(Pointer);
            }
            set
            {
                SetSpriteMass(Pointer, value);
            }
        }
    }

    /// It is used to determine how a sprite should act.
    public enum SpriteKind
    {
        /// <summary>
        /// StaticSprite will no animate at all.
        /// </summary>
        StaticSprite,
        /// <summary>
        /// AnimArraySprite will animate using an array of bitmaps.
        /// </summary>
        AnimArraySprite,
        /// <summary>
        /// AnimMultiSprite will animate using a single bitmap with multiple
        /// frames.
        /// </summary>
        AnimMultiSprite
    }

    /// Record: SpriteEndingAction
    ///
    /// It is used to determine what this sprite should do when it finishes
    /// animating.
    public enum SpriteEndingAction
    {
        /// <summary>
        /// Loops forward
        /// </summary>
        Loop,
        /// <summary>
        /// Loops back and forth
        /// </summary>
        ReverseLoop,
        /// <summary>
        /// Reverse Once
        /// </summary>
        ReverseOnce,
        /// <summary>
        /// No Loop
        /// </summary>
        Stop
    }

    /// <summary>
    /// The Graphics Class enables most of the Drawing features of SGSDK
    /// </summary>
    public class Graphics
    {
        /// <summary>
        /// Create a bitmap
        /// </summary>
        /// <param name="width">Width of a bitmap</param>
        /// <param name="height">Height of a bitmap</param>
        /// <returns>New bitmap</returns>
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern Bitmap CreateBitmap(int width, int height);

        /// <summary>
        /// Optimise the specified bitmap
        /// </summary>
        /// <param name="surface">Bitmap to optimise</param>
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern void OptimiseBitmap(Bitmap surface);

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "LoadBitmapWithTransparentColor")]
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

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "LoadTransparentBitmap")]
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

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "FreeBitmap")]
        private static extern void DLL_FreeBitmap(ref IntPtr bitmapToFree);

        /// <summary>
        /// Free the specified bitmap
        /// </summary>
        /// <param name="bitmapToFree">Bitmap to free</param>
        public static void FreeBitmap(ref Bitmap bitmapToFree)
        {
            DLL_FreeBitmap(ref bitmapToFree.pointer);
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetBitmapWidth")]
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

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetBitmapHeight")]
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

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ClearSurfaceWithColor")]
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

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawBitmapWithDestination")]
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

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawBitmapPartWithDestination")]
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

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawPixelWithDestination")]
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

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawRectangleWithDestination")]
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

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "FillRectangleWithDestination")]
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

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawLineWithDestination")]
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

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawHorizontalLineWithDestination")]
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

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawVerticalLineWithDestination")]
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

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawCircleWithDestination")]
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

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawEllipseWithDestination")]
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

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ClearScreen")]
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

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawBitmap")]
        private static extern void DLL_DrawBitmap(IntPtr bitmapToDraw, float x, float y);

        /// <summary>
        /// Draws one bitmap (bitmapToDraw) onto the screen
        /// </summary>
        /// <param name="bitmapToDraw">The bitmap to be drawn onto the screen</param>
        /// <param name="x">The x location to draw the bitmap to</param>
        /// <param name="y">The y location to draw the bitmap to</param>
        public static void DrawBitmap(Bitmap bitmapToDraw, float x, float y)
        {
            DLL_DrawBitmap(bitmapToDraw.pointer, x, y);
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawBitmapPart")]
        private static extern void DLL_DrawBitmapPart(IntPtr bitmapToDraw, int srcX, int srcY, int srcW, int srcH, float x, float y);

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
        public static void DrawBitmapPart(Bitmap bitmapToDraw, int srcX, int srcY, int srcW, int srcH, float x, float y)
        {
            DLL_DrawBitmapPart(bitmapToDraw.pointer, srcX, srcY, srcW, srcH, x, y);
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawPixel")]
        private static extern void DLL_DrawPixel(uint theColour, float x, float y);

        /// <summary>
        /// Draws a pixel onto the screen
        /// </summary>
        /// <param name="theColour">The color to draw the pixel</param>
        /// <param name="x">The x location to draw the pixel at</param>
        /// <param name="y">The y location to draw the pixel at</param>
        public static void DrawPixel(Color theColour, float x, float y)
        {
            int color = theColour.ToArgb();
            DLL_DrawPixel((uint)color, x, y);
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawRectangle")]
        private static extern void DLL_DrawRectangle(uint theColour, bool filled, float xPos, float yPos, int width, int height);

        /// <summary>
        /// Draws a rectangle on the screen
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
        public static void DrawRectangle(Color theColour, float xPos, float yPos, int width, int height)
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
        public static void FillRectangle(Color theColour, float xPos, float yPos, int width, int height)
        {
            int color = theColour.ToArgb();
            DLL_DrawRectangle((uint)color, true, xPos, yPos, width, height);
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawLine")]
        private static extern void DLL_DrawLine(uint theColour, float xPosStart, float yPosStart, float xPosEnd, float yPosEnd);

        /// <summary>
        /// Draws a line on the screen
        /// </summary>
        /// <param name="theColour">The color to draw the line</param>
        /// <param name="xPosStart">The x location to start the line at</param>
        /// <param name="yPosStart">The y location to start the line at</param>
        /// <param name="xPosEnd">The x location to end the line at</param>
        /// <param name="yPosEnd">The y location to end the line at</param>
        public static void DrawLine(Color theColour, float xPosStart, float yPosStart, float xPosEnd, float yPosEnd)
        {
            int color = theColour.ToArgb();
            DLL_DrawLine((uint)color, xPosStart, yPosStart, xPosEnd, yPosEnd);
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawHorizontalLine")]
        private static extern void DLL_DrawHorizontalLine(uint theColor, float y, float x1, float x2);

        /// <summary>
        /// Draws a horizontal line on the screen
        /// </summary>
        /// <param name="theColor">The color to draw the line</param>
        /// <param name="y">The y location of the line</param>
        /// <param name="x1">The starting x value of the line</param>
        /// <param name="x2">The ending x value of the line</param>
        public static void DrawHorizontalLine(Color theColor, float y, float x1, float x2)
        {
            int color = theColor.ToArgb();
            DLL_DrawHorizontalLine((uint)color, y, x1, x2);
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawVerticalLine")]
        private static extern void DLL_DrawVerticalLine(uint theColor, float x, float y1, float y2);

        /// <summary>
        /// Draws a vertical line on the screen
        /// </summary>
        /// <param name="theColor">The color to draw the line</param>
        /// <param name="x">The color to draw the line</param>
        /// <param name="y1">The starting y value of the line</param>
        /// <param name="y2">The ending y value of the line</param>
        public static void DrawVerticalLine(Color theColor, float x, float y1, float y2)
        {
            int color = theColor.ToArgb();
            DLL_DrawVerticalLine((uint)color, x, y1, y2);
        }






        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawCircle")]
        private static extern void DLL_DrawCircle(uint theColor, bool filled, float xc, float yc, int radius);

        /// <summary>
        /// Draws a circle centered on a given x, y location
        /// </summary>
        /// <param name="theColor">The color to draw the circle</param>
        /// <param name="filled">True to draw a filled circle, false for outline</param>
        /// <param name="xc">The x location of the center of the circle</param>
        /// <param name="yc">The y location of the center of the circle</param>
        /// <param name="radius">The radius of the circle</param>
        public static void DrawCircle(Color theColor, bool filled, float xc, float yc, int radius)
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
        public static void DrawCircle(Color theColour, float xc, float yc, int radius)
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
        public static void FillCircle(Color theColour, float xc, float yc, int radius)
        {
            int color = theColour.ToArgb();
            DLL_DrawCircle((uint)color, true, xc, yc, radius);
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawEllipse")]
        private static extern void DLL_DrawEllipse(uint theColor, bool filled, float xPos, float yPos, int width, int height);

        /// <summary>
        /// Draws a ellipse within a given rectangle on the screen
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
        public static void DrawEllipse(Color theColor, float xPos, float yPos, int width, int height)
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
        public static void FillEllipse(Color theColor, float xPos, float yPos, int width, int height)
        {
            int color = theColor.ToArgb();
            DLL_DrawEllipse((uint)color, true, xPos, yPos, width, height);
        }






        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CreateSprite")]
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

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "FreeSprite")]
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

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "AddBitmapToSprite")]
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

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CurrentHeight")]
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

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CurrentWidth")]
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

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawSprite")]
        private static extern void DLL_DrawSprite(IntPtr spriteToDraw);
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawSpriteOffset")]
        private static extern void DLL_DrawSpriteOffset(IntPtr spriteToDraw, int xOffset, int yOffset);

        /// <summary>
        /// Draws the sprite to the screen within a given view port
        /// </summary>
        /// <param name="spriteToDraw">The sprite to be drawn</param>
        /// <param name="vwPrtX">The x of the current view port (i.e. screen)</param>
        /// <param name="vwPrtY">The y of the current view port (i.e. screen)</param>
        /// <param name="vwPrtWidth">The width of the view port</param>
        /// <param name="vwPrtHeight">The height of the view port</param>
        public static void DrawSprite(Sprite spriteToDraw, int xOffset, int yOffset)
        {
            DLL_DrawSpriteOffset(spriteToDraw.Pointer, xOffset, yOffset);
        }

        /// <summary>
        /// Draws a sprite to the screen, without using a view port
        /// </summary>
        /// <param name="spriteToDraw">The sprite to be drawn</param>
        public static void DrawSprite(Sprite spriteToDraw)
        {
            DLL_DrawSprite(spriteToDraw.Pointer);
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "MoveSprite")]
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

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "MoveSpriteTo")]
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

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "IsSpriteOffscreen")]
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

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "IsSpriteOffscreenWithViewPort")]
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

        // Sprite Additions

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CreateSpriteMultiEnding")]
        private static extern IntPtr DLL_CreateSpriteMultiEnding(IntPtr startBitmap, Boolean isMulti, int length, int[] framesPerCell, SpriteEndingAction endingAction, int width, int height);

        /// <summary>
        /// Creates a new Sprite
        /// </summary>
        /// <param name="startBitmap">Bitmap to add</param>
        /// <param name="isMulti">set to true if the bitmap is a tileset</param>
        /// <param name="framesPerCell">framesPerCell sets howmany times each frame is drawn</param>
        /// <param name="endingAction">sets the ending action</param>
        /// <param name="width">The width of the Sprite</param>
        /// <param name="height">The height of the Sprite</param>
        /// <returns>A Sprite</returns>
        public static Sprite CreateSprite(Bitmap startBitmap, Boolean isMulti, int[] framesPerCell, SpriteEndingAction endingAction, int width, int height)
        {
            Sprite result;
            result.Pointer = DLL_CreateSpriteMultiEnding(startBitmap.pointer, isMulti, framesPerCell.Length, framesPerCell, endingAction, width, height);
            return result;
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CreateSpriteMulti")]
        private static extern IntPtr DLL_CreateSpriteMulti(IntPtr startBitmap, Boolean isMulti, int length, int[] framesPerCell, int width, int height);

        /// <summary>
        /// Creates a new Sprite
        /// </summary>
        /// <param name="startBitmap">Bitmap to add</param>
        /// <param name="isMulti">set to true if the bitmap is a tileset</param>
        /// <param name="framesPerCell">framesPerCell sets howmany times each frame is drawn</param>
        /// <param name="width">The width of the Sprite</param>
        /// <param name="height">The height of the Sprite</param>
        /// <returns>A Sprite</returns>
        public static Sprite CreateSprite(Bitmap startBitmap, Boolean isMulti, int[] framesPerCell, int width, int height)
        {
            Sprite result;
            result.Pointer = DLL_CreateSpriteMulti(startBitmap.pointer, isMulti,framesPerCell.Length, framesPerCell, width, height);
            return result;
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CreateSpriteArrayEnding")]
        private static extern IntPtr DLL_CreateSpriteArrayEnding(int bitmaplength, IntPtr[] startBitmap, int length, int[] framesPerCell, int endingAction);

        /// <summary>
        /// Creates a new Sprite
        /// </summary>
        /// <param name="startBitmap">Bitmap to add</param>
        /// <param name="framesPerCell">framesPerCell sets howmany times each frame is drawn</param>
        /// <param name="endingAction">sets the ending action</param>
        /// <param name="width">The width of the Sprite</param>
        /// <param name="height">The height of the Sprite</param>
        /// <returns>A Sprite</returns>
        public static Sprite CreateSprite(Bitmap[] startBitmap, int[] framesPerCell, SpriteEndingAction endingAction)
        {
            Sprite result;
            IntPtr[] temp = new IntPtr[startBitmap.Length];

            for (int i = 0; i < startBitmap.Length; i++)
            {
                temp[i] = startBitmap[i].pointer;
            }

            result.Pointer = DLL_CreateSpriteArrayEnding(temp.Length, temp,framesPerCell.Length, framesPerCell, (int)endingAction);
            return result;
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CreateSpriteArray")]
        private static extern IntPtr DLL_CreateSpriteArray(int bitmaplength, IntPtr[] startBitmap, int length, int[] framesPerCell);

        /// <summary>
        /// Creates a new Sprite
        /// </summary>
        /// <param name="startBitmap">Bitmap to add</param>
        /// <param name="framesPerCell">framesPerCell sets howmany times each frame is drawn</param>
        /// <param name="width">The width of the Sprite</param>
        /// <param name="height">The height of the Sprite</param>
        /// <returns>A Sprite</returns>
        public static Sprite CreateSprite(Bitmap[] startBitmap, int[] framesPerCell)
        {
            Sprite result;
            IntPtr[] temp = new IntPtr[startBitmap.Length];

            for (int i = 0; i < startBitmap.Length; i++)
            {
                temp[i] = startBitmap[i].pointer;
            }
            result.Pointer = DLL_CreateSpriteArray(temp.Length, temp, framesPerCell.Length, framesPerCell);
            return result;
        }



        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "UpdateSprite")]
        private static extern void DLL_UpdateSprite(IntPtr sprite);

        /// <summary>
        /// Updates a Sprite
        /// </summary>
        /// <param name="sprite">The Sprite</param>
        public static void UpdateSprite(Sprite sprite)
        {
            DLL_UpdateSprite(sprite.Pointer);
        }


        // Screen ViewPort Functions

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern int XOffset();

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern int YOffset();

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ScreenX(float x);

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ScreenY(float y);

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern float GameX(int x);

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern float GameY(int y);

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern float ToGameCoordinates(Vector screenVector);

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern void DLL_MoveVisualAreaWithVector(Vector v);
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern void DLL_MoveVisualArea(float dx, float dy);

        public static void MoveVisualArea(Vector v)
        {
            DLL_MoveVisualAreaWithVector(v);
        }

        public static void MoveVisualArea(float dx, float dy)
        {
            DLL_MoveVisualArea(dx, dy);
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern void SetScreenOffset(float dx, float dy);





        // Draw On Screen Functions


        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="DrawBitmapPartOnScreen")]
        private static extern void DLL_DrawBitmapPartOnScreen(IntPtr bitmapToDraw, int srcX, int srcY, int srcW, int srcH, int x, int y);

        public static void DrawBitmapPartOnScreen(Bitmap bitmapToDraw, int srcX, int srcY, int srcW, int srcH, int x, int y)
        {
            DLL_DrawBitmapPartOnScreen(bitmapToDraw.pointer, srcX, srcY, srcW, srcH, x, y);
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawBitmapOnScreen")]
        private static extern void DLL_DrawBitmapOnScreen(IntPtr bitmapToDraw, int x, int y);

        public static void DrawBitmapOnScreenn(Bitmap bitmapToDraw, int x, int y)
        {
            DLL_DrawBitmapOnScreen(bitmapToDraw.pointer, x, y);
        }
        
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawPixelOnScreen")]
        private static extern void DLL_DrawPixelOnScreen(Color theColor, int x, int y);

        public static void DrawPixelOnScreen(Color theColor, int x, int y)
        {
            DLL_DrawPixelOnScreen(theColor, x, y);
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawRectangleOnScreen")]
        private static extern void DLL_DrawRectangleOnScreen(Color theColor, bool filled, int x, int y, int width, int height);

        public static void DrawRectangleOnScreen(Color theColor, int x, int y, int width, int height)
        {
            DLL_DrawRectangleOnScreen(theColor, false, x, y, width, height);
        }

        public static void FillRectangleOnScreen(Color theColor, int x, int y, int width, int height)
        {
            DLL_DrawRectangleOnScreen(theColor, true, x, y, width, height);
        }



        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawLineOnScreen")]
        private static extern void DLL_DrawLineOnScreen(Color theColor, int x, int y, int x2, int y2);

        public static void DrawLineOnScreen(Color theColor, int xPosStart, int yPosStart, int xPosEnd, int yPosEnd)
        {
            DLL_DrawLineOnScreen(theColor, xPosStart, yPosStart, xPosEnd, yPosEnd);
        }


        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawHorizontalLineOnScreen")]
        private static extern void DLL_DrawHorizontalLineOnScreen(Color theColor, int y, int x1, int x2);

        public static void DrawHorizontalLineOnScreen(Color theColor, int y, int x1, int x2)
        {
            DLL_DrawHorizontalLineOnScreen(theColor, y, x1, x2);
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawVerticalLineOnScreen")]
        private static extern void DLL_DrawVerticalLineOnScreen(Color theColor, int x, int y1, int y2);

        public static void DrawVerticalLineOnScreen(Color theColor, int x, int y1, int y2)
        {
            DLL_DrawVerticalLineOnScreen(theColor, x, y1, y2);
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawCircleOnScreen")]
        private static extern void DLL_DrawCircleOnScreen(Color theColor, bool filled, int xc, int yc, int radius);

        public static void DrawCircleOnScreen(Color theColor, int xc, int yc, int radius)
        {
            DLL_DrawCircleOnScreen(theColor, false, xc, yc, radius);
        }

        public static void FillCircleOnScreen(Color theColor, int xc, int yc, int radius)
        {
            DLL_DrawCircleOnScreen(theColor, true, xc, yc, radius);
        }


        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawEllipseOnScreen")]
        private static extern void DLL_DrawEllipseOnScreen(Color theColor, bool filled, int x, int y, int width, int height);

        public static void DrawEllipseOnScreen(Color theColor, int x, int y, int width, int height)
        {
            DLL_DrawEllipseOnScreen(theColor, false, x, y, width, height);
        }

        public static void FillEllipseOnScreen(Color theColor, int x, int y, int width, int height)
        {
            DLL_DrawEllipseOnScreen(theColor, true, x, y, width, height);
        }


    }
}
