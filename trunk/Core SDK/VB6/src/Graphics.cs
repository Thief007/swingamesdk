using System;
using System.Collections.Generic;
using System.Text;
using SwinGame;
using System.Runtime.InteropServices;
using System.Drawing;
using System.IO;

namespace SwinGameVB
{


    /// <summary>
    /// This contains number of bitmaps and its position.
    /// </summary>
    [ClassInterface(ClassInterfaceType.None)]
    [Guid("4987E5FC-88C6-463f-B516-9F9EEFEE5BC4")]
    [ComVisible(true)]
    public class Sprite :ISprite
    {

        /// <summary>
        /// Array of bitmaps this sprite contains
        /// </summary>
        /// <param name="idx">Index number</param>
        /// <returns>Bitmap of the specified frame</returns>

        SwinGame.Sprite sprite = new SwinGame.Sprite();
        internal SwinGame.Sprite result
        {
            get
            {
                return sprite;
            }
            set
            {
                sprite = value;
            }
        }
        public Bitmap GetBimapInSprite(int idx)
        {
            Bitmap temp = new Bitmap();
            temp.result = sprite[idx];
            return temp; 
        }
        internal void Free()
        {
            SwinGame.Graphics.FreeSprite(ref sprite);
        }

        /// <summary>
        /// X position of this sprite
        /// </summary>
        public void SetX(float value)
        {
                sprite.xPos = value;
            
        }
        public float GetX()
        {
                return sprite.xPos;
            
        }

        /// <summary>
        /// Y position of this sprite
        /// </summary>
        public float GetY()
        {
                return sprite.yPos;
            
        }

        public void SetY(float value)
        {
                sprite.yPos = value;
            
        }

        /// <summary>
        /// Current animation frame of this sprite
        /// </summary>
        public int GetCurrentFrame()
        {
                return sprite.CurrentFrame;
            
        }
        public void SetCurrentFrame(int value)
        {
                sprite.CurrentFrame = value;
            
        }

        /// <summary>
        /// True if this sprite use pixel collision
        /// </summary>
        public void SetUsePixelCollision(bool value)
        {
                sprite.UsePixelCollision = value;
            
        }

        public bool GetUsePixelCollision()
        {
                return sprite.UsePixelCollision;
            
        }

        /// <summary>
        /// Gets the Sprite Kind
        /// </summary>
        public SpriteKind GetSpriteKind()
        {
                return (SpriteKind)sprite.SpriteKind;
            
        }


        /// <summary>
        /// Gets the number of Columns
        /// </summary>
        public int GetCols()
        {
                return sprite.Cols;
            
        }

        /// <summary>
        /// Gets the number of Rows
        /// </summary>
        public int GetRows()
        {
                return sprite.Rows;
            
        }

        /// <summary>
        /// Gets the Frame Count
        /// </summary>
        public int GetFrameCount()
        {
                return sprite.FrameCount;
            
        }

        /// <summary>
        /// Gets the Ending Action
        /// </summary>
        public SpriteEndingAction GetEndingAction()
        {
                return (SwinGameVB.SpriteEndingAction)sprite.EndingAction;
            
        }

        /// <summary>
        /// Gets whether the Sprite Animation has Ended
        /// </summary>
        public Boolean GetHasEnded()
        {
                return sprite.hasEnded;
            
        }

        /// <summary>
        /// Gets whether the Sprite is reversed
        /// </summary>
        public Boolean GetReverse()
        {
                return sprite.Reverse;
            
        }



        public float GetMovementX()
        {
            return sprite.Movement.X;
        }
        public void SetMovementX(float value)
        {
            sprite.Movement.X = value;
        }


        public float GetMovementY()
        {
            return sprite.Movement.Y;
        }
        public void SetMovementY(float value)
        {
            sprite.Movement.Y = value;
        }

        public void SetMovementVector(Vector v)
        {
            sprite.Movement.SetTo(v.result);
        }

        public Vector GetMovementVector()
        {
            Vector vector = new Vector();
            vector.result = sprite.Movement;
            return vector;
        }


        public void SetMass(float value)
        {
                sprite.Mass = value;
            
        }
        public float GetMass()
        {
                return sprite.Mass;
        }
    }

    [Guid("AE234F8A-BAFD-4f42-8090-5054216847EB")]
    [ComVisible(true)]
    public interface ISprite
    {
        Bitmap GetBimapInSprite(int idx);
        void SetX(float value);
        float GetX();
        float GetY();
        void SetY(float value);
        int GetCurrentFrame();
        void SetCurrentFrame(int value);
        void SetUsePixelCollision(bool value);
        bool GetUsePixelCollision();
        SpriteKind GetSpriteKind();
        int GetCols();
        int GetRows();
        int GetFrameCount();
        SpriteEndingAction GetEndingAction();
        Boolean GetHasEnded();
        Boolean GetReverse();
        float GetMovementX();
        void SetMovementX(float value);
        float GetMovementY();
        void SetMovementY(float value);
        void SetMovementVector(Vector v);
        Vector GetMovementVector();
        void SetMass(float value);
        float GetMass();
    }

    [Guid("A2B86CBD-7671-434c-8E83-F07FC5F30A8B")]
    [ComVisible(true)]
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
    [Guid("B9D7E783-40D2-4930-A560-2A98BFFD5D41")]
    [ComVisible(true)]
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
    [ClassInterface(ClassInterfaceType.None)]
    [Guid("D8E12F39-35E8-491b-8C4D-B84309B09C6B")]
    [ComVisible(true)]
    public class Graphics : IGraphics
    {
        /// <summary>
        /// Create a bitmap
        /// </summary>
        /// <param name="width">Width of a bitmap</param>
        /// <param name="height">Height of a bitmap</param>
        /// <returns>New bitmap</returns>
        public Bitmap CreateBitmap(int width, int height)
        {
            Bitmap bitmap = new Bitmap();
            bitmap.result = SwinGame.Graphics.CreateBitmap(width, height);
            return bitmap;
        }

        /// <summary>
        /// Optimise the specified bitmap
        /// </summary>
        /// <param name="surface">Bitmap to optimise</param>
        public void OptimiseBitmap(Bitmap surface)
        {
            SwinGame.Graphics.OptimiseBitmap(surface.result);
        }

        /// <summary>
        /// Load the specified image file
        /// </summary>
        /// <param name="pathToBitmap">Path to the image file</param>
        /// <returns>New bitmap</returns>
        public Bitmap LoadBitmap(String pathToBitmap)
        {
            Bitmap result = new Bitmap();
            result.result = SwinGame.Graphics.LoadBitmap(pathToBitmap);
            return result;
        }

        /// <summary>
        /// Load the specified image file with a transparent color
        /// </summary>
        /// <param name="pathToBitmap">Path to the image file</param>
        /// <param name="transparent">True if this image has transparent pixels</param>
        /// <param name="transparentColor">Color of the transparent pixels</param>
        /// <returns>New bitmap</returns>
        public Bitmap LoadBitmap_Transparent(String pathToBitmap, Boolean transparent, Color transparentColor)
        {
            Bitmap result = new Bitmap();
            result.result = SwinGame.Graphics.LoadBitmap(pathToBitmap, transparent, transparentColor);
            return result;
        }

        /// <summary>
        /// Load an image with transparency
        /// </summary>
        /// <param name="pathToBitmap">Path to the image file</param>
        /// <param name="transparentColor">Color of the transparent pixels</param>
        /// <returns>New bitmap</returns>
        public Bitmap LoadTransparentBitmap(string pathToBitmap, Color transparentColor)
        {
            Bitmap result = new Bitmap();
            result.result = SwinGame.Graphics.LoadTransparentBitmap(pathToBitmap, transparentColor);
            return result;
        }

        /// <summary>
        /// Free the specified bitmap
        /// </summary>
        /// <param name="bitmapToFree">Bitmap to free</param>
        public void FreeBitmap(ref Bitmap bitmapToFree)
        {
            bitmapToFree.Free();
        }

        /// <summary>
        /// Get the specified bitmap's width
        /// </summary>
        /// <param name="targetbitmap">Target bitmap</param>
        /// <returns>Width of the bitmap</returns>
        public int GetBitmapWidth(Bitmap targetbitmap)
        {
            return SwinGame.Graphics.GetBitmapWidth(targetbitmap.result);
        }

        /// <summary>
        /// Get the specified bitmap's height
        /// </summary>
        /// <param name="targetbitmap">Target bitmap</param>
        /// <returns>Height of the bitmap</returns>
        public int GetBitmapHeight(Bitmap targetbitmap)
        {
            return SwinGame.Graphics.GetBitmapHeight(targetbitmap.result);
        }

        /// <summary>
        /// Clear the bitmap with the specified color
        /// </summary>
        /// <param name="dest">Bitmap to clear</param>
        /// <param name="toColour">The color used to clear</param>
        public void ClearSurface_Colour(Bitmap dest, int toColour)
        {
            Color colour = Color.FromArgb(toColour);
            SwinGame.Graphics.ClearSurface(dest.result, colour);
        }

        /// <summary>
        /// Clear the bitmap
        /// </summary>
        /// <param name="dest">Bitmap to clear</param>
        public void ClearSurface(Bitmap dest)
        {
            SwinGame.Graphics.ClearSurface(dest.result, Color.Black);
        }

        /// <summary>
        /// Draw bitmap to the specified bitmap
        /// </summary>
        /// <param name="dest">Bitmap to draw on</param>
        /// <param name="bitmapToDraw">Bitmap to draw</param>
        /// <param name="x">X coordinate</param>
        /// <param name="y">Y coordinate</param>
        public void DrawBitmap_OnBitmap(Bitmap dest, Bitmap bitmapToDraw, int x, int y)
        {
            SwinGame.Graphics.DrawBitmap(dest.result, bitmapToDraw.result, x, y);
        }

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
        public void DrawBitmapPart_OnBitmap(Bitmap dest, Bitmap bitmapToDraw, int srcX, int srcY, int srcW, int srcH, int x, int y)
        {
            SwinGame.Graphics.DrawBitmapPart(dest.result, bitmapToDraw.result, srcX, srcY, srcW, srcH, x, y);
        }

        /// <summary>
        /// Draws a pixel onto the destination bitmap
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the pixel</param>
        /// <param name="x">The x location to draw the pixel at</param>
        /// <param name="y">The y location to draw the pixel at</param>
        public void DrawPixel_OnBitmap(Bitmap dest, int theColour, int x, int y)
        {
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawPixel(dest.result, color, x, y);
        }

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
        public void DrawRectangle_OnBitmap(Bitmap dest, int theColour, bool filled, int xPos, int yPos, int width, int height)
        {
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawRectangle(dest.result, color, filled, xPos, yPos, width, height);
        }

        /// <summary>
        /// Draws a filled rectangle on the destination bitmap
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the rectangle</param>
        /// <param name="xPos">The x location to draw the rectangle at</param>
        /// <param name="yPos">The y location to draw the rectangle at</param>
        /// <param name="width">The width of the rectangle</param>
        /// <param name="height">The height of the rectangle</param>
        public void FillRectangle_OnBitmap(Bitmap dest, int theColour, int xPos, int yPos, int width, int height)
        {
            Color color = Color.FromArgb(theColour);
           SwinGame.Graphics.FillRectangle(dest.result, color, xPos, yPos, width, height);
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
        public void DrawRectangle_OnBitmap_NoFill(Bitmap dest, int theColour, int xPos, int yPos, int width, int height)
        {
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawRectangle(dest.result, color, false, xPos, yPos, width, height);
        }

        /// <summary>
        /// Draws a line on the destination bitmap
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the line</param>
        /// <param name="xPosStart">The x location to start the line at</param>
        /// <param name="yPosStart">The y location to start the line at</param>
        /// <param name="xPosEnd">The x location to end the line at</param>
        /// <param name="yPosEnd">The y location to end the line at</param>
        public void DrawLine_OnBitmap(Bitmap dest, int theColour, int xPosStart, int yPosStart, int xPosEnd, int yPosEnd)
        {
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawLine(dest.result, color, xPosStart, yPosStart, xPosEnd, yPosEnd);
        }

        /// <summary>
        /// Draws a horizontal line on the destination bitmap
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the line</param>
        /// <param name="y">The y location of the line</param>
        /// <param name="x1">The starting x value of the line</param>
        /// <param name="x2">The starting y value of the line</param>
        public void DrawHorizontalLine_OnBitmap(Bitmap dest, int theColour, int y, int x1, int x2)
        {
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawHorizontalLine(dest.result, color, y, x1, x2);
        }

        /// <summary>
        /// Draws a vertical line on the destination bitmap
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the line</param>
        /// <param name="x">The x location of the line</param>
        /// <param name="y1">The starting y value of the line</param>
        /// <param name="y2">The ending y value of the line</param>
        public void DrawVerticalLine_OnBitmap(Bitmap dest, int theColour, int x, int y1, int y2)
        {
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawVerticalLine(dest.result, color, x, y1, y2);
        }

        /// <summary>
        /// Draws a circle centered on a given x, y location
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the circle</param>
        /// <param name="filled">True to draw a filled circle, false for outline</param>
        /// <param name="xc">The x location of the center of the circle</param>
        /// <param name="yc">The y location of the center of the circle</param>
        /// <param name="radius">The radius of the circle</param>
        public void DrawCircle_OnBitmap(Bitmap dest, int theColour, bool filled, int xc, int yc, int radius)
        {
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawCircle(dest.result, color, filled, xc, yc, radius);
        }

        /// <summary>
        /// Draws a circle outline centered on a given x, y location
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the circle</param>
        /// <param name="xc">The x location of the center of the circle</param>
        /// <param name="yc">The y location of the center of the circle</param>
        /// <param name="radius">The radius of the circle</param>
        public void DrawCircle_OnBitmap_NoFill(Bitmap dest, int theColour, int xc, int yc, int radius)
        {
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawCircle(dest.result, color, false, xc, yc, radius);
        }

        /// <summary>
        /// Draws a filled circle centered on a given x, y location
        /// </summary>
        /// <param name="dest">The destination bitmap</param>
        /// <param name="theColour">The color to draw the circle</param>
        /// <param name="xc">The x location of the center of the circle</param>
        /// <param name="yc">The y location of the center of the circle</param>
        /// <param name="radius">The radius of the circle</param>
        public void FillCircle_OnBitmap(Bitmap dest, int theColour, int xc, int yc, int radius)
        {
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.FillCircle(dest.result, color, xc, yc, radius);
        }

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
        public void DrawEllipse_OnBitmap(Bitmap dest, int theColour, bool filled, int xPos, int yPos, int width, int height)
        {
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawEllipse(dest.result, color, filled, xPos, yPos, width, height);
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
        public void DrawEllipse_OnBitmap_NoFill(Bitmap dest, int theColour, int xPos, int yPos, int width, int height)
        {
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawEllipse(dest.result, color, false, xPos, yPos, width, height);
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
        public void FillEllipse_OnBitmap(Bitmap dest, int theColour, int xPos, int yPos, int width, int height)
        {
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.FillEllipse(dest.result, color, xPos, yPos, width, height);
        }

        /// <summary>
        /// Clears the surface of the screen to the passed in color
        /// </summary>
        /// <param name="toColour">The colour to clear the bitmap to</param>
        public void ClearScreen_ToColour(int toColour)
        {
            Color color = Color.FromArgb(toColour);
            SwinGame.Graphics.ClearScreen(color);
        }

        /// <summary>
        /// Clears the screen to Black
        /// </summary>
        public void ClearScreen()
        {
            SwinGame.Graphics.ClearScreen(Color.Black);
        }

        /// <summary>
        /// Draws one bitmap (bitmapToDraw) onto the screen
        /// </summary>
        /// <param name="bitmapToDraw">The bitmap to be drawn onto the screen</param>
        /// <param name="x">The x location to draw the bitmap to</param>
        /// <param name="y">The y location to draw the bitmap to</param>
        public void DrawBitmap(Bitmap bitmapToDraw, float x, float y)
        {
            SwinGame.Graphics.DrawBitmap(bitmapToDraw.result, x, y);
        }
        
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
        public void DrawBitmapPart(Bitmap bitmapToDraw, int srcX, int srcY, int srcW, int srcH, float x, float y)
        {
            SwinGame.Graphics.DrawBitmapPart(bitmapToDraw.result, srcX, srcY, srcW, srcH, x, y);
        }

        /// <summary>
        /// Draws a pixel onto the screen
        /// </summary>
        /// <param name="theColour">The color to draw the pixel</param>
        /// <param name="x">The x location to draw the pixel at</param>
        /// <param name="y">The y location to draw the pixel at</param>
        public void DrawPixel(int theColour, float x, float y)
        {
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawPixel(color, x, y);
        }

        /// <summary>
        /// Draws a rectangle on the screen
        /// </summary>
        /// <param name="theColour">The color to draw the rectangle</param>
        /// <param name="filled">True to draw a filled rectangle, false for outline</param>
        /// <param name="xPos">The x location to draw the rectangle at</param>
        /// <param name="yPos">The y location to draw the rectangle at</param>
        /// <param name="width">The width of the rectangle</param>
        /// <param name="height">The height of the rectangle</param>
        public void DrawRectangle(int theColour, bool filled, float xPos, float yPos, int width, int height)
        {
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawRectangle(color, filled, xPos, yPos, width, height);
        }

        /// <summary>
        /// Draws the outline of a rectangle on the screen
        /// </summary>
        /// <param name="theColour">The color to draw the rectangle</param>
        /// <param name="xPos">The x location to draw the rectangle at</param>
        /// <param name="yPos">The y location to draw the rectangle at</param>
        /// <param name="width">The width of the rectangle</param>
        /// <param name="height">The height of the rectangle</param>
        public void DrawRectangle(int theColour, float xPos, float yPos, int width, int height)
        {
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawRectangle(color, false, xPos, yPos, width, height);
        }

        /// <summary>
        /// Draws a filled rectangle on the screen
        /// </summary>
        /// <param name="theColour">The color to draw the rectangle</param>
        /// <param name="xPos">The x location to draw the rectangle at</param>
        /// <param name="yPos">The y location to draw the rectangle at</param>
        /// <param name="width">The width of the rectangle</param>
        /// <param name="height">The height of the rectangle</param>
        public void FillRectangle(int theColour, float xPos, float yPos, int width, int height)
        {
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.FillRectangle(color, xPos, yPos, width, height);
        }

        /// <summary>
        /// Draws a line on the screen
        /// </summary>
        /// <param name="theColour">The color to draw the line</param>
        /// <param name="xPosStart">The x location to start the line at</param>
        /// <param name="yPosStart">The y location to start the line at</param>
        /// <param name="xPosEnd">The x location to end the line at</param>
        /// <param name="yPosEnd">The y location to end the line at</param>
        public void DrawLine(int theColour, float xPosStart, float yPosStart, float xPosEnd, float yPosEnd)
        {
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawLine(color, xPosStart, yPosStart, xPosEnd, yPosEnd);
        }

        /// <summary>
        /// Draws a horizontal line on the screen
        /// </summary>
        /// <param name="theColour">The color to draw the line</param>
        /// <param name="y">The y location of the line</param>
        /// <param name="x1">The starting x value of the line</param>
        /// <param name="x2">The ending x value of the line</param>
        public void DrawHorizontalLine(int theColour, float y, float x1, float x2)
        {
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawHorizontalLine(color, y, x1, x2);
        }

        /// <summary>
        /// Draws a vertical line on the screen
        /// </summary>
        /// <param name="theColour">The color to draw the line</param>
        /// <param name="x">The color to draw the line</param>
        /// <param name="y1">The starting y value of the line</param>
        /// <param name="y2">The ending y value of the line</param>
        public void DrawVerticalLine(int theColour, float x, float y1, float y2)
        {
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawVerticalLine(color, x, y1, y2);
        }

        /// <summary>
        /// Draws a circle centered on a given x, y location
        /// </summary>
        /// <param name="theColour">The color to draw the circle</param>
        /// <param name="filled">True to draw a filled circle, false for outline</param>
        /// <param name="xc">The x location of the center of the circle</param>
        /// <param name="yc">The y location of the center of the circle</param>
        /// <param name="radius">The radius of the circle</param>
        public void DrawCircle(int theColour, bool filled, float xc, float yc, int radius)
        {
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawCircle(color, filled, xc, yc, radius);
        }

        /// <summary>
        /// Draws a circle outline centered on a given x, y location
        /// </summary>
        /// <param name="theColour">The color to draw the circle</param>
        /// <param name="xc">The x location of the center of the circle</param>
        /// <param name="yc">The y location of the center of the circle</param>
        /// <param name="radius">The radius of the circle</param>
        public void DrawCircle_NoFill(int theColour, float xc, float yc, int radius)
        {
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawCircle(color, false, xc, yc, radius);
        }

        /// <summary>
        /// Draws a filled circle centered on a given x, y location
        /// </summary>
        /// <param name="theColour">The color to draw the circle</param>
        /// <param name="xc">The x location of the center of the circle</param>
        /// <param name="yc">The y location of the center of the circle</param>
        /// <param name="radius">The radius of the circle</param>
        public void FillCircle(int theColour, float xc, float yc, int radius)
        {
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.FillCircle(color, xc, yc, radius);
        }

        /// <summary>
        /// Draws a ellipse within a given rectangle on the screen
        /// </summary>
        /// <param name="theColour">The color to draw the ellipse</param>
        /// <param name="filled">True to draw a filled ellipse, false for outline</param>
        /// <param name="xPos">The x location of the top left of the ellipse</param>
        /// <param name="yPos">The y location of the top left of the ellipse</param>
        /// <param name="width">The width of the ellipse</param>
        /// <param name="height">The height of the ellipse</param>
        public void DrawEllipse(int theColour, bool filled, float xPos, float yPos, int width, int height)
        {
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawEllipse(color, filled, xPos, yPos, width, height);
        }

        /// <summary>
        /// Draws a ellipse outline within a given rectangle on the screen
        /// </summary>
        /// <param name="theColour">The color to draw the ellipse</param>
        /// <param name="xPos">The x,y location of the top left of the ellipse</param>
        /// <param name="yPos">The y location of the top left of the ellipse</param>
        /// <param name="width">The width and height of the ellipse</param>
        /// <param name="height">The height of the ellipse</param>
        public void DrawEllipse_NoFill(int theColour, float xPos, float yPos, int width, int height)
        {
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawEllipse(color, false, xPos, yPos, width, height);
        }

        /// <summary>
        /// Draws a filled ellipse within a given rectangle on the screen
        /// </summary>
        /// <param name="theColour">The color to draw the ellipse</param>
        /// <param name="xPos">The x location of the top left of the ellipse</param>
        /// <param name="yPos">The y location of the top left of the ellipse</param>
        /// <param name="width">The width of the ellipse</param>
        /// <param name="height">The height of the ellipse</param>
        public void FillEllipse(int theColour, float xPos, float yPos, int width, int height)
        {
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawEllipse(color, true, xPos, yPos, width, height);
        }
        
        /// <summary>
        /// Creates a sprites, and sets its firat bitmap
        /// </summary>
        /// <param name="startBitmap">The sprites first bitmap (index 0)</param>
        /// <returns>A new sprite with this bitmap as its first bitmap</returns>
        public Sprite CreateSprite(Bitmap startBitmap)
        {
            Sprite result = new Sprite();
            result.result = SwinGame.Graphics.CreateSprite(startBitmap.result);
            return result;
        }

        /// <summary>
        /// Frees a sprite, this does not free the sprite's bitmaps, which allows
        ///	bitmaps to be shared between sprites. All created sprites need to be
        ///	freed.
        /// </summary>
        /// <param name="spriteToFree">the sprite to free</param>
        public void FreeSprite(ref Sprite spriteToFree)
        {
            spriteToFree.Free();
        }

        /// <summary>
        /// Sprites may contain multiple images. These images can be used for things
        ///	line animation, facing, etc. This routine adds a bitmap to a sprite,
        ///	returning the index of the added bitmap.
        /// </summary>
        /// <param name="spriteToAddTo">the sprite to add the bitmap to</param>
        /// <param name="bitmapToAdd">the bitmap to add to the sprite</param>
        /// <returns>the index of the added bitmap</returns>
        public int AddBitmapToSprite(Sprite spriteToAddTo, Bitmap bitmapToAdd)
        {
            return SwinGame.Graphics.AddBitmapToSprite(spriteToAddTo.result, bitmapToAdd.result);
        }

        /// <summary>
        /// Returns the current height of the sprite
        /// </summary>
        /// <param name="sprite">The sprite to get the height of</param>
        /// <returns>The height of the sprite's current frame</returns>
        public int CurrentHeight(Sprite sprite)
        {
            return SwinGame.Graphics.CurrentHeight(sprite.result);
        }

        /// <summary>
        /// Returns the current width of the sprite
        /// </summary>
        /// <param name="sprite">The sprite to get the width of</param>
        /// <returns>The width of the sprite's current frame</returns>
        public int CurrentWidth(Sprite sprite)
        {
            return SwinGame.Graphics.CurrentWidth(sprite.result);
        }

        /// <summary>
        /// Draws the sprite to the screen within a given view port
        /// </summary>
        /// <param name="spriteToDraw">The sprite to be drawn</param>
        /// <param name="vwPrtX">The x of the current view port (i.e. screen)</param>
        /// <param name="vwPrtY">The y of the current view port (i.e. screen)</param>
        /// <param name="vwPrtWidth">The width of the view port</param>
        /// <param name="vwPrtHeight">The height of the view port</param>
        public void DrawSprite_WithOffset(Sprite spriteToDraw, int xOffset, int yOffset)
        {
            SwinGame.Graphics.DrawSprite(spriteToDraw.result, xOffset, yOffset);
        }

        /// <summary>
        /// Draws a sprite to the screen, without using a view port
        /// </summary>
        /// <param name="spriteToDraw">The sprite to be drawn</param>
        public void DrawSprite(Sprite spriteToDraw)
        {
            SwinGame.Graphics.DrawSprite(spriteToDraw.result);
        }

        /// <summary>
        /// Moves a sprite based on information in a movement vector
        /// </summary>
        /// <param name="spriteToMove">The sprite to move</param>
        /// <param name="movementVector">The vector containing the movement details</param>
        public void MoveSprite(Sprite spriteToMove, Vector movementVector)
        {
            SwinGame.Graphics.MoveSprite(spriteToMove.result, (SwinGame.Vector)movementVector.result);
        }

        /// <summary>
        /// Moves a sprite to a given x,y location
        /// </summary>
        /// <param name="spriteToMove">the sprite being moved</param>
        /// <param name="x">the new location of the sprite</param>
        /// <param name="y">the new location of the sprite</param>
        public void MoveSpriteTo(Sprite spriteToMove, int x, int y)
        {
            SwinGame.Graphics.MoveSpriteTo(spriteToMove.result, x, y);
        }

        /// <summary>
        /// Determines if a sprite is off the screen
        /// </summary>
        /// <param name="theSprite">The sprite to check the position of</param>
        /// <returns>True if the sprite is off the screen</returns>
        public bool IsSpriteOffscreen(Sprite theSprite)
        {
            return SwinGame.Graphics.IsSpriteOffscreen(theSprite.result);
        }

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
        public bool IsSpriteOffscreen_ViewPort(Sprite theSprite, int vwPrtX, int vwPrtY, int vwPrtWidth, int vwPrtHeight)
        {
            return SwinGame.Graphics.IsSpriteOffscreen(theSprite.result, vwPrtX, vwPrtY, vwPrtWidth, vwPrtHeight);
        }

        // Sprite Additions

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
        public Sprite CreateSprite_MultiEnding(Bitmap startBitmap, Boolean isMulti, [In] ref int[] framesPerCell, SpriteEndingAction endingAction, int width, int height)
        {
            Sprite result = new Sprite();
            result.result = SwinGame.Graphics.CreateSprite(startBitmap.result, isMulti, framesPerCell, (SwinGame.SpriteEndingAction)endingAction, width, height);
            return result;
        }

        /// <summary>
        /// Creates a new Sprite
        /// </summary>
        /// <param name="startBitmap">Bitmap to add</param>
        /// <param name="isMulti">set to true if the bitmap is a tileset</param>
        /// <param name="framesPerCell">framesPerCell sets howmany times each frame is drawn</param>
        /// <param name="width">The width of the Sprite</param>
        /// <param name="height">The height of the Sprite</param>
        /// <returns>A Sprite</returns>
        public Sprite CreateSprite_Multi(Bitmap startBitmap, Boolean isMulti, [In] ref int[] framesPerCell, int width, int height)
        {
            Sprite result =new Sprite();
            result.result = SwinGame.Graphics.CreateSprite(startBitmap.result, isMulti, framesPerCell, width, height);
            return result;
        }
        
        /// <summary>
        /// Creates a new Sprite
        /// </summary>
        /// <param name="startBitmap">Bitmap to add</param>
        /// <param name="framesPerCell">framesPerCell sets howmany times each frame is drawn</param>
        /// <param name="endingAction">sets the ending action</param>
        /// <param name="width">The width of the Sprite</param>
        /// <param name="height">The height of the Sprite</param>
        /// <returns>A Sprite</returns>
        public Sprite CreateSprite_ArrayEnding([In] ref Bitmap[] startBitmap, [In] ref int[] framesPerCell, SpriteEndingAction endingAction)
        {
            Sprite result = new Sprite();
            SwinGame.Bitmap[] temp = new SwinGame.Bitmap[startBitmap.Length];

            for (int i = 0; i < startBitmap.Length; i++)
            {
                temp[i] = startBitmap[i].result;
            }

            result.result = SwinGame.Graphics.CreateSprite(temp, framesPerCell, (SwinGame.SpriteEndingAction)endingAction);
            return result;
        }

        /// <summary>
        /// Creates a new Sprite
        /// </summary>
        /// <param name="startBitmap">Bitmap to add</param>
        /// <param name="framesPerCell">framesPerCell sets howmany times each frame is drawn</param>
        /// <param name="width">The width of the Sprite</param>
        /// <param name="height">The height of the Sprite</param>
        /// <returns>A Sprite</returns>
        public Sprite CreateSprite_Array([In] ref Bitmap[] startBitmap, [In] ref int[] framesPerCell)
        {
            Sprite result = new Sprite();
            SwinGame.Bitmap[] temp = new SwinGame.Bitmap[startBitmap.Length];

            for (int i = 0; i < startBitmap.Length; i++)
            {
                temp[i] = startBitmap[i].result;
            }
            result.result = SwinGame.Graphics.CreateSprite(temp, framesPerCell);
            return result;
        }

        /// <summary>
        /// Updates a Sprite
        /// </summary>
        /// <param name="sprite">The Sprite</param>
        public void UpdateSprite(Sprite sprite)
        {
            SwinGame.Graphics.UpdateSprite(sprite.result);
        }

        public void UpdateSpriteAnimation(Sprite sprite)
        {
            SwinGame.Graphics.UpdateSpriteAnimation(sprite.result);
        }


        // Screen ViewPort Functions
        /*
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
        */

        public void DrawBitmapPartOnScreen(Bitmap bitmapToDraw, int srcX, int srcY, int srcW, int srcH, int x, int y)
        {
            SwinGame.Graphics.DrawBitmapPartOnScreen(bitmapToDraw.result, srcX, srcY, srcW, srcH, x, y);
        }
        public void DrawBitmapOnScreen(Bitmap bitmapToDraw, int x, int y)
        {
            SwinGame.Graphics.DrawBitmapOnScreen(bitmapToDraw.result, x, y);
        }
        public void DrawPixelOnScreen(int theColour, int x, int y)
        {   
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawPixelOnScreen(color, x, y);
        }
        public void DrawRectangleOnScreen(int theColour, int x, int y, int width, int height)
        {   
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawRectangleOnScreen(color, x, y, width, height);
        }
        public void FillRectangleOnScreen(int theColour, int x, int y, int width, int height)
        {   
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.FillRectangleOnScreen(color, x, y, width, height);
        }
        public void DrawLineOnScreen(int theColour, int xPosStart, int yPosStart, int xPosEnd, int yPosEnd)
        {   
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawLineOnScreen(color, xPosStart, yPosStart, xPosEnd, yPosEnd);
        }
        public void DrawHorizontalLineOnScreen(int theColour, int y, int x1, int x2)
        {   
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawHorizontalLineOnScreen(color, y, x1, x2);
        }
        public void DrawVerticalLineOnScreen(int theColour, int x, int y1, int y2)
        {   
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawVerticalLineOnScreen(color, x, y1, y2);
        }
        public void DrawCircleOnScreen(int theColour, int xc, int yc, int radius)
        {   
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawCircleOnScreen(color, xc, yc, radius);
        }
        public void FillCircleOnScreen(int theColour, int xc, int yc, int radius)
        {   
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.FillCircleOnScreen(color, xc, yc, radius);
        }
        public void DrawEllipseOnScreen(int theColour, int x, int y, int width, int height)
        {   
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.DrawEllipseOnScreen(color, x, y, width, height);
        }
        public void FillEllipseOnScreen(int theColour, int x, int y, int width, int height)
        {   
            Color color = Color.FromArgb(theColour);
            SwinGame.Graphics.FillEllipseOnScreen(color, x, y, width, height);
        }
        public Sprite CreateSprite_ArrayFPC(Bitmap[] startBitmap, int framesPerCell, int frames)
        {
            Sprite result = new Sprite();
            SwinGame.Bitmap[] temp = new SwinGame.Bitmap[startBitmap.Length];

            for (int i = 0; i < startBitmap.Length; i++)
            {
                temp[i] = startBitmap[i].result;
            }
            result.result =SwinGame.Graphics.CreateSprite(temp, framesPerCell, frames);
            return result;
        }
        public Sprite CreateSprite_MultiFPC(Bitmap startBitmap, int framesPerCell, int frames, int width, int height)
        {
            Sprite result = new Sprite();
            result.result = SwinGame.Graphics.CreateSprite(startBitmap.result, framesPerCell, frames, width, height);
            return result;
        }
    }
    
    [Guid("C261890B-D65E-4d4c-A7AA-F15FCC0F825A")]
    [ComVisible(true)]
    public interface IGraphics
    {
        Bitmap CreateBitmap(int width, int height);
        void OptimiseBitmap(Bitmap surface);
        Bitmap LoadBitmap(String pathToBitmap);
        Bitmap LoadBitmap_Transparent(String pathToBitmap, Boolean transparent, Color transparentColor);
        Bitmap LoadTransparentBitmap(string pathToBitmap, Color transparentColor);
        void FreeBitmap(ref Bitmap bitmapToFree);
        int GetBitmapWidth(Bitmap targetbitmap);
        int GetBitmapHeight(Bitmap targetbitmap);
        void ClearSurface_Colour(Bitmap dest, int toColour);
        void ClearSurface(Bitmap dest);
        void DrawBitmap_OnBitmap(Bitmap dest, Bitmap bitmapToDraw, int x, int y);
        void DrawBitmapPart_OnBitmap(Bitmap dest, Bitmap bitmapToDraw, int srcX, int srcY, int srcW, int srcH, int x, int y);
        void DrawPixel_OnBitmap(Bitmap dest, int theColour, int x, int y);
        void DrawRectangle_OnBitmap(Bitmap dest, int theColour, bool filled, int xPos, int yPos, int width, int height);
        void FillRectangle_OnBitmap(Bitmap dest, int theColour, int xPos, int yPos, int width, int height);
        void DrawRectangle_OnBitmap_NoFill(Bitmap dest, int theColour, int xPos, int yPos, int width, int height);
        void DrawLine_OnBitmap(Bitmap dest, int theColour, int xPosStart, int yPosStart, int xPosEnd, int yPosEnd);
        void DrawHorizontalLine_OnBitmap(Bitmap dest, int theColour, int y, int x1, int x2);
        void DrawVerticalLine_OnBitmap(Bitmap dest, int theColour, int x, int y1, int y2);
        void DrawCircle_OnBitmap(Bitmap dest, int theColour, bool filled, int xc, int yc, int radius);
        void DrawCircle_OnBitmap_NoFill(Bitmap dest, int theColour, int xc, int yc, int radius);
        void FillCircle_OnBitmap(Bitmap dest, int theColour, int xc, int yc, int radius);
        void DrawEllipse_OnBitmap(Bitmap dest, int theColour, bool filled, int xPos, int yPos, int width, int height);
        void DrawEllipse_OnBitmap_NoFill(Bitmap dest, int theColour, int xPos, int yPos, int width, int height);
        void FillEllipse_OnBitmap(Bitmap dest, int theColour, int xPos, int yPos, int width, int height);
        void ClearScreen_ToColour(int toColour);
        void ClearScreen();
        void DrawBitmap(Bitmap bitmapToDraw, float x, float y);
        void DrawBitmapPart(Bitmap bitmapToDraw, int srcX, int srcY, int srcW, int srcH, float x, float y);
        void DrawPixel(int theColour, float x, float y);
        void DrawRectangle(int theColour, bool filled, float xPos, float yPos, int width, int height);
        void DrawRectangle(int theColour, float xPos, float yPos, int width, int height);
        void FillRectangle(int theColour, float xPos, float yPos, int width, int height);
        void DrawLine(int theColour, float xPosStart, float yPosStart, float xPosEnd, float yPosEnd);
        void DrawHorizontalLine(int theColour, float y, float x1, float x2);
        void DrawVerticalLine(int theColour, float x, float y1, float y2);
        void DrawCircle(int theColour, bool filled, float xc, float yc, int radius);
        void DrawCircle_NoFill(int theColour, float xc, float yc, int radius);
        void FillCircle(int theColour, float xc, float yc, int radius);
        void DrawEllipse(int theColour, bool filled, float xPos, float yPos, int width, int height);
        void DrawEllipse_NoFill(int theColour, float xPos, float yPos, int width, int height);
        void FillEllipse(int theColour, float xPos, float yPos, int width, int height);
        Sprite CreateSprite(Bitmap startBitmap);
        void FreeSprite(ref Sprite spriteToFree);
        int AddBitmapToSprite(Sprite spriteToAddTo, Bitmap bitmapToAdd);
        int CurrentHeight(Sprite sprite);
        int CurrentWidth(Sprite sprite);
        void DrawSprite_WithOffset(Sprite spriteToDraw, int xOffset, int yOffset);
        void DrawSprite(Sprite spriteToDraw);
        void MoveSprite(Sprite spriteToMove, Vector movementVector);
        void MoveSpriteTo(Sprite spriteToMove, int x, int y);
        bool IsSpriteOffscreen(Sprite theSprite);
        bool IsSpriteOffscreen_ViewPort(Sprite theSprite, int vwPrtX, int vwPrtY, int vwPrtWidth, int vwPrtHeight);
        Sprite CreateSprite_MultiEnding(Bitmap startBitmap, Boolean isMulti, [In] ref int[] framesPerCell, SpriteEndingAction endingAction, int width, int height);
        Sprite CreateSprite_Multi(Bitmap startBitmap, Boolean isMulti, [In] ref int[] framesPerCell, int width, int height);
        Sprite CreateSprite_MultiFPC(Bitmap startBitmap, int framesPerCell, int frames, int width, int height);
        Sprite CreateSprite_ArrayEnding([In] ref Bitmap[] startBitmap, [In] ref int[] framesPerCell, SpriteEndingAction endingAction);
        Sprite CreateSprite_Array([In] ref Bitmap[] startBitmap, [In] ref int[] framesPerCell);
        Sprite CreateSprite_ArrayFPC(Bitmap[] startBitmap, int framesPerCell, int frames);
        void UpdateSprite(Sprite sprite);
        void UpdateSpriteAnimation(Sprite sprite);
       


        
        void DrawBitmapPartOnScreen(Bitmap bitmapToDraw, int srcX, int srcY, int srcW, int srcH, int x, int y);
        void DrawBitmapOnScreen(Bitmap bitmapToDraw, int x, int y);
        void DrawPixelOnScreen(int theColour, int x, int y);
        void DrawRectangleOnScreen(int theColour, int x, int y, int width, int height);
        void FillRectangleOnScreen(int theColour, int x, int y, int width, int height);
        void DrawLineOnScreen(int theColour, int xPosStart, int yPosStart, int xPosEnd, int yPosEnd);
        void DrawHorizontalLineOnScreen(int theColour, int y, int x1, int x2);
        void DrawVerticalLineOnScreen(int theColour, int x, int y1, int y2);
        void DrawCircleOnScreen(int theColour, int xc, int yc, int radius);
        void FillCircleOnScreen(int theColour, int xc, int yc, int radius);
        void DrawEllipseOnScreen(int theColour, int x, int y, int width, int height);
        void FillEllipseOnScreen(int theColour, int x, int y, int width, int height);
    }

}
