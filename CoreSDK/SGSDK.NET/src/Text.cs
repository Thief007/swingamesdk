//-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
// 					Text
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// The Font unit relates to writing text to the screen,
// and to loading and styling the associated fonts.
//
// Change History:
//
// Version 2.0:
// - 2009-01-20: Andrew: Added simple text drawing.
// - 2009-01-20: Andrew: Moved to SGSDK and new types
//                       Moved from Font.cs
//
// Version 1.1:
// - 2008-01-30: Andrew: Fixed String Marshalling and Free
// - 2008-01-29: Andrew: Removed ref from Free
// - 2008-01-25: Andrew: Fixed DrawLinesToScreen
// - 2008-01-23: Andrew: Fixed exceptions
//               Added changes for 1.1 compatibility
//               Refactored some methods, to limit routines exposed by DLL
//               Added extra comments, and fixed code layout and line endings.
//               Using SwinGamePointer for safer management of pointers.
//               Added Point2D and Rect parameter versions of DrawText...
//               Added DrawFramerate with Point2D
// Version 1.0:
// - Various

using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Drawing;
using System.IO;

namespace SwinGame
{
    /// <summary>
    /// Text and Font Class
    /// </summary>
    public class Text
    {
        /// <summary>
        /// Loads a font from file with the specified side. Fonts must be freed using
        ///	the FreeFont routine once finished with. Once the font is loaded you
        ///	can set its style using SetFontStyle. Fonts are then used to draw and
        ///	measure text in your programs.
        /// </summary>
        /// <param name="fontName">The name of the font file to load from the file system</param>
        /// <param name="size">The point size of the font</param>
        /// <returns>The font loaded</returns>
        public static Font LoadFont(string fontName, int size)
        {
            return new Font(fontName, size);
        }

        /// <summary>
        /// Sets the style of the passed in font. This is time consuming, so load
        ///	fonts multiple times and set the style for each if needed.
        /// </summary>
        /// <param name="font">The font to set the style of</param>
        /// <param name="style">The new style for the font, values can be read together</param>
        public static void SetFontStyle(Font font, FontStyle style)
        {
            font.SetStyle(style);
        }

        /// <summary>
        /// Free a loaded font.
        /// </summary>
        /// <param name="fontToFree">The Font to free</param>
        public static void FreeFont(Font fontToFree)
        {
            fontToFree.Dispose();
        }

        /// <summary>
        /// Draws texts to the destination bitmap. Drawing text is a slow operation,
        ///	and drawing it to a bitmap, then drawing the bitmap to screen is a
        ///	good idea. Do not use this technique if the text changes frequently.
        /// </summary>
        /// <param name="dest">The destination bitmap - not optimised!</param>
        /// <param name="theText">The text to be drawn onto the destination</param>
        /// <param name="textColor">The color to draw the text</param>
        /// <param name="theFont">The font used to draw the text</param>
        /// <param name="x">The x location to draw the text at (top left)</param>
        /// <param name="y">The y location to draw the text at (top left)</param>
        public static void DrawText(Bitmap dest, string theText, Color textColor, Font theFont, int x, int y)
        {
            int color = textColor.ToArgb();
            SGSDK.DrawTextOnBitmap(dest.pointer, theText, (uint)color, theFont, x, y);
        }

        /// <summary>
        /// Draws texts to the destination bitmap.
        /// </summary>
        /// <param name="dest">The destination bitmap - not optimised!</param>
        /// <param name="theText">The text to be drawn onto the destination</param>
        /// <param name="textColor">The color to draw the text</param>
        /// <param name="x">The x location to draw the text at (top left)</param>
        /// <param name="y">The y location to draw the text at (top left)</param>
        public static void DrawText(Bitmap dest, string theText, Color textColor, int x, int y)
        {
            int color = textColor.ToArgb();
            SGSDK.DrawSimpleTextOn(dest.pointer, theText, (uint)color, x, y);
        }


        /// <summary>
        /// Draws texts to the destination bitmap. Drawing text is a slow operation,
        ///	and drawing it to a bitmap, then drawing the bitmap to screen is a
        ///	good idea. Do not use this technique if the text changes frequently.
        /// </summary>
        /// <param name="dest">The destination bitmap - not optimised!</param>
        /// <param name="theText">The text to be drawn onto the destination</param>
        /// <param name="textColor">The color to draw the text</param>
        /// <param name="theFont">The font used to draw the text</param>
        /// <param name="pnt">The point to draw the text at (x,y)</param>
        public static void DrawText(Bitmap dest, string theText, Color textColor, Font theFont, Point2D pnt)
        {
            DrawText(dest, theText, textColor, theFont, (int)pnt.X, (int)pnt.Y);
        }

        /// <summary>
        /// Draws texts to the screen. Drawing text is a slow operation,
        ///	and drawing it to a bitmap, then drawing the bitmap to screen is a
        ///	good idea. Do not use this technique if the text changes frequently.
        /// </summary>
        /// <param name="theText">The text to be drawn onto the screen</param>
        /// <param name="textColor">The color to draw the text</param>
        /// <param name="theFont">The font used to draw the text</param>
        /// <param name="x">The x location to draw the text at (top left)</param>
        /// <param name="y">The y location to draw the text at (top left)</param>
        public static void DrawText(string theText, Color textColor, Font theFont, float x, float y)
        {
            int color = textColor.ToArgb();
            SGSDK.DrawText(theText, (uint)color, theFont, x, y);
        }

        /// <summary>
        /// Draws texts to the screen.
        /// </summary>
        /// <param name="theText">The text to be drawn onto the screen</param>
        /// <param name="textColor">The color to draw the text</param>
        /// <param name="x">The x location to draw the text at (top left)</param>
        /// <param name="y">The y location to draw the text at (top left)</param>
        public static void DrawText(string theText, Color textColor, float x, float y)
        {
            int color = textColor.ToArgb();
            SGSDK.DrawSimpleText(theText, (uint)color, x, y);
        }
        
        /// <summary>
        /// Draws texts to the screen. Drawing text is a slow operation,
        ///	and drawing it to a bitmap, then drawing the bitmap to screen is a
        ///	good idea. Do not use this technique if the text changes frequently.
        /// </summary>
        /// <param name="theText">The text to be drawn onto the screen</param>
        /// <param name="textColor">The color to draw the text</param>
        /// <param name="theFont">The font used to draw the text</param>
        /// <param name="pnt">The point to draw the text at (x,y)</param>
        public static void DrawText(string theText, Color textColor, Font theFont, Point2D pnt)
        {
            DrawText(theText, textColor, theFont, pnt.X, pnt.Y);
        }

        /// <summary>
        /// Draws multiple lines of text to the screen. This is a very
        ///	slow operation, so if the text is not frequently changing save it to a
        ///	bitmap and draw that bitmap to screen instead.
        /// </summary>
        /// <param name="theText">The text to be drawn onto the destination</param>
        /// <param name="textColor">The color to draw the text</param>
        /// <param name="backColor">The color to draw behind the text</param>
        /// <param name="theFont">The font used to draw the text</param>
        /// <param name="align">The alignment for the text in the region</param>
        /// <param name="x">The x location to draw the text at (top left)</param>
        /// <param name="y">The y location to draw the text at (top left)</param>
        /// <param name="w">The width of the region to draw inside</param>
        /// <param name="h">The height of the region to draw inside</param>
        public static void DrawTextLines(string theText, Color textColor, Color backColor, Font theFont, FontAlignment align, float x, float y, int w, int h)
        {
            SGSDK.DrawTextLines(theText, (uint)textColor.ToArgb(), (uint)backColor.ToArgb(), theFont, (int)align, x, y, w, h);
        }

        /// <summary>
        /// Draws multiple lines of text to the screen. This is a very
        ///	slow operation, so if the text is not frequently changing save it to a
        ///	bitmap and draw that bitmap to screen instead.
        /// </summary>
        /// <param name="theText">The text to be drawn onto the destination</param>
        /// <param name="textColor">The color to draw the text</param>
        /// <param name="backColor">The color to draw behind the text</param>
        /// <param name="theFont">The font used to draw the text</param>
        /// <param name="align">The alignment for the text in the region</param>
        /// <param name="rect">The rectangle to draw the text into.</param>
        public static void DrawTextLines(string theText, Color textColor, Color backColor, Font theFont, FontAlignment align, Rectangle rect)
        {
            DrawTextLines(theText, textColor, backColor, theFont, align, rect.X, rect.Y, rect.Width, rect.Height);
        }

        /// <summary>
        /// Draws multiple lines of text to the destination bitmap. This is a very
        ///	slow operation, so if the text is not frequently changing save it to a
        ///	bitmap and draw that bitmap to screen instead.
        /// </summary>
        /// <param name="dest">The destination bitmap - not optimised!</param>
        /// <param name="theText">The text to be drawn onto the destination</param>
        /// <param name="textColor">The color to draw the text</param>
        /// <param name="backColor">The color to draw behind the text</param>
        /// <param name="theFont">The font used to draw the text</param>
        /// <param name="align">The alignment for the text in the region</param>
        /// <param name="x">The x location to draw the text at (top left)</param>
        /// <param name="y">The y location to draw the text at (top left)</param>
        /// <param name="w">The width of the region to draw inside</param>
        /// <param name="h">The height of the region to draw inside</param>
        public static void DrawTextLines(Bitmap dest, string theText, Color textColor, Color backColor, Font theFont, FontAlignment align, int x, int y, int w, int h)
        {
            SGSDK.DrawTextLinesOnBitmap(dest, theText, (uint)textColor.ToArgb(), (uint)backColor.ToArgb(), theFont, (int)align, x, y, w, h);
        }

        /// <summary>
        /// Draws multiple lines of text to the destination bitmap. This is a very
        ///	slow operation, so if the text is not frequently changing save it to a
        ///	bitmap and draw that bitmap to screen instead.
        /// </summary>
        /// <param name="dest">The destination bitmap - not optimised!</param>
        /// <param name="theText">The text to be drawn onto the destination</param>
        /// <param name="textColor">The color to draw the text</param>
        /// <param name="backColor">The color to draw behind the text</param>
        /// <param name="theFont">The font used to draw the text</param>
        /// <param name="align">The alignment for the text in the region</param>
        /// <param name="rect">The rectangle to draw the text into.</param>
        public static void DrawTextLines(Bitmap dest, string theText, Color textColor, Color backColor, Font theFont, FontAlignment align, Rectangle rect)
        {
            DrawTextLines(dest, theText, textColor, backColor, theFont, align, rect.X, rect.Y, rect.Width, rect.Height);
        }

        /// <summary>
        /// Calculates the width of a string when drawn with a given font.
        /// </summary>
        /// <param name="theText">The text to measure</param>
        /// <param name="theFont">The font used to draw the text</param>
        /// <returns>The width of the drawing in pixels</returns>
        public static int TextWidth(string theText, Font theFont)
        {
            return theFont.TextWidth(theText);
        }

        /// <summary>
        /// Calculates the height of a string when drawn with a given font.
        /// </summary>
        /// <param name="theText">The text to measure</param>
        /// <param name="theFont">The font used to draw the text</param>
        /// <returns>The height of the drawing in pixels</returns>
        public static int TextHeight(string theText, Font theFont)
        {
            return theFont.TextHeight(theText);
        }

        /// <summary>
        /// Draws the frame rate using the specified font at the indicated x, y.
        ///	Draws the FPS (min, max) current average
        /// </summary>
        /// <param name="x">The x location to draw to</param>
        /// <param name="y">The y location to draw to</param>
        /// <param name="theFont">The font used to draw the framerate</param>
        public static void DrawFramerate(int x, int y, Font theFont)
        {
            SGSDK.DrawFramerate(x, y, theFont);
        }

        /// <summary>
        /// Draws the frame rate using the specified font at the indicated x, y.
        ///	Draws the FPS (min, max) current average
        /// </summary>
        /// <param name="x">The x location to draw to</param>
        /// <param name="y">The y location to draw to</param>
        public static void DrawFramerate(int x, int y)
        {
            SGSDK.DrawSimpleFramerate(x, y);
        }

        /// <summary>
        /// Draws the frame rate using the specified font at the indicated x, y.
        ///	Draws the FPS (min, max) current average
        /// </summary>
        /// <param name="pnt">The point to draw at (x,y)</param>
        /// <param name="theFont">The font used to draw the framerate</param>
        public static void DrawFramerate(Point2D pnt, Font theFont)
        {
            DrawFramerate((int)pnt.X, (int)pnt.Y, theFont);
        }

        // Draw Text on Screen Stuff

        /// <summary>
        /// Draws texts to the screen. Drawing text is a slow operation,
        ///	and drawing it to a bitmap, then drawing the bitmap to screen is a
        ///	good idea. Do not use this technique if the text changes frequently.
        /// </summary>
        /// <param name="theText">The text to be drawn onto the screen</param>
        /// <param name="textColor">The color to draw the text</param>
        /// <param name="theFont">The font used to draw the text</param>
        /// <param name="x">The x location to draw the text at (top left)</param>
        /// <param name="y">The y location to draw the text at (top left)</param>
        public static void DrawTextOnScreen(string theText, Color textColor, Font theFont, int x, int y)
        {
            int color = textColor.ToArgb();
            SGSDK.DrawTextOnScreen(theText, (uint)color, theFont, x, y);
        }

        /// <summary>
        /// Draws texts to the screen.
        /// </summary>
        /// <param name="theText">The text to be drawn onto the screen</param>
        /// <param name="textColor">The color to draw the text</param>
        /// <param name="x">The x location to draw the text at (top left)</param>
        /// <param name="y">The y location to draw the text at (top left)</param>
        public static void DrawTextOnScreen(string theText, Color textColor, int x, int y)
        {
            int color = textColor.ToArgb();
            SGSDK.DrawSimpleTextOnScreen(theText, (uint)color, x, y);
        }

        /// <summary>
        /// Draws texts to the screen. Drawing text is a slow operation,
        ///	and drawing it to a bitmap, then drawing the bitmap to screen is a
        ///	good idea. Do not use this technique if the text changes frequently.
        /// </summary>
        /// <param name="theText">The text to be drawn onto the screen</param>
        /// <param name="textColor">The color to draw the text</param>
        /// <param name="theFont">The font used to draw the text</param>
        /// <param name="point">The Top left Position of the Text to be drawn</param>
        public static void DrawTextOnScreen(string theText, Color textColor, Font theFont, Point2D point)
        {
            DrawTextOnScreen(theText, textColor, theFont, (int)point.X, (int)point.Y);
        }

        /// <summary>
        /// Draws multiple lines of text to the screen. This is a very
        ///	slow operation, so if the text is not frequently changing save it to a
        ///	bitmap and draw that bitmap to screen instead.
        /// </summary>
        /// <param name="theText">The text to be drawn onto the destination</param>
        /// <param name="textColor">The color to draw the text</param>
        /// <param name="backColor">The color to draw behind the text</param>
        /// <param name="theFont">The font used to draw the text</param>
        /// <param name="align">The alignment for the text in the region</param>
        /// <param name="x">The x location to draw the text at (top left)</param>
        /// <param name="y">The y location to draw the text at (top left)</param>
        /// <param name="w">The width of the region to draw inside</param>
        /// <param name="h">The height of the region to draw inside</param>
        public static void DrawTextLinesOnScreen(string theText, Color textColor, Color backColor, Font theFont, FontAlignment align, int x, int y, int w, int h)
        {
            uint color = (uint)textColor.ToArgb();
            uint color2 = (uint)backColor.ToArgb();
            SGSDK.DrawTextLinesOnScreen(theText, color, color2, theFont, (int)align, x, y, w, h);
        }

        /// <summary>
        /// Draws multiple lines of text to the screen. This is a very
        ///	slow operation, so if the text is not frequently changing save it to a
        ///	bitmap and draw that bitmap to screen instead.
        /// </summary>
        /// <param name="theText">The text to be drawn onto the destination</param>
        /// <param name="textColor">The color to draw the text</param>
        /// <param name="backColor">The color to draw behind the text</param>
        /// <param name="theFont">The font used to draw the text</param>
        /// <param name="align">The alignment for the text in the region</param>
        /// <param name="rectangle">The Rectangle the Text will be within</param>
        public static void DrawTextLinesOnScreen(string theText, Color textColor, Color backColor, Font theFont, FontAlignment align, Rectangle rectangle)
        {
            DrawTextLinesOnScreen(theText, textColor, backColor, theFont, align, rectangle.X, rectangle.Y, rectangle.Width, rectangle.Height);
        }
    }
}
