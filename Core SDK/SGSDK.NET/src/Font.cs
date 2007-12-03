using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Drawing;
using System.IO;

namespace SwinGame
{
    /// <summary>
    /// Fonts are used to render text to bitmaps and to the screen.
    /// Fonts must be loaded using the CreateFont routine. Also see the
    ///	DrawText and DrawTextLines routines.
    /// </summary>
    public struct Font
    {
        internal IntPtr Pointer;
    }
    /// <summary>
    /// Use font styles to set the style of a font. Setting the style is time
    ///	consuming, so create alternative font variables for each different
    ///	style you want to work with. Note that these values can be logical
    ///	ORed together to combine styles, e.g. BoldFont or ItalicFont = both
    ///	bold and italic.
    /// </summary>
    public enum FontStyle
    {
        NormalFont = 0,
        BoldFont = 1,
        ItalicFont = 2,
        UnderlineFont = 4,
    }

    /// <summary>
    /// Use font alignment for certain drawing operations. With these
    ///	operations you specify the area to draw in as well as the alignment
    ///	within that area. See DrawTextLines.
    /// </summary>
    public enum FontAlignment
    {
        AlignLeft = 1,
        AlignCenter = 2,
        AlignRight = 4,
    }
    public class Text
    {
        [DllImport("SGSDK.dll", EntryPoint = "LoadFont")]
        private static extern IntPtr DLL_LoadFont(String fontName, int size);
        /// <summary>
        /// Loads a font from file with the specified side. Fonts must be freed using
        ///	the FreeFont routine once finished with. Once the font is loaded you
        ///	can set its style using SetFontStyle. Fonts are then used to draw and
        ///	measure text in your programs.
        /// </summary>
        /// <param name="fontName">The name of the font file to load from the file system</param>
        /// <param name="size">The point size of the font</param>
        /// <returns>The font loaded</returns>
        public static Font LoadFont(String fontName, int size)
        {
            Font font;
            font.Pointer = DLL_LoadFont(fontName, size);
            return font;
        }

        [DllImport("SGSDK.dll", EntryPoint = "SetFontStyle")]
        private static extern void DLL_SetFontStyle(IntPtr font, FontStyle style);
        /// <summary>
        /// Sets the style of the passed in font. This is time consuming, so load
        ///	fonts multiple times and set the style for each if needed.
        /// </summary>
        /// <param name="font">The font to set the style of</param>
        /// <param name="style">The new style for the font, values can be read together</param>
        public static void SetFontStyle(Font font, FontStyle style)
        {
            DLL_SetFontStyle(font.Pointer, style);
        }

        [DllImport("SGSDK.dll", EntryPoint = "FreeFont")]
        private static extern void DLL_FreeFont(ref IntPtr fontToFree);
        /// <summary>
        /// Free a loaded font.
        /// </summary>
        /// <param name="fontToFree">The Font to free</param>
        public static void FreeFont(ref Font fontToFree)
        {
            DLL_FreeFont(ref fontToFree.Pointer);
        }

        [DllImport("SGSDK.dll", EntryPoint = "DrawTextOnBitmap")]
        private static extern void DLL_DrawText(IntPtr dest , String theText, uint textColor, IntPtr theFont,int  x, int y);
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
        public static void DrawText(Bitmap dest, String theText, Color textColor, Font theFont, int x, int y)
        {
            int color = textColor.ToArgb();
            DLL_DrawText(dest.pointer, theText, (uint)color, theFont.Pointer, x, y);
        }

        [DllImport("SGSDK.dll", EntryPoint = "DrawText")]
        private static extern void DLL_DrawText(String theText, uint textColor, IntPtr theFont, int x, int y);
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
        public static void DrawText( String theText, Color textColor, Font theFont, int x, int y)
        {
            int color = textColor.ToArgb();
            DLL_DrawText(theText, (uint)color, theFont.Pointer, x, y);
        }

        [DllImport("SGSDK.dll", EntryPoint = "DrawTextLinesOnBitmap")]
        private static extern void DLL_DrawTextLines(String  theText, uint textColor,  uint backColor, IntPtr theFont,FontAlignment align,int x,int y,int w,int h);
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
        public static void DrawTextLines(String theText, Color textColor, Color backColor, Font theFont, FontAlignment align, int x, int y, int w, int h)
        {
            int color1 = textColor.ToArgb();
            int color2 = backColor.ToArgb();
            DLL_DrawTextLines(theText, (uint)textColor.ToArgb(), (uint)backColor.ToArgb(), theFont.Pointer, align, x, y, w, h);
        }

        [DllImport("SGSDK.dll", EntryPoint = "DrawText")]
        private static extern void DLL_DrawTextLines(IntPtr dest, String  theText, uint textColor,  uint backColor, IntPtr theFont,FontAlignment align,int x,int y,int w,int h);
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
        public static void DrawTextLines(Bitmap dest, String theText, Color textColor, Color backColor, Font theFont, FontAlignment align, int x, int y, int w, int h)
        {
            int color1 = textColor.ToArgb();
            int color2 = backColor.ToArgb();
            DLL_DrawTextLines(dest.pointer, theText, (uint)textColor.ToArgb(), (uint)backColor.ToArgb(), theFont.Pointer, align, x, y, w, h);
        }

        [DllImport("SGSDK.dll", EntryPoint = "TextWidth")]
        private static extern int DLL_TextWidth(String theText, IntPtr theFont);
        /// <summary>
        /// Calculates the width of a string when drawn with a given font.
        /// </summary>
        /// <param name="theText">The text to measure</param>
        /// <param name="theFont">The font used to draw the text</param>
        /// <returns>The width of the drawing in pixels</returns>
        public static int TextWidth(String theText, Font theFont)
        {
            return DLL_TextWidth(theText, theFont.Pointer);
        }

        [DllImport("SGSDK.dll", EntryPoint = "TextHeight")]
        private static extern int DLL_TextHeight(String theText, IntPtr theFont);
        /// <summary>
        /// Calculates the height of a string when drawn with a given font.
        /// </summary>
        /// <param name="theText">The text to measure</param>
        /// <param name="theFont">The font used to draw the text</param>
        /// <returns>The height of the drawing in pixels</returns>
        public static int TextHeight(String theText, Font theFont)
        {
            return DLL_TextHeight(theText, theFont.Pointer);
        }

        [DllImport("SGSDK.dll", EntryPoint = "DrawFramerate")]
        private static extern void DLL_DrawFramerate(int x, int y, IntPtr theFont);
        /// <summary>
        /// Draws the frame rate using the specified font at the indicated x, y.
        ///	Draws the FPS (min, max) current average
        /// </summary>
        /// <param name="x">The x location to draw to</param>
        /// <param name="y">The y location to draw to</param>
        /// <param name="theFont">The font used to draw the framerate</param>
        public static void DrawFramerate(int x, int y, Font theFont)
        {
            DLL_DrawFramerate(x, y, theFont.Pointer);
        }
    }
}
