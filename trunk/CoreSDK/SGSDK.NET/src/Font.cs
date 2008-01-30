//-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
// 					Font
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// The Font unit relates to writing text to the screen,
// and to loading and styling the associated fonts.
//
// Change History:
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
    /// Fonts are used to render text to bitmaps and to the screen.
    /// Fonts must be loaded using the CreateFont routine. Also see the
    ///	DrawText and DrawTextLines routines.
    /// </summary>
    public struct Font
    {
        //internal IntPtr Pointer;
        internal SwinGamePointer Pointer;
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
        /// <summary>
        /// Normal Font
        /// </summary>
        NormalFont = 0,
        /// <summary>
        /// Bold Font
        /// </summary>
        BoldFont = 1,
        /// <summary>
        /// Italic Font
        /// </summary>
        ItalicFont = 2,
        /// <summary>
        /// Underline Font
        /// </summary>
        UnderlineFont = 4,
    }

    /// <summary>
    /// Use font alignment for certain drawing operations. With these
    ///	operations you specify the area to draw in as well as the alignment
    ///	within that area. See DrawTextLines.
    /// </summary>
    public enum FontAlignment
    {
        /// <summary>
        /// Align to the Left
        /// </summary>
        AlignLeft = 1,
        /// <summary>
        /// Align to Center
        /// </summary>
        AlignCenter = 2,
        /// <summary>
        /// Align to the Right
        /// </summary>
        AlignRight = 4,
    }

    /// <summary>
    /// Text and Font Class
    /// </summary>
    public class Text
    {
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "LoadFont", CharSet=CharSet.Ansi)]
        private static extern IntPtr DLL_LoadFont([MarshalAs(UnmanagedType.LPStr)]string fontName, int size);
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
            Font font;
				try
				{
            	font.Pointer = new SwinGamePointer(DLL_LoadFont(fontName, size), PtrKind.Font);
				}catch(Exception exc) 
				{ 
					throw new SwinGameException(exc.Message); 
				}
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return font;
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SetFontStyle")]
        private static extern void DLL_SetFontStyle(IntPtr font, FontStyle style);
        /// <summary>
        /// Sets the style of the passed in font. This is time consuming, so load
        ///	fonts multiple times and set the style for each if needed.
        /// </summary>
        /// <param name="font">The font to set the style of</param>
        /// <param name="style">The new style for the font, values can be read together</param>
        public static void SetFontStyle(Font font, FontStyle style)
        {
            try
            {
                DLL_SetFontStyle(font.Pointer, style);
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
        /// Free a loaded font.
        /// </summary>
        /// <param name="fontToFree">The Font to free</param>
        public static void FreeFont(Font fontToFree)
        {
            fontToFree.Pointer.Free();
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawTextOnBitmap", CharSet=CharSet.Ansi)]
        private static extern void DLL_DrawTextOnBitmap(IntPtr dest, [MarshalAs(UnmanagedType.LPStr)]string theText, uint textColor, IntPtr theFont, int x, int y);
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
            try
            {
                int color = textColor.ToArgb();
                DLL_DrawTextOnBitmap(dest.pointer, theText, (uint)color, theFont.Pointer, x, y);
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

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawText", CharSet=CharSet.Ansi)]
        private static extern void DLL_DrawText([MarshalAs(UnmanagedType.LPStr)]string theText, uint textColor, IntPtr theFont, float x, float y);
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
            try
            {
                int color = textColor.ToArgb();
                DLL_DrawText(theText, (uint)color, theFont.Pointer, x, y);
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

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawTextLines", CharSet=CharSet.Ansi)]
        private static extern void DLL_DrawTextLines([MarshalAs(UnmanagedType.LPStr)]string theText, uint textColor, uint backColor, IntPtr theFont, FontAlignment align, float x, float y, int w, int h);
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
            try
            {
                DLL_DrawTextLines(theText, (uint)textColor.ToArgb(), (uint)backColor.ToArgb(), theFont.Pointer, align, x, y, w, h);
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


        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawTextLinesOnBitmap", CharSet=CharSet.Ansi)]
        private static extern void DLL_DrawTextLines(IntPtr dest, [MarshalAs(UnmanagedType.LPStr)]string theText, uint textColor, uint backColor, IntPtr theFont, FontAlignment align, int x, int y, int w, int h);
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
            try
            {
                DLL_DrawTextLines(dest.pointer, theText, (uint)textColor.ToArgb(), (uint)backColor.ToArgb(), theFont.Pointer, align, x, y, w, h);
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

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "TextWidth", CharSet=CharSet.Ansi)]
        private static extern int DLL_TextWidth([MarshalAs(UnmanagedType.LPStr)]string theText, IntPtr theFont);
        /// <summary>
        /// Calculates the width of a string when drawn with a given font.
        /// </summary>
        /// <param name="theText">The text to measure</param>
        /// <param name="theFont">The font used to draw the text</param>
        /// <returns>The width of the drawing in pixels</returns>
        public static int TextWidth(string theText, Font theFont)
        {
            int temp;

            try
            {
                temp = DLL_TextWidth(theText, theFont.Pointer);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "TextHeight", CharSet=CharSet.Ansi)]
        private static extern int DLL_TextHeight([MarshalAs(UnmanagedType.LPStr)]string theText, IntPtr theFont);
        /// <summary>
        /// Calculates the height of a string when drawn with a given font.
        /// </summary>
        /// <param name="theText">The text to measure</param>
        /// <param name="theFont">The font used to draw the text</param>
        /// <returns>The height of the drawing in pixels</returns>
        public static int TextHeight(string theText, Font theFont)
        {
            int temp;
            try
            {
                temp = DLL_TextHeight(theText, theFont.Pointer);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawFramerate")]
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
            try
            {
                DLL_DrawFramerate(x, y, theFont.Pointer);
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

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawTextOnScreen", CharSet=CharSet.Ansi)]
        private static extern void DLL_DrawTextOnScreen([MarshalAs(UnmanagedType.LPStr)]string theText, uint textColor, IntPtr theFont, int x, int y);
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
            try
            {
                int color = textColor.ToArgb();
                DLL_DrawTextOnScreen(theText, (uint)color, theFont.Pointer, x, y);
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

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawTextLinesOnScreen", CharSet=CharSet.Ansi)]
        private static extern void DLL_DrawTextLinesOnScreen([MarshalAs(UnmanagedType.LPStr)]string theText, uint textColor, uint backColor, IntPtr theFont, FontAlignment align, int x, int y, int w, int h);   
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
            try
            {
                uint color = (uint)textColor.ToArgb();
                uint color2 = (uint)backColor.ToArgb();
                DLL_DrawTextLinesOnScreen(theText, color, color2, theFont.Pointer, align, x, y, w, h);
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
