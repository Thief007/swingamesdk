using System;
using System.Collections.Generic;
using System.Text;
using SwinGame;
using System.Runtime.InteropServices;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
namespace SwinGameVB
{
    /// <summary>
    /// Fonts are used to render text to bitmaps and to the screen.
    /// Fonts must be loaded using the CreateFont routine. Also see the
    ///	DrawText and DrawTextLines routines.
    /// </summary>
    [ClassInterface(ClassInterfaceType.None)]
    [Guid("BAA6049F-2DF5-4ced-8C0F-DBD3A8B6A754")]
    [ComVisible(true)]
    public class Fonts : IFont
    {
        private SwinGame.Font font;
        internal void Free()
        {
            SwinGame.Text.FreeFont( font);
        }
        internal SwinGame.Font result
        {
            get
            {
                return font;
            }
            set
            {
                font = value;
            }
        }
    }

    [Guid("B506F387-349C-4079-8A58-B7E9AABD3CCC")]
    [ComVisible(true)]
    public interface IFont
    {
    }

    /// <summary>
    /// Use font styles to set the style of a font. Setting the style is time
    ///	consuming, so create alternative font variables for each different
    ///	style you want to work with. Note that these values can be logical
    ///	ORed together to combine styles, e.g. BoldFont or ItalicFont = both
    ///	bold and italic.
    /// </summary>
    [ComVisible(true)]
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
    [ComVisible(true)]
    public enum FontAlignment
    {
        AlignLeft = 1,
        AlignCenter = 2,
        AlignRight = 4,
    }

    [ClassInterface(ClassInterfaceType.None)]
    [Guid("41D03C97-481B-4904-8567-C34D3154B503")]
    [ComVisible(true)]
    public class Text :IText
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
        public Fonts LoadFont(String fontName, int size)
        {
            //try
            //{
                //System.Windows.Forms.MessageBox.Show("Loading " + fontName);
                Fonts font = new Fonts();
                font.result = SwinGame.Text.LoadFont(fontName, size);
                //System.Windows.Forms.MessageBox.Show("Loaded " + fontName + " = " + font.result.ToString());
                return font;
           //}
            //catch(Exception e)
            //{
                //System.Windows.Forms.MessageBox.Show(e.Message);
                //throw;
            //}
        }

        /// <summary>
        /// Sets the style of the passed in font. This is time consuming, so load
        ///	fonts multiple times and set the style for each if needed.
        /// </summary>
        /// <param name="font">The font to set the style of</param>
        /// <param name="style">The new style for the font, values can be read together</param>
        public void SetFontStyle(Fonts font, FontStyle style)
        {
            SwinGame.Text.SetFontStyle(font.result, (SwinGame.FontStyle) style);
        }

        /// <summary>
        /// Free a loaded font.
        /// </summary>
        /// <param name="fontToFree">The Font to free</param>
        public void FreeFont(Fonts fontToFree)
        {
            fontToFree.Free();
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
        public void DrawText_ToBitmap(Bitmap dest, String theText, int textColor, Fonts theFont, int x, int y)
        {
            Color color1 = Color.FromArgb(textColor);
            SwinGame.Text.DrawText(dest.result, theText, color1, theFont.result, x, y);
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
        public void DrawText(String theText, int textColor, Fonts theFont, float x, float y)
        {
            Color temp = Color.FromArgb(textColor);
            SwinGame.Text.DrawText(theText, temp, theFont.result, x, y);
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
        public void DrawTextLines(String theText, int textColor, int backColor, Fonts theFont, FontAlignment align, float x, float y, int w, int h)
        {
            Color color1 = Color.FromArgb(textColor);
            Color color2 = Color.FromArgb(backColor);
            SwinGame.Text.DrawTextLines(theText, color1, color2, theFont.result, (SwinGame.FontAlignment)align, x, y, w, h);
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
        public void DrawTextLines_ToBitmap(Bitmap dest, String theText, int textColor, int backColor, Fonts theFont, FontAlignment align, int x, int y, int w, int h)
        {
            Color color1 = Color.FromArgb(textColor);
            Color color2 = Color.FromArgb(backColor);
            SwinGame.Text.DrawTextLines(dest.result, theText, color1, color2, theFont.result, (SwinGame.FontAlignment)align, x, y, w, h);
        }

        /// <summary>
        /// Calculates the width of a string when drawn with a given font.
        /// </summary>
        /// <param name="theText">The text to measure</param>
        /// <param name="theFont">The font used to draw the text</param>
        /// <returns>The width of the drawing in pixels</returns>
        public int TextWidth(String theText, Fonts theFont)
        {
            return SwinGame.Text.TextWidth(theText, theFont.result);
        }

        /// <summary>
        /// Calculates the height of a string when drawn with a given font.
        /// </summary>
        /// <param name="theText">The text to measure</param>
        /// <param name="theFont">The font used to draw the text</param>
        /// <returns>The height of the drawing in pixels</returns>
        public int TextHeight(String theText, Fonts theFont)
        {
            return SwinGame.Text.TextHeight(theText, theFont.result);
        }

        /// <summary>
        /// Draws the frame rate using the specified font at the indicated x, y.
        ///	Draws the FPS (min, max) current average
        /// </summary>
        /// <param name="x">The x location to draw to</param>
        /// <param name="y">The y location to draw to</param>
        /// <param name="theFont">The font used to draw the framerate</param>
        public void DrawFramerate(int x, int y, Fonts theFont)
        {
            SwinGame.Text.DrawFramerate(x, y, theFont.result);
        }

        public void DrawTextOnScreen(String theText, int textColor, Fonts theFont, int x, int y)
        {
            Color color = Color.FromArgb(textColor);
            SwinGame.Text.DrawTextOnScreen(theText, color, theFont.result, x, y);
        }
        
        public void DrawTextLinesOnScreen(String theText, int textColor, int backColor, Fonts theFont, FontAlignment align, int x, int y, int w, int h)
        {
            Color color = Color.FromArgb(textColor);
            Color backcolor = Color.FromArgb(backColor);
            SwinGame.Text.DrawTextLinesOnScreen(theText, color, backcolor, theFont.result, (SwinGame.FontAlignment)align, x, y, w, h);
        }

        public void DrawText_ToBitmap_Point(Bitmap dest, String theText, int textColor, Fonts theFont, Point2D position)
        {
            DrawText_ToBitmap(dest, theText, textColor, theFont, (int)position.GetX(), (int)position.GetY());
        }
        public void DrawText_Point(String theText, int textColor, Fonts theFont, Point2D position)
        {
            DrawText(theText, textColor, theFont, position.GetX(), position.GetY());
        }
        public void DrawTextLines_Rectangle(String theText, int textColor, int backColor, Fonts theFont, FontAlignment align, Rectangle withinRec)
        {
            DrawTextLines(theText, textColor, backColor, theFont, align, (int)withinRec.GetX(), (int)withinRec.GetY(), (int)withinRec.GetWidth(), (int)withinRec.GetHeight());
        }
        public void DrawTextLines_ToBitmap_Rectangle(Bitmap dest, String theText, int textColor, int backColor, Fonts theFont, FontAlignment align, Rectangle withinRec)
        {
            DrawTextLines_ToBitmap(dest, theText, textColor, backColor, theFont, align, (int)withinRec.GetX(), (int)withinRec.GetY(), (int)withinRec.GetWidth(), (int)withinRec.GetHeight());
        }
        public void DrawFramerate_Point(Point2D position, Fonts theFont)
        {
            DrawFramerate((int)position.GetX(), (int)position.GetY(), theFont);
        }
        public void DrawTextOnScreen_Point(String theText, int textColor, Fonts theFont, Point2D position)
        {
            DrawTextOnScreen(theText, textColor, theFont, (int)position.GetX(), (int)position.GetY());
        }
        public void DrawTextLinesOnScreen_Rectangle(String theText, int textColor, int backColor, Fonts theFont, FontAlignment align, Rectangle withinRect)
        {
            DrawTextLinesOnScreen(theText, textColor, backColor, theFont, align, (int)withinRect.GetX(), (int)withinRect.GetY(), (int)withinRect.GetWidth(), (int)withinRect.GetHeight());
        }

    }

    [Guid("CF5F7CD4-6576-4862-ABB2-0A3B9B345B57")]
    [ComVisible(true)]
    public interface IText
    {
        Fonts LoadFont(String fontName, int size);
        void SetFontStyle(Fonts font, FontStyle style);
        void FreeFont(Fonts fontToFree);
        void DrawText_ToBitmap(Bitmap dest, String theText, int textColor, Fonts theFont, int x, int y);
        void DrawText_ToBitmap_Point(Bitmap dest, String theText, int textColor, Fonts theFont, Point2D position);
        void DrawText(String theText, int textColor, Fonts theFont, float x, float y);
        void DrawText_Point(String theText, int textColor, Fonts theFont, Point2D position);
        void DrawTextLines(String theText, int textColor, int backColor, Fonts theFont, FontAlignment align, float x, float y, int w, int h);
        void DrawTextLines_Rectangle(String theText, int textColor, int backColor, Fonts theFont, FontAlignment align, Rectangle withinRec);
        void DrawTextLines_ToBitmap(Bitmap dest, String theText, int textColor, int backColor, Fonts theFont, FontAlignment align, int x, int y, int w, int h);
        void DrawTextLines_ToBitmap_Rectangle(Bitmap dest, String theText, int textColor, int backColor, Fonts theFont, FontAlignment align, Rectangle withinRec);
        int TextWidth(String theText, Fonts theFont);
        int TextHeight(String theText, Fonts theFont);
        void DrawFramerate(int x, int y, Fonts theFont);
        void DrawFramerate_Point(Point2D position, Fonts theFont);
        void DrawTextOnScreen(String theText, int textColor, Fonts theFont, int x, int y);
        void DrawTextOnScreen_Point(String theText, int textColor, Fonts theFont, Point2D position);
        void DrawTextLinesOnScreen(String theText, int textColor, int backColor, Fonts theFont, FontAlignment align, int x, int y, int w, int h);
        void DrawTextLinesOnScreen_Rectangle(String theText, int textColor, int backColor, Fonts theFont, FontAlignment align, Rectangle withinRect);

    }


}