//-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
// 					Font
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// Change History:
//
// Version 2.0:
// - 2009-01-20: Andrew: Added version histroy 
//                       to newly created classes
//
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\

using System;
using System.Collections.Generic;
using System.Text;

namespace SwinGame
{
    /// <summary>
    /// Fonts are used to render text to bitmaps and to the screen.
    /// Fonts must be loaded using the CreateFont routine. Also see the
    ///	DrawText and DrawTextLines routines.
    /// </summary>
    public class Font
    {
        internal readonly SwinGamePointer pointer;

        internal Font(IntPtr devPtr, bool isCopy)
        {
            if(isCopy)
                pointer = new SwinGamePointer(devPtr, PtrKind.Copy);
            else
                pointer = new SwinGamePointer(devPtr, PtrKind.Timer);
        }

        /// <summary>
        /// Cast the bitmap to its native pointer.
        /// </summary>
        /// <param name="fnt">the Font</param>
        /// <returns>its native pointer</returns>
        public static implicit operator IntPtr(Font fnt)
        {
            return fnt.pointer.Pointer;
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
        /// Loads a font from file with the specified side. Fonts must be freed using
        ///	the FreeFont routine once finished with. Once the font is loaded you
        ///	can set its style using SetFontStyle. Fonts are then used to draw and
        ///	measure text in your programs.
        /// </summary>
        /// <param name="fontName">The name of the font file to load from the file system</param>
        /// <param name="size">The point size of the font</param>
        public Font(string fontName, int size)
        {
            pointer = new SwinGamePointer(SGSDK.LoadFont(fontName, size), PtrKind.Font);
        }

        /// <summary>
        /// Sets the style of the passed in font. This is time consuming, so load
        ///	fonts multiple times and set the style for each if needed.
        /// </summary>
        /// <param name="style">The new style for the font, values can be read together</param>
        public void SetStyle(FontStyle style)
        {
            SGSDK.SetFontStyle(this, (int)style);
        }

        /// <summary>
        /// Calculates the width of a string when drawn with a given font.
        /// </summary>
        /// <param name="theText">The text to measure</param>
        /// <returns>The width of the drawing in pixels</returns>
        public int TextWidth(string theText)
        {
            return SGSDK.TextWidth(theText, this);
        }

        /// <summary>
        /// Calculates the height of a string when drawn with a given font.
        /// </summary>
        /// <param name="theText">The text to measure</param>
        /// <returns>The height of the drawing in pixels</returns>
        public int TextHeight(string theText)
        {
            return SGSDK.TextHeight(theText, this);
        }

    }
}
