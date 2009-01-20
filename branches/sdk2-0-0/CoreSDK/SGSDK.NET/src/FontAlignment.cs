//-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
// 					Font Alignment
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
        AlignRight = 4
    }
}
