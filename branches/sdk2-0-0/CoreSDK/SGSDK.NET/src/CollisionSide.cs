//-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
// 					Collision Side
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
    /// CollisionSides
    /// </summary>
    public enum CollisionSide
    {
        /// <summary>
        /// Top
        /// </summary>
        Top,
        /// <summary>
        /// Bottom
        /// </summary>
        Bottom,
        /// <summary>
        /// Left
        /// </summary>
        Left,
        /// <summary>
        /// Right
        /// </summary>
        Right,
        /// <summary>
        /// TopLeft
        /// </summary>
        TopLeft,
        /// <summary>
        /// TopRight
        /// </summary>
        TopRight,
        /// <summary>
        /// BottomLeft
        /// </summary>
        BottomLeft,
        /// <summary>
        /// BottomRight
        /// </summary>
        BottomRight,
        /// <summary>
        /// None
        /// </summary>
        None
    }
}
