using System;
using System.Collections.Generic;
using System.Text;

namespace SwinGame
{
    /// <summary>
    /// Enumeration: CollisionDetectionRanges
    ///	This is used to indicate the kind of collision being checked with the
    ///	Sprite collision routines.  
    /// </summary>
    public enum CollisionDetectionRange
    {
        /// <summary>
        /// Collision Range is Equal
        /// </summary>
        CollisionRangeEquals = 0,
        /// <summary>
        /// Collision Range is Greater
        /// </summary>
        CollisionRangeGreaterThan = 1,
        /// <summary>
        /// Collision Range is Less
        /// </summary>
        CollisionRangeLessThan = 2
    }
}
