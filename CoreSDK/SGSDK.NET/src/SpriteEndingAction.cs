using System;
using System.Collections.Generic;
using System.Text;

namespace SwinGame
{
    ///<summary>
    /// Record: SpriteEndingAction
    ///
    /// It is used to determine what this sprite should do when it finishes
    /// animating.
    /// </summary>
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
}
