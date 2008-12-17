using System;
using System.Collections.Generic;
using System.Text;

namespace SwinGame
{
    /// It is used to determine how a sprite should act.
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
}
