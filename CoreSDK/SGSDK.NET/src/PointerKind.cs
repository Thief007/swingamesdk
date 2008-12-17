using System;
using System.Collections.Generic;
using System.Text;

namespace SwinGame
{
    //internal delegate void FreeDelegate(IntPtr toFree);
    internal enum PtrKind
    {
        Image,
        Font,
        Sound,
        Music,
        Map,
        Sprite,
        Matrix,
        Timer,
        Copy
    }
}
