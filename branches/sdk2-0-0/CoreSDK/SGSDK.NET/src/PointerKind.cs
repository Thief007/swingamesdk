//-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
// 					Pointer Kind
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
