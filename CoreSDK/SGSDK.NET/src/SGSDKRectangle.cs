using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace SwinGame
{
    [StructLayout(LayoutKind.Sequential)]
    internal struct SGSDKRectangle
    {
        public float X;
        public float Y;
        public int Width;
        public int Height;
    }
}
