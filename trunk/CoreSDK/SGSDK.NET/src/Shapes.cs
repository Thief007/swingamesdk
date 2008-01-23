using System;
using System.Runtime.InteropServices;
using System.Drawing;

namespace SwinGame
{
    [StructLayout(LayoutKind.Sequential)]
    public struct Point2D
    {
        public float X;
        public float Y;
    }

    [StructLayout(LayoutKind.Sequential)]
    internal struct SGSDKRectangle
    {
        public float X;
        public float Y;
        public int Width;
        public int Height;
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct LineSegment
    {
        public Point2D StartPoint;
        public Point2D EndPoint;
    }

    public class Shapes
    {
        internal SGSDKRectangle ToSGSDKRect(Rectangle rect)
        {
            SGSDKRectangle result;
            result.X = rect.X;
            result.Y = rect.Y;
            result.Width = rect.Width;
            result.Height = rect.Height;

            return result;
        }

        internal Rectangle ToDrawingRect(SGSDKRectangle rect)
        {
            return new Rectangle((int)rect.X, (int)rect.Y, rect.Width, rect.Height);
        }
    }
}
