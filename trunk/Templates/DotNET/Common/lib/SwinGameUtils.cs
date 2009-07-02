using System;
using System.Collections.Generic;
using System.Text;

namespace SwinGame
{
    internal static class Utils
    {
        internal static Matrix2D MatrixFromArray(float[,] data)
        {
            Matrix2D result;
            result.data = data;
            return result;
        }
        
        internal static Triangle TriangleFromArray(Point2D[] data)
        {
            Triangle result;
            result.data = data;
            return result;
        }

    }
}