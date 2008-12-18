using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace SwinGame
{
    /// <summary>
    /// LineSegments are a data type that holds 2 Points, the Start and Ending Points of a Line
    /// </summary>
    [StructLayout(LayoutKind.Sequential)]
    public struct LineSegment
    {
        /// <summary>
        /// Start Point of the Line Segment
        /// </summary>
        public Point2D StartPoint;
        /// <summary>
        /// End Point of the Line Segment
        /// </summary>
        public Point2D EndPoint;

        public Vector ToVector()
        {
            return Physics.VectorFromPoints(this.StartPoint, this.EndPoint);
        }
    }
}
