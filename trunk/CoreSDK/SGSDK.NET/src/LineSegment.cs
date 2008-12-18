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

        /// <summary>
        /// Creates a line from x1,y1 to x2,y2.
        /// </summary>
        /// <param name="x1">the x value of the starting point</param>
        /// <param name="y1">the y value of the starting point</param>
        /// <param name="x2">the x value of the ending point</param>
        /// <param name="y2">the y value of the ending point</param>
        public LineSegment(float x1, float y1, float x2, float y2)
        {
            StartPoint.X = x1;
            StartPoint.Y = y1;
            EndPoint.X = x2;
            EndPoint.Y = y2;
        }


        /// <summary>
        /// Creates a line from a point, to the end of the indicated Vector. This can be
        /// useful for drawing a vector as a line, for debugging or level editing.
        /// </summary>
        /// <param name="pnt">the starting point of the line</param>
        /// <param name="mvt">the offset from the starting point to the ending point</param>
        public LineSegment(Point2D pnt, Vector mvt)
        {
            StartPoint = pnt;
            EndPoint.X = pnt.X + mvt.X;
            EndPoint.Y = pnt.Y + mvt.Y;
        }

        /// <summary>
        /// Represent a LineSegment as a vector.
        /// </summary>
        /// <returns></returns>
        public Vector ToVector()
        {
            return Physics.VectorFromPoints(this.StartPoint, this.EndPoint);
        }

        /// <summary>
        /// Determines if a point exists on a line.
        /// </summary>
        /// <param name="pnt">The point that you want to check</param>
        /// <returns>true if pnt is on the line</returns>
        public bool IsPointOnLine(Point2D pnt)
        {
            return SGSDK.IsPointOnLine(pnt, this) == -1;
        }

        /// <summary>
        /// Returns the mid point of the line.
        /// </summary>
        /// <returns>The point at the middle of the line</returns>
        public Point2D MidPoint()
        {
            Point2D result;
            result.X = StartPoint.X + (EndPoint.X - StartPoint.X) / 2;
            result.Y = StartPoint.Y + (EndPoint.Y - StartPoint.Y) / 2;
            return result;
        }

    }
}
