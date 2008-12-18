using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace SwinGame
{
    /// <summary>
    /// The Point is a data type that holds an X and Y Coordinate.
    /// </summary>
    [StructLayout(LayoutKind.Sequential)]
    public struct Point2D
    {
        /// <summary>
        /// X Coordinate of the Point
        /// </summary>
        public float X;
        /// <summary>
        /// Y Coordinate of the Point
        /// </summary>
        public float Y;

        /// <summary>
        /// Casts the Point2D to a Vector.
        /// </summary>
        /// <param name="pnt">the point to cast</param>
        /// <returns>a vector to that point from the origin</returns>
        public static implicit operator Vector(Point2D pnt)
        {
            return new Vector(pnt.X, pnt.Y);
        }

        /// <summary>
        /// Creates a point2d at an x, y location.
        /// </summary>
        /// <param name="x">the x location of the point</param>
        /// <param name="y">the y loaction of the point</param>
        public Point2D(float x, float y)
        {
            this.X = x;
            this.Y = y;
        }

        /// <summary>
        /// Determines the distance between two points.
        /// </summary>
        /// <param name="dest">the other point</param>
        /// <returns>the distance between pt1 and pt2</returns>
        public float DistanceTo(Point2D dest)
        {
            Vector temp = Physics.CreateVector(dest.X - X, dest.Y - Y);
            return Physics.Magnitude(temp);
        }

    }
}
