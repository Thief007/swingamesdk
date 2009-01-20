//-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
// 					Triangle
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
using System.Runtime.InteropServices;

namespace SwinGame
{
    /// <summary>
    /// A Triangle is a data type that holds 3 points, that when connected, form a triangle
    /// </summary>
    [StructLayout(LayoutKind.Sequential)]
    public struct Triangle
    {
        /// <summary>
        /// First Point Coordinate of the Triangle
        /// </summary>
        public Point2D PointA;
        /// <summary>
        /// Second Point Coordinate of the Triangle
        /// </summary>
        public Point2D PointB;
        /// <summary>
        /// Third Point Coordinate of the Triangle
        /// </summary>
        public Point2D PointC;

        /// <summary>
        /// Creates a Triangle from the points given
        /// </summary>
        /// <param name="ax">PointA X Coordinate</param>
        /// <param name="ay">PointA Y Coordinate</param>
        /// <param name="bx">PointB X Coordinate</param>
        /// <param name="by">PointB Y Coordinate</param>
        /// <param name="cx">PointC X Coordinate</param>
        /// <param name="cy">PointC Y Coordinate</param>
        /// <returns></returns>
        public Triangle(Single ax, Single ay, Single bx, Single by, Single cx, Single cy)
        {
            PointA.X = ax; PointA.Y = ay;
            PointB.X = bx; PointB.Y = by;
            PointC.X = cx; PointC.Y = cy;
        }

        /// <summary>
        /// Creates a Triangl from the points given
        /// </summary>
        /// <param name="a">PointA</param>
        /// <param name="b">PointB</param>
        /// <param name="c">PointC</param>
        public Triangle(Point2D a, Point2D b, Point2D c) : this(a.X, a.Y, b.X, b.Y, c.X, c.Y)
        {}

        /// <summary>
        /// Access the three points on the triangle.
        /// </summary>
        /// <param name="index">point index</param>
        /// <returns>a point of the triangle</returns>
        public Point2D this[int index]
        {
            get
            {
                switch (index)
                {
                    case 0: return PointA;
                    case 1: return PointB;
                    case 2: return PointC;
                    default: throw new IndexOutOfRangeException();
                }
            }
        }

        /// <summary>
        /// This function will return true if the given point can be found within the given triangle
        /// </summary>
        /// <param name="point">Point</param>
        /// <returns>True if the point is in the Triangle</returns>
        public bool IsPointInTriangle(Point2D point)
        {
            return SGSDK.IsPointInTriangle(point, this.ToArray()) == -1;
        }

        /// <summary>
        /// Canculates the Barycenter of the passed in Triangle.
        /// </summary>
        /// <returns>The center point of the triangle (Barycenter)</returns>
        public Point2D Barycenter()
        {
            return SGSDK.TriangleBarycenter(this.ToArray());
        }

        /// <summary>
        /// Apply the passed in matrix to all of the points in the passed in triangle. The
        /// passed in Triangle's data will be changed using the details from the Matrix.
        /// </summary>
        /// <param name="m">the matrix containing the translations to be applied to the triangle</param>
        public void ApplyMatrix(Matrix2D m)
        {
            Point2D[] data = this.ToArray();
            SGSDK.ApplyMatrixToTriangle(m, data, out PointA, out PointB, out PointC);
        }

        internal Point2D[] ToArray()
        {
            return new Point2D[] { PointA, PointB, PointC };
        }
    }
}
