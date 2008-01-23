///-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
// 					Audio
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// The Shapes unit contains the code for the bascic shapes
// available in the SGDK : Point2D, LineSegment, and Rectangle,
// and the code to create and manipulate these.
//
// Change History:
//
// Version 1.1:
// - 2008-01-23: Andrew: Fixed exceptions
//               Added changes for 1.1 compatibility
//               
// - 2008-01-22: Andrew : Added initial version with basic types

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

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DistancePointToLine")]
        private static extern float DLL_DistancePointToLine(float x, float y, LineSegment line);

        /// <summary>
        /// Returns the shortest distance from a given point (x,y) to
        /// a line segment.
        /// </summary>
        /// <param name="x">the x value of the point</param>
        /// <param name="y">the y value of the point</param>
        /// <param name="line">the line to find the distance to</param>
        /// <returns>the length of the shortest line from point (x,y) to a point
        /// on the line</returns>
        public static float DistancePointToLine(float x, float y, LineSegment line)
        {
            float temp;
            try
            {
                temp = DLL_DistancePointToLine(x, y, line);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        /// <summary>
        /// Returns the shortest distance from a given point (x,y) to
        /// a line segment.
        /// </summary>
        /// <param name="pnt">the point to check from</param>
        /// <param name="line">the line to find the distance to</param>
        /// <returns>the length of the shortest line from point (x,y) to a point
        /// on the line</returns>
        public static float DistancePointToLine(Point2D pnt, LineSegment line)
        {
            return DistancePointToLine(pnt.X, pnt.Y, line);
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ClosestPointOnLine")]
        private static extern Point2D DLL_ClosestPointOnLine(float x, float y, LineSegment line);

        /// <summary>
        /// Returns the closest point on a line to another point (x, y). 
        /// </summary>
        /// <param name="x">the x value of the point to check from</param>
        /// <param name="y">the y value of the point to check from</param>
        /// <param name="line">the line to find the closest point on</param>
        /// <returns>the point on the line closest to the specified point (x,y)</returns>
        public static Point2D ClosestPointOnLine(float x, float y, LineSegment line)
        {
            Point2D temp;
            try
            {
                temp = DLL_ClosestPointOnLine(x, y, line);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        /// <summary>
        /// Returns the closest point on a line to another point (x, y). 
        /// </summary>
        /// <param name="pnt">The point to check from</param>
        /// <param name="line">the line to find the closest point on</param>
        /// <returns>the point on the line closest to the specified point (x,y)</returns>
        public static Point2D ClosestPointOnLine(Point2D fromPt, LineSegment line)
        {
            return ClosestPointOnLine(fromPt, line);
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CenterPoint")]
        private static extern Point2D DLL_CenterPoint(Sprite sprt);

        /// <summary>
        /// Returns the center point of the sprite. This does not take into
        /// consideration the shape of the bitmap being drawn, just returns
        /// the center point of the sprites bounding rectangle.
        /// </summary>
        /// <param name="sprt">The sprite to get the center point of.</param>
        /// <returns></returns>
        public static Point2D CenterPoint(Sprite sprt)
        {
            Point2D temp;
            try
            {
                temp = DLL_CenterPoint(sprt);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "IsPointOnLine")]
        private static extern int DLL_IsPointOnLine(Point2D pnt, LineSegment line);

        /// <summary>
        /// Determines if a point exists on a line.
        /// </summary>
        /// <param name="pnt">The point that you want to check</param>
        /// <param name="line">The line you want to see if the point is on</param>
        /// <returns>true if pnt is on the line</returns>
        public static bool IsPointOnLine(Point2D pnt, LineSegment line)
        {
            bool temp;
            try
            {
                temp = DLL_IsPointOnLine(pnt, line) == -1;
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        /// <summary>
        /// Creates a point at the given x and y. This is useful for 
        /// initialising points as it sets both the x and y value.
        /// </summary>
        /// <param name="x">the x value for the point</param>
        /// <param name="y">the y value for the point</param>
        /// <returns>The point with x and y value set</returns>
        public static Point2D CreatePoint(float x, float y)
        {
            Point2D result;
            result.X = x;
            result.Y = y;

            return result;
        }

        /// <summary>
        /// Gets the line segments that make up the boundary of a rectangle.
        /// </summary>
        /// <param name="rect">the rectangle</param>
        /// <returns>an array of lines that represent the boundary of the rectangle</returns>
        public static LineSegment[] LinesFromRect(Rectangle rect)
        {
            LineSegment[] result = new LineSegment[4];
   			result[0] = CreateLine(rect.Left, rect.Top, rect.Right, rect.Top);
			result[1] = CreateLine(rect.Left, rect.Top, rect.Left, rect.Bottom);
			result[2] = CreateLine(rect.Right, rect.Top, rect.Right, rect.Bottom);
			result[3] = CreateLine(rect.Left, rect.Bottom, rect.Right, rect.Bottom);   
            return result;
        }

	    /// <summary>
	    /// Creates a line from x1,y1 to x2,y2.
	    /// </summary>
	    /// <param name="x1">the x value of the starting point</param>
	    /// <param name="y1">the y value of the starting point</param>
	    /// <param name="x2">the x value of the ending point</param>
	    /// <param name="y2">the y value of the ending point</param>
	    /// <returns>A line from point x1,y1 to point x2, y2</returns>
        public static LineSegment CreateLine(float x1, float y1, float x2, float y2)
        {
            LineSegment result;
            result.StartPoint.X = x1;
            result.StartPoint.Y = y1;
            result.EndPoint.X = x2;
            result.EndPoint.Y = y2;
            return result;
        }

        /// <summary>
        /// Creates a line from a point, to the end of the indicated Vector. This can be
        /// useful for drawing a vector as a line, for debugging or level editing.
        /// </summary>
        /// <param name="pnt">the starting point of the line</param>
        /// <param name="mvt">the offset from the starting point to the ending point</param>
        /// <returns>a line from pnt, ending at the end of the vector placed at this point</returns>
        public static LineSegment LineFromVector(Point2D pnt, Vector mvt)
        {
            LineSegment result;
            result.StartPoint = pnt;
            result.EndPoint.X = pnt.X + mvt.X;
            result.EndPoint.Y = pnt.Y + mvt.Y;
            return result;
        }

        /// <summary>
        /// Creates a line from a point, to the end of the indicated Vector. This can be
        /// useful for drawing a vector as a line, for debugging or level editing.
        /// </summary>
        /// <param name="x">the x value of the point</param>
        /// <param name="y">the y value of the point</param>
        /// <param name="mvt">the offset from the starting point to the ending point</param>
        /// <returns>a line from pnt, ending at the end of the vector placed at this point</returns>
        public static LineSegment LineFromVector(float x, float y, Vector mvt)
        {
            return LineFromVector(CreatePoint(x, y), mvt);
        }

        /// <summary>
        /// Creates a line from the origin (0,0), to the end of the indicated Vector. This can be
        /// useful for drawing a vector as a line, for debugging or level editing.
        /// </summary>
        /// <param name="mvt">the offset from the starting point to the ending point</param>
        /// <returns>a line from the origin (0,0), ending at the end of the vector placed at this point</returns>
        public static LineSegment LineFromVector(Vector mvt)
        {
            return LineFromVector(CreatePoint(0, 0), mvt);
        }

        /// <summary>
        /// Returns the mid point of the line.
        /// </summary>
        /// <param name="line">the line to get the mid point of</param>
        /// <returns>The point at the middle of the line</returns>
        public static Point2D MidPoint(LineSegment line)
        {
            Point2D result;
		    result.X = line.StartPoint.X + (line.EndPoint.X - line.StartPoint.X) / 2; 
		    result.Y = line.StartPoint.Y + (line.EndPoint.Y - line.StartPoint.Y) / 2;
            return result;
        }

        /// <summary>
        /// Creates a rectangle using the supplied values. This can be done using
        /// the .NET Rectangle code: rect = new Rectangle(x, y, w, h).
        /// </summary>
        /// <param name="x">the x value for the rectangle</param>
        /// <param name="y">the y value for the rectangle</param>
        /// <param name="w">the rectangle's width</param>
        /// <param name="h">the rectangle's height</param>
        /// <returns>A rectangle at x, y with the indicated width and height</returns>
        public static Rectangle CreateRectangle(float x, float y, int w, int h)
        {
            return new Rectangle((int)x, (int)y, w, h);
        }

        /// <summary>
        /// Creates a rectangle to surround a bitmap. The x,y of the rectangle is
        /// at the origin (0,0).
        /// </summary>
        /// <param name="bmp">the bitmap to surround</param>
        /// <returns>A rectangle at 0,0 with the width and height from the bitmap</returns>
        public static Rectangle CreateRectangle(Bitmap bmp)
        {
            return CreateRectangle(0, 0, bmp);
        }

        /// <summary>
        /// Creates a rectangle to surround a bitmap, setting the x, y of the rectangle
        /// to the supplied values.
        /// </summary>
        /// <param name="x">the x value for the rectangle</param>
        /// <param name="y">the y value for the rectangle</param>
        /// <param name="bmp">the bitmap to surround</param>
        /// <returns>A rectangle at x,y with the width and height from the bitmap</returns>
        public static Rectangle CreateRectangle(float x, float y, Bitmap bmp)
        {
            return new Rectangle((int)x, (int)y, bmp.Width, bmp.Height);
        }

        /// <summary>
        /// Creates a rectangle to surround a bitmap, setting the x, y of the rectangle
        /// to the supplied point.
        /// </summary>
        /// <param name="pt">the point to set the x,y of the rectangle</param>
        /// <param name="bmp">the bitmap to surround</param>
        /// <returns>A rectangle at pt with the width and height from the bitmap</returns>
        public static Rectangle CreateRectangle(Point2D pt, Bitmap bmp)
        {
            return CreateRectangle(pt.X, pt.Y, bmp);
        }

        /// <summary>
        /// Creates a rectangle at the specified point with the indicated
        /// width and height
        /// </summary>
        /// <param name="pt">the point to set the x,y of the rectangle</param>
        /// <param name="height">the height of the rectangle</param>
        /// <param name="width">the width of the rectangle</param>
        /// <returns>A rectangle at pt with the width and height from the bitmap</returns>
        public static Rectangle CreateRectangle(Point2D pt, int width, int height)
        {
            return CreateRectangle(pt.X, pt.Y, width, height);
        }

        /// <summary>
        /// Creates a rectangle to surround a Sprite. The x and y value are set
        /// from the position of the sprite, the width and height from the sprites
        /// current width and height.
        /// </summary>
        /// <param name="sprt">the sprite to surround</param>
        /// <returns>A rectangle to surround the sprite</returns>
        public static Rectangle CreateRectangle(Sprite sprt)
        {
            return CreateRectangle(sprt.xPos, sprt.yPos, sprt.Width, sprt.Height);
        }

        /// <summary>
        /// Moves the rectangle by the amount specified in the vector.
        /// </summary>
        /// <param name="rect">The rectangle to move</param>
        /// <param name="movement">The amount to move the rectangle</param>
        /// <returns>A new rectangle at the new location</returns>
        public static Rectangle RectangleAfterMove(Rectangle rect, Vector movement)
        {
            return CreateRectangle(rect.X + (int)movement.X, rect.Y + (int)movement.Y, rect.Width, rect.Height);
        }

        /// <summary>
        /// Indicates if the two rectangles intersect. You can also use the 
        /// method on the rectangle to perform this operation: rect1.IntersectsWith(rect2).
        /// </summary>
        /// <param name="rect1">a rectangle</param>
        /// <param name="rect2">another rectangle</param>
        /// <returns>true is rect1 and rect2 intersect</returns>
        public static bool RectanglesIntersect(Rectangle rect1, Rectangle rect2)
        {
            return rect1.IntersectsWith(rect2);
        }

        /// <summary>
        /// Returns the intersection of rect1 and rect2. You can also use
        /// the .NET code to perform this: rect1.Intersect(rect2) which will
        /// change rect1, where as this code returns a new rectangle.
        /// </summary>
        /// <param name="rect1">a rectangle</param>
        /// <param name="rect2">another rectangle</param>
        /// <returns>the intersection of rect1 and rect2</returns>
        public static Rectangle Intersection(Rectangle rect1, Rectangle rect2)
        {
            Rectangle result = rect1;
            result.Intersect(rect2);
            return result;
        }

        /// <summary>
        /// Determines the distance between two points.
        /// </summary>
        /// <param name="pt1">one point</param>
        /// <param name="pt2">the other point</param>
        /// <returns>the distance between pt1 and pt2</returns>
        public static float DistanceBetween(Point2D pt1, Point2D pt2)
        {
            Vector temp = Physics.CreateVector(pt2.X - pt1.X, pt2.Y - pt1.Y);
		    return Physics.Magnitude(temp);
        }
    }
}
