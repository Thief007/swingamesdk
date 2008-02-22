using System;
using System.Collections.Generic;
using System.Text;
using SwinGame;
using System.Runtime.InteropServices;
using System.Drawing;
using System.IO;

namespace SwinGameVB
{
    [ClassInterface(ClassInterfaceType.None)]
    [Guid("767BEEFF-A826-4c7f-B30C-C448CDB1BA1E")]
    [ComVisible(true)]
    public class Point2D: IPoint2D
    {
        private SwinGame.Point2D point;
        public float GetX()
        {
            return point.X;
        }
        public float GetY()
        {
            return point.Y;
        }
        public void SetX(float value)
        {
            point.X = value;
        }
        public void SetY(float value)
        {
            point.Y = value;
        }

        internal SwinGame.Point2D result
        {
            get
            {
                return point;
            }
            set
            {
                point = new SwinGame.Point2D();
                point = value;
            }
        }
    }

    [Guid("EA58FAE9-1B48-4c79-807C-33449FC25958")]
    [ComVisible(true)]
    public interface IPoint2D
    {
        float GetX();
        float GetY();
        void SetX(float value);
        void SetY(float value);
    }



    [ClassInterface(ClassInterfaceType.None)]
    [Guid("E4845722-BA74-49a9-8B88-1137A2136D5A")]
    [ComVisible(true)]
    public class LineSegment : ILineSegment
    {
        //private SwinGame.LineSegment line;
        private Point2D StartPoint;
        private Point2D EndPoint;
        public Point2D GetStartPoint()
        {
            return StartPoint;
        }
        public Point2D GetEndPoint()
        {
            return EndPoint;
        }
        public void SetStartPoint(Point2D value)
        {
            StartPoint = value;
        }
        public void SetEndPoint(Point2D value)
        {
            EndPoint = value;
        }
        internal SwinGame.LineSegment result
        {
            get
            {
                SwinGame.LineSegment line;
                line.StartPoint = StartPoint.result;
                line.EndPoint = EndPoint.result;
                return line;
            }
            set
            {
                StartPoint = new Point2D();
                EndPoint = new Point2D();
                StartPoint.result = value.StartPoint;
                EndPoint.result = value.EndPoint;
            }
        }
    }

    [Guid("3CF769DA-4F89-4a45-BD91-DD855030955E")]
    [ComVisible(true)]
    public interface ILineSegment
    {
        Point2D GetStartPoint();
        Point2D GetEndPoint();
        void SetStartPoint(Point2D value);
        void SetEndPoint(Point2D value);
    }

    [ClassInterface(ClassInterfaceType.None)]
    [Guid("4F80A984-F1D8-4f63-B78D-B957615A6400")]
    [ComVisible(true)]
    public class Rectangle :IRectangle
    {
        private float X;
        private float Y;
        public float GetX()
        {
            return X;
        }
        public float GetY()
        {
            return Y;
        }
        public void SetX(float value)
        {
            X = value;
        }
        public void SetY(float value)
        {
            Y = value;
        }
        
        private float Height;
        private float Width;
        public float GetHeight()
        {
            return Height;
        }
        public float GetWidth()
        {
            return Width;
        }
        public void SetHeight(float value)
        {
            Height = value;
        }
        public void SetWidth(float value)
        {
            Width = value;
        }

        internal System.Drawing.Rectangle result
        {
            get
            {
                System.Drawing.Rectangle temp = new System.Drawing.Rectangle((int)X, (int)Y, (int)Width, (int)Height);
                return temp;
            }
            set
            {
                X = value.X;
                Y = value.Y;
                Width = value.Width;
                Height = value.Height;
            }
        }
    }
    [Guid("B72F78FC-7C3E-4fb4-B707-51B8E5186B56")]
    [ComVisible(true)]
    public interface IRectangle
    {
        float GetX();
        float GetY();
        void SetX(float value);
        void SetY(float value);
        float GetWidth();
        float GetHeight();
        void SetWidth(float value);
        void SetHeight(float value);
    }


    [ClassInterface(ClassInterfaceType.None)]
    [Guid("ADD251D7-953F-4ccd-8232-7AFCBA4FCB52")]
    [ComVisible(true)]
    public class Shapes: IShapes
    {
        public float DistancePointToLine(float x, float y, LineSegment line)
        {
            return SwinGame.Shapes.DistancePointToLine(x, y, line.result);
        }

        public float DistancePointToLine_Point(Point2D pnt, LineSegment line)
        {
            return SwinGame.Shapes.DistancePointToLine(pnt.result, line.result);
        }
        public Point2D ClosestPointOnLine(float x, float y, LineSegment line)
        {
            Point2D temp = new Point2D();
            temp.result = SwinGame.Shapes.ClosestPointOnLine(x, y, line.result);
            return temp;
        }

        public Point2D ClosestPointOnLine_Point(Point2D fromPt, LineSegment line)
        {
            Point2D temp = new Point2D();
            temp.result = SwinGame.Shapes.ClosestPointOnLine(fromPt.result, line.result);
            return temp;
        }
        public Point2D CenterPoint(Sprite sprt)
        {
            Point2D temp = new Point2D();
            temp.result = SwinGame.Shapes.CenterPoint(sprt.result);
            return temp;
        }
        public bool IsPointOnLine(Point2D pnt, LineSegment line)
        {
            return SwinGame.Shapes.IsPointOnLine(pnt.result, line.result);
        }
        public Point2D CreatePoint(float x, float y)
        {
            Point2D temp = new Point2D();
            temp.result = SwinGame.Shapes.CreatePoint(x, y);
            return temp;
        }
        public LineSegment[] LinesFromRect(Rectangle rect)
        {
            SwinGame.LineSegment[] temp;
            temp = SwinGame.Shapes.LinesFromRect(rect.result);
            LineSegment[] lines = new LineSegment[temp.Length];
            for (int i = 0; i < temp.Length; i++)
            {
                lines[i] = new LineSegment();
                lines[i].result = temp[i];
            }
            return lines;
        }
        public LineSegment CreateLine(float x1, float y1, float x2, float y2)
        {
            LineSegment temp = new LineSegment();
            temp.result = SwinGame.Shapes.CreateLine(x1, y1, x2, y2);
            return temp;
        }
        public LineSegment LineFromVector_Point(Point2D pnt, Vector mvt)
        {
            LineSegment temp = new LineSegment();
            temp.result = SwinGame.Shapes.LineFromVector(pnt.result, mvt.result);
            return temp;
        }

        public LineSegment LineFromVector_xy(float x, float y, Vector mvt)
        {
            LineSegment temp = new LineSegment();
            temp.result = SwinGame.Shapes.LineFromVector(x, y, mvt.result);
            return temp;
        }

        public LineSegment LineFromVector(Vector mvt)
        {
            LineSegment temp = new LineSegment();
            temp.result = SwinGame.Shapes.LineFromVector(mvt.result);
            return temp;
        }
        public Point2D MidPoint(LineSegment line)
        {
            Point2D temp = new Point2D();
            temp.result = SwinGame.Shapes.MidPoint(line.result);
            return temp;
        }
        public Rectangle CreateRectangle(float x, float y, int w, int h)
        {
            Rectangle temp = new Rectangle();
            temp.result = SwinGame.Shapes.CreateRectangle(x, y, w, h);
            return temp;
        }
        public Rectangle CreateRectangle_Bitmap(Bitmap bmp)
        {
            Rectangle temp = new Rectangle();
            temp.result = SwinGame.Shapes.CreateRectangle(bmp.result);
            return temp;
        }
        public Rectangle CreateRectangle_xy_Bitmap(float x, float y, Bitmap bmp)
        {
            Rectangle temp = new Rectangle();
            temp.result = SwinGame.Shapes.CreateRectangle(x, y, bmp.result);
            return temp;
        }
        public Rectangle CreateRectangle_Point_Bitmap(Point2D pt, Bitmap bmp)
        {
            Rectangle temp = new Rectangle();
            temp.result = SwinGame.Shapes.CreateRectangle(pt.result, bmp.result);
            return temp;
        }

        public Rectangle CreateRectangle_Point(Point2D pt, int width, int height)
        {
            Rectangle temp = new Rectangle();
            temp.result = SwinGame.Shapes.CreateRectangle(pt.result, width, height);
            return temp;
        }

        public Rectangle CreateRectangle_Sprite(Sprite sprt)
        {
            Rectangle temp = new Rectangle();
            temp.result = SwinGame.Shapes.CreateRectangle(sprt.result);
            return temp;
        }
        public Rectangle RectangleAfterMove(Rectangle rect, Vector movement)
        {
            Rectangle temp = new Rectangle();
            temp.result = SwinGame.Shapes.RectangleAfterMove(rect.result, movement.result);
            return temp;
        }

        public bool RectanglesIntersect(Rectangle rect1, Rectangle rect2)
        {
            return SwinGame.Shapes.RectanglesIntersect(rect1.result, rect2.result);
        }
        public Rectangle Intersection(Rectangle rect1, Rectangle rect2)
        {
            Rectangle temp = new Rectangle();
            temp.result = SwinGame.Shapes.Intersection(rect1.result, rect2.result);
            return temp;
        }
        public float DistanceBetween(Point2D pt1, Point2D pt2)
        {
            return SwinGame.Shapes.DistanceBetween(pt1.result, pt2.result);
        }
        public bool GetLineIntersectionPoint(LineSegment line1, LineSegment line2, out Point2D pnt)
        {
            SwinGame.Point2D temp;
            bool tempb = SwinGame.Shapes.GetLineIntersectionPoint(line1.result, line2.result, out temp);
            pnt = new Point2D();
            pnt.result = temp;
            return tempb;
        }
        public bool LineIntersectsWithLines_Array(LineSegment target, [In] ref LineSegment[] lines)
        {
            SwinGame.LineSegment[] temp = new SwinGame.LineSegment[lines.Length];
            for (int i = 0; i < lines.Length; i++)
			{
			    temp[i] = lines[i].result;
			}
            return SwinGame.Shapes.LineIntersectsWithLines(target.result,temp);
        }

        public bool LineIntersectsWithRect(LineSegment target, Rectangle rect)
        {
            return SwinGame.Shapes.LineIntersectsWithRect(target.result, rect.result);
        }

        public bool PointIsWithinRect(Point2D v, float x, float y, float w, float h)
        {
            return SwinGame.Shapes.PointIsWithinRect(v.result, x, y,w, h);
        }
        public bool PointIsWithinRect_Rectangle(Point2D v, Rectangle rect)
        {
            return SwinGame.Shapes.PointIsWithinRect(v.result, rect.result);
        }
        public CollisionSide GetSideForCollisionTest(Vector movement)
        {
            CollisionSide temp;
            temp =(CollisionSide) SwinGame.Shapes.GetSideForCollisionTest(movement.result);
            return temp;
        }
        
        public Single RectangleTop(Rectangle rect)
        {
            return SwinGame.Shapes.RectangleTop(rect.result);
        }

        public Single RectangleBottom(Rectangle rect)
        {
            return SwinGame.Shapes.RectangleBottom(rect.result);
        }

        public Single RectangleLeft(Rectangle rect)
        {
            return SwinGame.Shapes.RectangleLeft(rect.result);
        }

        public Single RectangleRight(Rectangle rect)
        {
            return SwinGame.Shapes.RectangleRight(rect.result);
        }
    }

    [Guid("FD7A335C-47C5-4632-BE9E-9345A8A5B80E")]
    [ComVisible(true)]
    public interface IShapes
    {
        float DistancePointToLine(float x, float y, LineSegment line);
        float DistancePointToLine_Point(Point2D pnt, LineSegment line);
        Point2D ClosestPointOnLine(float x, float y, LineSegment line);
        Point2D ClosestPointOnLine_Point(Point2D fromPt, LineSegment line);
        Point2D CenterPoint(Sprite sprt);
        bool IsPointOnLine(Point2D pnt, LineSegment line);
        Point2D CreatePoint(float x, float y);
        LineSegment[] LinesFromRect(Rectangle rect);
        LineSegment CreateLine(float x1, float y1, float x2, float y2);
        LineSegment LineFromVector_Point(Point2D pnt, Vector mvt);
        LineSegment LineFromVector_xy(float x, float y, Vector mvt);
        LineSegment LineFromVector(Vector mvt);
        Point2D MidPoint(LineSegment line);
        Rectangle CreateRectangle(float x, float y, int w, int h);
        Rectangle CreateRectangle_Bitmap(Bitmap bmp);
        Rectangle CreateRectangle_xy_Bitmap(float x, float y, Bitmap bmp);
        Rectangle CreateRectangle_Point_Bitmap(Point2D pt, Bitmap bmp);
        Rectangle CreateRectangle_Point(Point2D pt, int width, int height);
        Rectangle CreateRectangle_Sprite(Sprite sprt);
        Rectangle RectangleAfterMove(Rectangle rect, Vector movement);
        bool RectanglesIntersect(Rectangle rect1, Rectangle rect2);
        Rectangle Intersection(Rectangle rect1, Rectangle rect2);
        float DistanceBetween(Point2D pt1, Point2D pt2);
        bool GetLineIntersectionPoint(LineSegment line1, LineSegment line2, out Point2D pnt);
        bool LineIntersectsWithLines_Array(LineSegment target, [In] ref LineSegment[] lines);
        bool LineIntersectsWithRect(LineSegment target, Rectangle rect);
        bool PointIsWithinRect(Point2D v, float x, float y, float w, float h);
        bool PointIsWithinRect_Rectangle(Point2D v, Rectangle rect);
        CollisionSide GetSideForCollisionTest(Vector movement);

        Single RectangleTop(Rectangle rect);
        Single RectangleBottom(Rectangle rect);
        Single RectangleLeft(Rectangle rect);
        Single RectangleRight(Rectangle rect);
    }
}

