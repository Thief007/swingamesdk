using System;
using System.Collections.Generic;
using System.Text;
using Color = System.Drawing.Color;
using SwinGameVB;

namespace Tests
{
    class ShapeTests : IGameTestLoader
    {
        #region IGameTestLoader Members

        public void AddTo(List<TestSuite> list)
        {
            TestSuite result = new TestSuite("Shape Tests");
            result.Add(new TestShapes1());
            result.Add(new TestShapes2());
            list.Add(result);
        }

        #endregion

        private class TestShapes1 : TestSet
        {
            private readonly static string METHS =
                "Shapes Test Part 1";

            private readonly static string INST =
                "Use the arrow keys to move" + Environment.NewLine + "the rectangle";

			private Rectangle staticRect1 = Consts.Shapes.CreateRectangle(100, 100, 100, 150);
			private Rectangle staticRect2 = Consts.Shapes.CreateRectangle(250, 100, 100, 150);
			private Rectangle movRect = Consts.Shapes.CreateRectangle(100, 300, 70, 60);
			private Sprite ball = Consts.Graphics.CreateSprite(GameResources.GameImage("BallImage1"));

            public TestShapes1() : base(METHS, INST) 
            { 
                ball.SetX( 250); 
                ball.SetY ( 270);
            }

            protected override void ToRun(Rectangle toDrawIn)
            {
                Vector movVec = Consts.Physics.CreateVector_NoInvert(0, 0);
		
		        if (Consts.Input.IsKeyPressed(Keys.VK_LEFT)) movVec = Consts.Physics.AddVectors(movVec, Consts.Physics.CreateVector_NoInvert(-2, 0));
		        if (Consts.Input.IsKeyPressed(Keys.VK_RIGHT)) movVec = Consts.Physics.AddVectors(movVec, Consts.Physics.CreateVector_NoInvert(2, 0));
		        if (Consts.Input.IsKeyPressed(Keys.VK_UP)) movVec = Consts.Physics.AddVectors(movVec, Consts.Physics.CreateVector_NoInvert(0, -2));
		        if (Consts.Input.IsKeyPressed(Keys.VK_DOWN)) movVec = Consts.Physics.AddVectors(movVec, Consts.Physics.CreateVector_NoInvert(0, 	2));
        		
		        movRect = Consts.Shapes.RectangleAfterMove(movRect, movVec);
        		
		        Consts.Graphics.DrawSprite(ball);
		        Consts.Graphics.DrawCircle_Fill_Point(Color.White.ToArgb(), true, Consts.Shapes.CenterPoint(ball), 5);
        		
		        for (int i = 0; i < 4; i++)
		        {
                    Consts.Graphics.DrawLine_Line(Color.White.ToArgb(), Consts.Shapes.LinesFromRect(staticRect1)[i]);
                    Consts.Graphics.DrawPixel_Point(Color.Black.ToArgb(), Consts.Shapes.MidPoint(Consts.Shapes.LinesFromRect(staticRect1)[i]));
		        }
        		
		        if (Consts.Shapes.RectanglesIntersect(movRect, staticRect1) || Consts.Shapes.RectanglesIntersect(movRect, staticRect2))
		        {
                    Consts.Text.DrawText("The green rectangle is intersecting with a", Color.White.ToArgb(), GameResources.GameFont("Courier"), 0, 0);
                    Consts.Text.DrawText("rectangle", Color.White.ToArgb(), GameResources.GameFont("Courier"), 0, 15);
		        }

                Consts.Graphics.DrawRectangle_Rectangle(Color.Yellow.ToArgb(), staticRect2);
                Consts.Graphics.DrawRectangle_Rectangle(Color.Green.ToArgb(), movRect);

                Consts.Graphics.DrawLine(Color.White.ToArgb(), Consts.Shapes.ClosestPointOnLine(movRect.GetX(), movRect.GetY(), Consts.Shapes.LinesFromRect(staticRect1)[2]).GetX(), Consts.Shapes.ClosestPointOnLine(movRect.GetX(), movRect.GetY(), Consts.Shapes.LinesFromRect(staticRect1)[2]).GetY(), movRect.GetX(), movRect.GetY());
                Consts.Graphics.DrawLine(Color.White.ToArgb(), Consts.Shapes.ClosestPointOnLine_Point(Consts.Shapes.CreatePoint(movRect.GetX(), movRect.GetY()), Consts.Shapes.LinesFromRect(staticRect1)[3]).GetX(), Consts.Shapes.ClosestPointOnLine(movRect.GetX(), movRect.GetY(), Consts.Shapes.LinesFromRect(staticRect1)[3]).GetY(), movRect.GetX(), movRect.GetY());
                Consts.Graphics.DrawLine(Color.Red.ToArgb(), Consts.Shapes.ClosestPointOnLine(movRect.GetX(), movRect.GetY(), Consts.Shapes.LinesFromRect(staticRect1)[0]).GetX(), Consts.Shapes.ClosestPointOnLine(movRect.GetX(), movRect.GetY(), Consts.Shapes.LinesFromRect(staticRect1)[0]).GetY(), movRect.GetX(), movRect.GetY());
                Consts.Text.DrawText_Point("The magnitude of the red line: " + Convert.ToString(Consts.Shapes.DistancePointToLine_Point(Consts.Shapes.CreatePoint(movRect.GetX(), movRect.GetY()), Consts.Shapes.LinesFromRect(staticRect1)[0])), Color.Red.ToArgb(), GameResources.GameFont("Courier"), Consts.Shapes.CreatePoint(0, 30));
                Consts.Graphics.DrawLine(Color.Blue.ToArgb(), Consts.Shapes.ClosestPointOnLine_Point(Consts.Shapes.CreatePoint(movRect.GetX(), movRect.GetY()), Consts.Shapes.LinesFromRect(staticRect1)[1]).GetX(), Consts.Shapes.ClosestPointOnLine(movRect.GetX(), movRect.GetY(), Consts.Shapes.LinesFromRect(staticRect1)[1]).GetY(), movRect.GetX(), movRect.GetY());
                Consts.Text.DrawText_Point("The magnitude of the blue line: " + Convert.ToString(Consts.Shapes.DistancePointToLine(movRect.GetX(), movRect.GetY(), Consts.Shapes.LinesFromRect(staticRect1)[1])), Color.Blue.ToArgb(), GameResources.GameFont("Courier"), Consts.Shapes.CreatePoint(0, 45));
        	
            }
        }
/*
        private class TestShapes2 : TestSet
        {
            private readonly static string METHS =
                "Shapes Test Part 2";

            private readonly static string INST =
                "Use the arrow keys to move" + Environment.NewLine + "the line";

            public TestShapes2()
                : base(METHS, INST)
            {
                staticRect1 = Consts.Shapes.CreateRectangle(100, 100, 100, 150);
                staticRect2 = Consts.Shapes.CreateRectangle(250, 100, 100, 150);
                movRect = Consts.Shapes.CreateRectangle(100, 300, 70, 60);
            }

            private LineSegment movLine = Consts.Shapes.CreateLine(200, 200, 250, 250);
            private Point2D tempPoint;
            private Point2D[] points;
            private int i;
            private LineSegment tempLine;
            private Color rectCol;

            private Rectangle staticRect1, staticRect2, movRect;

            protected override void ToRun(Rectangle toDrawIn)
            {
                Array.Resize(ref points, 4);
		
		        if (Consts.Input.IsKeyPressed(Keys.VK_LEFT)) MoveLine(-2, 0);
		        if (Consts.Input.IsKeyPressed(Keys.VK_RIGHT)) MoveLine(2, 0);
		        if (Consts.Input.IsKeyPressed(Keys.VK_UP)) MoveLine(0, -2);
		        if (Consts.Input.IsKeyPressed(Keys.VK_DOWN)) MoveLine(0, 2);
        		
		        if (Consts.Shapes.PointIsWithinRect_Rectangle(movLine.GetStartPoint(), staticRect1) || Consts.Shapes.PointIsWithinRect_Rectangle(movLine.GetEndPoint(), staticRect1))
			    {
                    rectCol = Color.Red;
                }
		        else 
                {
                    rectCol = Color.White;
                }
                LineSegment [] temp  = Consts.Shapes.LinesFromRect(staticRect1);
        		
		        if (Consts.Shapes.LineIntersectsWithLines_Array(movLine, ref temp) && Consts.Shapes.LineIntersectsWithRect(movLine, staticRect1))
		        {
			        Consts.Text.DrawText("The line intersects with the rectangle!", Color.White.ToArgb(), GameResources.GameFont("Courier"), 0, 0);
		        }

                Consts.Graphics.DrawLine_Line(Color.White.ToArgb(), movLine);

                Consts.Graphics.DrawHorizontalLine(Color.Green.ToArgb(), Consts.Shapes.RectangleTop(staticRect1), 0, Consts.Core.ScreenWidth());
                Consts.Graphics.DrawHorizontalLine(Color.Green.ToArgb(), Consts.Shapes.RectangleBottom(staticRect1), 0, Consts.Core.ScreenWidth());
                Consts.Graphics.DrawVerticalLine(Color.Green.ToArgb(), Consts.Shapes.RectangleLeft(staticRect1), 0, Consts.Core.ScreenHeight());
                Consts.Graphics.DrawVerticalLine(Color.Green.ToArgb(), Consts.Shapes.RectangleRight(staticRect1), 0, Consts.Core.ScreenHeight());

                Consts.Graphics.DrawRectangle_Rectangle(rectCol.ToArgb(), staticRect1);

		        for (int i = 0; i < 4; i++)
		        {
			        tempLine = Consts.Shapes.LinesFromRect(staticRect1)[i];
        			
			        if (Consts.Shapes.GetLineIntersectionPoint(movLine, tempLine, out tempPoint))
			        {
                        Consts.Graphics.DrawCircle_Point(Color.Red.ToArgb(), tempPoint, 5);
				        points[i] = tempPoint;
                    }
        			
			        if (Consts.Shapes.IsPointOnLine(movLine.GetStartPoint(), tempLine) || Consts.Shapes.IsPointOnLine(movLine.GetEndPoint(), tempLine))
			        {
                        Consts.Graphics.DrawLine_Line(Color.Yellow.ToArgb(), tempLine);
                    }
		        }
        		
		        //Consts.Graphics.DrawLine(Color.Red, Consts.Shapes.CreateLine(100, 100, 150, 150));
		        //if (Consts.Shapes.IsPointOnLine(Input.GetMousePosition(), Consts.Shapes.CreateLine(100, 100, 150, 150)))
		        //{
			    //    Consts.Text.DrawText("on the line", Color.White, GameResources.GameFont("Courier"), 100, 100);
		        //}
        		
		        if (Consts.Input.IsKeyPressed(Keys.VK_1)) ShowDistance(0);
                if (Consts.Input.IsKeyPressed(Keys.VK_2)) ShowDistance(1);
                if (Consts.Input.IsKeyPressed(Keys.VK_3)) ShowDistance(2);
                if (Consts.Input.IsKeyPressed(Keys.VK_4)) ShowDistance(3);
            }

            private void MoveLine(int xOffset, int yOffset)
		    {
			    movLine.SetEndPoint(Consts.Shapes.CreatePoint( movLine.GetStartPoint().GetX() + xOffset, movLine.GetStartPoint().GetY() + yOffset));
			    movLine.SetEndPoint(Consts.Shapes.CreatePoint( movLine.GetEndPoint().GetX() + xOffset, movLine.GetEndPoint().GetY() + yOffset));
            }
		
		    private void ShowDistance(int num)
		    {
                Consts.Graphics.DrawLine(Color.Blue.ToArgb(), points[num].GetX(), points[num].GetY(), movLine.GetStartPoint().GetX(), movLine.GetStartPoint().GetY());
                Consts.Text.DrawText("m = " + Convert.ToString(Consts.Shapes.DistanceBetween(points[num], movLine.GetStartPoint())), Color.White.ToArgb(), GameResources.GameFont("Courier"), points[num].GetX(), points[num].GetY());
		    }
        }
 */
        private class TestShapes2 : TestSet
        {
            private readonly static string METHS =
                "Shapes Test Part 2";

            private readonly static string INST =
                "Use the arrow keys to move" + Environment.NewLine + "the rectangle";

            private Point2D tempPoint, mp;
            private int i;
            private Point2D[] points = new Point2D[4];
            private LineSegment tempLine, diag, movLine;
            private Rectangle staticRect1;
            private int rectCol;
            public TestShapes2() : base(METHS, INST) 
            { 
                movLine = Consts.Shapes.CreateLine(200, 200, 250, 250);
                staticRect1 = Consts.Shapes.CreateRectangle(100, 100, 100, 150);
            }

            public void MoveLine(int xOffset, int yOffset)
            {
                movLine.SetStartPoint(Consts.Shapes.CreatePoint(movLine.GetStartPoint().GetX() + xOffset, movLine.GetStartPoint().GetY() + yOffset));
                movLine.SetEndPoint(Consts.Shapes.CreatePoint(movLine.GetEndPoint().GetX() + xOffset, movLine.GetEndPoint().GetY() + yOffset));
            }
            public void ShowDistance(int num)
            {
                Consts.Graphics.DrawLine(Color.Blue.ToArgb(), points[num].GetX(), points[num].GetY(), movLine.GetStartPoint().GetX(), movLine.GetStartPoint().GetY());
                Consts.Text.DrawText("m = "+ Consts.Shapes.DistanceBetween(points[num], movLine.GetStartPoint()), Color.White.ToArgb(), GameResources.GameFont("Courier"), points[num].GetX(), points[num].GetY());
            }
            protected override void ToRun(Rectangle toDrawIn)
            {
                mp = Consts.Shapes.CreatePoint(Consts.Camera.GameX((int)Consts.Input.GetMousePosition().GetX()), Consts.Camera.GameY((int)Consts.Input.GetMousePosition().GetY()));
                if (Consts.Input.IsKeyPressed(Keys.VK_LEFT)) MoveLine(-2, 0);
                if (Consts.Input.IsKeyPressed(Keys.VK_RIGHT)) MoveLine(2, 0);
                if (Consts.Input.IsKeyPressed(Keys.VK_UP)) MoveLine(0, -2);
                if (Consts.Input.IsKeyPressed(Keys.VK_DOWN)) MoveLine(0, 2);

                if (Consts.Shapes.PointIsWithinRect_Rectangle(movLine.GetStartPoint(), staticRect1) || Consts.Shapes.PointIsWithinRect_Rectangle(movLine.GetEndPoint(), staticRect1))
                {
                    rectCol = Color.Red.ToArgb();
                }
                else
                {
                    rectCol = Color.White.ToArgb();
                }
                LineSegment [] temp =  Consts.Shapes.LinesFromRect(staticRect1);
                if (Consts.Shapes.LineIntersectsWithLines_Array(movLine, ref temp) && Consts.Shapes.LineIntersectsWithRect(movLine, staticRect1))
                {
                    Consts.Text.DrawText("The line intersects with the rectangel!", Color.White.ToArgb(), GameResources.GameFont("Courier"), 0, 0);
                }

                Consts.Graphics.DrawLine_Line(Color.White.ToArgb(), movLine);
                if (Consts.Shapes.IsPointOnLine(mp, movLine)) Consts.Graphics.DrawCircle_Point(Color.Green.ToArgb(), mp, 5);

                Consts.Graphics.DrawHorizontalLine(Color.Green.ToArgb(), Consts.Shapes.RectangleTop(staticRect1), 0, Consts.Core.ScreenWidth());
                Consts.Graphics.DrawHorizontalLine(Color.Green.ToArgb(), Consts.Shapes.RectangleBottom(staticRect1), 0, Consts.Core.ScreenWidth());
                Consts.Graphics.DrawVerticalLine(Color.Green.ToArgb(), Consts.Shapes.RectangleLeft(staticRect1), 0, Consts.Core.ScreenHeight());
                Consts.Graphics.DrawVerticalLine(Color.Green.ToArgb(), Consts.Shapes.RectangleRight(staticRect1), 0, Consts.Core.ScreenHeight());

                Consts.Graphics.DrawRectangle_Rectangle(rectCol, staticRect1);

                for (int i = 0; i < 3; i++)
                {
                    tempLine = Consts.Shapes.LinesFromRect(staticRect1)[i];
                    if (Consts.Shapes.GetLineIntersectionPoint(movLine, tempLine, out tempPoint))
                    {
                        points[i] = tempPoint;
                        if (Consts.Shapes.IsPointOnLine(tempPoint, movLine))
                        {
                            Consts.Graphics.DrawCircle_Point(Color.Green.ToArgb(), tempPoint, 5);
                        }
                        else
                        {
                            if (Consts.Shapes.IsPointOnLine(tempPoint, tempLine))
                            {
                                Consts.Graphics.DrawCircle_Point(Color.Green.ToArgb(), tempPoint, 5);
                            }
                            else
                            {
                                Consts.Graphics.DrawCircle_Point(Color.Red.ToArgb(), tempPoint, 5);
                            }
                        }
                        if (Consts.Shapes.IsPointOnLine(mp, tempLine)) Consts.Graphics.DrawCircle_Point(Color.Green.ToArgb(), mp, 5);
                    }
                }
                diag = Consts.Shapes.CreateLine(Consts.Shapes.RectangleLeft(staticRect1), Consts.Shapes.RectangleTop(staticRect1), Consts.Shapes.RectangleRight(staticRect1), Consts.Shapes.RectangleBottom(staticRect1));
                Consts.Graphics.DrawLine_Line(rectCol, diag);
                if (Consts.Shapes.IsPointOnLine(mp, diag))
                {
                    Consts.Graphics.DrawCircle_Point(Color.Green.ToArgb(), mp,5);
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_1)) ShowDistance(0);
                if (Consts.Input.IsKeyPressed(Keys.VK_2)) ShowDistance(1);
                if (Consts.Input.IsKeyPressed(Keys.VK_3)) ShowDistance(2);
                //if (Consts.Input.IsKeyPressed(Keys.VK_4)) ShowDistance(3);
            }

        }
/*		
 * 
		
		if IsKeyPressed(VK_1) then ShowDistance(0);
		if IsKeyPressed(VK_2) then ShowDistance(1);
		if IsKeyPressed(VK_3) then ShowDistance(2);
		if IsKeyPressed(VK_4) then ShowDistance(3);
	end;
 */

    }
}
