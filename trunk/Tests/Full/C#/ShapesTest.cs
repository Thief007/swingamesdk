using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing;
using SwinGame;

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

			private Rectangle staticRect1 = Shapes.CreateRectangle(100, 100, 100, 150);
			private Rectangle staticRect2 = Shapes.CreateRectangle(250, 100, 100, 150);
			private Rectangle movRect = Shapes.CreateRectangle(100, 300, 70, 60);
			private Sprite ball = SwinGame.Graphics.CreateSprite(GameResources.GameImage("BallImage1"));

            public TestShapes1() : base(METHS, INST) 
            { 
                ball.X = 250; 
                ball.Y = 270;
            }

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                Vector movVec = Physics.CreateVector(0, 0);
		
		        if (Input.IsKeyPressed(Keys.VK_LEFT)) movVec = Physics.AddVectors(movVec, Physics.CreateVector(-2, 0));
		        if (Input.IsKeyPressed(Keys.VK_RIGHT)) movVec = Physics.AddVectors(movVec, Physics.CreateVector(2, 0));
		        if (Input.IsKeyPressed(Keys.VK_UP)) movVec = Physics.AddVectors(movVec, Physics.CreateVector(0, -2));
		        if (Input.IsKeyPressed(Keys.VK_DOWN)) movVec = Physics.AddVectors(movVec, Physics.CreateVector(0, 	2));
        		
		        movRect = Shapes.RectangleAfterMove(movRect, movVec);
        		
		        SwinGame.Graphics.DrawSprite(ball);
		        SwinGame.Graphics.DrawCircle(Color.White, true, Shapes.CenterPoint(ball), 5);
        		
		        for (int i = 0; i < 4; i++)
		        {
			        SwinGame.Graphics.DrawLine(Color.White, Shapes.LinesFromRect(staticRect1)[i]);
			        SwinGame.Graphics.DrawPixel(Color.Black, Shapes.MidPoint(Shapes.LinesFromRect(staticRect1)[i]));
		        }
        		
		        if (Shapes.RectanglesIntersect(movRect, staticRect1) || Shapes.RectanglesIntersect(movRect, staticRect2))
		        {
			        Text.DrawText("The green rectangle is intersecting with a", Color.White, GameResources.GameFont("Courier"), 0, 0);
			        Text.DrawText("rectangle", Color.White, GameResources.GameFont("Courier"), 0, 15);
		        }
        		
		        SwinGame.Graphics.DrawRectangle(Color.Yellow, staticRect2);
		        SwinGame.Graphics.DrawRectangle(Color.Green, movRect);
        		
		        SwinGame.Graphics.DrawLine(Color.White, Shapes.ClosestPointOnLine(movRect.X, movRect.Y, Shapes.LinesFromRect(staticRect1)[2]).X, Shapes.ClosestPointOnLine(movRect.X, movRect.Y, Shapes.LinesFromRect(staticRect1)[2]).Y, movRect.X, movRect.Y);
		        SwinGame.Graphics.DrawLine(Color.White, Shapes.ClosestPointOnLine(Shapes.CreatePoint(movRect.X, movRect.Y), Shapes.LinesFromRect(staticRect1)[3]).X, Shapes.ClosestPointOnLine(movRect.X, movRect.Y, Shapes.LinesFromRect(staticRect1)[3]).Y, movRect.X, movRect.Y);
		        SwinGame.Graphics.DrawLine(Color.Red, Shapes.ClosestPointOnLine(movRect.X, movRect.Y, Shapes.LinesFromRect(staticRect1)[0]).X, Shapes.ClosestPointOnLine(movRect.X, movRect.Y, Shapes.LinesFromRect(staticRect1)[0]).Y, movRect.X, movRect.Y);
                Text.DrawText("The magnitude of the red line: " + Convert.ToString(Shapes.DistancePointToLine(Shapes.CreatePoint(movRect.X, movRect.Y), Shapes.LinesFromRect(staticRect1)[0])), Color.Red, GameResources.GameFont("Courier"), Shapes.CreatePoint(0, 30));
		        SwinGame.Graphics.DrawLine(Color.Blue, Shapes.ClosestPointOnLine(Shapes.CreatePoint(movRect.X, movRect.Y), Shapes.LinesFromRect(staticRect1)[1]).X, Shapes.ClosestPointOnLine(movRect.X, movRect.Y, Shapes.LinesFromRect(staticRect1)[1]).Y, movRect.X, movRect.Y);
		        Text.DrawText("The magnitude of the blue line: " + Convert.ToString(Shapes.DistancePointToLine(movRect.X, movRect.Y, Shapes.LinesFromRect(staticRect1)[1])), Color.Blue, GameResources.GameFont("Courier"), Shapes.CreatePoint(0, 45));
        	
            }
        }

        private class TestShapes2 : TestSet
        {
            private readonly static string METHS =
                "Shapes Test Part 2";

            private readonly static string INST =
                "Use the arrow keys to move" + Environment.NewLine + "the line";

            public TestShapes2()
                : base(METHS, INST)
            {
                staticRect1 = Shapes.CreateRectangle(100, 100, 100, 150);
                staticRect2 = Shapes.CreateRectangle(250, 100, 100, 150);
                movRect = Shapes.CreateRectangle(100, 300, 70, 60);
            }

            private LineSegment movLine = Shapes.CreateLine(200, 200, 250, 250);
            private Point2D tempPoint;
            private Point2D[] points;
            private int i;
            private LineSegment tempLine;
            private Color rectCol;

            private Rectangle staticRect1, staticRect2, movRect;

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                Array.Resize(ref points, 4);
		
		        if (Input.IsKeyPressed(Keys.VK_LEFT)) MoveLine(-2, 0);
		        if (Input.IsKeyPressed(Keys.VK_RIGHT)) MoveLine(2, 0);
		        if (Input.IsKeyPressed(Keys.VK_UP)) MoveLine(0, -2);
		        if (Input.IsKeyPressed(Keys.VK_DOWN)) MoveLine(0, 2);
        		
		        if (Shapes.PointIsWithinRect(movLine.StartPoint, staticRect1) || Shapes.PointIsWithinRect(movLine.EndPoint, staticRect1))
			    {
                    rectCol = Color.Red;
                }
		        else 
                {
                    rectCol = Color.White;
                }
        		
		        if (Shapes.LineIntersectsWithLines(movLine, Shapes.LinesFromRect(staticRect1)) && Shapes.LineIntersectsWithRect(movLine, staticRect1))
		        {
			        Text.DrawText("The line intersects with the rectangle!", Color.White, GameResources.GameFont("Courier"), 0, 0);
		        }
        		
		        SwinGame.Graphics.DrawLine(Color.White, movLine);
        		
		        SwinGame.Graphics.DrawHorizontalLine(Color.Green, Shapes.RectangleTop(staticRect1), 0, Core.ScreenWidth());
		        SwinGame.Graphics.DrawHorizontalLine(Color.Green, Shapes.RectangleBottom(staticRect1), 0, Core.ScreenWidth());
		        SwinGame.Graphics.DrawVerticalLine(Color.Green, Shapes.RectangleLeft(staticRect1), 0, Core.ScreenHeight());
		        SwinGame.Graphics.DrawVerticalLine(Color.Green, Shapes.RectangleRight(staticRect1), 0, Core.ScreenHeight());
        		
		        SwinGame.Graphics.DrawRectangle(rectCol, staticRect1);

		        for (int i = 0; i < 4; i++)
		        {
			        tempLine = Shapes.LinesFromRect(staticRect1)[i];
        			
			        if (Shapes.GetLineIntersectionPoint(movLine, tempLine, out tempPoint))
			        {
				        SwinGame.Graphics.DrawCircle(Color.Red, tempPoint, 5);
				        points[i] = tempPoint;
                    }
        			
			        if (Shapes.IsPointOnLine(movLine.StartPoint, tempLine) || Shapes.IsPointOnLine(movLine.EndPoint, tempLine))
			        {
				        SwinGame.Graphics.DrawLine(Color.Yellow, tempLine);
                    }
		        }
        		
		        //SwinGame.Graphics.DrawLine(Color.Red, Shapes.CreateLine(100, 100, 150, 150));
		        //if (Shapes.IsPointOnLine(Input.GetMousePosition(), Shapes.CreateLine(100, 100, 150, 150)))
		        //{
			    //    Text.DrawText("on the line", Color.White, GameResources.GameFont("Courier"), 100, 100);
		        //}
        		
		        if (Input.IsKeyPressed(Keys.VK_1)) ShowDistance(0);
                if (Input.IsKeyPressed(Keys.VK_2)) ShowDistance(1);
                if (Input.IsKeyPressed(Keys.VK_3)) ShowDistance(2);
                if (Input.IsKeyPressed(Keys.VK_4)) ShowDistance(3);
            }

            private void MoveLine(int xOffset, int yOffset)
		    {
			    movLine.StartPoint.X = movLine.StartPoint.X + xOffset;
			    movLine.StartPoint.Y = movLine.StartPoint.Y + yOffset;
			    movLine.EndPoint.X = movLine.EndPoint.X + xOffset;
			    movLine.EndPoint.Y = movLine.EndPoint.Y + yOffset;
            }
		
		    private void ShowDistance(int num)
		    {
			    SwinGame.Graphics.DrawLine(Color.Blue, points[num].X, points[num].Y, movLine.StartPoint.X, movLine.StartPoint.Y);
			    Text.DrawText("m = " + Convert.ToString(Shapes.DistanceBetween(points[num], movLine.StartPoint)), Color.White, GameResources.GameFont("Courier"), points[num].X, points[num].Y);
		    }
        }
    }
}
