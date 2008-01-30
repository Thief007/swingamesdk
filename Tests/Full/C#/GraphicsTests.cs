using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing;
using SwinGame;

namespace Tests
{
    class GraphicsTests : IGameTestLoader
    {
        #region IGameTestLoader Members

        public void AddTo(List<TestSuite> list)
        {
            TestSuite result = new TestSuite("Graphics Tests");
            result.Add(new BitmapTest());
            result.Add(new DrawPixelTest());
            result.Add(new DrawLineTest());
            result.Add(new TestDrawRectangle());
            result.Add(new TestDrawCircle());
            result.Add(new TestDrawEllipse());
            result.Add(new TestFill1());
            result.Add(new TestFill2());
            result.Add(new TestSprite());
            result.Add(new TestAddBitmap());
            list.Add(result);
        }

        #endregion

        private class BitmapTest : TestSet
        {
            private readonly static string METHS =
                "Bitmap Routines";

            private readonly static string INST =
                "Use the arrow keys to move" + Environment.NewLine +
                "The ship";

            public BitmapTest()
                : base(METHS, INST)
            {
                Array.Resize(ref frameB, 6);
                frameB[0] = GameResources.GameImage("Frame1");
                frameB[1] = GameResources.GameImage("Frame2");
                frameB[2] = GameResources.GameImage("Frame3");
                frameB[3] = GameResources.GameImage("Frame4");
                frameB[4] = GameResources.GameImage("Frame5");
                frameB[5] = GameResources.GameImage("Frame6");
            }

            private SwinGame.Bitmap[] frameB;
            private SwinGame.Bitmap ship = GameResources.GameImage("enShip");
            private Point2D framePos = Shapes.CreatePoint(0, 0);
            private SwinGame.Bitmap smallScreen = SwinGame.Graphics.CreateBitmap(418, 418);

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                if (Input.IsKeyPressed(Keys.VK_RIGHT)) framePos.X = framePos.X - 2;
                if (Input.IsKeyPressed(Keys.VK_LEFT)) framePos.X = framePos.X + 2;
                if (Input.IsKeyPressed(Keys.VK_UP)) framePos.Y = framePos.Y + 2;
                if (Input.IsKeyPressed(Keys.VK_DOWN)) framePos.Y = framePos.Y - 2;

                SwinGame.Graphics.ClearSurface(smallScreen, Color.Black);

                SwinGame.Graphics.DrawBitmap(smallScreen, frameB[0], Shapes.CreatePoint(0,0));
                SwinGame.Graphics.DrawBitmap(smallScreen, frameB[2], 278, 0);

                SwinGame.Graphics.DrawBitmapPart(smallScreen, ship, Shapes.CreateRectangle(framePos.X + 29, framePos.Y + 50, 100, 100), Shapes.CreatePoint(29, 50));
                SwinGame.Graphics.DrawBitmapPart(smallScreen, ship, Shapes.CreateRectangle(framePos.X + 149, framePos.Y + 50, 120, 100), 149, 50);
                SwinGame.Graphics.DrawBitmapPart(smallScreen, ship, (int)Math.Round(framePos.X + 289), (int)Math.Round(framePos.Y + 50), 100, 100, 289, 50);
		        
		        SwinGame.Graphics.DrawBitmap(smallScreen, 0, 0);

                SwinGame.Graphics.DrawBitmap(frameB[1], Shapes.CreatePoint(139, 0));
		        SwinGame.Graphics.DrawBitmap(frameB[3], 0, 209);

                SwinGame.Graphics.DrawBitmapOnScreen(frameB[4], Shapes.CreatePoint(139 + toDrawIn.X, 209 + toDrawIn.Y));
                SwinGame.Graphics.DrawBitmapOnScreen(frameB[5], (int)Math.Round((double)(278 + toDrawIn.X)), (int)Math.Round((double)(209 + toDrawIn.Y)));
		
		        SwinGame.Graphics.DrawBitmapPart(ship, Shapes.CreateRectangle(framePos.X + 29, framePos.Y + 159, 100, 100), 29, 159);
		        SwinGame.Graphics.DrawBitmapPart(ship, Shapes.CreateRectangle(framePos.X + 149, framePos.Y + 159, 120, 100), Shapes.CreatePoint(149, 159));
                SwinGame.Graphics.DrawBitmapPart(ship, (int)Math.Round(framePos.X + 289), (int)Math.Round(framePos.Y + 159), 100, 100, 289, 159);

                SwinGame.Graphics.DrawBitmapPartOnScreen(ship, Shapes.CreateRectangle(framePos.X + 29, framePos.Y + 268, 100, 100), (int)Math.Round((double)(29 + toDrawIn.X)), (int)Math.Round((double)(268 + toDrawIn.Y)));
                SwinGame.Graphics.DrawBitmapPartOnScreen(ship, Shapes.CreateRectangle(framePos.X + 149, framePos.Y + 268, 120, 100), Shapes.CreatePoint(149 + toDrawIn.X, 268 + toDrawIn.Y));
                SwinGame.Graphics.DrawBitmapPartOnScreen(ship, (int)Math.Round(framePos.X + 289), (int)Math.Round(framePos.Y + 268), 100, 100, (int)Math.Round((double)(289 + toDrawIn.X)), (int)Math.Round((double)(268 + toDrawIn.Y)));
            }
        }

        private class DrawPixelTest : TestSet
        {
            private readonly static string METHS =
                "DrawPixel";

            private readonly static string INST =
                "Click and drag to draw with" + Environment.NewLine +
                "your mouse cursor." + Environment.NewLine +
                "1: Increment Blue" + Environment.NewLine +
                "2: Increment Green" + Environment.NewLine +
                "3: Inrrement Blue";

            public DrawPixelTest()
                : base(METHS, INST)
            {
                base.ClearScreen = false;
            }

            private Color _curCol = Color.Transparent;
            private SwinGame.Bitmap smallScreen = SwinGame.Graphics.CreateBitmap(418, 418);
            private Boolean started = false;

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                if (!started)
                {
                    started = !started;
                    SwinGame.Graphics.ClearScreen();
                }

                if (Input.IsKeyPressed(Keys.VK_Z))
                {
                    SwinGame.Graphics.ClearSurface(smallScreen, Color.Transparent);
                    SwinGame.Graphics.ClearScreen(Color.Transparent);
                    SwinGame.Graphics.DrawBitmapOnScreen(GameResources.GameImage("BGA"), (int)(toDrawIn.X), (int)(toDrawIn.Y));
                }

                if (Input.IsKeyPressed(Keys.VK_1)) _curCol = Core.GetColor((byte)(_curCol.R + 5), _curCol.G, _curCol.B);
                if (Input.IsKeyPressed(Keys.VK_2)) _curCol = Core.GetColor(_curCol.R, (byte)(_curCol.G + 5), _curCol.B);
                if (Input.IsKeyPressed(Keys.VK_3)) _curCol = Core.GetColor(_curCol.R, _curCol.G, (byte)(_curCol.B + 5));

                SwinGame.Graphics.FillRectangle(Color.Black, 0, 0, 300, 45);

                Text.DrawText("Red   : " + Convert.ToString(_curCol.R), Color.Red, GameResources.GameFont("Courier"), 0, 0);
		        Text.DrawText("Green : " + Convert.ToString(_curCol.G), Color.Green, GameResources.GameFont("Courier"), 0, 15);
		        Text.DrawText("Blue  : " + Convert.ToString(_curCol.B), Color.Blue, GameResources.GameFont("Courier"), 0, 30);
		
                if (Input.IsMouseDown(MouseButton.LeftButton))
                {
                    SwinGame.Graphics.DrawPixel(smallScreen, _curCol, Shapes.CreatePoint(Input.GetMousePosition().X - toDrawIn.X - 10, Input.GetMousePosition().Y - toDrawIn.Y - 10)); 
                    SwinGame.Graphics.DrawPixel(smallScreen, _curCol, (int)(Input.GetMousePosition().X - toDrawIn.X - 10), (int)(Input.GetMousePosition().Y- toDrawIn.Y - 10));   
                
                    SwinGame.Graphics.DrawPixel(_curCol, Shapes.CreatePoint(Input.GetMousePosition().X - 10 - toDrawIn.X, Input.GetMousePosition().Y - toDrawIn.Y));
                    SwinGame.Graphics.DrawPixel(_curCol, (int)(Input.GetMousePosition().X - toDrawIn.X + 10), (int)(Input.GetMousePosition().Y - toDrawIn.Y));

                    SwinGame.Graphics.DrawPixelOnScreen(_curCol, Shapes.CreatePoint(Input.GetMousePosition().X - 10, Input.GetMousePosition().Y + 10));
                    SwinGame.Graphics.DrawPixelOnScreen(_curCol, (int)(Input.GetMousePosition().X + 10), (int)(Input.GetMousePosition().Y + 10));
                }

		        SwinGame.Graphics.DrawBitmap(smallScreen, 0, 0);
            }
        }

        private class DrawLineTest : TestSet
        {
            private readonly static string METHS =
                "All routines related to line, RotateMatrix, Multiply";

            private readonly static string INST =
                "Use the arrow keys to rotate" + Environment.NewLine +
                "the white lines";

            private LineSegment[] tempLines;
            private SwinGame.Bitmap smallScreen = SwinGame.Graphics.CreateBitmap(418, 418);
            private int angle;

            public DrawLineTest()
                : base(METHS, INST)
            {
                Array.Resize(ref tempLines, 7);
                tempLines[1] = Shapes.LineFromVector(225, 225, Physics.GetVectorFromAngle(0, 125));
			    tempLines[2] = Shapes.LineFromVector(225, 225, Physics.GetVectorFromAngle(60, 125));
			    tempLines[3] = Shapes.LineFromVector(225, 225, Physics.GetVectorFromAngle(120, 125));
			    tempLines[4] = Shapes.LineFromVector(225, 225, Physics.GetVectorFromAngle(180, 125));
			    tempLines[5] = Shapes.LineFromVector(248, 354, Physics.GetVectorFromAngle(240, 125));
			    tempLines[6] = Shapes.LineFromVector(248, 354, Physics.GetVectorFromAngle(300, 125));
            }

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                SwinGame.Graphics.ClearSurface(smallScreen, Color.Transparent);
		
                if (Input.IsKeyPressed(Keys.VK_RIGHT)) angle = angle + 2;
                if (Input.IsKeyPressed(Keys.VK_LEFT)) angle = angle - 2;

		
		        if (angle != 0)
		        {
			        for (int i = 1; i < 7; i++)
			        {
				        tempLines[i] = Shapes.LineFromVector(tempLines[i].StartPoint.X, tempLines[i].StartPoint.Y, Physics.Multiply(Physics.RotationMatrix(angle), Physics.LineAsVector(tempLines[i])));
                    }
		        }
		
		        SwinGame.Graphics.DrawHorizontalLine(smallScreen, Color.Green, 100, 100, 350);
		        SwinGame.Graphics.DrawVerticalLine(smallScreen, Color.Green, 100, 100, 350);
		        SwinGame.Graphics.DrawBitmap(smallScreen, 0, 0);
		        SwinGame.Graphics.ClearSurface(smallScreen, Color.Transparent);
		        SwinGame.Graphics.DrawLine(smallScreen, Color.White, tempLines[1]);
		        SwinGame.Graphics.DrawLine(smallScreen, Color.White, (int)(tempLines[3].StartPoint.X), (int)(tempLines[3].StartPoint.Y), (int)(tempLines[3].EndPoint.X), (int)(tempLines[3].EndPoint.Y));
		
		        SwinGame.Graphics.DrawHorizontalLine(Color.Green, 225, 100, 350);
		        SwinGame.Graphics.DrawVerticalLine(Color.Green, 225, 100, 350);
                SwinGame.Graphics.DrawHorizontalLineOnScreen(Color.Green, (int)(350 + toDrawIn.Y), (int)(100 + toDrawIn.X), (int)(350 + toDrawIn.X));
                SwinGame.Graphics.DrawVerticalLineOnScreen(Color.Green, (int)(350 + toDrawIn.X), (int)(100 + toDrawIn.Y), (int)(350 + toDrawIn.Y));
		
		        SwinGame.Graphics.DrawLine(Color.White, tempLines[2]);
		        SwinGame.Graphics.DrawLine(Color.White, tempLines[4].StartPoint.X, tempLines[4].StartPoint.Y, tempLines[4].EndPoint.X, tempLines[4].EndPoint.Y);
		
		        SwinGame.Graphics.DrawBitmap(smallScreen, 0, 0);
		
		        SwinGame.Graphics.DrawLineOnScreen(Color.White, tempLines[5]);
		        SwinGame.Graphics.DrawLineOnScreen(Color.White, (int)(tempLines[6].StartPoint.X), (int)(tempLines[6].StartPoint.Y), (int)(tempLines[6].EndPoint.X), (int)(tempLines[6].EndPoint.Y));
		
                for (int i = 1; i < 5; i++)
		        {
			        SwinGame.Graphics.DrawLine(Color.White, Shapes.LineFromVector(tempLines[i].EndPoint, Physics.MultiplyVector(Physics.LineNormal(tempLines[i]), 10)));
		        }

                for (int i = 5; i < 7; i++)
                {
                    SwinGame.Graphics.DrawLineOnScreen(Color.White, Shapes.LineFromVector(tempLines[i].EndPoint, Physics.MultiplyVector(Physics.VectorNormal(Physics.LineAsVector(tempLines[i])), 10)));
                }

		        angle = 0;
            }
        }

        private class TestDrawRectangle : TestSet
        {
            private readonly static string METHS =
                "DrawRectangle, DrawRectangleOnScreen, ClearSurface, DrawBitmap";

            private readonly static string INST =
                "Use the arrow keys to move" + Environment.NewLine +
                "the green box."  + Environment.NewLine +
                "A : Shrink the box width" + Environment.NewLine +
                "S : Expand the box width" + Environment.NewLine +
                "Z : Shrink the box height" + Environment.NewLine +
                "X : Expand the box height" + Environment.NewLine +
                "T : Toggle fill" + Environment.NewLine +
                "White : Normal" + Environment.NewLine +
                "Green : On destination bitmap" + Environment.NewLine +
                "Yello : On screen";

            private SwinGame.Bitmap smallScreen = SwinGame.Graphics.CreateBitmap(418, 418);
            private Rectangle tempRect = Shapes.CreateRectangle(0, 0, 50, 50);
            private Rectangle tempRect4 = Shapes.CreateRectangle(0, 0, 50, 50);
            private bool filled = false;

            private Rectangle tempRect2, tempRect3, tempRect5, tempRect6;

            public TestDrawRectangle()
                : base(METHS, INST)
            {
            }

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                SwinGame.Graphics.ClearSurface(smallScreen, Color.Transparent);
		
		        if (Input.IsKeyPressed(Keys.VK_RIGHT)) tempRect.X = tempRect.X + 2;
                if (Input.IsKeyPressed(Keys.VK_LEFT)) tempRect.X = tempRect.X - 2;
                if (Input.IsKeyPressed(Keys.VK_UP)) tempRect.Y = tempRect.Y - 2;
                if (Input.IsKeyPressed(Keys.VK_DOWN)) tempRect.Y = tempRect.Y + 2;
                if (Input.IsKeyPressed(Keys.VK_A)) tempRect.Width = tempRect.Width - 2;
                if (Input.IsKeyPressed(Keys.VK_S)) tempRect.Width = tempRect.Width + 2;
                if (Input.IsKeyPressed(Keys.VK_Z)) tempRect.Height = tempRect.Height - 2;
                if (Input.IsKeyPressed(Keys.VK_X)) tempRect.Height = tempRect.Height + 2;
                if (Input.WasKeyTyped(Keys.VK_T)) filled = filled == false;
		
		        if (tempRect.Width < -100) tempRect.Width = -100;
		        if (tempRect.Height < -100) tempRect.Height = -100;
		        if (tempRect.Width > 100) tempRect.Width = 100;
		        if (tempRect.Height > 100) tempRect.Height = 100;
		
		        SwinGame.Graphics.DrawRectangle(smallScreen, Color.Green, filled, tempRect);
		        SwinGame.Graphics.DrawRectangle(smallScreen, Color.Green, filled, (int)(tempRect.Width + tempRect.X), (int)(tempRect.Y + tempRect.Height), tempRect.Width, tempRect.Height);
		        SwinGame.Graphics.DrawRectangle(smallScreen, Color.Green, (int)(tempRect.X), (int)(tempRect.Y + tempRect.Height), tempRect.Width, tempRect.Height);
		        tempRect4.X = tempRect.X + tempRect.Width;
		        tempRect4.Y = tempRect.Y;
		        tempRect4.Width = tempRect.Width;
		        tempRect4.Height = tempRect.Height;
		        SwinGame.Graphics.DrawRectangle(smallScreen, Color.Green, tempRect4);
		        SwinGame.Graphics.DrawBitmap(smallScreen, 0, 0);
        		
		        SwinGame.Graphics.DrawRectangle(Color.White, filled, 0, 0, tempRect.Width, tempRect.Height);
		        tempRect2.X = tempRect.Width;
		        tempRect2.Y = tempRect.Height;
		        tempRect2.Width = tempRect.Width;
		        tempRect2.Height = tempRect.Height;
		        SwinGame.Graphics.DrawRectangle(Color.White, filled, tempRect2);
		        SwinGame.Graphics.DrawRectangle(Color.White, 0, tempRect.Height, tempRect.Width, tempRect.Height);
		        tempRect3.X = tempRect.Width;
		        tempRect3.Width = tempRect.Width;
		        tempRect3.Height = tempRect.Height;
		        SwinGame.Graphics.DrawRectangle(Color.White, tempRect3);

                SwinGame.Graphics.DrawRectangleOnScreen(Color.Yellow, filled, (int)(toDrawIn.X), (int)(Shapes.RectangleBottom(toDrawIn) - tempRect.Height) - 1, tempRect.Width, tempRect.Height);
		        tempRect5.X = tempRect.Width + toDrawIn.X;
		        tempRect5.Y = (int)(Shapes.RectangleBottom(toDrawIn) - tempRect.Height * 2 - 1);
		        tempRect5.Width = tempRect.Width;
		        tempRect5.Height = tempRect.Height;
                SwinGame.Graphics.DrawRectangleOnScreen(Color.Yellow, filled, tempRect5);
                SwinGame.Graphics.DrawRectangleOnScreen(Color.Yellow, (int)(toDrawIn.X), (int)(Shapes.RectangleBottom(toDrawIn) - tempRect.Height * 2) - 1, tempRect.Width, tempRect.Height);
		        tempRect6.X = tempRect.Width + toDrawIn.X;
		        tempRect6.Y = (int)(Shapes.RectangleBottom(toDrawIn) - tempRect.Height - 1);
		        tempRect6.Width = tempRect.Width;
		        tempRect6.Height = tempRect.Height;
                SwinGame.Graphics.DrawRectangleOnScreen(Color.Yellow, tempRect6);
            }
        }

        private class TestDrawCircle : TestSet
        {
            private readonly static string METHS =
                "DrawCircle, DrawCircleOnScreen";

            private readonly static string INST =
                "Use the arrow keys to move" + Environment.NewLine +
                "the green circles." + Environment.NewLine +
                "Z : Shrink the circle radius" + Environment.NewLine +
                "X : Expand the circle radius" + Environment.NewLine +
                "T : Toggle fill" + Environment.NewLine +
                "White : Normal" + Environment.NewLine +
                "Green : On destination bitmap" + Environment.NewLine +
                "Yellow: On screen";
               
            private SwinGame.Bitmap smallScreen = SwinGame.Graphics.CreateBitmap(418, 418);
            private Point2D tempPoint = Shapes.CreatePoint(15,15);
            private Point2D tempPoint2, tempPoint3, tempPoint4, tempPoint5, tempPoint6;
            private int curRadius = 15;
            private bool filled = false;

            public TestDrawCircle()
                : base(METHS, INST)
            {
            }

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                SwinGame.Graphics.ClearSurface(smallScreen, Color.Transparent);

		        if (Input.IsKeyPressed(Keys.VK_RIGHT)) tempPoint.X = tempPoint.X + 2;
                if (Input.IsKeyPressed(Keys.VK_LEFT)) tempPoint.X = tempPoint.X - 2;
                if (Input.IsKeyPressed(Keys.VK_UP)) tempPoint.Y = tempPoint.Y - 2;
                if (Input.IsKeyPressed(Keys.VK_DOWN)) tempPoint.Y = tempPoint.Y + 2;
                if (Input.IsKeyPressed(Keys.VK_Z)) curRadius = curRadius - 1;
                if (Input.IsKeyPressed(Keys.VK_X)) curRadius = curRadius + 1;
                if (Input.WasKeyTyped(Keys.VK_T)) filled = filled == false;
        		
		        if (curRadius < 0) curRadius = 0;
		        if (curRadius > 50) curRadius = 50;
        		
		        SwinGame.Graphics.DrawCircle(smallScreen, Color.Green, filled, tempPoint, curRadius);
		        SwinGame.Graphics.DrawCircle(smallScreen, Color.Green, filled, (int)(tempPoint.X + curRadius * 2), (int)(tempPoint.Y + curRadius * 2), curRadius);
		        SwinGame.Graphics.DrawCircle(smallScreen, Color.Green, (int)(tempPoint.X), (int)(tempPoint.Y + curRadius * 2), curRadius);
		        tempPoint2.X = tempPoint.X + curRadius * 2;
		        tempPoint2.Y = tempPoint.Y;
		        SwinGame.Graphics.DrawCircle(smallScreen, Color.Green, tempPoint2, curRadius);
		        SwinGame.Graphics.DrawBitmap(smallScreen, 0, 0);
        		
		        SwinGame.Graphics.DrawCircle(Color.White, filled, curRadius, curRadius, curRadius);
		        tempPoint3.X = curRadius * 3;
		        tempPoint3.Y = curRadius * 3;
                SwinGame.Graphics.DrawCircle(Color.White, filled, tempPoint3, curRadius);
                SwinGame.Graphics.DrawCircle(Color.White, curRadius, curRadius * 3, curRadius);
		        tempPoint4.X = curRadius * 3;
		        tempPoint4.Y = curRadius;
                SwinGame.Graphics.DrawCircle(Color.White, tempPoint4, curRadius);

                SwinGame.Graphics.DrawCircleOnScreen(Color.Yellow, filled, (int)(curRadius + toDrawIn.X), (int)(Shapes.RectangleBottom(toDrawIn) - curRadius) - 1, curRadius);
		        tempPoint5.X = curRadius * 3 + toDrawIn.X;
		        tempPoint5.Y = Shapes.RectangleBottom(toDrawIn) - curRadius * 3 - 1;
                SwinGame.Graphics.DrawCircleOnScreen(Color.Yellow, filled, tempPoint5, curRadius);
                SwinGame.Graphics.DrawCircleOnScreen(Color.Yellow, (int)(curRadius + toDrawIn.X), (int)(Shapes.RectangleBottom(toDrawIn) - curRadius * 3) - 1, curRadius);
		        tempPoint6.X = curRadius * 3 + toDrawIn.X;
		        tempPoint6.Y = Shapes.RectangleBottom(toDrawIn) - curRadius - 1;
                SwinGame.Graphics.DrawCircleOnScreen(Color.Yellow, tempPoint6, curRadius);
            }
        }

        private class TestDrawEllipse : TestSet
        {
            private readonly static string METHS =
                "DrawEllipse, DrawEllipseOnScreen";

            private readonly static string INST =
                "Use the arrow keys to move" + Environment.NewLine +
                "the green ellipses." + Environment.NewLine +
                "A : Shrink the ellipse width" + Environment.NewLine +
                "S : Expand the ellipse width" + Environment.NewLine +
                "Z : Shrink the ellipse height" + Environment.NewLine +
                "X : Expand the ellipse height" + Environment.NewLine +
                "T : Toggle fill" + Environment.NewLine +
                "White : Normal" + Environment.NewLine +
                "Green : On destination bitmap"  + Environment.NewLine +
                "Yellow: On screen";

            private SwinGame.Bitmap smallScreen = SwinGame.Graphics.CreateBitmap(418, 418);
            private bool filled = false;
            private Rectangle tempRect2, tempRect3, tempRect5, tempRect6;

            private Rectangle tempRect = Shapes.CreateRectangle(0, 0, 50, 50);
			private Rectangle tempRect4 = Shapes.CreateRectangle(0, 0, 50, 50);

            public TestDrawEllipse()
                : base(METHS, INST)
            {
            }

            protected override void ToRun(System.Drawing.Rectangle drawIn)
            {
                SwinGame.Graphics.ClearSurface(smallScreen, Color.Transparent);

		        if (Input.IsKeyPressed(Keys.VK_RIGHT)) tempRect.X = tempRect.X + 2;
                if (Input.IsKeyPressed(Keys.VK_LEFT)) tempRect.X = tempRect.X - 2;
                if (Input.IsKeyPressed(Keys.VK_UP)) tempRect.Y = tempRect.Y - 2;
                if (Input.IsKeyPressed(Keys.VK_DOWN)) tempRect.Y = tempRect.Y + 2;
                if (Input.IsKeyPressed(Keys.VK_A)) tempRect.Width = tempRect.Width - 2;
                if (Input.IsKeyPressed(Keys.VK_S)) tempRect.Width = tempRect.Width + 2;
                if (Input.IsKeyPressed(Keys.VK_Z)) tempRect.Height = tempRect.Height - 2;
                if (Input.IsKeyPressed(Keys.VK_X)) tempRect.Height = tempRect.Height + 2;
                if (Input.WasKeyTyped(Keys.VK_T)) filled = filled == false;

		        if (tempRect.Width < 0) tempRect.Width = 0;
		        if (tempRect.Height < 0) tempRect.Height = 0;
		        if (tempRect.Width > 100) tempRect.Width = 100;
		        if (tempRect.Height > 100) tempRect.Height = 100;

		        SwinGame.Graphics.DrawEllipse(smallScreen, Color.Green, filled, tempRect);
                SwinGame.Graphics.DrawEllipse(smallScreen, Color.Green, filled, (int)(tempRect.X + tempRect.Width), (int)(tempRect.Y + tempRect.Height), tempRect.Width, tempRect.Height);
                SwinGame.Graphics.DrawEllipse(smallScreen, Color.Green, (int)(tempRect.X), (int)(tempRect.Y + tempRect.Height), tempRect.Width, tempRect.Height);
		        tempRect4.X = tempRect.X + tempRect.Width;
		        tempRect4.Y = tempRect.Y;
		        tempRect4.Width = tempRect.Width;
		        tempRect4.Height = tempRect.Height;
                SwinGame.Graphics.DrawEllipse(smallScreen, Color.Green, tempRect4);
                SwinGame.Graphics.DrawBitmap(smallScreen, 0, 0);

                SwinGame.Graphics.DrawEllipse(Color.White, filled, 0, 0, tempRect.Width, tempRect.Height);
		        tempRect2.X = tempRect.Width;
		        tempRect2.Y = tempRect.Height;
		        tempRect2.Width = tempRect.Width;
		        tempRect2.Height = tempRect.Height;
                SwinGame.Graphics.DrawEllipse(Color.White, filled, tempRect2);
                SwinGame.Graphics.DrawEllipse(Color.White, 0, tempRect.Height, tempRect.Width, tempRect.Height);
		        tempRect3.X = tempRect.Width;
		        tempRect3.Width = tempRect.Width;
		        tempRect3.Height = tempRect.Height;
                SwinGame.Graphics.DrawEllipse(Color.White, tempRect3);

                SwinGame.Graphics.DrawEllipseOnScreen(Color.Yellow, filled, (int)(drawIn.X), (int)(Shapes.RectangleBottom(drawIn) - tempRect.Height), tempRect.Width, tempRect.Height);
		        tempRect5.X = tempRect.Width + drawIn.X;
		        tempRect5.Y = (int)(Shapes.RectangleBottom(drawIn) - tempRect.Height * 2);
		        tempRect5.Width = tempRect.Width;
		        tempRect5.Height = tempRect.Height;
                SwinGame.Graphics.DrawEllipseOnScreen(Color.Yellow, filled, tempRect5);
                SwinGame.Graphics.DrawEllipseOnScreen(Color.Yellow, (int)(drawIn.X), (int)(Shapes.RectangleBottom(drawIn) - tempRect.Height * 2), tempRect.Width, tempRect.Height);
		        tempRect6.X = tempRect.Width + drawIn.X;
		        tempRect6.Y = (int)(Shapes.RectangleBottom(drawIn) - tempRect.Height);
		        tempRect6.Width = tempRect.Width;
		        tempRect6.Height = tempRect.Height;
                SwinGame.Graphics.DrawEllipseOnScreen(Color.Yellow, tempRect6);
            }
        }

        private class TestFill1 : TestSet
        {
            private readonly static string METHS =
                "FillRectangle, FillEllipse";

            private readonly static string INST =
                "Use the arrow keys to move" + Environment.NewLine +
                "the green shapes." + Environment.NewLine +
                "A : Shrink the shape width" + Environment.NewLine +
                "S : Expand the shape width" + Environment.NewLine +
                "Z : Shrink the shape height" + Environment.NewLine +
                "X : Expand the shape height" + Environment.NewLine +
                "White : Normal" + Environment.NewLine +
                "Green : On destination bitmap" + Environment.NewLine +
                "Yellow: On screen";

            private SwinGame.Bitmap smallScreen = SwinGame.Graphics.CreateBitmap(418, 418);
            private Rectangle tempRect2, tempRect3, tempRect5, tempRect6;

            private Rectangle tempRect = Shapes.CreateRectangle(0, 0, 50, 50);
            private Rectangle tempRect4 = Shapes.CreateRectangle(0, 0, 50, 50);

            public TestFill1()
                : base(METHS, INST)
            {
            }

            protected override void ToRun(System.Drawing.Rectangle drawIn)
            {
                SwinGame.Graphics.ClearSurface(smallScreen, Color.Transparent);

		        if (Input.IsKeyPressed(Keys.VK_RIGHT)) tempRect.X = tempRect.X + 2;
		        if (Input.IsKeyPressed(Keys.VK_LEFT))tempRect.X = tempRect.X - 2;
		        if (Input.IsKeyPressed(Keys.VK_UP)) tempRect.Y = tempRect.Y - 2;
		        if (Input.IsKeyPressed(Keys.VK_DOWN)) tempRect.Y = tempRect.Y + 2;
		        if (Input.IsKeyPressed(Keys.VK_A)) tempRect.Width = tempRect.Width - 2;
		        if (Input.IsKeyPressed(Keys.VK_S)) tempRect.Width = tempRect.Width + 2;
		        if (Input.IsKeyPressed(Keys.VK_Z)) tempRect.Height = tempRect.Height - 2;
		        if (Input.IsKeyPressed(Keys.VK_X)) tempRect.Height = tempRect.Height + 2;
        		
		        if (tempRect.Width < 0) tempRect.Width = 0;
		        if (tempRect.Height < 0) tempRect.Height = 0;
		        if (tempRect.Width > 100) tempRect.Width = 100;
		        if (tempRect.Height > 100) tempRect.Height = 100;
        		
		        SwinGame.Graphics.FillRectangle(smallScreen, Color.Green, tempRect);
                SwinGame.Graphics.FillRectangle(smallScreen, Color.Green, (int)(tempRect.Width + tempRect.X), (int)(tempRect.Y + tempRect.Height), tempRect.Width, tempRect.Height);
                SwinGame.Graphics.FillEllipse(smallScreen, Color.Green, (int)(tempRect.X), (int)(tempRect.Y + tempRect.Height), tempRect.Width, tempRect.Height);
		        tempRect4.X = tempRect.X + tempRect.Width;
		        tempRect4.Y = tempRect.Y;
		        tempRect4.Width = tempRect.Width;
		        tempRect4.Height = tempRect.Height;
                SwinGame.Graphics.FillEllipse(smallScreen, Color.Green, tempRect4);
		        SwinGame.Graphics.DrawBitmap(smallScreen, 0, 0);

                SwinGame.Graphics.FillRectangle(Color.White, 0, 0, tempRect.Width, tempRect.Height);
		        tempRect2.X = tempRect.Width;
		        tempRect2.Y = tempRect.Height;
		        tempRect2.Width = tempRect.Width;
		        tempRect2.Height = tempRect.Height;
                SwinGame.Graphics.FillRectangle(Color.White, tempRect2);
                SwinGame.Graphics.FillEllipse(Color.White, 0, tempRect.Height, tempRect.Width, tempRect.Height);
		        tempRect3.X = tempRect.Width;
		        tempRect3.Width = tempRect.Width;
		        tempRect3.Height = tempRect.Height;
                SwinGame.Graphics.FillEllipse(Color.White, tempRect3);

                SwinGame.Graphics.FillRectangleOnScreen(Color.Yellow, (int)(drawIn.X), (int)(Shapes.RectangleBottom(drawIn) - tempRect.Height) - 1, tempRect.Width, tempRect.Height);
		        tempRect5.X = tempRect.Width + drawIn.X;
		        tempRect5.Y = (int)(Shapes.RectangleBottom(drawIn) - tempRect.Height * 2 - 1);
		        tempRect5.Width = tempRect.Width;
		        tempRect5.Height = tempRect.Height;
		        SwinGame.Graphics.FillRectangleOnScreen(Color.Yellow, tempRect5);

                SwinGame.Graphics.FillEllipseOnScreen(Color.Yellow, (int)(drawIn.X), (int)(Shapes.RectangleBottom(drawIn) - tempRect.Height * 2) - 1, tempRect.Width, tempRect.Height);
		        tempRect6.X = tempRect.Width + drawIn.X;
		        tempRect6.Y = (int)(Shapes.RectangleBottom(drawIn) - tempRect.Height - 1);
		        tempRect6.Width = tempRect.Width;
		        tempRect6.Height = tempRect.Height;
                SwinGame.Graphics.FillEllipseOnScreen(Color.Yellow, tempRect6);
            }
        }

        private class TestFill2 : TestSet
        {
            private readonly static string METHS =
                "FillCircle, FillCircleOnScreen";

            private readonly static string INST =
                "Use the arrow keys to move" + Environment.NewLine +
                "the green circles." + Environment.NewLine +
                "Z : Shrink the circle height" + Environment.NewLine +
                "X : Expand the circle height" + Environment.NewLine +
                "White : Normal" + Environment.NewLine +
                "Green : On destination bitmap" + Environment.NewLine +
                "Yellow: On screen";

            private SwinGame.Bitmap smallScreen = SwinGame.Graphics.CreateBitmap(418, 418);
            private Point2D tempPoint = Shapes.CreatePoint(15, 15);
			private int curRadius = 15;

            private Point2D tempPoint3, tempPoint5;

            public TestFill2()
                : base(METHS, INST)
            {
            }

            protected override void ToRun(System.Drawing.Rectangle drawIn)
            {
                SwinGame.Graphics.ClearSurface(smallScreen, Color.Transparent);
		
		        if (Input.IsKeyPressed(Keys.VK_RIGHT)) tempPoint.X = tempPoint.X + 2;
		        if (Input.IsKeyPressed(Keys.VK_LEFT)) tempPoint.X = tempPoint.X - 2;
		        if (Input.IsKeyPressed(Keys.VK_UP)) tempPoint.Y = tempPoint.Y - 2;
		        if (Input.IsKeyPressed(Keys.VK_DOWN)) tempPoint.Y = tempPoint.Y + 2;
		        if (Input.IsKeyPressed(Keys.VK_Z)) curRadius = curRadius - 1;
		        if (Input.IsKeyPressed(Keys.VK_X)) curRadius = curRadius + 1;
        		
		        if (curRadius < 0) curRadius = 0;
		        if (curRadius > 50) curRadius = 50;

                SwinGame.Graphics.FillCircle(smallScreen, Color.Green, tempPoint, curRadius);
                SwinGame.Graphics.FillCircle(smallScreen, Color.Green, (int)(tempPoint.X + curRadius * 2), (int)(tempPoint.Y + curRadius * 2), curRadius);
                SwinGame.Graphics.DrawBitmap(smallScreen, 0, 0);

                SwinGame.Graphics.FillCircle(Color.White, curRadius, curRadius, curRadius);
		        tempPoint3.X = curRadius * 3;
		        tempPoint3.Y = curRadius * 3;
                SwinGame.Graphics.FillCircle(Color.White, tempPoint3, curRadius);

                SwinGame.Graphics.FillCircleOnScreen(Color.Yellow, (int)(curRadius + drawIn.X), (int)(Shapes.RectangleBottom(drawIn) - curRadius) - 1, curRadius);
		        tempPoint5.X = curRadius * 3 + drawIn.X;
		        tempPoint5.Y = Shapes.RectangleBottom(drawIn) - curRadius * 3 - 1;
                SwinGame.Graphics.FillCircleOnScreen(Color.Yellow, tempPoint5, curRadius);
            }
        }

        private class TestSprite : TestSet
        {
            private readonly static string METHS =
                "Sprite routines";

            private readonly static string INST =
                "Use the number keys from" + Environment.NewLine +
                "1 to 7 to change the sprite." + Environment.NewLine +
                "Use the arrow keys to" + Environment.NewLine +
                "move the sprite." + Environment.NewLine +
                "Use the M key to reset the" + Environment.NewLine +
                "position of the sprite." + Environment.NewLine +
                "Press Q to change the animation" + Environment.NewLine +
                "to Loop." + Environment.NewLine +
                "Press W to change the animation" + Environment.NewLine +
                "to ReverseLoop." + Environment.NewLine +
                "Press E to change the animation" + Environment.NewLine +
                "to ReverseOnce." + Environment.NewLine +
                "Press R to change the animation" + Environment.NewLine +
                "to Stop." + Environment.NewLine;

            private SwinGame.Bitmap smallScreen = SwinGame.Graphics.CreateBitmap(418, 418);
            private int[] fps;
            private SwinGame.Bitmap[] explodeAnim;
            private Sprite[] sprites;
            private int currentSpr;

            public TestSprite()
                : base(METHS, INST)
            {
                Array.Resize(ref fps, 40);
                Array.Resize(ref explodeAnim, 40);

                for (int i = 0; i < 40; i++)
                {
                    fps[i] = 2;
                    explodeAnim[i] = GameResources.GameImage("Explode_" + Convert.ToString(i));
                }

                Array.Resize(ref sprites, 7);

                sprites[0] = SwinGame.Graphics.CreateSprite(GameResources.GameImage("BlueExplosion"), true, fps, SpriteEndingAction.Loop, 180, 180);
                sprites[1] = SwinGame.Graphics.CreateSprite(GameResources.GameImage("BlueExplosion"), true, fps, 180, 180);
                sprites[2] = SwinGame.Graphics.CreateSprite(GameResources.GameImage("BlueExplosion"), 2, 40, 180, 180);
                sprites[3] = SwinGame.Graphics.CreateSprite(GameResources.GameImage("BallImage1"));
                sprites[4] = SwinGame.Graphics.CreateSprite(explodeAnim, fps, SpriteEndingAction.Loop);
                sprites[5] = SwinGame.Graphics.CreateSprite(explodeAnim, fps);
                sprites[6] = SwinGame.Graphics.CreateSprite(explodeAnim, 2, 40);
            }

            protected override void ToRun(System.Drawing.Rectangle drawIn)
            {
		        if (Input.IsKeyPressed(Keys.VK_1)) ChangeSprite(0);
		        if (Input.IsKeyPressed(Keys.VK_2)) ChangeSprite(1);
		        if (Input.IsKeyPressed(Keys.VK_3)) ChangeSprite(2);
		        if (Input.IsKeyPressed(Keys.VK_4)) ChangeSprite(3);
		        if (Input.IsKeyPressed(Keys.VK_5)) ChangeSprite(4);
		        if (Input.IsKeyPressed(Keys.VK_6)) ChangeSprite(5);
		        if (Input.IsKeyPressed(Keys.VK_7)) ChangeSprite(6);
		        if (Input.IsKeyPressed(Keys.VK_LEFT)) sprites[currentSpr].Movement.SetTo(Physics.AddVectors(sprites[currentSpr].Movement, Physics.CreateVector(-1, 0)));
		        if (Input.IsKeyPressed(Keys.VK_RIGHT)) sprites[currentSpr].Movement.SetTo(Physics.AddVectors(sprites[currentSpr].Movement, Physics.CreateVector(1, 0)));
		        if (Input.IsKeyPressed(Keys.VK_UP)) sprites[currentSpr].Movement.SetTo(Physics.AddVectors(sprites[currentSpr].Movement, Physics.CreateVector(0, -1)));
		        if (Input.IsKeyPressed(Keys.VK_DOWN)) sprites[currentSpr].Movement.SetTo(Physics.AddVectors(sprites[currentSpr].Movement, Physics.CreateVector(0, 1)));
		        if (Input.IsKeyPressed(Keys.VK_M)) ResetPos();
		        if (Input.IsKeyPressed(Keys.VK_Q)) ChangeAnim(SpriteEndingAction.Loop);
		        if (Input.IsKeyPressed(Keys.VK_W)) ChangeAnim(SpriteEndingAction.ReverseLoop);
		        if (Input.IsKeyPressed(Keys.VK_E)) ChangeAnim(SpriteEndingAction.ReverseOnce);
		        if (Input.IsKeyPressed(Keys.VK_R)) ChangeAnim(SpriteEndingAction.Stop);
        		
		        sprites[currentSpr].Movement.SetTo(Physics.MultiplyVector(sprites[currentSpr].Movement, (float)0.95));
		        sprites[currentSpr].Movement.SetTo(Physics.LimitMagnitude(sprites[currentSpr].Movement, 5));
        		
		        SwinGame.Graphics.DrawSprite(sprites[currentSpr]);
		        SwinGame.Graphics.UpdateSprite(sprites[currentSpr]);
        		
		        if (SwinGame.Graphics.IsSpriteOffscreen(sprites[currentSpr]))
		        {
			        Text.DrawText("The sprite is not on the screen", Color.White, GameResources.GameFont("Courier"), 0, 0);
                }
            }

            private void ChangeSprite(int ChangeTo)
            {
                SpriteEndingAction curAction = sprites[currentSpr].EndingAction;
                Vector vec = sprites[currentSpr].Movement;
                SwinGame.Graphics.ReplayAnimation(sprites[currentSpr]);

                Single tempX = sprites[currentSpr].X;
                Single tempY = sprites[currentSpr].Y;

                currentSpr = ChangeTo;

                sprites[currentSpr].Movement.SetTo(vec);
                sprites[currentSpr].X = tempX;
			    sprites[currentSpr].Y = tempY;
			    sprites[currentSpr].EndingAction = curAction;
            }
		
            private void ResetPos()
            {
                sprites[currentSpr].Movement.SetTo(Physics.CreateVector(0,0));
                SwinGame.Graphics.MoveSpriteTo(sprites[currentSpr], 0, 0);
            }

            private void ChangeAnim(SpriteEndingAction act)
            {
                sprites[currentSpr].EndingAction = act;
                SwinGame.Graphics.ReplayAnimation(sprites[currentSpr]);  
            }
		
        }

        private class TestAddBitmap : TestSet
        {
            private readonly static string METHS =
                "AddBitmapToSprite";

            private readonly static string INST =
                "Press Space to add an" + Environment.NewLine +
                "another bitmap.";

            private SwinGame.Bitmap smallScreen = SwinGame.Graphics.CreateBitmap(418, 418);
            private SwinGame.Bitmap[] tempBitmap;
            private SwinGame.Bitmap testingBitmap;
            private Sprite numSprite;
            private int curNum = 1;

            private int[] fps;

            public TestAddBitmap()
                : base(METHS, INST)
            {
                Array.Resize(ref tempBitmap, 1);
                tempBitmap[0] = SwinGame.Graphics.CreateBitmap(121, 120);
                SwinGame.Graphics.DrawCircle(tempBitmap[0], Color.White, true, 60, 60, 60);
			    Text.DrawTextLines(tempBitmap[0], "1", Color.Black, Color.Transparent, GameResources.GameFont("ArialLarge"), FontAlignment.AlignCenter, Shapes.CreateRectangle(tempBitmap[0]));
			    numSprite = SwinGame.Graphics.CreateSprite(tempBitmap, 10, 1);
			    numSprite.X = 149;
			    numSprite.Y = 149;
            }

            protected override void ToRun(System.Drawing.Rectangle drawIn)
            {
                if (Input.WasKeyTyped(Keys.VK_SPACE))
		        {
			        curNum = curNum + 1;
                    testingBitmap = SwinGame.Graphics.CreateBitmap(121, 120);
                    SwinGame.Graphics.DrawCircle(testingBitmap, Color.White, true, 60, 60, 60);
                    Text.DrawTextLines(testingBitmap, Convert.ToString(curNum), Color.Black, Color.Transparent, GameResources.GameFont("ArialLarge"), FontAlignment.AlignCenter, Shapes.CreateRectangle(testingBitmap));
                    SwinGame.Graphics.AddBitmapToSprite(numSprite, testingBitmap);

                    fps = numSprite.FramesPerCell;

                    Array.Resize(ref fps, fps.Length + 1);
			        fps[fps.Length - 1] = 10;

                    numSprite.FramesPerCell = fps;
		        }

                SwinGame.Graphics.DrawSprite(numSprite);
                SwinGame.Graphics.UpdateSpriteAnimation(numSprite);
            }
        }
    }
}