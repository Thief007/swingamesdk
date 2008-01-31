using System;
using System.Collections.Generic;
using System.Text;
using Color = System.Drawing.Color;
using SwinGameVB;

namespace Tests
{
    class GraphicsTests : IGameTestLoader
    {
        #region IGameTestLoader Members

        public void AddTo(List<TestSuite> list)
        {
            TestSuite result = new TestSuite("Consts.Graphics Tests");
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

            private SwinGameVB.Bitmap[] frameB;
            private SwinGameVB.Bitmap ship = GameResources.GameImage("enShip");
            private Point2D framePos =Consts.Shapes.CreatePoint(0, 0);
            private SwinGameVB.Bitmap smallScreen = Consts.Graphics.CreateBitmap(418, 418);

            protected override void ToRun(Rectangle toDrawIn)
            {
                if (Consts.Input.IsKeyPressed(Keys.VK_RIGHT)) framePos.SetX(framePos.GetX() - 2);
                if (Consts.Input.IsKeyPressed(Keys.VK_LEFT)) framePos.SetX(framePos.GetX() + 2);
                if (Consts.Input.IsKeyPressed(Keys.VK_UP)) framePos.SetY(framePos.GetY() + 2);
                if (Consts.Input.IsKeyPressed(Keys.VK_DOWN)) framePos.SetY(framePos.GetY() - 2);

                Consts.Graphics.ClearSurface_Colour(smallScreen, Color.Black.ToArgb());

                Consts.Graphics.DrawBitmap_OnBitmap_Point(smallScreen, frameB[0],Consts.Shapes.CreatePoint(0,0));
                Consts.Graphics.DrawBitmap_OnBitmap(smallScreen, frameB[2], 278, 0);

                Consts.Graphics.DrawBitmapPart_OnBitmap_Point(smallScreen, ship,Consts.Shapes.CreateRectangle(framePos.GetX() + 29, framePos.GetY() + 50, 100, 100),Consts.Shapes.CreatePoint(29, 50));
                Consts.Graphics.DrawBitmapPart_OnBitmap_Rectangle(smallScreen, ship,Consts.Shapes.CreateRectangle(framePos.GetX() + 149, framePos.GetY() + 50, 120, 100), 149, 50);
                Consts.Graphics.DrawBitmapPart_OnBitmap(smallScreen, ship, (int)Math.Round(framePos.GetX() + 289), (int)Math.Round(framePos.GetY() + 50), 100, 100, 289, 50);
		        
		        Consts.Graphics.DrawBitmap(smallScreen, 0, 0);

                Consts.Graphics.DrawBitmap_Point(frameB[1],Consts.Shapes.CreatePoint(139, 0));
		        Consts.Graphics.DrawBitmap(frameB[3], 0, 209);

                Consts.Graphics.DrawBitmapOnScreen_Point(frameB[4],Consts.Shapes.CreatePoint(139 + toDrawIn.GetX(), 209 + toDrawIn.GetY()));
                Consts.Graphics.DrawBitmapOnScreen(frameB[5], (int)Math.Round((double)(278 + toDrawIn.GetX())), (int)Math.Round((double)(209 + toDrawIn.GetY())));
		
		        Consts.Graphics.DrawBitmapPart_Rectangle(ship, Consts.Shapes.CreateRectangle(framePos.GetX() + 29, framePos.GetY() + 159, 100, 100), 29, 159);
		        Consts.Graphics.DrawBitmapPart_Point(ship, Consts.Shapes.CreateRectangle(framePos.GetX() + 149, framePos.GetY() + 159, 120, 100),Consts.Shapes.CreatePoint(149, 159));
                Consts.Graphics.DrawBitmapPart(ship, (int)Math.Round(framePos.GetX() + 289), (int)Math.Round(framePos.GetY() + 159), 100, 100, 289, 159);

                Consts.Graphics.DrawBitmapPartOnScreen_Rectangle(ship,Consts.Shapes.CreateRectangle(framePos.GetX() + 29, framePos.GetY() + 268, 100, 100), (int)Math.Round((double)(29 + toDrawIn.GetX())), (int)Math.Round((double)(268 + toDrawIn.GetY())));
                Consts.Graphics.DrawBitmapPartOnScreen_Point(ship,Consts.Shapes.CreateRectangle(framePos.GetX() + 149, framePos.GetY() + 268, 120, 100),Consts.Shapes.CreatePoint(149 + toDrawIn.GetX(), 268 + toDrawIn.GetY()));
                Consts.Graphics.DrawBitmapPartOnScreen(ship, (int)Math.Round(framePos.GetX() + 289), (int)Math.Round(framePos.GetY() + 268), 100, 100, (int)Math.Round((double)(289 + toDrawIn.GetX())), (int)Math.Round((double)(268 + toDrawIn.GetY())));
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
            private SwinGameVB.Bitmap smallScreen = Consts.Graphics.CreateBitmap(418, 418);
            private Boolean started = false;

            protected override void ToRun(Rectangle toDrawIn)
            {
                if (!started)
                {
                    started = !started;
                    Consts.Graphics.ClearScreen();
                }

                if (Consts.Input.IsKeyPressed(Keys.VK_Z))
                {
                    Consts.Graphics.ClearSurface_Colour(smallScreen, Color.Transparent.ToArgb());
                    Consts.Graphics.ClearScreen_ToColour(Color.Transparent.ToArgb());
                    Consts.Graphics.DrawBitmapOnScreen(GameResources.GameImage("BGA"), (int)(toDrawIn.GetX()), (int)(toDrawIn.GetY()));
                }

                if (Consts.Input.IsKeyPressed(Keys.VK_1)) _curCol = Color.FromArgb(Consts.Core.GetColor((byte)(_curCol.R + 5), _curCol.G, _curCol.B));
                if (Consts.Input.IsKeyPressed(Keys.VK_2)) _curCol = Color.FromArgb(Consts.Core.GetColor(_curCol.R, (byte)(_curCol.G + 5), _curCol.B));
                if (Consts.Input.IsKeyPressed(Keys.VK_3)) _curCol = Color.FromArgb(Consts.Core.GetColor(_curCol.R, _curCol.G, (byte)(_curCol.B + 5)));

                Consts.Graphics.FillRectangle(Color.Black.ToArgb(), 0, 0, 100, 45);

               Consts.Text.DrawText("Red   : " + Convert.ToString(_curCol.R), Color.Red.ToArgb(), GameResources.GameFont("Courier"), 0, 0);
		       Consts.Text.DrawText("Green : " + Convert.ToString(_curCol.G), Color.Green.ToArgb(), GameResources.GameFont("Courier"), 0, 15);
		       Consts.Text.DrawText("Blue  : " + Convert.ToString(_curCol.B), Color.Blue.ToArgb(), GameResources.GameFont("Courier"), 0, 30);
		
                if (Consts.Input.IsMouseDown(MouseButton.LeftButton))
                {
                    Consts.Graphics.DrawPixel_OnBitmap_Point(smallScreen, _curCol.ToArgb(),Consts.Shapes.CreatePoint(Consts.Input.GetMousePosition().GetX() - toDrawIn.GetX() - 10, Consts.Input.GetMousePosition().GetY() - toDrawIn.GetY() - 10));
                    Consts.Graphics.DrawPixel_OnBitmap(smallScreen, _curCol.ToArgb(), (int)(Consts.Input.GetMousePosition().GetX() - toDrawIn.GetX() + 10), (int)(Consts.Input.GetMousePosition().GetY() - toDrawIn.GetY() - 10));

                    Consts.Graphics.DrawPixel_Point(_curCol.ToArgb(), Consts.Shapes.CreatePoint(Consts.Input.GetMousePosition().GetX() - 10 - toDrawIn.GetX(), Consts.Input.GetMousePosition().GetY() - toDrawIn.GetY()));
                    Consts.Graphics.DrawPixel(_curCol.ToArgb(), (int)(Consts.Input.GetMousePosition().GetX() - toDrawIn.GetX() + 10), (int)(Consts.Input.GetMousePosition().GetY() - toDrawIn.GetY()));

                    Consts.Graphics.DrawPixelOnScreen_Point(_curCol.ToArgb(), Consts.Shapes.CreatePoint(Consts.Input.GetMousePosition().GetX() - 10, Consts.Input.GetMousePosition().GetY() + 10));
                    Consts.Graphics.DrawPixelOnScreen(_curCol.ToArgb(), (int)(Consts.Input.GetMousePosition().GetX() + 10), (int)(Consts.Input.GetMousePosition().GetY() + 10));
                }

		        Consts.Graphics.DrawBitmap(smallScreen, 0, 0);
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
            private SwinGameVB.Bitmap smallScreen = Consts.Graphics.CreateBitmap(418, 418);
            private int angle;

            public DrawLineTest()
                : base(METHS, INST)
            {
                Array.Resize(ref tempLines, 7);
                tempLines[1] = Consts.Shapes.LineFromVector_xy(225, 225, Consts.Physics.GetVectorFromAngle(0, 125));
                tempLines[2] = Consts.Shapes.LineFromVector_xy(225, 225, Consts.Physics.GetVectorFromAngle(60, 125));
                tempLines[3] = Consts.Shapes.LineFromVector_xy(225, 225, Consts.Physics.GetVectorFromAngle(120, 125));
                tempLines[4] = Consts.Shapes.LineFromVector_xy(225, 225, Consts.Physics.GetVectorFromAngle(180, 125));
                tempLines[5] = Consts.Shapes.LineFromVector_xy(248, 354, Consts.Physics.GetVectorFromAngle(240, 125));
                tempLines[6] = Consts.Shapes.LineFromVector_xy(248, 354, Consts.Physics.GetVectorFromAngle(300, 125));
            }

            protected override void ToRun(Rectangle toDrawIn)
            {
                Consts.Graphics.ClearSurface_Colour(smallScreen, Color.Transparent.ToArgb());
		
                if (Consts.Input.IsKeyPressed(Keys.VK_RIGHT)) angle = angle + 2;
                if (Consts.Input.IsKeyPressed(Keys.VK_LEFT)) angle = angle - 2;

		
		        if (angle != 0)
		        {
			        for (int i = 1; i < 7; i++)
			        {
                        tempLines[i] = Consts.Shapes.LineFromVector_xy(tempLines[i].GetStartPoint().GetX(), tempLines[i].GetStartPoint().GetY(), Consts.Physics.Multiply_Vector(Consts.Physics.RotationMatrix(angle), Consts.Physics.LineAsVector(tempLines[i])));
                    }
		        }
		
		        Consts.Graphics.DrawHorizontalLine_OnBitmap(smallScreen, Color.Green.ToArgb(), 100, 100, 350);
		        Consts.Graphics.DrawVerticalLine_OnBitmap(smallScreen, Color.Green.ToArgb(), 100, 100, 350);
		        Consts.Graphics.DrawBitmap(smallScreen, 0, 0);
		        Consts.Graphics.ClearSurface_Colour(smallScreen, Color.Transparent.ToArgb());
		        Consts.Graphics.DrawLine_OnBitmap_Line(smallScreen, Color.White.ToArgb(), tempLines[1]);
                Consts.Graphics.DrawLine_OnBitmap(smallScreen, Color.White.ToArgb(), (int)(tempLines[3].GetStartPoint().GetX()), (int)(tempLines[3].GetStartPoint().GetY()), (int)(tempLines[3].GetEndPoint().GetX()), (int)(tempLines[3].GetEndPoint().GetY()));
		
		        Consts.Graphics.DrawHorizontalLine(Color.Green.ToArgb(), 225, 100, 350);
		        Consts.Graphics.DrawVerticalLine(Color.Green.ToArgb(), 225, 100, 350);
                Consts.Graphics.DrawHorizontalLineOnScreen(Color.Green.ToArgb(), (int)(350 + toDrawIn.GetY()), (int)(100 + toDrawIn.GetX()), (int)(350 + toDrawIn.GetX()));
                Consts.Graphics.DrawVerticalLineOnScreen(Color.Green.ToArgb(), (int)(350 + toDrawIn.GetX()), (int)(100 + toDrawIn.GetY()), (int)(350 + toDrawIn.GetY()));
		
		        Consts.Graphics.DrawLine_Line(Color.White.ToArgb(), tempLines[2]);
                Consts.Graphics.DrawLine(Color.White.ToArgb(), tempLines[4].GetStartPoint().GetX(), tempLines[4].GetStartPoint().GetY(), tempLines[4].GetEndPoint().GetX(), tempLines[4].GetEndPoint().GetY());
		
		        Consts.Graphics.DrawBitmap(smallScreen, 0, 0);
		
		        Consts.Graphics.DrawLineOnScreen_Line(Color.White.ToArgb(), tempLines[5]);
                Consts.Graphics.DrawLineOnScreen(Color.White.ToArgb(), (int)(tempLines[6].GetStartPoint().GetX()), (int)(tempLines[6].GetStartPoint().GetY()), (int)(tempLines[6].GetEndPoint().GetX()), (int)(tempLines[6].GetEndPoint().GetY()));
		
                for (int i = 1; i < 5; i++)
		        {
                    Consts.Graphics.DrawLine_Line(Color.White.ToArgb(), Consts.Shapes.LineFromVector_Point(tempLines[i].GetEndPoint(), Consts.Physics.MultiplyVector(Consts.Physics.LineNormal(tempLines[i]), 10)));
		        }

                for (int i = 5; i < 7; i++)
                {
                    Consts.Graphics.DrawLineOnScreen_Line(Color.White.ToArgb(), Consts.Shapes.LineFromVector_Point(tempLines[i].GetEndPoint(), Consts.Physics.MultiplyVector(Consts.Physics.VectorNormal(Consts.Physics.LineAsVector(tempLines[i])), 10)));
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

            private SwinGameVB.Bitmap smallScreen = Consts.Graphics.CreateBitmap(418, 418);
            private SwinGameVB.Rectangle tempRect =Consts.Shapes.CreateRectangle(0, 0, 50, 50);
            private SwinGameVB.Rectangle tempRect4 =Consts.Shapes.CreateRectangle(0, 0, 50, 50);
            private bool filled = false;

            private SwinGameVB.Rectangle tempRect2, tempRect3, tempRect5, tempRect6;

            public TestDrawRectangle()
                : base(METHS, INST)
            {
                tempRect2 = new Rectangle();
                tempRect3 = new Rectangle();
                tempRect5 = new Rectangle();
                tempRect6 = new Rectangle();
            }

            protected override void ToRun(Rectangle toDrawIn)
            {
                Consts.Graphics.ClearSurface_Colour(smallScreen, Color.Transparent.ToArgb());
		
		        if (Consts.Input.IsKeyPressed(Keys.VK_RIGHT)) tempRect.SetX(tempRect.GetX() + 2);
                if (Consts.Input.IsKeyPressed(Keys.VK_LEFT)) tempRect.SetX(tempRect.GetX() - 2);
                if (Consts.Input.IsKeyPressed(Keys.VK_UP)) tempRect.SetY(tempRect.GetY() - 2);
                if (Consts.Input.IsKeyPressed(Keys.VK_DOWN)) tempRect.SetY(tempRect.GetY() + 2);
                if (Consts.Input.IsKeyPressed(Keys.VK_A)) tempRect.SetWidth(tempRect.GetWidth() - 2);
                if (Consts.Input.IsKeyPressed(Keys.VK_S)) tempRect.SetWidth(tempRect.GetWidth() + 2);
                if (Consts.Input.IsKeyPressed(Keys.VK_Z)) tempRect.SetHeight(tempRect.GetHeight() - 2);
                if (Consts.Input.IsKeyPressed(Keys.VK_X)) tempRect.SetHeight(tempRect.GetHeight() + 2);
                if (Consts.Input.WasKeyTyped(Keys.VK_T)) filled = filled == false;

                if (tempRect.GetWidth() < -100) tempRect.SetWidth( -100);
                if (tempRect.GetHeight() < -100) tempRect.SetHeight( -100);
                if (tempRect.GetWidth() > 100) tempRect.SetWidth( 100);
                if (tempRect.GetHeight() > 100) tempRect.SetHeight( 100);
		
		        Consts.Graphics.DrawRectangle_OnBitmap_Fill_Rectangle(smallScreen, Color.Green.ToArgb(), filled, tempRect);
                Consts.Graphics.DrawRectangle_OnBitmap(smallScreen, Color.Green.ToArgb(), filled, (int)(tempRect.GetWidth() + tempRect.GetX()), (int)(tempRect.GetY() + tempRect.GetHeight()), (int)tempRect.GetWidth(), (int)tempRect.GetHeight());
                Consts.Graphics.DrawRectangle_OnBitmap_NoFill(smallScreen, Color.Green.ToArgb(), (int)(tempRect.GetX()), (int)(tempRect.GetY() + tempRect.GetHeight()), (int)tempRect.GetWidth(), (int)tempRect.GetHeight());
		        tempRect4.SetX(tempRect.GetX() + tempRect.GetWidth());
		        tempRect4.SetY(tempRect.GetY());
		        tempRect4.SetWidth(tempRect.GetWidth());
		        tempRect4.SetHeight(tempRect.GetHeight());
		        Consts.Graphics.DrawRectangle_OnBitmap_Rectangle(smallScreen, Color.Green.ToArgb(), tempRect4);
		        Consts.Graphics.DrawBitmap(smallScreen, 0, 0);

                Consts.Graphics.DrawRectangle(Color.White.ToArgb(), filled, 0, 0, (int)tempRect.GetWidth(), (int)tempRect.GetHeight());
		        tempRect2.SetX(tempRect.GetWidth());
		        tempRect2.SetY(tempRect.GetHeight());
		        tempRect2.SetWidth(tempRect.GetWidth());
		        tempRect2.SetHeight(tempRect.GetHeight());
		        Consts.Graphics.DrawRectangle_Fill_Rectangle(Color.White.ToArgb(), filled, tempRect2);
                Consts.Graphics.DrawRectangle_NoFill(Color.White.ToArgb(), 0, tempRect.GetHeight(), (int)tempRect.GetWidth(), (int)tempRect.GetHeight());
		        tempRect3.SetX(tempRect.GetWidth());
		        tempRect3.SetWidth(tempRect.GetWidth());
		        tempRect3.SetHeight(tempRect.GetHeight());
		        Consts.Graphics.DrawRectangle_Rectangle(Color.White.ToArgb(), tempRect3);

                Consts.Graphics.DrawRectangleOnScreen_Fill(Color.Yellow.ToArgb(), filled, (int)(toDrawIn.GetX()), (int)(Consts.Shapes.RectangleBottom(toDrawIn) - tempRect.GetHeight()) - 1, (int)tempRect.GetWidth(), (int)tempRect.GetHeight());
		        tempRect5.SetX(tempRect.GetWidth() + toDrawIn.GetX());
                tempRect5.SetY((int)(Consts.Shapes.RectangleBottom(toDrawIn) - tempRect.GetHeight() * 2 - 1));
		        tempRect5.SetWidth(tempRect.GetWidth());
		        tempRect5.SetHeight(tempRect.GetHeight());
                Consts.Graphics.DrawRectangleOnScreen_Fill_Rectangle(Color.Yellow.ToArgb(), filled, tempRect5);
                Consts.Graphics.DrawRectangleOnScreen(Color.Yellow.ToArgb(), (int)(toDrawIn.GetX()), (int)(Consts.Shapes.RectangleBottom(toDrawIn) - tempRect.GetHeight() * 2) - 1, (int)tempRect.GetWidth(), (int)tempRect.GetHeight());
		        tempRect6.SetX(tempRect.GetWidth() + toDrawIn.GetX());
                tempRect6.SetY((int)(Consts.Shapes.RectangleBottom(toDrawIn) - tempRect.GetHeight() - 1));
		        tempRect6.SetWidth(tempRect.GetWidth());
		        tempRect6.SetHeight(tempRect.GetHeight());
                Consts.Graphics.DrawRectangleOnScreen_Rectangle(Color.Yellow.ToArgb(), tempRect6);
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
               
            private SwinGameVB.Bitmap smallScreen = Consts.Graphics.CreateBitmap(418, 418);
            private Point2D tempPoint =Consts.Shapes.CreatePoint(15,15);
            private Point2D tempPoint2, tempPoint3, tempPoint4, tempPoint5, tempPoint6;
            private int curRadius = 15;
            private bool filled = false;

            public TestDrawCircle()
                : base(METHS, INST)
            {
                tempPoint2 = new Point2D();
                tempPoint3 = new Point2D();
                tempPoint4 = new Point2D();
                tempPoint5 = new Point2D();
                tempPoint6 = new Point2D();

            }

            protected override void ToRun(Rectangle toDrawIn)
            {
                Consts.Graphics.ClearSurface_Colour(smallScreen, Color.Transparent.ToArgb());

		        if (Consts.Input.IsKeyPressed(Keys.VK_RIGHT)) tempPoint.SetX(tempPoint.GetX() + 2);
                if (Consts.Input.IsKeyPressed(Keys.VK_LEFT)) tempPoint.SetX(tempPoint.GetX() - 2);
                if (Consts.Input.IsKeyPressed(Keys.VK_UP)) tempPoint.SetY(tempPoint.GetY() - 2);
                if (Consts.Input.IsKeyPressed(Keys.VK_DOWN)) tempPoint.SetY(tempPoint.GetY() + 2);
                if (Consts.Input.IsKeyPressed(Keys.VK_Z)) curRadius = curRadius - 1;
                if (Consts.Input.IsKeyPressed(Keys.VK_X)) curRadius = curRadius + 1;
                if (Consts.Input.WasKeyTyped(Keys.VK_T)) filled = filled == false;
        		
		        if (curRadius < 0) curRadius = 0;
		        if (curRadius > 50) curRadius = 50;
        		
		        Consts.Graphics.DrawCircle_OnBitmap_Fill_Point(smallScreen, Color.Green.ToArgb(), filled, tempPoint, curRadius);
		        Consts.Graphics.DrawCircle_OnBitmap(smallScreen, Color.Green.ToArgb(), filled, (int)(tempPoint.GetX() + curRadius * 2), (int)(tempPoint.GetY() + curRadius * 2), curRadius);
		        Consts.Graphics.DrawCircle_OnBitmap_NoFill(smallScreen, Color.Green.ToArgb(), (int)(tempPoint.GetX()), (int)(tempPoint.GetY() + curRadius * 2), curRadius);
		        tempPoint2.SetX(tempPoint.GetX() + curRadius * 2);
		        tempPoint2.SetY(tempPoint.GetY());
		        Consts.Graphics.DrawCircle_OnBitmap_Point(smallScreen, Color.Green.ToArgb(), tempPoint2, curRadius);
		        Consts.Graphics.DrawBitmap(smallScreen, 0, 0);
        		
		        Consts.Graphics.DrawCircle(Color.White.ToArgb(), filled, curRadius, curRadius, curRadius);
		        tempPoint3.SetX(curRadius * 3);
		        tempPoint3.SetY(curRadius * 3);
                Consts.Graphics.DrawCircle_Fill_Point(Color.White.ToArgb(), filled, tempPoint3, curRadius);
                Consts.Graphics.DrawCircle_NoFill(Color.White.ToArgb(), curRadius, curRadius * 3, curRadius);
		        tempPoint4.SetX(curRadius * 3);
		        tempPoint4.SetY(curRadius);
                Consts.Graphics.DrawCircle_Point(Color.White.ToArgb(), tempPoint4, curRadius);

                Consts.Graphics.DrawCircleOnScreen_Fill(Color.Yellow.ToArgb(), filled, (int)(curRadius + toDrawIn.GetX()), (int)(Consts.Shapes.RectangleBottom(toDrawIn) - curRadius) - 1, curRadius);
		        tempPoint5.SetX(curRadius * 3 + toDrawIn.GetX());
		        tempPoint5.SetY( Consts.Shapes.RectangleBottom(toDrawIn) - curRadius * 3 - 1);
                Consts.Graphics.DrawCircleOnScreen_Fill_Point(Color.Yellow.ToArgb(), filled, tempPoint5, curRadius);
                Consts.Graphics.DrawCircleOnScreen(Color.Yellow.ToArgb(), (int)(curRadius + toDrawIn.GetX()), (int)(Consts.Shapes.RectangleBottom(toDrawIn) - curRadius * 3) - 1, curRadius);
		        tempPoint6.SetX(curRadius * 3 + toDrawIn.GetX());
		        tempPoint6.SetY( Consts.Shapes.RectangleBottom(toDrawIn) - curRadius - 1);
                Consts.Graphics.DrawCircleOnScreen_Point(Color.Yellow.ToArgb(), tempPoint6, curRadius);
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

            private SwinGameVB.Bitmap smallScreen = Consts.Graphics.CreateBitmap(418, 418);
            private bool filled = false;
            private SwinGameVB.Rectangle tempRect2, tempRect3, tempRect5, tempRect6;

            private SwinGameVB.Rectangle tempRect =Consts.Shapes.CreateRectangle(0, 0, 50, 50);
			private SwinGameVB.Rectangle tempRect4 =Consts.Shapes.CreateRectangle(0, 0, 50, 50);

            public TestDrawEllipse()
                : base(METHS, INST)
            {
                tempRect2 = new Rectangle();
                tempRect3 = new Rectangle();
                tempRect5 = new Rectangle();
                tempRect6 = new Rectangle();

            }

            protected override void ToRun(Rectangle drawIn)
            {
                Consts.Graphics.ClearSurface_Colour(smallScreen, Color.Transparent.ToArgb());

		        if (Consts.Input.IsKeyPressed(Keys.VK_RIGHT)) tempRect.SetX(tempRect.GetX() + 2);
                if (Consts.Input.IsKeyPressed(Keys.VK_LEFT)) tempRect.SetX(tempRect.GetX() - 2);
                if (Consts.Input.IsKeyPressed(Keys.VK_UP)) tempRect.SetY(tempRect.GetY() - 2);
                if (Consts.Input.IsKeyPressed(Keys.VK_DOWN)) tempRect.SetY(tempRect.GetY() + 2);
                if (Consts.Input.IsKeyPressed(Keys.VK_A)) tempRect.SetWidth(tempRect.GetWidth() - 2);
                if (Consts.Input.IsKeyPressed(Keys.VK_S)) tempRect.SetWidth(tempRect.GetWidth() + 2);
                if (Consts.Input.IsKeyPressed(Keys.VK_Z)) tempRect.SetHeight(tempRect.GetHeight() - 2);
                if (Consts.Input.IsKeyPressed(Keys.VK_X)) tempRect.SetHeight(tempRect.GetHeight() + 2);
                if (Consts.Input.WasKeyTyped(Keys.VK_T)) filled = filled == false;

		        if (tempRect.GetWidth() < 0) tempRect.SetWidth(0);
		        if (tempRect.GetHeight() < 0) tempRect.SetHeight(0);
		        if (tempRect.GetWidth() > 100) tempRect.SetWidth(100);
		        if (tempRect.GetHeight() > 100) tempRect.SetHeight(100);

		        Consts.Graphics.DrawEllipse_OnBitmap_Fill_Rectangle(smallScreen, Color.Green.ToArgb(), filled, tempRect);
                Consts.Graphics.DrawEllipse_OnBitmap(smallScreen, Color.Green.ToArgb(), filled, (int)(tempRect.GetX() + tempRect.GetWidth()), (int)(tempRect.GetY() + tempRect.GetHeight()), (int)tempRect.GetWidth(), (int)tempRect.GetHeight());
                Consts.Graphics.DrawEllipse_OnBitmap_NoFill(smallScreen, Color.Green.ToArgb(), (int)(tempRect.GetX()), (int)(tempRect.GetY() + tempRect.GetHeight()), (int)tempRect.GetWidth(), (int)tempRect.GetHeight());
		        tempRect4.SetX(tempRect.GetX() + tempRect.GetWidth());
		        tempRect4.SetY(tempRect.GetY());
		        tempRect4.SetWidth(tempRect.GetWidth());
		        tempRect4.SetHeight(tempRect.GetHeight());
                Consts.Graphics.DrawEllipse_OnBitmap_Rectangle(smallScreen, Color.Green.ToArgb(), tempRect4);
                Consts.Graphics.DrawBitmap(smallScreen, 0, 0);

                Consts.Graphics.DrawEllipse(Color.White.ToArgb(), filled, 0, 0, (int)tempRect.GetWidth(), (int)tempRect.GetHeight());
		        tempRect2.SetX(tempRect.GetWidth());
		        tempRect2.SetY(tempRect.GetHeight());
		        tempRect2.SetWidth(tempRect.GetWidth());
		        tempRect2.SetHeight(tempRect.GetHeight());
                Consts.Graphics.DrawEllipse_Fill_Rectangle(Color.White.ToArgb(), filled, tempRect2);
                Consts.Graphics.DrawEllipse_NoFill(Color.White.ToArgb(), 0, tempRect.GetHeight(), (int)tempRect.GetWidth(), (int)tempRect.GetHeight());
		        tempRect3.SetX(tempRect.GetWidth());
		        tempRect3.SetWidth(tempRect.GetWidth());
		        tempRect3.SetHeight(tempRect.GetHeight());
                Consts.Graphics.DrawEllipse_Rectangle(Color.White.ToArgb(), tempRect3);

                Consts.Graphics.DrawEllipseOnScreen_Fill(Color.Yellow.ToArgb(), filled, (int)(drawIn.GetX()), (int)(Consts.Shapes.RectangleBottom(drawIn) - tempRect.GetHeight()), (int)tempRect.GetWidth(), (int)tempRect.GetHeight());
		        tempRect5.SetX(tempRect.GetWidth() + drawIn.GetX());
		        tempRect5.SetY((int)(Consts.Shapes.RectangleBottom(drawIn) - tempRect.GetHeight() * 2));
		        tempRect5.SetWidth(tempRect.GetWidth());
		        tempRect5.SetHeight(tempRect.GetHeight());
                Consts.Graphics.DrawEllipseOnScreen_Fill_Rectangle(Color.Yellow.ToArgb(), filled, tempRect5);
                Consts.Graphics.DrawEllipseOnScreen(Color.Yellow.ToArgb(), (int)(drawIn.GetX()), (int)(Consts.Shapes.RectangleBottom(drawIn) - tempRect.GetHeight() * 2), (int)tempRect.GetWidth(), (int)tempRect.GetHeight());
		        tempRect6.SetX(tempRect.GetWidth() + drawIn.GetX());
		        tempRect6.SetY((int)(Consts.Shapes.RectangleBottom(drawIn) - tempRect.GetHeight()));
		        tempRect6.SetWidth(tempRect.GetWidth());
		        tempRect6.SetHeight(tempRect.GetHeight());
                Consts.Graphics.DrawEllipseOnScreen_Rectangle(Color.Yellow.ToArgb(), tempRect6);
            }
        }

        private class TestFill1 : TestSet
        {
            private readonly static string METHS =
                "FillRectangle, FillEllipse";

            private readonly static string INST =
                "Use the arrow keys to move" + Environment.NewLine +
                "the green Shapes" + Environment.NewLine +
                "A : Shrink the shape width" + Environment.NewLine +
                "S : Expand the shape width" + Environment.NewLine +
                "Z : Shrink the shape height" + Environment.NewLine +
                "X : Expand the shape height" + Environment.NewLine +
                "White : Normal" + Environment.NewLine +
                "Green : On destination bitmap" + Environment.NewLine +
                "Yellow: On screen";

            private SwinGameVB.Bitmap smallScreen = Consts.Graphics.CreateBitmap(418, 418);
            private SwinGameVB.Rectangle tempRect2, tempRect3, tempRect5, tempRect6;

            private SwinGameVB.Rectangle tempRect =Consts.Shapes.CreateRectangle(0, 0, 50, 50);
            private SwinGameVB.Rectangle tempRect4 =Consts.Shapes.CreateRectangle(0, 0, 50, 50);

            public TestFill1()
                : base(METHS, INST)
            {
                tempRect2 = new Rectangle();
                tempRect3 = new Rectangle();
                tempRect5 = new Rectangle();
                tempRect6 = new Rectangle();
            }

            protected override void ToRun(Rectangle drawIn)
            {
                Consts.Graphics.ClearSurface_Colour(smallScreen, Color.Transparent.ToArgb());

		        if (Consts.Input.IsKeyPressed(Keys.VK_RIGHT)) tempRect.SetX(tempRect.GetX() + 2);
		        if (Consts.Input.IsKeyPressed(Keys.VK_LEFT))tempRect.SetX(tempRect.GetX() - 2);
		        if (Consts.Input.IsKeyPressed(Keys.VK_UP)) tempRect.SetY(tempRect.GetY() - 2);
		        if (Consts.Input.IsKeyPressed(Keys.VK_DOWN)) tempRect.SetY(tempRect.GetY() + 2);
		        if (Consts.Input.IsKeyPressed(Keys.VK_A)) tempRect.SetWidth(tempRect.GetWidth() - 2);
		        if (Consts.Input.IsKeyPressed(Keys.VK_S)) tempRect.SetWidth(tempRect.GetWidth() + 2);
		        if (Consts.Input.IsKeyPressed(Keys.VK_Z)) tempRect.SetHeight(tempRect.GetHeight() - 2);
		        if (Consts.Input.IsKeyPressed(Keys.VK_X)) tempRect.SetHeight(tempRect.GetHeight() + 2);
        		
		        if (tempRect.GetWidth() < 0) tempRect.SetWidth(0);
		        if (tempRect.GetHeight() < 0) tempRect.SetHeight(0);
		        if (tempRect.GetWidth() > 100) tempRect.SetWidth(100);
		        if (tempRect.GetHeight() > 100) tempRect.SetHeight(100);
        		
		        Consts.Graphics.FillRectangle_OnBitmap_Rectangle(smallScreen, Color.Green.ToArgb(), tempRect);
                Consts.Graphics.FillRectangle_OnBitmap(smallScreen, Color.Green.ToArgb(), (int)(tempRect.GetWidth() + tempRect.GetX()), (int)(tempRect.GetY() + tempRect.GetHeight()), (int)tempRect.GetWidth(), (int)tempRect.GetHeight());
                Consts.Graphics.FillEllipse_OnBitmap(smallScreen, Color.Green.ToArgb(), (int)(tempRect.GetX()), (int)(tempRect.GetY() + tempRect.GetHeight()), (int)tempRect.GetWidth(), (int)tempRect.GetHeight());
		        tempRect4.SetX(tempRect.GetX() + tempRect.GetWidth());
		        tempRect4.SetY(tempRect.GetY());
		        tempRect4.SetWidth(tempRect.GetWidth());
		        tempRect4.SetHeight( tempRect.GetHeight());
                Consts.Graphics.FillEllipse_OnBitmap_Rectangle(smallScreen, Color.Green.ToArgb(), tempRect4);
		        Consts.Graphics.DrawBitmap(smallScreen, 0, 0);

                Consts.Graphics.FillRectangle(Color.White.ToArgb(), 0, 0, (int)tempRect.GetWidth(), (int)tempRect.GetHeight());
		        tempRect2.SetX(tempRect.GetWidth());
		        tempRect2.SetY(tempRect.GetHeight());
		        tempRect2.SetWidth(tempRect.GetWidth());
		        tempRect2.SetHeight(tempRect.GetHeight());
                Consts.Graphics.FillRectangle_Rectangle(Color.White.ToArgb(), tempRect2);
                Consts.Graphics.FillEllipse(Color.White.ToArgb(), 0, tempRect.GetHeight(), (int)tempRect.GetWidth(), (int)tempRect.GetHeight());
		        tempRect3.SetX(tempRect.GetWidth());
		        tempRect3.SetWidth(tempRect.GetWidth());
		        tempRect3.SetHeight(tempRect.GetHeight());
                Consts.Graphics.FillEllipse_Rectangle(Color.White.ToArgb(), tempRect3);

                Consts.Graphics.FillRectangleOnScreen(Color.Yellow.ToArgb(), (int)(drawIn.GetX()), (int)(Consts.Shapes.RectangleBottom(drawIn) - tempRect.GetHeight()) - 1, (int)tempRect.GetWidth(), (int)tempRect.GetHeight());
		        tempRect5.SetX(tempRect.GetWidth() + drawIn.GetX());
		        tempRect5.SetY((int)(Consts.Shapes.RectangleBottom(drawIn) - tempRect.GetHeight() * 2 - 1));
		        tempRect5.SetWidth(tempRect.GetWidth());
		        tempRect5.SetHeight(tempRect.GetHeight());
		        Consts.Graphics.FillRectangleOnScreen_Rectangle(Color.Yellow.ToArgb(), tempRect5);

                Consts.Graphics.FillEllipseOnScreen(Color.Yellow.ToArgb(), (int)(drawIn.GetX()), (int)(Consts.Shapes.RectangleBottom(drawIn) - tempRect.GetHeight() * 2) - 1, (int)tempRect.GetWidth(), (int)tempRect.GetHeight());
		        tempRect6.SetX(tempRect.GetWidth() + drawIn.GetX());
		        tempRect6.SetY((int)(Consts.Shapes.RectangleBottom(drawIn) - tempRect.GetHeight() - 1));
		        tempRect6.SetWidth(tempRect.GetWidth());
		        tempRect6.SetHeight(tempRect.GetHeight());
                Consts.Graphics.FillEllipseOnScreen_Rectangle(Color.Yellow.ToArgb(), tempRect6);
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

            private SwinGameVB.Bitmap smallScreen = Consts.Graphics.CreateBitmap(418, 418);
            private Point2D tempPoint =Consts.Shapes.CreatePoint(15, 15);
			private int curRadius = 15;

            private Point2D tempPoint3, tempPoint5;

            public TestFill2()
                : base(METHS, INST)
            {
                tempPoint3 = new Point2D();
                tempPoint5 = new Point2D();
            }

            protected override void ToRun(Rectangle drawIn)
            {
                Consts.Graphics.ClearSurface_Colour(smallScreen, Color.Transparent.ToArgb());
		
		        if (Consts.Input.IsKeyPressed(Keys.VK_RIGHT)) tempPoint.SetX(tempPoint.GetX() + 2);
		        if (Consts.Input.IsKeyPressed(Keys.VK_LEFT)) tempPoint.SetX(tempPoint.GetX() - 2);
		        if (Consts.Input.IsKeyPressed(Keys.VK_UP)) tempPoint.SetY(tempPoint.GetY() - 2);
		        if (Consts.Input.IsKeyPressed(Keys.VK_DOWN)) tempPoint.SetY(tempPoint.GetY() + 2);
		        if (Consts.Input.IsKeyPressed(Keys.VK_Z)) curRadius = curRadius - 1;
		        if (Consts.Input.IsKeyPressed(Keys.VK_X)) curRadius = curRadius + 1;
        		
		        if (curRadius < 0) curRadius = 0;
		        if (curRadius > 50) curRadius = 50;

                Consts.Graphics.FillCircle_OnBitmap_Point(smallScreen, Color.Green.ToArgb(), tempPoint, curRadius);
                Consts.Graphics.FillCircle_OnBitmap(smallScreen, Color.Green.ToArgb(), (int)(tempPoint.GetX() + curRadius * 2), (int)(tempPoint.GetY() + curRadius * 2), curRadius);
                Consts.Graphics.DrawBitmap(smallScreen, 0, 0);

                Consts.Graphics.FillCircle(Color.White.ToArgb(), curRadius, curRadius, curRadius);
		        tempPoint3.SetX(curRadius * 3);
		        tempPoint3.SetY(curRadius * 3);
                Consts.Graphics.FillCircle_Point(Color.White.ToArgb(), tempPoint3, curRadius);

                Consts.Graphics.FillCircleOnScreen(Color.Yellow.ToArgb(), (int)(curRadius + drawIn.GetX()), (int)(Consts.Shapes.RectangleBottom(drawIn) - curRadius) - 1, curRadius);
		        tempPoint5.SetX(curRadius * 3 + drawIn.GetX());
		        tempPoint5.SetY( Consts.Shapes.RectangleBottom(drawIn) - curRadius * 3 - 1);
                Consts.Graphics.FillCircleOnScreen_Point(Color.Yellow.ToArgb(), tempPoint5, curRadius);
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

            private SwinGameVB.Bitmap smallScreen = Consts.Graphics.CreateBitmap(418, 418);
            private int[] fps;
            private SwinGameVB.Bitmap[] explodeAnim;
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

                sprites[0] = Consts.Graphics.CreateSprite_MultiEnding(GameResources.GameImage("BlueExplosion"), true, ref fps, SpriteEndingAction.Loop, 180, 180);
                sprites[1] = Consts.Graphics.CreateSprite_Multi(GameResources.GameImage("BlueExplosion"), true, ref fps, 180, 180);
                sprites[2] = Consts.Graphics.CreateSprite_MultiFPC(GameResources.GameImage("BlueExplosion"), 2, 40, 180, 180);
                sprites[3] = Consts.Graphics.CreateSprite(GameResources.GameImage("BallImage1"));
                sprites[4] = Consts.Graphics.CreateSprite_ArrayEnding(ref explodeAnim, ref fps, SpriteEndingAction.Loop);
                sprites[5] = Consts.Graphics.CreateSprite_Array(ref explodeAnim, ref fps);
                sprites[6] = Consts.Graphics.CreateSprite_ArrayFPC(explodeAnim, 2, 40);
            }

            protected override void ToRun(Rectangle drawIn)
            {
		        if (Consts.Input.IsKeyPressed(Keys.VK_1)) ChangeSprite(0);
		        if (Consts.Input.IsKeyPressed(Keys.VK_2)) ChangeSprite(1);
		        if (Consts.Input.IsKeyPressed(Keys.VK_3)) ChangeSprite(2);
		        if (Consts.Input.IsKeyPressed(Keys.VK_4)) ChangeSprite(3);
		        if (Consts.Input.IsKeyPressed(Keys.VK_5)) ChangeSprite(4);
		        if (Consts.Input.IsKeyPressed(Keys.VK_6)) ChangeSprite(5);
		        if (Consts.Input.IsKeyPressed(Keys.VK_7)) ChangeSprite(6);
                if (Consts.Input.IsKeyPressed(Keys.VK_LEFT)) sprites[currentSpr].SetMovementVector((Consts.Physics.AddVectors(sprites[currentSpr].GetMovementVector(), Consts.Physics.CreateVector_NoInvert(-1, 0))));
                if (Consts.Input.IsKeyPressed(Keys.VK_RIGHT)) sprites[currentSpr].SetMovementVector((Consts.Physics.AddVectors(sprites[currentSpr].GetMovementVector(), Consts.Physics.CreateVector_NoInvert(1, 0))));
                if (Consts.Input.IsKeyPressed(Keys.VK_UP)) sprites[currentSpr].SetMovementVector((Consts.Physics.AddVectors(sprites[currentSpr].GetMovementVector(), Consts.Physics.CreateVector_NoInvert(0, -1))));
                if (Consts.Input.IsKeyPressed(Keys.VK_DOWN)) sprites[currentSpr].SetMovementVector((Consts.Physics.AddVectors(sprites[currentSpr].GetMovementVector(), Consts.Physics.CreateVector_NoInvert(0, 1))));
		        if (Consts.Input.IsKeyPressed(Keys.VK_M)) ResetPos();
		        if (Consts.Input.IsKeyPressed(Keys.VK_Q)) ChangeAnim(SpriteEndingAction.Loop);
		        if (Consts.Input.IsKeyPressed(Keys.VK_W)) ChangeAnim(SpriteEndingAction.ReverseLoop);
		        if (Consts.Input.IsKeyPressed(Keys.VK_E)) ChangeAnim(SpriteEndingAction.ReverseOnce);
		        if (Consts.Input.IsKeyPressed(Keys.VK_R)) ChangeAnim(SpriteEndingAction.Stop);

                sprites[currentSpr].SetMovementVector(Consts.Physics.MultiplyVector(sprites[currentSpr].GetMovementVector(), (float)0.95));
                sprites[currentSpr].SetMovementVector(Consts.Physics.LimitMagnitude(sprites[currentSpr].GetMovementVector(), 5));
        		
		        Consts.Graphics.DrawSprite(sprites[currentSpr]);
		        Consts.Graphics.UpdateSprite(sprites[currentSpr]);
        		
		        if (Consts.Graphics.IsSpriteOffscreen(sprites[currentSpr]))
		        {
			       Consts.Text.DrawText("The sprite is not on the screen", Color.White.ToArgb(), GameResources.GameFont("Courier"), 0, 0);
                }
            }

            private void ChangeSprite(int ChangeTo)
            {
                SpriteEndingAction curAction = sprites[currentSpr].GetEndingAction();
                Vector vec = sprites[currentSpr].GetMovementVector();
                Consts.Graphics.ReplayAnimation(sprites[currentSpr]);

                Single tempX = sprites[currentSpr].GetX();
                Single tempY = sprites[currentSpr].GetY();

                currentSpr = ChangeTo;

                sprites[currentSpr].SetMovementVector(vec);
                sprites[currentSpr].SetX(tempX);
			    sprites[currentSpr].SetY(tempY);
			    sprites[currentSpr].SetEndingAction ( curAction);
            }
		
            private void ResetPos()
            {
                sprites[currentSpr].SetMovementVector(Consts.Physics.CreateVector_NoInvert(0, 0));
                Consts.Graphics.MoveSpriteTo(sprites[currentSpr], 0, 0);
            }

            private void ChangeAnim(SpriteEndingAction act)
            {
                sprites[currentSpr].SetEndingAction( act);
                Consts.Graphics.ReplayAnimation(sprites[currentSpr]);  
            }
		
        }

        private class TestAddBitmap : TestSet
        {
            private readonly static string METHS =
                "AddBitmapToSprite";

            private readonly static string INST =
                "Press Space to add an" + Environment.NewLine +
                "another bitmap.";

            private SwinGameVB.Bitmap smallScreen = Consts.Graphics.CreateBitmap(418, 418);
            private SwinGameVB.Bitmap[] tempBitmap;
            private SwinGameVB.Bitmap testingBitmap;
            private Sprite numSprite;
            private int curNum = 1;

            private int[] fps;

            public TestAddBitmap()
                : base(METHS, INST)
            {
                Array.Resize(ref tempBitmap, 1);
                tempBitmap[0] = Consts.Graphics.CreateBitmap(121, 120);
                Consts.Graphics.DrawCircle_OnBitmap(tempBitmap[0], Color.White.ToArgb(), true, 60, 60, 60);
			   Consts.Text.DrawTextLines_ToBitmap_Rectangle(tempBitmap[0], "1", Color.Black.ToArgb(), Color.Transparent.ToArgb(), GameResources.GameFont("ArialLarge"), FontAlignment.AlignCenter,Consts.Shapes.CreateRectangle_Bitmap(tempBitmap[0]));
			    numSprite = Consts.Graphics.CreateSprite_ArrayFPC(tempBitmap, 10, 1);
			    numSprite.SetX(149);
			    numSprite.SetY(149);
            }

            protected override void ToRun(Rectangle drawIn)
            {
                if (Consts.Input.WasKeyTyped(Keys.VK_SPACE))
		        {
			        curNum = curNum + 1;
                    testingBitmap = Consts.Graphics.CreateBitmap(121, 120);
                    Consts.Graphics.DrawCircle_OnBitmap(testingBitmap, Color.White.ToArgb(), true, 60, 60, 60);
                   Consts.Text.DrawTextLines_ToBitmap_Rectangle(testingBitmap, Convert.ToString(curNum), Color.Black.ToArgb(), Color.Transparent.ToArgb(), GameResources.GameFont("ArialLarge"), FontAlignment.AlignCenter,Consts.Shapes.CreateRectangle_Bitmap(testingBitmap));
                    Consts.Graphics.AddBitmapToSprite(numSprite, testingBitmap);

                    fps = numSprite.GetFramesPerCell();

                    Array.Resize(ref fps, fps.Length + 1);
			        fps[fps.Length - 1] = 10;

                    numSprite.SetFramesPerCell(ref fps);
		        }

                Consts.Graphics.DrawSprite(numSprite);
                Consts.Graphics.UpdateSpriteAnimation(numSprite);
            }
        }
    }
}