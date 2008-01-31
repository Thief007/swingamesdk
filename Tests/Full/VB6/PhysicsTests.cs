using System;
using System.Collections.Generic;
using System.Text;
using Color = System.Drawing.Color;
using SwinGameVB;
using Graphics = SwinGameVB.Graphics;

namespace Tests
{
    class PhysicsTests : IGameTestLoader
    {
        #region IGameTestLoader Members

        public void AddTo(List<TestSuite> list)
        {
            TestSuite result = new TestSuite("Physics Tests");
            result.Add(new CollisionSpriteTest());
            result.Add(new CollissionBitmapTest());
			result.Add(new CircleCollissionTest());
            result.Add(new CircleCollissionTest2());
            result.Add(new RectangleCollissionTest());
            result.Add(new VectorMathsTest());
            result.Add(new SpriteTests());
            result.Add(new PointTest());
            result.Add(new TranslationTest());
            result.Add(new VectorIsWithinRectTest());
				result.Add(new VectorAngle());
				result.Add(new PointOutOfRect());
                result.Add(new RectOutOfRect());
                result.Add(new PointOutOfCircle());
                result.Add(new CircleOutOfCircle());

            list.Add(result);
        }

        #endregion

        private class VectorIsWithinRectTest : TestSet
        {
            private Rectangle rect =Consts.Shapes.CreateRectangle(100, 200, 25, 25);

            private Rectangle rect2 =Consts.Shapes.CreateRectangle(300, 200, 50, 50);
            private Vector v =Consts.Physics.CreateVector_NoInvert(2,2);

            private readonly static string METHS =
                "VectorIsWithinRect";

            private readonly static string INST = "This test is used to check if" + Environment.NewLine + "a vector is within a rectangle." + Environment.NewLine + Environment.NewLine +
                                "Use the Mouse to change" + Environment.NewLine + "the vector";

            public VectorIsWithinRectTest()
                : base(METHS, INST)
            {
            }

            protected override void ToRun(Rectangle toDrawIn)
            {

                v =Consts.Input.GetMousePositionAsVector();
                v.setX(Consts.Camera.GameX((int)v.getX()));
                v.setY(Consts.Camera.GameY((int)v.getY()));
                
               Consts.Graphics.DrawRectangle_Rectangle(Color.Red.ToArgb(), rect);
               Consts.Graphics.DrawRectangle_Rectangle(Color.Red.ToArgb(), rect2);
               if (Consts.Physics.VectorIsWithinRect_Rectangle(v, rect))
                {
                    Consts.Graphics.FillRectangle_Rectangle(Color.Yellow.ToArgb(), rect);
                }
                if (Consts.Physics.VectorIsWithinRect(v, rect2.GetX(), rect2.GetY(), (int)rect2.GetWidth(), (int)rect2.GetHeight()))
                {
                    Consts.Graphics.FillRectangle_Rectangle(Color.Yellow.ToArgb(), rect2);
                }
               Consts.Graphics.DrawLine(Color.White.ToArgb(), 0, 0, v.getX(), v.getY());
            }

        }


        private class TranslationTest : TestSet
        {
            private Point2D point = Consts.Shapes.CreatePoint(209, 209);
            private Point2D point2 = Consts.Shapes.CreatePoint(309, 209);
            private Point2D temp = Consts.Shapes.CreatePoint(309, 209);
            private Matrix2D m;
            private Vector v;
            private float rot = 0.0f;

            private readonly static string METHS =
                "Matrix Multiplication, Translation";

            private readonly static string INST = "Press Space to Rotate the vector" + Environment.NewLine + "by 45 degrees.";

            public TranslationTest()
                : base(METHS, INST)
            {

            }

            protected override void ToRun(Rectangle toDrawIn)
            {
                if (Consts.Input.WasKeyTyped(Keys.VK_SPACE))
                {
                    rot = (rot + 45) % 360;
                    v = Consts.Physics.PointToVector(point2);
                    m = Consts.Physics.TranslationMatrix(-point.GetX(), -point.GetY());
                    m = Consts.Physics.Multiply(Consts.Physics.RotationMatrix(rot), m);
                    m = Consts.Physics.Multiply(Consts.Physics.TranslationMatrix(point.GetX(), point.GetY()), m);
                    v = Consts.Physics.Multiply_Vector(m, v);
                    temp = Consts.Shapes.CreatePoint(v.getX(), v.getY());

                    //rot = (rot + 45) % 360;
                    //v = Physics.PointToVector(point2);
                    //m = Physics.TranslationMatrix(-point.X, -point.Y);
                    //m = Physics.Multiply(Physics.RotationMatrix(rot), m);
                    //m = Physics.Multiply(Physics.TranslationMatrix(point.X, point.Y), m);
                    //v = Physics.Multiply(m, v);
                    //temp = Shapes.CreatePoint(v.X, v.Y);
                }

                Consts.Graphics.DrawLine(Color.Gray.ToArgb(), point.GetX(), point.GetY(), point2.GetX(), point2.GetY());
                Consts.Graphics.DrawLine(Color.Gray.ToArgb(), point.GetX(), point.GetY(), temp.GetX(), temp.GetY());

                Consts.Graphics.FillRectangle(Color.White.ToArgb(), point.GetX() - 1, point.GetY() - 1, 3, 3);
                Consts.Graphics.FillRectangle(Color.Red.ToArgb(), point2.GetX() - 1, point2.GetY() - 1, 3, 3);
                Consts.Graphics.FillRectangle(Color.Blue.ToArgb(), temp.GetX() - 1, temp.GetY() - 1, 3, 3);
            }

        }


        private class PointTest : TestSet
        {
            private SwinGameVB.Point2D point =Consts.Shapes.CreatePoint(100, 100);
            private SwinGameVB.Point2D point2 =Consts.Shapes.CreatePoint(150, 150);
            private Rectangle rect =Consts.Shapes.CreateRectangle(200, 200, 50, 50);

            private readonly static string METHS =
                "VectorFromPoints, VectorFromPointToRectangle";

            private readonly static string INST =
                "Use the arrow keys to move" + Environment.NewLine + "the point around.";

            public PointTest()
                : base(METHS, INST)
            {
            }

            protected override void ToRun(Rectangle toDrawIn)
            {

                if (Consts.Input.IsKeyPressed(Keys.VK_UP)) point.SetY(point.GetY() - 1);
                if (Consts.Input.IsKeyPressed(Keys.VK_DOWN)) point.SetY(point.GetY() + 1);
                if (Consts.Input.IsKeyPressed(Keys.VK_LEFT)) point.SetX(point.GetX() - 1);
                if (Consts.Input.IsKeyPressed(Keys.VK_RIGHT)) point.SetX(point.GetX() + 1);

                Vector temp =Consts.Physics.VectorFromPoints(point, point2);
               Consts.Text.DrawText("Vector from Point to Point X:" +temp.getX()+", Y:"+temp.getY(), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 10);

                temp =Consts.Physics.VectorFromPointToRectangle_Point(point, rect);
               Consts.Text.DrawText("Vector from Point to Rectangle X:" + temp.getX() + ", Y:" + temp.getY(), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 30);

                temp =Consts.Physics.VectorFromPointToRectangle_Rectangle(point.GetX(), point.GetY(), rect);
               Consts.Text.DrawText("Vector from Point to Rectangle X:" + temp.getX() + ", Y:" + temp.getY(), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 50);

                temp =Consts.Physics.VectorFromPointToRectangle(point.GetX(),point.GetY(), rect.GetX(),rect.GetY(),(int)rect.GetWidth(),(int)rect.GetHeight());
               Consts.Text.DrawText("Vector from Point to Rectangle X:" + temp.getX() + ", Y:" + temp.getY(), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 70);

               Consts.Graphics.DrawRectangle_Rectangle(Color.Red.ToArgb(), rect);
               Consts.Graphics.FillRectangle(Color.Red.ToArgb(), point2.GetX() - 1, point2.GetY() - 1, 3, 3);
               Consts.Graphics.FillRectangle(Color.White.ToArgb(), point.GetX() - 1, point.GetY() - 1, 3, 3);

                temp =Consts.Physics.PointToVector(point);
               Consts.Text.DrawText("Vector from Point X:" + temp.getX() + ", Y:" + temp.getY(), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 90);                

            }

        }


        private class SpriteTests : TestSet
        {
            private SwinGameVB.Sprite ball =Consts.Graphics.CreateSprite(GameResources.GameImage("SmallBall"));
            private SwinGameVB.Sprite bigball =Consts.Graphics.CreateSprite(GameResources.GameImage("BallImage1"));


            private readonly static string METHS =
                "CalculateAngle, IsSpriteOnScreenAt, CalculateVectorFromTo, VectorFromCenterSpriteToPoint";

            private readonly static string INST =
                "Use the arrow keys to move" + Environment.NewLine + "the small ball." + Environment.NewLine + Environment.NewLine +
                                "Use the ASDW keys to move" + Environment.NewLine + "the large ball.";

            public SpriteTests()
                : base(METHS, INST)
            {
                ball.SetX( 100);
                ball.SetY( 300);
                bigball.SetX( 200);
                bigball.SetY( 300);
            }

            protected override void ToRun(Rectangle toDrawIn)
            {

                if (Consts.Input.IsKeyPressed(Keys.VK_UP)) ball.SetY(ball.GetY() - 1);
                if (Consts.Input.IsKeyPressed(Keys.VK_DOWN)) ball.SetY(ball.GetY() + 1);
                if (Consts.Input.IsKeyPressed(Keys.VK_LEFT)) ball.SetX(ball.GetX() - 1);
                if (Consts.Input.IsKeyPressed(Keys.VK_RIGHT)) ball.SetX(ball.GetX() + 1);

                if (Consts.Input.IsKeyPressed(Keys.VK_W)) bigball.SetY(bigball.GetY() - 1);
                if (Consts.Input.IsKeyPressed(Keys.VK_S)) bigball.SetY(bigball.GetY() + 1);
                if (Consts.Input.IsKeyPressed(Keys.VK_A)) bigball.SetX(bigball.GetX() - 1);
                if (Consts.Input.IsKeyPressed(Keys.VK_D)) bigball.SetX(bigball.GetX() + 1);

               Consts.Graphics.DrawSprite(bigball);
               Consts.Graphics.DrawSprite(ball);
                //IsSpriteOnScreenAt
                if (Consts.Physics.IsSpriteOnScreenAt_Point(ball,Consts.Shapes.CreatePoint(Consts.Camera.ScreenX(209),Consts.Camera.ScreenY(209))))
                {
                   Consts.Text.DrawText("The small ball is on the screen ", Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 10);
                   Consts.Text.DrawText("at X:209, Y:209    X:" + ball.GetX() + ", Y:" + ball.GetY(), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 30);

                }
                else
                {
                   Consts.Text.DrawText("The small ball is not on the screen ", Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 10);
                   Consts.Text.DrawText("at X:209, Y:209    X:" + ball.GetX() + ", Y:" + ball.GetY(), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 30);
                }
                if (Consts.Physics.IsSpriteOnScreenAt(bigball, (int)Consts.Camera.ScreenX(209), (int)Consts.Camera.ScreenY(209)))
                {
                   Consts.Text.DrawText("The large ball is on the screen ", Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 50);
                   Consts.Text.DrawText("at X:209, Y:209    X:" + bigball.GetX() + ", Y:" + bigball.GetY(), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 70);
                }
                else
                {
                   Consts.Text.DrawText("The large ball is not on the screen ", Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 50);
                   Consts.Text.DrawText("at X:209, Y:209    X:" + bigball.GetX() + ", Y:" + bigball.GetY(), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 70);
                }
               Consts.Graphics.DrawPixel(Color.White.ToArgb(), 209, 209);
                //CalculateAngle
                Single temp =Consts.Physics.CalculateAngle(ball, bigball);
                //Text.DrawText(Shapes.CenterPoint(ball).GetX() + ", " +Consts.ShapesCenterPoint(ball).GetY() + " "+Consts.ShapesCenterPoint(bigball).GetX() + ", " +Consts.ShapesCenterPoint(bigball).GetY(), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 110);
               Consts.Text.DrawText("The angle betwen the balls is " + temp, Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 90);

                //CalculateVectorFromTo
                Vector tempV =Consts.Physics.CalculateVectorFromTo(ball, bigball);
               Consts.Text.DrawText("The vector from the small ball ", Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 110);
               Consts.Text.DrawText("to the large ball is X:" + tempV.getX() + ", Y:" + tempV.getY(), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 130);

                //VectorFromCenterSpriteToPoint
                tempV =Consts.Physics.VectorFromCenterSpriteToPoint(ball,Consts.Shapes.CreatePoint(100, 200));
               Consts.Text.DrawText("Vector from ball to point X:" + tempV.getX() + ", Y:" + tempV.getY(), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 150);
               Consts.Graphics.FillRectangle(Color.Red.ToArgb(), 100 - 1, 200 - 1, 3, 3);
            }

        }


        private class VectorMathsTest : TestSet
        {
            private readonly static string METHS =
                "RectangleHasCollidedWithLine";

            private readonly static string INST =
                "This test is used to test the" + Environment.NewLine + "mathematical calculation of" + Environment.NewLine + "a vector." + Environment.NewLine + Environment.NewLine +
                                "UP, DOWN: Change Vector1 X" + Environment.NewLine +
                                "LEFT, RIGHT: Change Vector1 Y" + Environment.NewLine + Environment.NewLine +
                                "W, S: Change Vector2 X" + Environment.NewLine +
                                "A, D: Change Vector2 Y";


            private SwinGameVB.Vector v1 =Consts.Physics.CreateVector_NoInvert(2,2);
            private SwinGameVB.Vector v2 =Consts.Physics.CreateVector(2, 2, true);

            private static SwinGameVB.Bitmap draw = Consts.Graphics.CreateBitmap(300, 32);

            public VectorMathsTest()
                : base(METHS, INST)
            {


            }



            protected override void ToRun(Rectangle toDrawIn)
            {
                if (Consts.Input.IsKeyPressed(Keys.VK_LEFT))
                {
                    v1.setY(v1.getY() - 1);
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_RIGHT))
                {
                    v1.setY(v1.getY() + 1);
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_DOWN))
                {
                    v1.setX(v1.getX() - 1);
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_UP))
                {
                    v1.setX(v1.getX() + 1);
                }

                if (Consts.Input.IsKeyPressed(Keys.VK_A))
                {
                    v2.setY(v2.getY() - 1);
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_D))
                {
                    v2.setY(v2.getY() + 1);
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_S))
                {
                    v2.setX(v2.getX() - 1);
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_W))
                {
                    v2.setX(v2.getX() + 1);
                }

               Consts.Text.DrawText("Vector 1 = X:" + v1.getX() + ", Y:" + v1.getY(), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 10);
               Consts.Text.DrawText("Vector 2 = X:" + v2.getX() + ", Y:" + v2.getY(), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 30);
                Vector temp =Consts.Physics.AddVectors(v1, v2);
               Consts.Text.DrawText("Vector 1 + Vector 2 = X:" + temp.getX() + ", Y:" + temp.getY(), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 50);
                temp =Consts.Physics.SubtractVectors(v1, v2);
               Consts.Text.DrawText("Vector 1 - Vector 2 = X:" + temp.getX() + ", Y:" + temp.getY(), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 70);
                temp =Consts.Physics.LimitMagnitude(v1, 20);
               Consts.Text.DrawText("limt Vector 1 to 20 = X:" + temp.getX() + ", Y:" + temp.getY(), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 90);
                temp =Consts.Physics.InvertVector(v1);
               Consts.Text.DrawText("Vector 1 inverted = X:" + temp.getX() + ", Y:" + temp.getY(), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 110);
                if (Consts.Physics.IsZeroVector(v1))
                {
                   Consts.Text.DrawText("Vector 1 is zero", Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 130);
                }
                else
                {
                   Consts.Text.DrawText("Vector 1 is not zero", Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 130);

                }

            }
        }


        private class RectangleCollissionTest : TestSet
        {
            private readonly static string METHS =
                "RectangleHasCollidedWithLine";

            private readonly static string INST =
                "This test is used to test the" + Environment.NewLine + "collision between the rectangle" + Environment.NewLine + "and a line." + Environment.NewLine + Environment.NewLine + "Use the arrow keys to move the" + Environment.NewLine + "rectangle around.";


            private SwinGameVB.Sprite ball =Consts.Graphics.CreateSprite(GameResources.GameImage("SmallBall"));
            private SwinGameVB.Sprite bigball =Consts.Graphics.CreateSprite(GameResources.GameImage("BallImage1"));

            private static SwinGameVB.Bitmap draw = Consts.Graphics.CreateBitmap(300, 32);

            public RectangleCollissionTest()
                : base(METHS, INST)
            {


            }


            private int X = 0;
            private int Y = 0;

            protected override void ToRun(Rectangle toDrawIn)
            {
                if (Consts.Input.IsKeyPressed(Keys.VK_LEFT))
                {
                    X = X - 1;
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_RIGHT))
                {
                    X = X + 1;
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_DOWN))
                {
                    Y = Y + 1;
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_UP))
                {
                    Y = Y - 1;
                }
                //RectangleHasCollidedWithLine
                if (Consts.Physics.RectangleHasCollidedWithLine_Rectangel(Consts.Shapes.CreateRectangle(X, Y, 50, 50), Consts.Shapes.CreateLine(100, 0, 100, 418)))
                {
                   Consts.Graphics.FillRectangle(Color.Red.ToArgb(), 98, 0, 5, 418);
                }
                if (Consts.Physics.RectangleHasCollidedWithLine_Rectangel(Consts.Shapes.CreateRectangle(X, Y, 50, 50), Consts.Shapes.CreateLine(200, 0, 200, 418)))
                {
                   Consts.Graphics.FillRectangle(Color.Red.ToArgb(), 198, 0, 5, 418);
                }
               Consts.Graphics.DrawRectangle_Rectangle(Color.White.ToArgb(),Consts.Shapes.CreateRectangle(X, Y, 50, 50));
               Consts.Graphics.DrawLine_Line(Color.Yellow.ToArgb(),Consts.Shapes.CreateLine(100, 0, 100, 418));
               Consts.Graphics.DrawLine_Line(Color.Yellow.ToArgb(), Consts.Shapes.CreateLine(200, 0, 200, 418));
            }
        }


        private class CircleCollissionTest2 : TestSet
        {
            private readonly static string METHS =
                "CircularCollision";

            private readonly static string INST =
                "Use the following keys to run" + Environment.NewLine + "the test:" + Environment.NewLine + Environment.NewLine + "UP, DOWN: Increase or decrease" + Environment.NewLine + "the vector of the big ball." + Environment.NewLine +
                                "LEFT, RIGHT: Rotate the vector" + Environment.NewLine + "of the big ball." + Environment.NewLine + Environment.NewLine + "W, S: Increase or decrease" + Environment.NewLine + "the vector of the small ball." + Environment.NewLine +
                                "A, D: Rotate the vector of" + Environment.NewLine + "the small ball" + Environment.NewLine + Environment.NewLine +
                                "T: Toggle the collision test" + Environment.NewLine + "SPACE: See the effect";


            private SwinGameVB.Sprite ball =Consts.Graphics.CreateSprite(GameResources.GameImage("SmallBall"));
            private SwinGameVB.Sprite bigball =Consts.Graphics.CreateSprite(GameResources.GameImage("BallImage1"));

            private static SwinGameVB.Bitmap draw = Consts.Graphics.CreateBitmap(300, 32);

            public CircleCollissionTest2()
                : base(METHS, INST)
            {
                ball.SetMovementX(1);
                ball.SetMovementY(1);
                ball.SetMass( 1);
                ball.SetX(150);
                ball.SetY(150);
                bigball.SetMovementX(1);
                bigball.SetMovementY(1);
                bigball.SetMass( 5);
                bigball.SetX(155);
                bigball.SetY(155);
            }


            protected override void ToRun(Rectangle toDrawIn)
            {
                if (Consts.Input.IsKeyPressed(Keys.VK_DOWN))
                {
                    if (Consts.Physics.Magnitude(bigball.GetMovementVector()) > 2)
                    {
                        bigball.SetMovementVector(Consts.Physics.MultiplyVector(Consts.Physics.GetUnitVector(bigball.GetMovementVector()), Consts.Physics.Magnitude(bigball.GetMovementVector()) - 1));
                    }
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_UP))
                {
                    bigball.SetMovementVector(Consts.Physics.MultiplyVector(Consts.Physics.GetUnitVector(bigball.GetMovementVector()), Consts.Physics.Magnitude(bigball.GetMovementVector()) + 1));
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_LEFT))
                {
                    bigball.SetMovementVector(Consts.Physics.Multiply_Vector(Consts.Physics.RotationMatrix(-1), bigball.GetMovementVector()));
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_RIGHT))
                {
                    bigball.SetMovementVector(Consts.Physics.Multiply_Vector(Consts.Physics.RotationMatrix(1), bigball.GetMovementVector()));
                }

                if (Consts.Input.IsKeyPressed(Keys.VK_S))
                {
                    if (Consts.Physics.Magnitude(ball.GetMovementVector()) > 2)
                    {
                        ball.SetMovementVector(Consts.Physics.MultiplyVector(Consts.Physics.GetUnitVector(ball.GetMovementVector()), Consts.Physics.Magnitude(ball.GetMovementVector()) - 1));
                    }
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_W))
                {
                    ball.SetMovementVector(Consts.Physics.MultiplyVector(Consts.Physics.GetUnitVector(ball.GetMovementVector()), Consts.Physics.Magnitude(ball.GetMovementVector()) + 1));
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_A))
                {
                    ball.SetMovementVector(Consts.Physics.Multiply_Vector(Consts.Physics.RotationMatrix(-1), ball.GetMovementVector()));
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_D))
                {
                    ball.SetMovementVector(Consts.Physics.Multiply_Vector(Consts.Physics.RotationMatrix(1), ball.GetMovementVector()));
                }
                if (Consts.Input.WasKeyTyped(Keys.VK_SPACE))
                {
                    if (Consts.Physics.HaveSpritesCollided(ball, bigball))
                    {
                       Consts.Physics.CircularCollision(ball, bigball);
                       Consts.Graphics.UpdateSprite(ball);
                       Consts.Graphics.UpdateSprite(bigball);
                    }
                    else
                    {
                        ball.SetX(150);
                        ball.SetY(150);
                        bigball.SetX(155);
                        bigball.SetY(155);
                    }
                }

               Consts.Graphics.DrawSprite(bigball);
               Consts.Graphics.DrawSprite(ball);
               float tempX = ball.GetX() + (Consts.Graphics.CurrentWidth(ball) / 2);
               float tempY = ball.GetY() + (Consts.Graphics.CurrentHeight(ball) / 2);
                float bigtempX = bigball.GetX() + (Consts.Graphics.CurrentWidth(bigball) / 2);
                float bigtempY = bigball.GetY() + (Consts.Graphics.CurrentHeight(bigball) / 2);
                Consts.Graphics.DrawLine(Color.RoyalBlue.ToArgb(), tempX, tempY, tempX + ball.GetMovementVector().getX(), tempY + ball.GetMovementVector().getY());
                Consts.Graphics.DrawLine(Color.RoyalBlue.ToArgb(), bigtempX, bigtempY, bigtempX + bigball.GetMovementVector().getX(), bigtempY + bigball.GetMovementVector().getY());
                //Graphics.DrawRectangle(Color.White.ToArgb(), ball.GetX(), ball.GetY(),Consts.GraphicsCurrentWidth(ball),Consts.GraphicsCurrentHeight(ball));
            }
        }


        private class CircleCollissionTest : TestSet
        {
            private readonly static string METHS =
                "HaveBitmapsCollided, CircleHasCollidedWithLine, GetUnitVector, RotationMatrix...";

            private readonly static string INST =
                "Use the arrow keys to move" + Environment.NewLine + "the sprite." + Environment.NewLine + "The blue line represents the" + Environment.NewLine + "vector of the sprite." + Environment.NewLine +
                                "A, D: Rotate the vector." + Environment.NewLine + "W, S: Increase or decrese the" + Environment.NewLine + "magnitude." + Environment.NewLine +
                                "Space: Move ball out of line" + Environment.NewLine + "if colliding.";


            private SwinGameVB.Sprite ball =Consts.Graphics.CreateSprite(GameResources.GameImage("SmallBall"));
            private SwinGameVB.Bitmap mediumball = GameResources.GameImage("BallImage1");

            private static SwinGameVB.Bitmap draw = Consts.Graphics.CreateBitmap(300, 32);

            public CircleCollissionTest() : base(METHS, INST) 
            {
                ball.SetMovementX(1);
                ball.SetMovementY(1);
            }

            private int X = 0;
            private int Y = 0;

            protected override void ToRun(Rectangle toDrawIn)
            {
                if (Consts.Input.IsKeyPressed(Keys.VK_LEFT))
                {
                    ball.SetX(ball.GetX() - 1);
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_RIGHT))
                {
                    ball.SetX(ball.GetX() + 1);
                }

                if (Consts.Input.IsKeyPressed(Keys.VK_S))
                {
                    if (Consts.Physics.Magnitude(ball.GetMovementVector()) > 2)
                    {
                        ball.SetMovementVector(Consts.Physics.MultiplyVector(Consts.Physics.GetUnitVector(ball.GetMovementVector()), Consts.Physics.Magnitude(ball.GetMovementVector()) - 1));
                    }
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_DOWN))
                {
                    ball.SetY(ball.GetY() + 1);
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_UP))
                {
                    ball.SetY(ball.GetY() - 1);
                }

                if (Consts.Input.IsKeyPressed(Keys.VK_W))
                {
                    ball.SetMovementVector(Consts.Physics.MultiplyVector(Consts.Physics.GetUnitVector(ball.GetMovementVector()), Consts.Physics.Magnitude(ball.GetMovementVector()) + 1));
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_A))
                {
                    ball.SetMovementVector(Consts.Physics.Multiply_Vector(Consts.Physics.RotationMatrix(1), ball.GetMovementVector()));
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_D))
                {
                    ball.SetMovementVector(Consts.Physics.Multiply_Vector(Consts.Physics.RotationMatrix(-1), ball.GetMovementVector()));
                }
                //CircleHasCollidedWithLine
                if (Consts.Physics.CircleHasCollidedWithLine(ball,Consts.Shapes.CreateLine(300, 0, 300, 418)))
                {
                    Consts.Graphics.FillRectangle(Color.LightCyan.ToArgb(), 298, 0, 5, 418);
                }
               Consts.Graphics.DrawVerticalLine(Color.Red.ToArgb(), 300, 0, 418);

                //CircleHasCollidedWithLine
                if (Consts.Physics.CircleHasCollidedWithLine(ball,Consts.Shapes.CreateLine(300, 0, 300, 418)) &&Consts.Input.IsKeyPressed(Keys.VK_SPACE))
                {
                   Consts.Physics.CircleCollisionWithLine(ball,Consts.Shapes.CreateLine(300, 0, 300, 418));
                   Consts.Graphics.UpdateSprite(ball);
                }
                

               Consts.Graphics.DrawSprite(ball);
               float tempX = ball.GetX() + (Consts.Graphics.CurrentWidth(ball) / 2);
               float tempY = ball.GetY() + (Consts.Graphics.CurrentHeight(ball) / 2);
               Consts.Graphics.DrawLine(Color.RoyalBlue.ToArgb(), tempX, tempY, tempX + ball.GetMovementVector().getX(), tempY + ball.GetMovementVector().getY());
               Consts.Graphics.DrawRectangle_NoFill(Color.White.ToArgb(), ball.GetX(), ball.GetY(),Consts.Graphics.CurrentWidth(ball),Consts.Graphics.CurrentHeight(ball));
            }
        }


        private class CollissionBitmapTest : TestSet
        {
            private readonly static string METHS =
                "HaveBitmapsCollided, HasBitmapCollidedWithRect";

            private readonly static string INST =
                "Use the arrow keys to move" + Environment.NewLine + "the sprite." + Environment.NewLine +
                                "This test is used to check" + Environment.NewLine + "if the collision works properly.";

            private SwinGameVB.Bitmap smallball = GameResources.GameImage("SmallBall");
            private SwinGameVB.Bitmap mediumball = GameResources.GameImage("BallImage1");

            private static SwinGameVB.Bitmap draw = Consts.Graphics.CreateBitmap(300, 32);

            public CollissionBitmapTest() : base(METHS, INST) { }

            private int X = 0;
            private int Y = 0;

            protected override void ToRun(Rectangle toDrawIn)
            {
                if (Consts.Input.IsKeyPressed(Keys.VK_LEFT))
                {
                    X = X - 1;
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_RIGHT))
                {
                    X = X + 1;
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_DOWN))
                {
                    Y = Y + 1;
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_UP))
                {
                    Y = Y - 1;
                }
                //HaveBitmapsCollided
                Consts.Text.DrawText("Default", Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 5);
                if (Consts.Physics.HaveBitmapsCollided_NoBound(mediumball, 10, 30, smallball, X, Y))
                {
                   Consts.Graphics.FillRectangle(Color.Blue.ToArgb(), 5, 25, mediumball.Height() + 10, mediumball.Width() + 10);
                }
               Consts.Graphics.DrawBitmap(mediumball, 10, 30);
                
               Consts.Graphics.DrawRectangle_NoFill(Color.White.ToArgb(), 10, 30, (int)mediumball.Width(), (int)mediumball.Height());

                if (Consts.Physics.HaveBitmapsCollied_Point(mediumball,Consts.Shapes.CreatePoint(10, 150), smallball,Consts.Shapes.CreatePoint(X, Y)))
                {
                   Consts.Graphics.FillRectangle(Color.Pink.ToArgb(), 5, 145, mediumball.Height() + 10, mediumball.Width() + 10);
                }
               Consts.Graphics.DrawBitmap(mediumball, 10, 150);
               Consts.Graphics.DrawRectangle_NoFill(Color.White.ToArgb(), 10, 150, (int)mediumball.Width(), (int)mediumball.Height());

                if (Consts.Physics.HaveBitmapsCollided_Rectangle(mediumball,Consts.Shapes.CreatePoint(10, 270),Consts.Shapes.CreateRectangle_Bitmap(mediumball), smallball,Consts.Shapes.CreatePoint(X, Y),Consts.Shapes.CreateRectangle_Bitmap(smallball)))
                {
                   Consts.Graphics.FillRectangle(Color.Pink.ToArgb(), 5, 265, mediumball.Height() + 10, mediumball.Width() + 10);
                }
               Consts.Graphics.DrawBitmap(mediumball, 10, 270);
               Consts.Graphics.DrawRectangle_NoFill(Color.White.ToArgb(), 10, 270, (int)mediumball.Width(), (int)mediumball.Height());



               Consts.Text.DrawText("No Bound", Color.White.ToArgb(), GameResources.GameFont("Courier"), 120, 5);
                if (Consts.Physics.HaveBitmapsCollided(mediumball, 120, 30, false, smallball, X, Y, false))
                {
                   Consts.Graphics.FillRectangle(Color.Blue.ToArgb(), 115, 25,  mediumball.Height() + 10, mediumball.Width() + 10);
                }
               Consts.Graphics.DrawBitmap(mediumball, 120, 30);
               Consts.Graphics.DrawRectangle_NoFill(Color.White.ToArgb(), 120, 30, (int)mediumball.Width(), (int)mediumball.Height());

                if (Consts.Physics.HaveBitmapsCollied_Bounded_Point(mediumball,Consts.Shapes.CreatePoint(120, 150),false, smallball,Consts.Shapes.CreatePoint(X, Y), false))
                {
                   Consts.Graphics.FillRectangle(Color.Blue.ToArgb(), 115, 145, mediumball.Height() + 10, mediumball.Width() + 10);
                }
               Consts.Graphics.DrawBitmap(mediumball, 120, 150);
               Consts.Graphics.DrawRectangle_NoFill(Color.White.ToArgb(), 120, 150, (int)mediumball.Width(), (int)mediumball.Height());

                if (Consts.Physics.HaveBitmapsCollided_Bounded_Rectangle(mediumball,Consts.Shapes.CreatePoint(120, 270),Consts.Shapes.CreateRectangle_Bitmap(mediumball), false, smallball,Consts.Shapes.CreatePoint(X, Y), Consts.Shapes.CreateRectangle_Bitmap(smallball),false))
                {
                   Consts.Graphics.FillRectangle(Color.Blue.ToArgb(), 115, 265, mediumball.Height() + 10, mediumball.Width() + 10);
                }
               Consts.Graphics.DrawBitmap(mediumball, 120, 270);
               Consts.Graphics.DrawRectangle_NoFill(Color.White.ToArgb(), 120, 270, (int)mediumball.Width(), (int)mediumball.Height());




               Consts.Text.DrawText("Big ball Bound", Color.White.ToArgb(), GameResources.GameFont("Courier"), 240, 5);
                if (Consts.Physics.HaveBitmapsCollided(mediumball, 240, 30, true, smallball, X, Y, false))
                {
                   Consts.Graphics.FillRectangle(Color.Blue.ToArgb(), 235, 25, mediumball.Height() + 10, mediumball.Width() + 10);
                }
               Consts.Graphics.DrawBitmap(mediumball, 240, 30);
               Consts.Graphics.DrawRectangle_NoFill(Color.White.ToArgb(), 240, 30, (int)mediumball.Width(), (int)mediumball.Height());

               Consts.Text.DrawText("small ball Bound", Color.White.ToArgb(), GameResources.GameFont("Courier"), 240, 150);
                if (Consts.Physics.HaveBitmapsCollied_Bounded_Point(mediumball,Consts.Shapes.CreatePoint(240, 170), false, smallball,Consts.Shapes.CreatePoint(X, Y), true))
                {
                   Consts.Graphics.FillRectangle(Color.Blue.ToArgb(), 235, 165, mediumball.Height() + 10, mediumball.Width() + 10);
                }
               Consts.Graphics.DrawBitmap(mediumball, 240, 170);
               Consts.Graphics.DrawRectangle_NoFill(Color.White.ToArgb(), 240, 170, (int)mediumball.Width(), (int)mediumball.Height());

                //HasBitmapCollidedWithRect
               Consts.Graphics.DrawRectangle_NoFill(Color.DarkGreen.ToArgb(), 400, 350, 10, 10);
                if (Consts.Physics.HasBitmapCollidedWithRect(smallball, X, Y, 400, 350, 10, 10))
                {
                    Consts.Graphics.FillRectangle(Color.LightGreen.ToArgb(), 400, 350, 10, 10);
                }

               Consts.Graphics.DrawRectangle_NoFill(Color.DarkGreen.ToArgb(), 400, 365, 10, 10);
                if (Consts.Physics.HasBitmapCollidedWithRect_Rectangle(smallball, X, Y,Consts.Shapes.CreateRectangle(400, 365, 10, 10)))
                {
                    Consts.Graphics.FillRectangle(Color.LightGreen.ToArgb(), 400, 365, 10, 10);
                }




               Consts.Graphics.DrawBitmap(smallball,X,Y);
               Consts.Graphics.DrawRectangle_NoFill(Color.White.ToArgb(), X, Y, (int)smallball.Width(), (int)smallball.Height());
            }
        }


        private class CollisionSpriteTest : TestSet
        {
            private readonly static string METHS =
                "HasSpriteCollidedWith..., HaveSpritesCollided";

            private readonly static string INST =
                "Use the arrow keys to move the" + Environment.NewLine + "sprite around." + Environment.NewLine +
                                "The sprite with red rectangle" + Environment.NewLine + "uses bounded collisions." + Environment.NewLine +
                                "The sprite with blue rectangle" + Environment.NewLine + "uses non-bounded collisions.";

            private Sprite ship =Consts.Graphics.CreateSprite_MultiFPC(GameResources.GameImage("Ship"),1,2,40,43);

            private Sprite explosion =Consts.Graphics.CreateSprite_MultiFPC(GameResources.GameImage("BlueExplosion2"), 20, 40, 72, 72);
            

            private SwinGameVB.Bitmap smallball = GameResources.GameImage("SmallBall");

            private static SwinGameVB.Bitmap draw = Consts.Graphics.CreateBitmap(300, 32);

            public CollisionSpriteTest() : base(METHS, INST) { }

            protected override void ToRun(Rectangle toDrawIn)
            {
                //Graphics.ClearScreen(Color.White.ToArgb());
                //ship.Movement.SetTo(Consts.Physics.CreateVector(1,1));
               Consts.Graphics.UpdateSpriteAnimation(ship);
                ship.SetUsePixelCollision( true);
                //Text.DrawText(ship.Movement.GetX() + ", " + ship.Movement.GetY(), Color.White.ToArgb(), GameResources.GameFont("Courier"), 10, 200);
                
                if (Consts.Input.IsKeyPressed(Keys.VK_LEFT))
                {
                    ship.SetX(ship.GetX() - 1);
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_RIGHT))
                {
                    ship.SetX(ship.GetX() + 1);
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_DOWN))
                {
                    ship.SetY(ship.GetY() + 1);
                }
                if (Consts.Input.IsKeyPressed(Keys.VK_UP))
                {
                    ship.SetY(ship.GetY() - 1);
                }

                //HasSpriteCollidedX
               Consts.Graphics.DrawVerticalLine(Color.Red.ToArgb(), 400, 0, 418);
                if (Consts.Physics.HasSpriteCollidedX(ship, 400, CollisionDetectionRange.CollisionRangeEquals))
                {
                   Consts.Graphics.DrawRectangle_NoFill(Color.White.ToArgb(), 395, 405, 10, 10);
                }
                if (Consts.Physics.HasSpriteCollidedX(ship, 400, CollisionDetectionRange.CollisionRangeGreaterThan))
                {
                   Consts.Graphics.DrawRectangle_NoFill(Color.White.ToArgb(), 407, 405, 10, 10);
                }
                if (Consts.Physics.HasSpriteCollidedX(ship, 400, CollisionDetectionRange.CollisionRangeLessThan))
                {
                   Consts.Graphics.DrawRectangle_NoFill(Color.White.ToArgb(), 383, 405, 10, 10);
                }

                //HasSpriteCollidedY
               Consts.Graphics.DrawHorizontalLine(Color.LightBlue.ToArgb(), 400, 0, 418);
                if (Consts.Physics.HasSpriteCollidedY(ship, 400, CollisionDetectionRange.CollisionRangeEquals))
                {
                   Consts.Graphics.DrawRectangle_NoFill(Color.White.ToArgb(), 10, 395, 10, 10);
                }
                if (Consts.Physics.HasSpriteCollidedY(ship, 400, CollisionDetectionRange.CollisionRangeGreaterThan))
                {
                   Consts.Graphics.DrawRectangle_NoFill(Color.White.ToArgb(), 10, 407, 10, 10);
                }
                if (Consts.Physics.HasSpriteCollidedY(ship, 400, CollisionDetectionRange.CollisionRangeLessThan))
                {
                   Consts.Graphics.DrawRectangle_NoFill(Color.White.ToArgb(), 10, 383, 10, 10);
                }

                //HasSpriteCollidedWithRect

                Consts.Graphics.DrawRectangle_NoFill(Color.DarkGreen.ToArgb(), 10, 10, 10, 10);
                if (Consts.Physics.HasSpriteCollidedWithRect(ship, 10, 10, 10, 10))
                {
                    Consts.Graphics.FillRectangle(Color.LightGreen.ToArgb(), 10, 10, 10, 10);
                }

                Consts.Graphics.DrawRectangle_NoFill(Color.DarkGreen.ToArgb(), 10, 25, 10, 10);
                if (Consts.Physics.HasSpriteCollidedWithRect_Rectangle(ship,Consts.Shapes.CreateRectangle(10, 25, 10, 10)))
                {
                    Consts.Graphics.FillRectangle(Color.LightGreen.ToArgb(), 10, 25, 10, 10);
                }

                //HaveSpritesCollided
                explosion.SetUsePixelCollision( true);

                explosion.SetX(70);
                explosion.SetY(10);
               Consts.Graphics.UpdateSpriteAnimation(explosion);
                //Graphics.FillRectangle(Color.White.ToArgb(), 64, 4, 45, 45);
                if (Consts.Physics.HaveSpritesCollided(ship, explosion))
                {
                   Consts.Graphics.DrawRectangle_NoFill(Color.Blue.ToArgb(), 65, 5, 78, 78);
                   Consts.Graphics.DrawRectangle_NoFill(Color.Blue.ToArgb(), 64, 4, 80, 80);
                }
               Consts.Graphics.DrawSprite(explosion);

                //HasSpriteCollidedWithBitmap
                if (Consts.Physics.HasSpriteCollidedWithBitmap_Bound(ship, smallball, 10,100))
                {
                    Consts.Graphics.DrawRectangle(Color.Pink.ToArgb(), true, 5, 95, smallball.Width() + 10, smallball.Height() + 10);
                }
               Consts.Graphics.DrawBitmap(smallball, 10, 100);
               Consts.Graphics.DrawRectangle_NoFill(Color.White.ToArgb(), 10, 100, (int)smallball.Width(), (int)smallball.Height());

                if (Consts.Physics.HasSpriteCollidedWithBitmap(ship, smallball, 10, 150, false))
                {
                   Consts.Graphics.DrawRectangle(Color.Blue.ToArgb(), true, 5, 145, smallball.Width() + 10, smallball.Height() + 10);
                }
               Consts.Graphics.DrawBitmap(smallball, 10, 150);
               Consts.Graphics.DrawRectangle_NoFill(Color.White.ToArgb(), 10, 150, (int)smallball.Width(), (int)smallball.Height());
                //using points
                if (Consts.Physics.HasSpriteCollidedWithBitmap_Bound_Rectangle(ship, smallball,Consts.Shapes.CreatePoint(70,100),Consts.Shapes.CreateRectangle_Bitmap(smallball), false))
                {
                   Consts.Graphics.DrawRectangle(Color.Blue.ToArgb(), true, 65, 95, smallball.Width() + 10, smallball.Height() + 10);
                }
               Consts.Graphics.DrawBitmap(smallball, 70, 100);
               Consts.Graphics.DrawRectangle_NoFill(Color.White.ToArgb(), 70, 100, (int)smallball.Width(), (int)smallball.Height());

                if (Consts.Physics.HasSpriteCollidedWithBitmap_Bound_Point(ship, smallball,Consts.Shapes.CreatePoint(70, 150), false))
                {
                   Consts.Graphics.DrawRectangle(Color.Blue.ToArgb(), true, 65, 145, smallball.Width() + 10, smallball.Height() + 10);
                }
               Consts.Graphics.DrawBitmap(smallball, 70, 150);
               Consts.Graphics.DrawRectangle_NoFill(Color.White.ToArgb(), 70, 150, (int)smallball.Width(), (int)smallball.Height());


               Consts.Graphics.DrawSprite(ship);
               Consts.Graphics.DrawRectangle_NoFill(Color.White.ToArgb(), ship.GetX(), ship.GetY(),Consts.Graphics.CurrentWidth(ship),Consts.Graphics.CurrentHeight(ship));
            }
        }

		private class VectorAngle : TestSet
			{
				private readonly static int CX = 209;
				private readonly static int CY = 209;
				private readonly static int RADIUS = 20;
				private readonly static int LINE_LENGTH = 100;
				
				private Vector v1, v2;

                private readonly static string METHS =
                    "CalculateAngle, Matrix Multiply, Rotation Matrix";

                private readonly static string INST =
                    "LEFT, RIGHT: Control the white" + Environment.NewLine + "line." + Environment.NewLine +
                                    "UP, DOWN: Control the red line" + Environment.NewLine +
                                    "Space: Move the red over white";

            public VectorAngle() : base(METHS, INST) 
				{
					v1 =Consts.Physics.CreateVector_NoInvert(LINE_LENGTH, 0);
					v2 =Consts.Physics.CreateVector_NoInvert(0 , LINE_LENGTH); 
				}

            protected override void ToRun(Rectangle toDrawIn)
            {
               Consts.Graphics.FillCircle(Color.Red.ToArgb(), CX, CY, RADIUS);
               Consts.Graphics.DrawLine(Color.Red.ToArgb(), CX, CY, CX + v1.getX(), CY + v1.getY());
					Consts.Graphics.DrawLine(Color.White.ToArgb(), CX, CY, CX + v2.getX(), CY + v2.getY());
					
 					 float angle =Consts.Physics.CalculateAngleBetween(v1, v2);
					 float v1a =Consts.Physics.CalculateAngle_Number(0, 0, v1.getX(), v1.getY());
					 float v2a =Consts.Physics.CalculateAngle_Number(0, 0, v2.getX(), v2.getY());
					 
					Consts.Text.DrawText("White: " + v2a, Color.White.ToArgb(), GameResources.GameFont("Courier"), 5, 50);
					Consts.Text.DrawText("Red: " + v1a, Color.Red.ToArgb(), GameResources.GameFont("Courier"), 5, 70);
					Consts.Text.DrawText("Between: " + angle, Color.White.ToArgb(), GameResources.GameFont("Courier"), 5, 90);
					
					 float rot = 0;
					 Matrix2D rm = null;

					 if (Consts.Input.IsKeyPressed(Keys.VK_LEFT)) rot = 5;
					 else if (Consts.Input.IsKeyPressed(Keys.VK_RIGHT)) rot = -5;

					 if (rot != 0)
					 {
						rm =Consts.Physics.RotationMatrix(rot);
						v2 =Consts.Physics.Multiply_Vector(rm, v2);
					 }

					 rot = 0;

					 if (Consts.Input.IsKeyPressed(Keys.VK_UP)) rot = 5;
					 else if (Consts.Input.IsKeyPressed(Keys.VK_DOWN)) rot = -5;

					 if (rot != 0)
					 {
						rm =Consts.Physics.RotationMatrix(rot);
						v1 =Consts.Physics.Multiply_Vector(rm, v1);
					 }

					 if (Consts.Input.WasKeyTyped(Keys.VK_SPACE))
					 {
						rm =Consts.Physics.RotationMatrix(angle);
						v1 =Consts.Physics.Multiply_Vector(rm, v1);
					 }
				}				
			}

		private class PointOutOfRect : TestSet
			{
				private readonly static int RW = 100;
				private readonly static int RH = 100;
				private readonly static int RX = 159;
				private readonly static int RY = 159;
				private readonly static int LINE_LENGTH = 100;
				
				private Point2D p;
				private Rectangle rect;
				private Vector movement, mvOut;

                private readonly static string METHS =
                    "VectorOutOfRectFromPoint";

                private readonly static string INST =
                    "Use the arrow keys to move" + Environment.NewLine + "the point." + Environment.NewLine +
                                    "A, Z: Rotate the movement" + Environment.NewLine +
                                    "Space: Move the point out";

            public PointOutOfRect() : base(METHS, INST) 
				{
					p =Consts.Shapes.CreatePoint(100 / 2 + RX, 100 / 2 + RY);
					rect =Consts.Shapes.CreateRectangle(RX, RY, RW, RH);

					movement =Consts.Physics.CreateVector_NoInvert(100, 0);
				}

            protected override void ToRun(Rectangle toDrawIn)
            {
					int r = 1, r2 = 2;
					
					if (Consts.Input.IsKeyPressed(Keys.VK_A)) movement =Consts.Physics.Multiply_Vector(Consts.Physics.RotationMatrix(-4.0f), movement);
					if (Consts.Input.IsKeyPressed(Keys.VK_Z)) movement =Consts.Physics.Multiply_Vector(Consts.Physics.RotationMatrix(4.0f), movement);

					if (Consts.Input.IsKeyPressed(Keys.VK_UP)) p.SetY(p.GetY() - 5);
					if (Consts.Input.IsKeyPressed(Keys.VK_DOWN)) p.SetY(p.GetY() + 5);
					if (Consts.Input.IsKeyPressed(Keys.VK_LEFT)) p.SetX(p.GetX() - 5);
					if (Consts.Input.IsKeyPressed(Keys.VK_RIGHT)) p.SetX(p.GetX() + 5);

					mvOut =Consts.Physics.VectorOutOfRectFromPoint(p, rect, movement);
					
					if (Consts.Input.IsKeyPressed(Keys.VK_SPACE))
					{
						p.SetY( p.GetY()+ mvOut.getY());
						p.SetX( p.GetX()+ mvOut.getX());
						mvOut =Consts.Physics.CreateVector_NoInvert(0, 0);
					}

                    Consts.Graphics.ClearScreen_ToColour(Color.Black.ToArgb());

                    Consts.Graphics.DrawRectangle_NoFill(Color.Red.ToArgb(), RX, RY, RW, RH);
                    Consts.Graphics.DrawRectangle_NoFill(Color.White.ToArgb(), p.GetX() - r, p.GetY() - r, r2, r2);
                    Consts.Graphics.DrawLine(Color.White.ToArgb(), p.GetX(), p.GetY(), p.GetX() + movement.getX(), p.GetY() + movement.getY());

					if (false == ((mvOut.getX() == 0) && (mvOut.getY() == 0)))
					{
                        Consts.Graphics.DrawLine(Color.Green.ToArgb(), p.GetX(), p.GetY(), p.GetX() + mvOut.getX(), p.GetY() + mvOut.getY());
					}

				}
				
			}

        private class RectOutOfRect : TestSet
        {
            private readonly static int RW = 100;
            private readonly static int RH = 100;
            private readonly static int RX = 159;
            private readonly static int RY = 159;
            private readonly static int LINE_LENGTH = 100;
		

            private Single px, py;
            private int halfH, halfW;
            private CollisionSide enterRectFrom;
            private Rectangle mvRect, tgtRect;
            private Vector movement, mvOut;

            private readonly static string METHS =
                "VectorOutOfRectFromRect";

            private readonly static string INST =
                "Use the arrow keys to move" + Environment.NewLine + "the point." + Environment.NewLine +
                                "A, Z: Rotate the movement" + Environment.NewLine +
                                "Space: Move the rectangle out" + Environment.NewLine + Environment.NewLine +
                                "0 = set movement to 0 degrees" + Environment.NewLine +
                                "9 = set movement to 90 degrees" + Environment.NewLine +
                                "8 = set movement to 180 degrees" + Environment.NewLine +
                                "2 = set movement to 270 degrees" + Environment.NewLine;

            public RectOutOfRect()
                : base(METHS, INST)
            {

                movement =Consts.Physics.CreateVector_NoInvert(100, 0);
                halfH = 10;
                halfW = 5;

		        movement =Consts.Physics.CreateVector_NoInvert(100, 0);
		
		        tgtRect =Consts.Shapes.CreateRectangle(RX, RY, RW, RH);
                mvRect =Consts.Shapes.CreateRectangle(100 + RX, 100 + RY, 10, 20);
            }

            protected override void ToRun(Rectangle toDrawIn)
            {
                int r = 1, r2 = 2;

                if (Consts.Input.IsKeyPressed(Keys.VK_A)) movement =Consts.Physics.Multiply_Vector(Consts.Physics.RotationMatrix(-4.0f), movement);
                if (Consts.Input.IsKeyPressed(Keys.VK_Z)) movement =Consts.Physics.Multiply_Vector(Consts.Physics.RotationMatrix(4.0f), movement);

                if (Consts.Input.IsKeyPressed(Keys.VK_UP)) mvRect.SetY(mvRect.GetY() - 5);
                if (Consts.Input.IsKeyPressed(Keys.VK_DOWN)) mvRect.SetY(mvRect.GetY() + 5);
                if (Consts.Input.IsKeyPressed(Keys.VK_LEFT)) mvRect.SetX( mvRect.GetX() - 5);
                if (Consts.Input.IsKeyPressed(Keys.VK_RIGHT)) mvRect.SetX(mvRect.GetX() + 5);

                mvOut =Consts.Physics.VectorOutOfRectFromRect(mvRect, tgtRect, movement);

                enterRectFrom =Consts.Shapes.GetSideForCollisionTest(movement);
                
			    px =Consts.Shapes.RectangleLeft(mvRect); 
			    py =Consts.Shapes.RectangleTop(mvRect);
                switch (enterRectFrom)
	            {
		            case CollisionSide.Bottom:
                     break;
                    case CollisionSide.BottomLeft:
                        px =Consts.Shapes.RectangleRight(mvRect);
                     break;
                    case CollisionSide.BottomRight:
                     break;
                    case CollisionSide.Left:
                        px =Consts.Shapes.RectangleRight(mvRect);
                     break;
                    case CollisionSide.LeftRight:
                     break;
                    case CollisionSide.None:
                     break;
                    case CollisionSide.Right:
                     break;
                    case CollisionSide.Top:
                        py =Consts.Shapes.RectangleBottom(mvRect);
                     break;
                    case CollisionSide.TopBottom:
                     break;
                    case CollisionSide.TopLeft:
                        
                        px =Consts.Shapes.RectangleRight(mvRect);
                        py =Consts.Shapes.RectangleBottom(mvRect);
                     break;
                    case CollisionSide.TopRight:
                        py =Consts.Shapes.RectangleBottom(mvRect);
                     break;default:
                     break;
	            }

                if (Consts.Input.IsKeyPressed(Keys.VK_SPACE))
                {
                    mvRect.SetY (mvRect.GetY()+ (int)mvOut.getY());
                    mvRect.SetX(mvRect.GetX() + (int)mvOut.getX());
                    mvOut =Consts.Physics.CreateVector_NoInvert(0, 0);
                }

               Consts.Graphics.ClearScreen_ToColour(Color.Black.ToArgb());

               Consts.Graphics.DrawRectangle_NoFill(Color.Red.ToArgb(), RX, RY, RW, RH);
               Consts.Graphics.DrawRectangle_NoFill(Color.White.ToArgb(), mvRect.GetX(), mvRect.GetY(), (int)mvRect.GetWidth(), (int)mvRect.GetHeight());
               Consts.Graphics.DrawLine(Color.White.ToArgb(), mvRect.GetX() + halfW, mvRect.GetY() + halfH, mvRect.GetX() + halfW + movement.getX(), mvRect.GetY() + halfH + movement.getY());

                if (false == ((mvOut.getX() == 0) && (mvOut.getY() == 0)))
                {

                    Consts.Graphics.DrawLine(Color.Gray.ToArgb(), mvRect.GetX(), mvRect.GetY(), mvRect.GetX() + mvOut.getX(), mvRect.GetY() + mvOut.getY());
                    Consts.Graphics.DrawRectangle_NoFill(Color.Green.ToArgb(), mvRect.GetX() + mvOut.getX(), mvRect.GetY() + mvOut.getY(), (int)mvRect.GetWidth(), (int)mvRect.GetHeight());
                   Consts.Graphics.DrawLine(Color.Green.ToArgb(), px, py, px + mvOut.getX() , py + mvOut.getY() );

                }

            }

        }


        private class PointOutOfCircle : TestSet
        {
            private readonly static int CR = 100;
            private readonly static int CX = 209;
            private readonly static int CY = 209;
            private readonly static int LINE_LENGTH = 100;

            private Point2D p, c;
            private Vector movement, mvOut;

            private readonly static string METHS =
                "VectorOutOfCircleFromPoint";

            private readonly static string INST =
                "Use the arrow keys to move" + Environment.NewLine + "the point." + Environment.NewLine +
                                "A, Z: Rotate the movement" + Environment.NewLine +
                                "Space: Move the point out";

            public PointOutOfCircle()
                : base(METHS, INST)
            {
                p =Consts.Shapes.CreatePoint(100 / 2 + CX, 100 / 2 + CY);
                c =Consts.Shapes.CreatePoint(CX, CY);

                movement =Consts.Physics.CreateVector_NoInvert(100, 0);
            }

            protected override void ToRun(Rectangle toDrawIn)
            {
                int r = 1, r2 = 2;

                if (Consts.Input.IsKeyPressed(Keys.VK_A)) movement =Consts.Physics.Multiply_Vector(Consts.Physics.RotationMatrix(-4.0f), movement);
                if (Consts.Input.IsKeyPressed(Keys.VK_Z)) movement =Consts.Physics.Multiply_Vector(Consts.Physics.RotationMatrix(4.0f), movement);

                if (Consts.Input.IsKeyPressed(Keys.VK_UP)) p.SetY(p.GetY() - 5);
                if (Consts.Input.IsKeyPressed(Keys.VK_DOWN)) p.SetY(p.GetY() + 5);
                if (Consts.Input.IsKeyPressed(Keys.VK_LEFT)) p.SetX(  p.GetX() - 5);
                if (Consts.Input.IsKeyPressed(Keys.VK_RIGHT)) p.SetX(p.GetX() + 5);

                mvOut =Consts.Physics.VectorOutOfCircleFromPoint(p, c, CR, movement);

                if (Consts.Input.IsKeyPressed(Keys.VK_SPACE))
                {
                    p.SetY (p.GetY()+ mvOut.getY());
                    p.SetX (p.GetX()+  mvOut.getX());
                    mvOut =Consts.Physics.CreateVector_NoInvert(0, 0);
                }

                //Graphics.ClearScreen(Color.Black.ToArgb());

               Consts.Graphics.DrawCircle_NoFill(Color.Red.ToArgb(), CX, CY, CR);
               Consts.Graphics.DrawRectangle_NoFill(Color.White.ToArgb(), p.GetX() - r, p.GetY() - r, r2, r2);
               Consts.Graphics.DrawLine(Color.White.ToArgb(), p.GetX(), p.GetY(), p.GetX() + movement.getX(), p.GetY() + movement.getY());

                if (false == ((mvOut.getX() == 0) && (mvOut.getY() == 0)))
                {
                   Consts.Graphics.DrawLine(Color.Green.ToArgb(), p.GetX(), p.GetY(), p.GetX() + mvOut.getX(), p.GetY() + mvOut.getY());
                }

            }

        }


        private class CircleOutOfCircle : TestSet
        {
            private readonly static int CR = 100;
            private readonly static int CX = 209;
            private readonly static int CY = 209;
            private readonly static int LINE_LENGTH = 100;
            private readonly static int PR = 10;

            private Point2D p, c;
            private Vector movement, mvOut;

            private readonly static string METHS =
                "VectorOutOfCircleFromCircle";

            private readonly static string INST =
                "Use the arrow keys to move" + Environment.NewLine + "the point." + Environment.NewLine +
                                "A, Z: Rotate the movement" + Environment.NewLine +
                                "Space: Move the point out";

            public CircleOutOfCircle()
                : base(METHS, INST)
            {
                p =Consts.Shapes.CreatePoint(100 / 2 + CX, 100 / 2 + CY);
                c =Consts.Shapes.CreatePoint(CX, CY);

                movement =Consts.Physics.CreateVector_NoInvert(100, 0);
            }

            protected override void ToRun(Rectangle toDrawIn)
            {
                int r = 1, r2 = 2;

                if (Consts.Input.IsKeyPressed(Keys.VK_A)) movement =Consts.Physics.Multiply_Vector(Consts.Physics.RotationMatrix(-4.0f), movement);
                if (Consts.Input.IsKeyPressed(Keys.VK_Z)) movement =Consts.Physics.Multiply_Vector(Consts.Physics.RotationMatrix(4.0f), movement);

                if (Consts.Input.IsKeyPressed(Keys.VK_UP)) p.SetY(p.GetY() - 5);
                if (Consts.Input.IsKeyPressed(Keys.VK_DOWN)) p.SetY(p.GetY() + 5);
                if (Consts.Input.IsKeyPressed(Keys.VK_LEFT)) p.SetX(p.GetX() - 5);
                if (Consts.Input.IsKeyPressed(Keys.VK_RIGHT)) p.SetX(p.GetX() + 5);

                mvOut =Consts.Physics.VectorOutOfCircleFromCircle(p,PR, c, CR, movement);

                if (Consts.Input.IsKeyPressed(Keys.VK_SPACE))
                {
                    p.SetY (p.GetY()+  mvOut.getY());
                    p.SetX (p.GetX()+  mvOut.getX());
                    mvOut =Consts.Physics.CreateVector_NoInvert(0, 0);
                }


               Consts.Graphics.DrawCircle_NoFill(Color.Red.ToArgb(), CX, CY, CR);
               Consts.Graphics.DrawCircle_NoFill(Color.White.ToArgb(), p.GetX(), p.GetY(), PR);
               Consts.Graphics.DrawLine(Color.White.ToArgb(), p.GetX(), p.GetY(), p.GetX() + movement.getX(), p.GetY() + movement.getY());

                if (false == ((mvOut.getX() == 0) && (mvOut.getY() == 0)))
                {
                   Consts.Graphics.DrawLine(Color.Green.ToArgb(), p.GetX(), p.GetY(), p.GetX() + mvOut.getX(), p.GetY() + mvOut.getY());
                   Consts.Graphics.DrawCircle_NoFill(Color.Green.ToArgb(), p.GetX() + mvOut.getX(), p.GetY() + mvOut.getY(), PR);
                }

            }

        }

    }

}
