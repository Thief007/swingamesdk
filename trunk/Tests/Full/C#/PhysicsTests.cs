using System;
using System.Collections.Generic;
using System.Text;
using Color = System.Drawing.Color;
using Rectangle = System.Drawing.Rectangle;
using SwinGame;
using Graphics = SwinGame.Graphics;

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
            private Rectangle rect = Shapes.CreateRectangle(100, 200, 25, 25);

            private Rectangle rect2 = Shapes.CreateRectangle(300, 200, 50, 50);
            private Vector v = Physics.CreateVector(2,2);

            private readonly static string METHS =
                "VectorIsWithinRect";

            private readonly static string INST =
                "Mouse to move vector" + Environment.NewLine
                ;

            public VectorIsWithinRectTest()
                : base(METHS, INST)
            {
            }

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {

                v = Input.GetMousePositionAsVector();
                v.X = Camera.GameX((int)v.X);
                v.Y = Camera.GameY((int)v.Y);
                
                Graphics.DrawRectangle(Color.Red, rect);
                Graphics.DrawRectangle(Color.Red, rect2);
                if (Physics.VectorIsWithinRect(v, rect))
                {
                    Graphics.FillRectangle(Color.Yellow, rect);
                }
                if (Physics.VectorIsWithinRect(v, rect2.X, rect2.Y, rect2.Width, rect2.Height))
                {
                    Graphics.FillRectangle(Color.Yellow, rect2);
                }
                Graphics.DrawLine(Color.White, 0, 0, v.X, v.Y);
            }

        }


        private class TranslationTest : TestSet
        {
            private SwinGame.Point2D point = Shapes.CreatePoint(209, 209);
            private SwinGame.Point2D point2 = Shapes.CreatePoint(309, 209);
            private SwinGame.Point2D temp = Shapes.CreatePoint(309, 209);
            private Matrix2D m;
            private Vector v;
				private float rot = 0.0f;

            private readonly static string METHS =
                "Matrix Multiplication, Translation";

            private readonly static string INST =
                "Space to Rotate by 45deg";

            public TranslationTest()
                : base(METHS, INST)
            {

            }

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                if (Input.WasKeyTyped(Keys.VK_SPACE))
                {
						  rot = (rot + 45) % 360;
                    v = Physics.PointToVector(point2);
                    m = Physics.TranslationMatrix(-point.X,-point.Y);
						  m = Physics.Multiply(Physics.RotationMatrix(rot), m);
						  m = Physics.Multiply(Physics.TranslationMatrix(point.X,point.Y), m);
						  v = Physics.Multiply(m, v);
                    temp = Shapes.CreatePoint( v.X,  v.Y);
                }

					 Graphics.DrawLine(Color.Gray, point.X, point.Y, point2.X, point2.Y);
					 Graphics.DrawLine(Color.Gray, point.X, point.Y, temp.X, temp.Y);
										
                Graphics.FillRectangle(Color.White, point.X - 1, point.Y - 1, 3, 3);
                Graphics.FillRectangle(Color.Red, point2.X - 1, point2.Y - 1, 3, 3);
                Graphics.FillRectangle(Color.Blue, temp.X - 1, temp.Y - 1, 3, 3);
            }

        }


        private class PointTest : TestSet
        {
            private SwinGame.Point2D point = Shapes.CreatePoint(100, 100);
            private SwinGame.Point2D point2 = Shapes.CreatePoint(150, 150);
            private Rectangle rect = Shapes.CreateRectangle(200, 200, 50, 50);

            private readonly static string METHS =
                "VectorFromPoints, VectorFromPointToRectangle";

            private readonly static string INST =
                "Arrows move point" + Environment.NewLine;

            public PointTest()
                : base(METHS, INST)
            { }

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {

                if (Input.IsKeyPressed(Keys.VK_UP)) point.Y = point.Y - 1;
                if (Input.IsKeyPressed(Keys.VK_DOWN)) point.Y = point.Y + 1;
                if (Input.IsKeyPressed(Keys.VK_LEFT)) point.X = point.X - 1;
                if (Input.IsKeyPressed(Keys.VK_RIGHT)) point.X = point.X + 1;

                Vector temp = Physics.VectorFromPoints(point, point2);
                Text.DrawText("Vector from Point to Point X:" +temp.X+", Y:"+temp.Y, Color.White, GameResources.GameFont("Courier"), 10, 10);

                temp = Physics.VectorFromPointToRectangle(point, rect);
                Text.DrawText("Vector from Point to Rectangle X:" + temp.X + ", Y:" + temp.Y, Color.White, GameResources.GameFont("Courier"), 10, 30);

                temp = Physics.VectorFromPointToRectangle(point.X, point.Y, rect);
                Text.DrawText("Vector from Point to Rectangle X:" + temp.X + ", Y:" + temp.Y, Color.White, GameResources.GameFont("Courier"), 10, 50);

                temp = Physics.VectorFromPointToRectangle(point.X,point.Y, rect.X,rect.Y,rect.Width,rect.Height);
                Text.DrawText("Vector from Point to Rectangle X:" + temp.X + ", Y:" + temp.Y, Color.White, GameResources.GameFont("Courier"), 10, 70);

                Graphics.DrawRectangle(Color.Red, rect);
                Graphics.FillRectangle(Color.Red, point2.X - 1, point2.Y - 1, 3, 3);
                Graphics.FillRectangle(Color.White, point.X - 1, point.Y - 1, 3, 3);

                temp = Physics.PointToVector(point);
                Text.DrawText("Vector from Point X:" + temp.X + ", Y:" + temp.Y, Color.White, GameResources.GameFont("Courier"), 10, 90);                

            }

        }


        private class SpriteTests : TestSet
        {
            private SwinGame.Sprite ball = Graphics.CreateSprite(GameResources.GameImage("SmallBall"));
            private SwinGame.Sprite bigball = Graphics.CreateSprite(GameResources.GameImage("BallImage1"));


            private readonly static string METHS =
                "CalculateAngle, IsSpriteOnScreenAt, CalculateVectorFromTo, VectorFromCenterSpriteToPoint";

            private readonly static string INST =
                "Arrows move small ball" + Environment.NewLine +
                     "ASDW move large ball" + Environment.NewLine 
                ;

            public SpriteTests()
                : base(METHS, INST)
            {
                ball.xPos = 100;
                ball.yPos = 300;
                bigball.xPos = 200;
                bigball.yPos = 300;
            }

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {

                if (Input.IsKeyPressed(Keys.VK_UP)) ball.Y = ball.Y - 1;
                if (Input.IsKeyPressed(Keys.VK_DOWN)) ball.Y = ball.Y + 1;
                if (Input.IsKeyPressed(Keys.VK_LEFT)) ball.X = ball.X - 1;
                if (Input.IsKeyPressed(Keys.VK_RIGHT)) ball.X = ball.X + 1;

                if (Input.IsKeyPressed(Keys.VK_W)) bigball.Y = bigball.Y - 1;
                if (Input.IsKeyPressed(Keys.VK_S)) bigball.Y = bigball.Y + 1;
                if (Input.IsKeyPressed(Keys.VK_A)) bigball.X = bigball.X - 1;
                if (Input.IsKeyPressed(Keys.VK_D)) bigball.X = bigball.X + 1;

                Graphics.DrawSprite(bigball);
                Graphics.DrawSprite(ball);
                //IsSpriteOnScreenAt
                if (Physics.IsSpriteOnScreenAt(ball, Shapes.CreatePoint(Camera.ScreenX(209), Camera.ScreenY(209))))
                {
                    Text.DrawText("The small ball is on the screen ", Color.White, GameResources.GameFont("Courier"), 10, 10);
                    Text.DrawText("at X:209, Y:209    X:" + ball.xPos + ", Y:" + ball.yPos, Color.White, GameResources.GameFont("Courier"), 10, 30);

                }
                else
                {
                    Text.DrawText("The small ball is not on the screen ", Color.White, GameResources.GameFont("Courier"), 10, 10);
                    Text.DrawText("at X:209, Y:209    X:" + ball.xPos + ", Y:" + ball.yPos, Color.White, GameResources.GameFont("Courier"), 10, 30);
                }
                if (Physics.IsSpriteOnScreenAt(bigball, (int)Camera.ScreenX(209), (int)Camera.ScreenY(209)))
                {
                    Text.DrawText("The large ball is on the screen ", Color.White, GameResources.GameFont("Courier"), 10, 50);
                    Text.DrawText("at X:209, Y:209    X:" + bigball.xPos + ", Y:" + bigball.yPos, Color.White, GameResources.GameFont("Courier"), 10, 70);
                }
                else
                {
                    Text.DrawText("The large ball is not on the screen ", Color.White, GameResources.GameFont("Courier"), 10, 50);
                    Text.DrawText("at X:209, Y:209    X:" + bigball.xPos + ", Y:" + bigball.yPos, Color.White, GameResources.GameFont("Courier"), 10, 70);
                }
                Graphics.DrawPixel(Color.White, 209, 209);
                //CalculateAngle
                Single temp = Physics.CalculateAngle(ball, bigball);
                //Text.DrawText(Shapes.CenterPoint(ball).X + ", " + Shapes.CenterPoint(ball).Y + " "+ Shapes.CenterPoint(bigball).X + ", " + Shapes.CenterPoint(bigball).Y, Color.White, GameResources.GameFont("Courier"), 10, 110);
                Text.DrawText("The angle betwen the balls is " + temp, Color.White, GameResources.GameFont("Courier"), 10, 90);

                //CalculateVectorFromTo
                Vector tempV = Physics.CalculateVectorFromTo(ball, bigball);
                Text.DrawText("The vector from the small ball ", Color.White, GameResources.GameFont("Courier"), 10, 110);
                Text.DrawText("to the large ball is X:" + tempV.X + ", Y:" + tempV.Y, Color.White, GameResources.GameFont("Courier"), 10, 130);

                //VectorFromCenterSpriteToPoint
                tempV = Physics.VectorFromCenterSpriteToPoint(ball, Shapes.CreatePoint(100, 200));
                Text.DrawText("Vector from ball to point X:" + tempV.X + ", Y:" + tempV.Y, Color.White, GameResources.GameFont("Courier"), 10, 150);
                Graphics.FillRectangle(Color.Red, 100 - 1, 200 - 1, 3, 3);
            }

        }


        private class VectorMathsTest : TestSet
        {
            private readonly static string METHS =
                "RectangleHasCollidedWithLine";

            private readonly static string INST =
                "Arrow Up/Down to change Vector1 X" + Environment.NewLine +
                "Arrow Left/Right to change" + Environment.NewLine +"Vector1 Y" + Environment.NewLine +
                "Arrow W/S to change Vector2 X" + Environment.NewLine +
                "Arrow A/D to change Vector2 Y" + Environment.NewLine;


            private SwinGame.Vector v1 = Physics.CreateVector(2,2);
            private SwinGame.Vector v2 = Physics.CreateVector(2, 2, true);

            private static SwinGame.Bitmap draw = SwinGame.Graphics.CreateBitmap(300, 32);

            public VectorMathsTest()
                : base(METHS, INST)
            {


            }



            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                if (Input.IsKeyPressed(Keys.VK_LEFT))
                {
                    v1.Y = v1.Y - 1;
                }
                if (Input.IsKeyPressed(Keys.VK_RIGHT))
                {
                    v1.Y = v1.Y + 1;
                }
                if (Input.IsKeyPressed(Keys.VK_DOWN))
                {
                    v1.X = v1.X - 1;
                }
                if (Input.IsKeyPressed(Keys.VK_UP))
                {
                    v1.X = v1.X + 1;
                }

                if (Input.IsKeyPressed(Keys.VK_A))
                {
                    v2.Y = v2.Y - 1;
                }
                if (Input.IsKeyPressed(Keys.VK_D))
                {
                    v2.Y = v2.Y + 1;
                }
                if (Input.IsKeyPressed(Keys.VK_S))
                {
                    v2.X = v2.X - 1;
                }
                if (Input.IsKeyPressed(Keys.VK_W))
                {
                    v2.X = v2.X + 1;
                }

                Text.DrawText("Vector 1 = X:" + v1.X + ", Y:" + v1.Y, Color.White, GameResources.GameFont("Courier"), 10, 10);
                Text.DrawText("Vector 2 = X:" + v2.X + ", Y:" + v2.Y, Color.White, GameResources.GameFont("Courier"), 10, 30);
                Vector temp = Physics.AddVectors(v1, v2);
                Text.DrawText("Vector 1 + Vector 2 = X:" + temp.X + ", Y:" + temp.Y, Color.White, GameResources.GameFont("Courier"), 10, 50);
                temp = Physics.SubtractVectors(v1, v2);
                Text.DrawText("Vector 1 - Vector 2 = X:" + temp.X + ", Y:" + temp.Y, Color.White, GameResources.GameFont("Courier"), 10, 70);
                temp = Physics.LimitMagnitude(v1, 20);
                Text.DrawText("limt Vector 1 to 20 = X:" + temp.X + ", Y:" + temp.Y, Color.White, GameResources.GameFont("Courier"), 10, 90);
                temp = Physics.InvertVector(v1);
                Text.DrawText("Vector 1 inverted = X:" + temp.X + ", Y:" + temp.Y, Color.White, GameResources.GameFont("Courier"), 10, 110);
                if (Physics.IsZeroVector(v1))
                {
                    Text.DrawText("Vector 1 is zero", Color.White, GameResources.GameFont("Courier"), 10, 130);
                }
                else
                {
                    Text.DrawText("Vector 1 is not zero", Color.White, GameResources.GameFont("Courier"), 10, 130);

                }

            }
        }


        private class RectangleCollissionTest : TestSet
        {
            private readonly static string METHS =
                "RectangleHasCollidedWithLine";

            private readonly static string INST =
                "Arrow keys to move the rectangle" + Environment.NewLine + "of  around" + Environment.NewLine;


            private SwinGame.Sprite ball = Graphics.CreateSprite(GameResources.GameImage("SmallBall"));
            private SwinGame.Sprite bigball = Graphics.CreateSprite(GameResources.GameImage("BallImage1"));

            private static SwinGame.Bitmap draw = SwinGame.Graphics.CreateBitmap(300, 32);

            public RectangleCollissionTest()
                : base(METHS, INST)
            {


            }


            private int X = 0;
            private int Y = 0;

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                if (Input.IsKeyPressed(Keys.VK_LEFT))
                {
                    X = X - 1;
                }
                if (Input.IsKeyPressed(Keys.VK_RIGHT))
                {
                    X = X + 1;
                }
                if (Input.IsKeyPressed(Keys.VK_DOWN))
                {
                    Y = Y + 1;
                }
                if (Input.IsKeyPressed(Keys.VK_UP))
                {
                    Y = Y - 1;
                }
                //RectangleHasCollidedWithLine
                if (Physics.RectangleHasCollidedWithLine(Shapes.CreateRectangle(X,Y, 50,50), Shapes.CreateLine(100, 0, 100, 418)))
                {
                    Graphics.FillRectangle(Color.Red, 98, 0, 5, 418);
                }
                if (Physics.RectangleHasCollidedWithLine(Shapes.CreateRectangle(X, Y, 50, 50), Shapes.CreateLine(200, 0, 200, 418)))
                {
                    Graphics.FillRectangle(Color.Red, 198, 0, 5, 418);
                }
                Graphics.DrawRectangle(Color.White, Shapes.CreateRectangle(X, Y, 50, 50));
                Graphics.DrawLine(Color.Yellow, Shapes.CreateLine(100, 0, 100, 418));
                Graphics.DrawLine(Color.Yellow, Shapes.CreateLine(200, 0, 200, 418));
            }
        }


        private class CircleCollissionTest2 : TestSet
        {
            private readonly static string METHS =
                "CircularCollision";

            private readonly static string INST =
                "Arrow keys to change the vector" + Environment.NewLine + "of  the big ball" + Environment.NewLine +
                "AWSD keys to change the vector" + Environment.NewLine + "of  the small ball" + Environment.NewLine+
                "Space to do the collision of " + Environment.NewLine +
                "the two balls";


            private SwinGame.Sprite ball = Graphics.CreateSprite(GameResources.GameImage("SmallBall"));
            private SwinGame.Sprite bigball = Graphics.CreateSprite(GameResources.GameImage("BallImage1"));

            private static SwinGame.Bitmap draw = SwinGame.Graphics.CreateBitmap(300, 32);

            public CircleCollissionTest2()
                : base(METHS, INST)
            {
                ball.Movement.X = 1;
                ball.Movement.Y = 1;
                ball.Mass = 1;
                ball.X = 150;
                ball.Y = 150;
                bigball.Movement.X = 1;
                bigball.Movement.Y = 1;
                bigball.Mass = 5;
                bigball.X = 155;
                bigball.Y = 155;
            }


            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                if (Input.IsKeyPressed(Keys.VK_DOWN))
                {
                    if (Physics.Magnitude(bigball.Movement) > 2)
                    {
                        bigball.Movement.SetTo(Physics.MultiplyVector(Physics.GetUnitVector(bigball.Movement), Physics.Magnitude(bigball.Movement) - 1));
                    }
                }
                if (Input.IsKeyPressed(Keys.VK_UP))
                {
                    bigball.Movement.SetTo(Physics.MultiplyVector(Physics.GetUnitVector(bigball.Movement), Physics.Magnitude(bigball.Movement) + 1));
                }
                if (Input.IsKeyPressed(Keys.VK_LEFT))
                {
                    bigball.Movement.SetTo(Physics.Multiply(Physics.RotationMatrix(-1), bigball.Movement));
                }
                if (Input.IsKeyPressed(Keys.VK_RIGHT))
                {
                    bigball.Movement.SetTo(Physics.Multiply(Physics.RotationMatrix(1), bigball.Movement));
                }

                if (Input.IsKeyPressed(Keys.VK_S))
                {
                    if (Physics.Magnitude(ball.Movement) > 2)
                    {
                        ball.Movement.SetTo(Physics.MultiplyVector(Physics.GetUnitVector(ball.Movement), Physics.Magnitude(ball.Movement) - 1));
                    }
                }
                if (Input.IsKeyPressed(Keys.VK_W))
                {
                    ball.Movement.SetTo(Physics.MultiplyVector(Physics.GetUnitVector(ball.Movement), Physics.Magnitude(ball.Movement) + 1));
                }
                if (Input.IsKeyPressed(Keys.VK_A))
                {
                    ball.Movement.SetTo(Physics.Multiply(Physics.RotationMatrix(-1), ball.Movement));
                }
                if (Input.IsKeyPressed(Keys.VK_D))
                {
                    ball.Movement.SetTo(Physics.Multiply(Physics.RotationMatrix(1), ball.Movement));
                }
                if (Input.WasKeyTyped(Keys.VK_SPACE))
                {
                    if (Physics.HaveSpritesCollided(ball, bigball))
                    {
                        Physics.CircularCollision(ball, bigball);
                        Graphics.UpdateSprite(ball);
                        Graphics.UpdateSprite(bigball);
                    }
                    else
                    {
                        ball.X = 150;
                        ball.Y = 150;
                        bigball.X = 155;
                        bigball.Y = 155;
                    }
                }

                Graphics.DrawSprite(bigball);
                Graphics.DrawSprite(ball);
                float tempX = ball.X + (Graphics.CurrentWidth(ball) / 2);
                float tempY = ball.Y + (Graphics.CurrentHeight(ball) / 2);
                float bigtempX = bigball.X + (Graphics.CurrentWidth(bigball) / 2);
                float bigtempY = bigball.Y + (Graphics.CurrentHeight(bigball) / 2);
                Graphics.DrawLine(Color.RoyalBlue, tempX, tempY, tempX + ball.Movement.X, tempY + ball.Movement.Y);
                Graphics.DrawLine(Color.RoyalBlue, bigtempX, bigtempY, bigtempX + bigball.Movement.X, bigtempY + bigball.Movement.Y);
                //Graphics.DrawRectangle(Color.White, ball.X, ball.Y, Graphics.CurrentWidth(ball), Graphics.CurrentHeight(ball));
            }
        }


        private class CircleCollissionTest : TestSet
        {
            private readonly static string METHS =
                "HaveBitmapsCollided, CircleHasCollidedWithLine, GetUnitVector, RotationMatrix...";

            private readonly static string INST =
                "Arrow keys to move the sprite " + Environment.NewLine + "around" +Environment.NewLine+
                "AWSD keys to change the vector " + Environment.NewLine+
                "Space to move ball out of line";


            private SwinGame.Sprite ball = Graphics.CreateSprite(GameResources.GameImage("SmallBall"));
            private SwinGame.Bitmap mediumball = GameResources.GameImage("BallImage1");

            private static SwinGame.Bitmap draw = SwinGame.Graphics.CreateBitmap(300, 32);

            public CircleCollissionTest() : base(METHS, INST) 
            {
                ball.Movement.X = 1;
                ball.Movement.Y = 1;
            }

            private int X = 0;
            private int Y = 0;

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                if (Input.IsKeyPressed(Keys.VK_LEFT))
                {
                    ball.X = ball.X - 1;
                }
                if (Input.IsKeyPressed(Keys.VK_RIGHT))
                {
                    ball.X = ball.X + 1;
                }

                if (Input.IsKeyPressed(Keys.VK_S))
                {
                    if (Physics.Magnitude(ball.Movement) > 2)
                    {
                        ball.Movement.SetTo(Physics.MultiplyVector(Physics.GetUnitVector(ball.Movement), Physics.Magnitude(ball.Movement) - 1));
                    }
                }
                if (Input.IsKeyPressed(Keys.VK_DOWN))
                {
                    ball.Y = ball.Y + 1;
                }
                if (Input.IsKeyPressed(Keys.VK_UP))
                {
                    ball.Y = ball.Y - 1;
                }

                if (Input.IsKeyPressed(Keys.VK_W))
                {
                    ball.Movement.SetTo(Physics.MultiplyVector(Physics.GetUnitVector(ball.Movement), Physics.Magnitude(ball.Movement) + 1));
                }
                if (Input.IsKeyPressed(Keys.VK_A))
                {
                    ball.Movement.SetTo(Physics.Multiply(Physics.RotationMatrix(1),ball.Movement));
                }
                if (Input.IsKeyPressed(Keys.VK_D))
                {
                    ball.Movement.SetTo(Physics.Multiply(Physics.RotationMatrix(-1),ball.Movement));
                }
                //CircleHasCollidedWithLine
                if (Physics.CircleHasCollidedWithLine(ball, Shapes.CreateLine(300, 0, 300, 418)))
                {
                    Graphics.FillRectangle(Color.LightCyan, 298, 0, 5, 418);
                }
                Graphics.DrawVerticalLine(Color.Red, 300, 0, 418);

                //CircleHasCollidedWithLine
                if (Physics.CircleHasCollidedWithLine(ball, Shapes.CreateLine(300, 0, 300, 418)) && Input.IsKeyPressed(Keys.VK_SPACE))
                {
                    Physics.CircleCollisionWithLine(ball, Shapes.CreateLine(300, 0, 300, 418));
                    Graphics.UpdateSprite(ball);
                }
                

                Graphics.DrawSprite(ball);
                float tempX = ball.X + (Graphics.CurrentWidth(ball) /2);
                float tempY = ball.Y + (Graphics.CurrentHeight(ball) / 2);
                Graphics.DrawLine(Color.RoyalBlue, tempX, tempY, tempX + ball.Movement.X, tempY + ball.Movement.Y);
                Graphics.DrawRectangle(Color.White, ball.X, ball.Y, Graphics.CurrentWidth(ball), Graphics.CurrentHeight(ball));
            }
        }


        private class CollissionBitmapTest : TestSet
        {
            private readonly static string METHS =
                "HaveBitmapsCollided, HasBitmapCollidedWithRect";

            private readonly static string INST =
                "Arrow keys to move the bitmap " + Environment.NewLine + "around";

            private SwinGame.Bitmap smallball = GameResources.GameImage("SmallBall");
            private SwinGame.Bitmap mediumball = GameResources.GameImage("BallImage1");

            private static SwinGame.Bitmap draw = SwinGame.Graphics.CreateBitmap(300, 32);

            public CollissionBitmapTest() : base(METHS, INST) { }

            private int X = 0;
            private int Y = 0;

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                if (Input.IsKeyPressed(Keys.VK_LEFT))
                {
                    X = X - 1;
                }
                if (Input.IsKeyPressed(Keys.VK_RIGHT))
                {
                    X = X + 1;
                }
                if (Input.IsKeyPressed(Keys.VK_DOWN))
                {
                    Y = Y + 1;
                }
                if (Input.IsKeyPressed(Keys.VK_UP))
                {
                    Y = Y - 1;
                }
                //HaveBitmapsCollided
                Text.DrawText("All Bound", Color.White, GameResources.GameFont("Courier"), 10, 5);
                if (Physics.HaveBitmapsCollided(mediumball, 10, 30, smallball, X, Y))
                {
                    Graphics.FillRectangle(Color.Pink, 5, 25, mediumball.Height + 10, mediumball.Width + 10);
                }
                Graphics.DrawBitmap(mediumball, 10, 30);
                
                Graphics.DrawRectangle(Color.White, 10, 30, mediumball.Width, mediumball.Height);

                if (Physics.HaveBitmapsCollided(mediumball, Shapes.CreatePoint(10, 150), smallball, Shapes.CreatePoint(X, Y)))
                {
                    Graphics.FillRectangle(Color.Pink, 5, 145, mediumball.Height + 10, mediumball.Width + 10);
                }
                Graphics.DrawBitmap(mediumball, 10, 150);
                Graphics.DrawRectangle(Color.White, 10, 150, mediumball.Width, mediumball.Height);

                if (Physics.HaveBitmapsCollided(mediumball, Shapes.CreatePoint(10, 270), Shapes.CreateRectangle(mediumball), smallball, Shapes.CreatePoint(X, Y), Shapes.CreateRectangle(smallball)))
                {
                    Graphics.FillRectangle(Color.Pink, 5, 265, mediumball.Height + 10, mediumball.Width + 10);
                }
                Graphics.DrawBitmap(mediumball, 10, 270);
                Graphics.DrawRectangle(Color.White, 10, 270, mediumball.Width, mediumball.Height);



                Text.DrawText("No Bound", Color.White, GameResources.GameFont("Courier"), 120, 5);
                if (Physics.HaveBitmapsCollided(mediumball, 120, 30, false, smallball, X, Y, false))
                {
                    Graphics.FillRectangle(Color.Blue, 115, 25,  mediumball.Height + 10, mediumball.Width + 10);
                }
                Graphics.DrawBitmap(mediumball, 120, 30);
                Graphics.DrawRectangle(Color.White, 120, 30, mediumball.Width, mediumball.Height);

                if (Physics.HaveBitmapsCollided(mediumball, Shapes.CreatePoint(120, 150),false, smallball, Shapes.CreatePoint(X, Y), false))
                {
                    Graphics.FillRectangle(Color.Blue, 115, 145, mediumball.Height + 10, mediumball.Width + 10);
                }
                Graphics.DrawBitmap(mediumball, 120, 150);
                Graphics.DrawRectangle(Color.White, 120, 150, mediumball.Width, mediumball.Height);

                if (Physics.HaveBitmapsCollided(mediumball, Shapes.CreatePoint(120, 270),Shapes.CreateRectangle(mediumball), false, smallball, Shapes.CreatePoint(X, Y),  Shapes.CreateRectangle(smallball),false))
                {
                    Graphics.FillRectangle(Color.Blue, 115, 265, mediumball.Height + 10, mediumball.Width + 10);
                }
                Graphics.DrawBitmap(mediumball, 120, 270);
                Graphics.DrawRectangle(Color.White, 120, 270, mediumball.Width, mediumball.Height);




                Text.DrawText("Big ball Bound", Color.White, GameResources.GameFont("Courier"), 240, 5);
                if (Physics.HaveBitmapsCollided(mediumball, 240, 30, true, smallball, X, Y, false))
                {
                    Graphics.FillRectangle(Color.Blue, 235, 25, mediumball.Height + 10, mediumball.Width + 10);
                }
                Graphics.DrawBitmap(mediumball, 240, 30);
                Graphics.DrawRectangle(Color.White, 240, 30, mediumball.Width, mediumball.Height);

                Text.DrawText("small ball Bound", Color.White, GameResources.GameFont("Courier"), 240, 150);
                if (Physics.HaveBitmapsCollided(mediumball, Shapes.CreatePoint(240, 170), false, smallball, Shapes.CreatePoint(X, Y), true))
                {
                    Graphics.FillRectangle(Color.Blue, 235, 165, mediumball.Height + 10, mediumball.Width + 10);
                }
                Graphics.DrawBitmap(mediumball, 240, 170);
                Graphics.DrawRectangle(Color.White, 240, 170, mediumball.Width, mediumball.Height);

                //HasBitmapCollidedWithRect
                Graphics.DrawRectangle(Color.DarkGreen, 400, 350, 10, 10);
                if (Physics.HasBitmapCollidedWithRect(smallball, X, Y, 400, 350, 10, 10))
                {
                    Graphics.FillRectangle(Color.LightGreen, 400, 350, 10, 10);
                }

                Graphics.DrawRectangle(Color.DarkGreen, 400, 365, 10, 10);
                if (Physics.HasBitmapCollidedWithRect(smallball, X, Y, Shapes.CreateRectangle(400, 365, 10, 10)))
                {
                    Graphics.FillRectangle(Color.LightGreen, 400, 365, 10, 10);
                }




                Graphics.DrawBitmap(smallball,X,Y);
                Graphics.DrawRectangle(Color.White, X, Y, smallball.Width, smallball.Height);
            }
        }


        private class CollisionSpriteTest : TestSet
        {
            private readonly static string METHS =
                "HasSpriteCollidedWith..., HaveSpritesCollided";

            private readonly static string INST =
                "Arrow keys to move the sprite "+ Environment.NewLine +"around"+ Environment.NewLine +
                "Pink for bounded collissions"+ Environment.NewLine +
                "Blue for non bounded collissions";

            private Sprite ship = Graphics.CreateSprite(GameResources.GameImage("Ship"),1,2,40,43);

            private Sprite explosion = Graphics.CreateSprite(GameResources.GameImage("BlueExplosion2"), 20, 40, 72, 72);
            

            private SwinGame.Bitmap smallball = GameResources.GameImage("SmallBall");

            private static SwinGame.Bitmap draw = SwinGame.Graphics.CreateBitmap(300, 32);

            public CollisionSpriteTest() : base(METHS, INST) { }

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                //Graphics.ClearScreen(Color.White);
                //ship.Movement.SetTo(Physics.CreateVector(1,1));
                Graphics.UpdateSpriteAnimation(ship);
                ship.UsePixelCollision = true;
                //Text.DrawText(ship.Movement.X + ", " + ship.Movement.Y, Color.White, GameResources.GameFont("Courier"), 10, 200);
                
                if (Input.IsKeyPressed(Keys.VK_LEFT))
                {
                    ship.X = ship.X - 1;
                }
                if (Input.IsKeyPressed(Keys.VK_RIGHT))
                {
                    ship.X = ship.X + 1;
                }
                if (Input.IsKeyPressed(Keys.VK_DOWN))
                {
                    ship.Y = ship.Y + 1;
                }
                if (Input.IsKeyPressed(Keys.VK_UP))
                {
                    ship.Y = ship.Y - 1;
                }

                //HasSpriteCollidedX
                Graphics.DrawVerticalLine(Color.Red, 400, 0, 418);
                if (Physics.HasSpriteCollidedX(ship, 400, CollisionDetectionRange.CollisionRangeEquals))
                {
                    Graphics.DrawRectangle(Color.White, 395, 405, 10, 10);
                }
                if (Physics.HasSpriteCollidedX(ship, 400, CollisionDetectionRange.CollisionRangeGreaterThan))
                {
                    Graphics.DrawRectangle(Color.White, 407, 405, 10, 10);
                }
                if (Physics.HasSpriteCollidedX(ship, 400, CollisionDetectionRange.CollisionRangeLessThan))
                {
                    Graphics.DrawRectangle(Color.White, 383, 405, 10, 10);
                }

                //HasSpriteCollidedY
                Graphics.DrawHorizontalLine(Color.LightBlue, 400, 0, 418);
                if (Physics.HasSpriteCollidedY(ship, 400, CollisionDetectionRange.CollisionRangeEquals))
                {
                    Graphics.DrawRectangle(Color.White, 10, 395, 10, 10);
                }
                if (Physics.HasSpriteCollidedY(ship, 400, CollisionDetectionRange.CollisionRangeGreaterThan))
                {
                    Graphics.DrawRectangle(Color.White, 10, 407, 10, 10);
                }
                if (Physics.HasSpriteCollidedY(ship, 400, CollisionDetectionRange.CollisionRangeLessThan))
                {
                    Graphics.DrawRectangle(Color.White, 10, 383, 10, 10);
                }

                //HasSpriteCollidedWithRect
                
                Graphics.DrawRectangle(Color.DarkGreen, 10, 10, 10, 10);
                if (Physics.HasSpriteCollidedWithRect(ship, 10, 10, 10, 10))
                {
                    Graphics.FillRectangle(Color.LightGreen, 10, 10, 10, 10);
                }

                Graphics.DrawRectangle(Color.DarkGreen, 10, 25, 10, 10);
                if (Physics.HasSpriteCollidedWithRect(ship, Shapes.CreateRectangle(10, 25, 10, 10)))
                {
                    Graphics.FillRectangle(Color.LightGreen, 10, 25, 10, 10);
                }

                //HaveSpritesCollided
                explosion.UsePixelCollision = true;

                explosion.X = 70;
                explosion.Y = 10;
                Graphics.UpdateSpriteAnimation(explosion);
                //Graphics.FillRectangle(Color.White, 64, 4, 45, 45);
                if (Physics.HaveSpritesCollided(ship, explosion))
                {
                    Graphics.DrawRectangle(Color.Blue, 65, 5, 78, 78);
                    Graphics.DrawRectangle(Color.Blue, 64, 4, 80, 80);
                }
                Graphics.DrawSprite(explosion);

                //HasSpriteCollidedWithBitmap
                if (Physics.HasSpriteCollidedWithBitmap(ship, smallball, 10,100))
                {
                    Graphics.DrawRectangle(Color.Pink, true, 5, 95, smallball.Width + 10, smallball.Height + 10);
                }
                Graphics.DrawBitmap(smallball, 10, 100);
                Graphics.DrawRectangle(Color.White, 10, 100, smallball.Width, smallball.Height);

                if (Physics.HasSpriteCollidedWithBitmap(ship, smallball, 10, 150, false))
                {
                    Graphics.DrawRectangle(Color.Blue, true, 5, 145, smallball.Width + 10, smallball.Height + 10);
                }
                Graphics.DrawBitmap(smallball, 10, 150);
                Graphics.DrawRectangle(Color.White, 10, 150, smallball.Width, smallball.Height);
                //using points
                if (Physics.HasSpriteCollidedWithBitmap(ship, smallball, Shapes.CreatePoint(70,100),Shapes.CreateRectangle(smallball), false))
                {
                    Graphics.DrawRectangle(Color.Blue, true, 65, 95, smallball.Width + 10, smallball.Height + 10);
                }
                Graphics.DrawBitmap(smallball, 70, 100);
                Graphics.DrawRectangle(Color.White, 70, 100, smallball.Width, smallball.Height);

                if (Physics.HasSpriteCollidedWithBitmap(ship, smallball, Shapes.CreatePoint(70, 150), false))
                {
                    Graphics.DrawRectangle(Color.Blue, true, 65, 145, smallball.Width + 10, smallball.Height + 10);
                }
                Graphics.DrawBitmap(smallball, 70, 150);
                Graphics.DrawRectangle(Color.White, 70, 150, smallball.Width, smallball.Height);


                Graphics.DrawSprite(ship);
                Graphics.DrawRectangle(Color.White, ship.X, ship.Y, Graphics.CurrentWidth(ship), Graphics.CurrentHeight(ship));
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
                "Left/Right controls White" + Environment.NewLine +
					 "Up/Down controls Red" + Environment.NewLine +
                "Space moved red over white" + Environment.NewLine +
                "Blue for non bounded collissions";

            public VectorAngle() : base(METHS, INST) 
				{
					v1 = Physics.CreateVector(LINE_LENGTH, 0);
					v2 = Physics.CreateVector(0 , LINE_LENGTH); 
				}

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                Graphics.FillCircle(Color.Red, CX, CY, RADIUS);
                Graphics.DrawLine(Color.Red, CX, CY, CX + v1.X, CY + v1.Y);
					 Graphics.DrawLine(Color.White, CX, CY, CX + v2.X, CY + v2.Y);
					
 					 float angle = Physics.CalculateAngleBetween(v1, v2);
					 float v1a = Physics.CalculateAngle(0, 0, v1.X, v1.Y);
					 float v2a = Physics.CalculateAngle(0, 0, v2.X, v2.Y);
					 
					 Text.DrawText("White: " + v2a, Color.White, GameResources.GameFont("Courier"), 5, 50);
					 Text.DrawText("Red: " + v1a, Color.Red, GameResources.GameFont("Courier"), 5, 70);
					 Text.DrawText("Between: " + angle, Color.White, GameResources.GameFont("Courier"), 5, 90);
					
					 float rot = 0;
					 Matrix2D rm = null;

					 if (Input.IsKeyPressed(Keys.VK_LEFT)) rot = 5;
					 else if (Input.IsKeyPressed(Keys.VK_RIGHT)) rot = -5;

					 if (rot != 0)
					 {
						rm = Physics.RotationMatrix(rot);
						v2 = Physics.Multiply(rm, v2);
					 }

					 rot = 0;

					 if (Input.IsKeyPressed(Keys.VK_UP)) rot = 5;
					 else if (Input.IsKeyPressed(Keys.VK_DOWN)) rot = -5;

					 if (rot != 0)
					 {
						rm = Physics.RotationMatrix(rot);
						v1 = Physics.Multiply(rm, v1);
					 }

					 if (Input.WasKeyTyped(Keys.VK_SPACE))
					 {
						rm = Physics.RotationMatrix(angle);
						v1 = Physics.Multiply(rm, v1);
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
                "Arrows move point" + Environment.NewLine +
					 "A/Z rotate movement" + Environment.NewLine +
                "Space move point out";

            public PointOutOfRect() : base(METHS, INST) 
				{
					p = Shapes.CreatePoint(100 / 2 + RX, 100 / 2 + RY);
					rect = Shapes.CreateRectangle(RX, RY, RW, RH);

					movement = Physics.CreateVector(100, 0);
				}

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
					int r = 1, r2 = 2;
					
					if ( Input.IsKeyPressed(Keys.VK_A)) movement = Physics.Multiply(Physics.RotationMatrix(-4.0f), movement);
					if ( Input.IsKeyPressed(Keys.VK_Z)) movement = Physics.Multiply(Physics.RotationMatrix(4.0f), movement);

					if ( Input.IsKeyPressed(Keys.VK_UP)) p.Y = p.Y - 5;
					if ( Input.IsKeyPressed(Keys.VK_DOWN)) p.Y = p.Y + 5;
					if ( Input.IsKeyPressed(Keys.VK_LEFT)) p.X = p.X - 5;
					if ( Input.IsKeyPressed(Keys.VK_RIGHT)) p.X = p.X + 5;

					mvOut = Physics.VectorOutOfRectFromPoint(p, rect, movement);
					
					if ( Input.IsKeyPressed(Keys.VK_SPACE))
					{
						p.Y += mvOut.Y;
						p.X += mvOut.X;
						mvOut = Physics.CreateVector(0, 0);
					}

					Graphics.ClearScreen(Color.Black);

					Graphics.DrawRectangle(Color.Red, RX, RY, RW, RH);			
					Graphics.DrawRectangle(Color.White, p.X - r, p.Y - r, r2, r2);
					Graphics.DrawLine(Color.White, p.X, p.Y, p.X + movement.X, p.Y + movement.Y);

					if (false == ((mvOut.X == 0) && (mvOut.Y == 0)))
					{
						Graphics.DrawLine(Color.Green, p.X, p.Y, p.X + mvOut.X, p.Y + mvOut.Y);
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
                "Arrows move point" + Environment.NewLine +
                     "A/Z rotate movement" + Environment.NewLine +
                "Space move point out";

            public RectOutOfRect()
                : base(METHS, INST)
            {

                movement = Physics.CreateVector(100, 0);
                halfH = 10;
                halfW = 5;

		        movement = Physics.CreateVector(100, 0);
		
		        tgtRect = Shapes.CreateRectangle(RX, RY, RW, RH);
                mvRect = Shapes.CreateRectangle(100 + RX, 100 + RY, 10, 20);
            }

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                int r = 1, r2 = 2;

                if (Input.IsKeyPressed(Keys.VK_A)) movement = Physics.Multiply(Physics.RotationMatrix(-4.0f), movement);
                if (Input.IsKeyPressed(Keys.VK_Z)) movement = Physics.Multiply(Physics.RotationMatrix(4.0f), movement);

                if (Input.IsKeyPressed(Keys.VK_UP)) mvRect.Y = mvRect.Y - 5;
                if (Input.IsKeyPressed(Keys.VK_DOWN)) mvRect.Y = mvRect.Y + 5;
                if (Input.IsKeyPressed(Keys.VK_LEFT)) mvRect.X = mvRect.X - 5;
                if (Input.IsKeyPressed(Keys.VK_RIGHT)) mvRect.X = mvRect.X + 5;

                mvOut = Physics.VectorOutOfRectFromRect(mvRect, tgtRect, movement);

                enterRectFrom = Shapes.GetSideForCollisionTest(movement);
                
			    px = Shapes.RectangleLeft(mvRect); 
			    py = Shapes.RectangleTop(mvRect);
                switch (enterRectFrom)
	            {
		            case CollisionSide.Bottom:
                     break;
                    case CollisionSide.BottomLeft:
                        px = Shapes.RectangleRight(mvRect);
                     break;
                    case CollisionSide.BottomRight:
                     break;
                    case CollisionSide.Left:
                        px = Shapes.RectangleRight(mvRect);
                     break;
                    case CollisionSide.LeftRight:
                     break;
                    case CollisionSide.None:
                     break;
                    case CollisionSide.Right:
                     break;
                    case CollisionSide.Top:
                        py = Shapes.RectangleBottom(mvRect);
                     break;
                    case CollisionSide.TopBottom:
                     break;
                    case CollisionSide.TopLeft:
                        
                        px = Shapes.RectangleRight(mvRect);
                        py = Shapes.RectangleBottom(mvRect);
                     break;
                    case CollisionSide.TopRight:
                        py = Shapes.RectangleBottom(mvRect);
                     break;default:
                     break;
	            }

                if (Input.IsKeyPressed(Keys.VK_SPACE))
                {
                    mvRect.Y += (int)mvOut.Y;
                    mvRect.X += (int)mvOut.X;
                    mvOut = Physics.CreateVector(0, 0);
                }

                Graphics.ClearScreen(Color.Black);

                Graphics.DrawRectangle(Color.Red, RX, RY, RW, RH);
                Graphics.DrawRectangle(Color.White, mvRect.X , mvRect.Y , mvRect.Width, mvRect.Height);
                Graphics.DrawLine(Color.White, mvRect.X + halfW, mvRect.Y + halfH, mvRect.X + halfW + movement.X, mvRect.Y + halfH + movement.Y);

                if (false == ((mvOut.X == 0) && (mvOut.Y == 0)))
                {

                    Graphics.DrawLine(Color.Gray, mvRect.X, mvRect.Y, mvRect.X + mvOut.X, mvRect.Y + mvOut.Y);
                    Graphics.DrawRectangle(Color.Green, mvRect.X + mvOut.X, mvRect.Y + mvOut.Y, mvRect.Width, mvRect.Height);
                    Graphics.DrawLine(Color.Green, px, py, px + mvOut.X , py + mvOut.Y );

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
                "Arrows move point" + Environment.NewLine +
                     "A/Z rotate movement" + Environment.NewLine +
                "Space move point out";

            public PointOutOfCircle()
                : base(METHS, INST)
            {
                p = Shapes.CreatePoint(100 / 2 + CX, 100 / 2 + CY);
                c = Shapes.CreatePoint(CX, CY);

                movement = Physics.CreateVector(100, 0);
            }

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                int r = 1, r2 = 2;

                if (Input.IsKeyPressed(Keys.VK_A)) movement = Physics.Multiply(Physics.RotationMatrix(-4.0f), movement);
                if (Input.IsKeyPressed(Keys.VK_Z)) movement = Physics.Multiply(Physics.RotationMatrix(4.0f), movement);

                if (Input.IsKeyPressed(Keys.VK_UP)) p.Y = p.Y - 5;
                if (Input.IsKeyPressed(Keys.VK_DOWN)) p.Y = p.Y + 5;
                if (Input.IsKeyPressed(Keys.VK_LEFT)) p.X = p.X - 5;
                if (Input.IsKeyPressed(Keys.VK_RIGHT)) p.X = p.X + 5;

                mvOut = Physics.VectorOutOfCircleFromPoint(p, c, CR, movement);

                if (Input.IsKeyPressed(Keys.VK_SPACE))
                {
                    p.Y += mvOut.Y;
                    p.X += mvOut.X;
                    mvOut = Physics.CreateVector(0, 0);
                }

                //Graphics.ClearScreen(Color.Black);

                Graphics.DrawCircle(Color.Red, CX, CY, CR);
                Graphics.DrawRectangle(Color.White, p.X - r, p.Y - r, r2, r2);
                Graphics.DrawLine(Color.White, p.X, p.Y, p.X + movement.X, p.Y + movement.Y);

                if (false == ((mvOut.X == 0) && (mvOut.Y == 0)))
                {
                    Graphics.DrawLine(Color.Green, p.X, p.Y, p.X + mvOut.X, p.Y + mvOut.Y);
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
                "Arrows move point" + Environment.NewLine +
                     "A/Z rotate movement" + Environment.NewLine +
                "Space move point out";

            public CircleOutOfCircle()
                : base(METHS, INST)
            {
                p = Shapes.CreatePoint(100 / 2 + CX, 100 / 2 + CY);
                c = Shapes.CreatePoint(CX, CY);

                movement = Physics.CreateVector(100, 0);
            }

            protected override void ToRun(System.Drawing.Rectangle toDrawIn)
            {
                int r = 1, r2 = 2;

                if (Input.IsKeyPressed(Keys.VK_A)) movement = Physics.Multiply(Physics.RotationMatrix(-4.0f), movement);
                if (Input.IsKeyPressed(Keys.VK_Z)) movement = Physics.Multiply(Physics.RotationMatrix(4.0f), movement);

                if (Input.IsKeyPressed(Keys.VK_UP)) p.Y = p.Y - 5;
                if (Input.IsKeyPressed(Keys.VK_DOWN)) p.Y = p.Y + 5;
                if (Input.IsKeyPressed(Keys.VK_LEFT)) p.X = p.X - 5;
                if (Input.IsKeyPressed(Keys.VK_RIGHT)) p.X = p.X + 5;

                mvOut = Physics.VectorOutOfCircleFromCircle(p,PR, c, CR, movement);

                if (Input.IsKeyPressed(Keys.VK_SPACE))
                {
                    p.Y += mvOut.Y;
                    p.X += mvOut.X;
                    mvOut = Physics.CreateVector(0, 0);
                }


                Graphics.DrawCircle(Color.Red, CX, CY, CR);
                Graphics.DrawCircle(Color.White, p.X, p.Y, PR);
                Graphics.DrawLine(Color.White, p.X, p.Y, p.X + movement.X, p.Y + movement.Y);

                if (false == ((mvOut.X == 0) && (mvOut.Y == 0)))
                {
                    Graphics.DrawLine(Color.Green, p.X, p.Y, p.X + mvOut.X, p.Y + mvOut.Y);
                    Graphics.DrawCircle(Color.Green, p.X + mvOut.X, p.Y + mvOut.Y, PR);
                }

            }

        }

    }

}
