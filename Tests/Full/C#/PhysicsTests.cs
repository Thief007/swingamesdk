using System;
using System.Collections.Generic;
using System.Text;
using Color = System.Drawing.Color;
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
          
            list.Add(result);
        }

        #endregion

        private class CollissionBitmapTest : TestSet
        {
            private readonly static string METHS =
                "HaveBitmapsCollided";

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

            private Sprite explosion = Graphics.CreateSprite(/*GameResources.GameImage("Explosion")*/Graphics.LoadBitmap(Core.GetPathToResource("explosion_blue.jpg", ResourceKind.ImageResource), true, Color.Black), 20, 40, 72, 72);
            

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

                if (Physics.HasSpriteCollidedWithBitmap(ship, smallball, 10, 150, false))
                {
                    Graphics.DrawRectangle(Color.Blue, true, 5, 145, smallball.Width + 10, smallball.Height + 10);
                }
                Graphics.DrawBitmap(smallball, 10, 150);
                //using points
                if (Physics.HasSpriteCollidedWithBitmap(ship, smallball, Shapes.CreatePoint(70,100),Shapes.CreateRectangle(smallball), false))
                {
                    Graphics.DrawRectangle(Color.Blue, true, 65, 95, smallball.Width + 10, smallball.Height + 10);
                }
                Graphics.DrawBitmap(smallball, 70, 100);

                if (Physics.HasSpriteCollidedWithBitmap(ship, smallball, Shapes.CreatePoint(70, 150), false))
                {
                    Graphics.DrawRectangle(Color.Blue, true, 65, 145, smallball.Width + 10, smallball.Height + 10);
                }
                Graphics.DrawBitmap(smallball, 70, 150);


                Graphics.DrawSprite(ship);
                Graphics.DrawRectangle(Color.White, ship.X, ship.Y, Graphics.CurrentWidth(ship), Graphics.CurrentHeight(ship));
            }
        }
    }
}
