using System;
using System.Collections.Generic;
using System.Windows.Forms;
using System.Drawing;

using SwinGame;
using Graphics = SwinGame.Graphics;
using Bitmap = SwinGame.Bitmap;
using Font = SwinGame.Font;
using FontStyle = SwinGame.FontStyle;
using Sprite = SwinGame.Sprite;

namespace SGSDK_Showcase
{
    public static class VectorCollision
    {

        private static Sprite ball1;
        private static Sprite ball2;

        public static void Run()
        {
            Graphics.ClearScreen();
            ball1 = Graphics.CreateSprite(Graphics.LoadBitmap(Core.GetPathToResource("ball.png", ResourceKind.ImageResource)));
            ball2 = Graphics.CreateSprite(Graphics.LoadBitmap(Core.GetPathToResource("ball2.png", ResourceKind.ImageResource)));
            ball1.Movement.SetTo(Physics.CreateVector((Single)3.0, (Single)3.0));
            ball2.Movement.SetTo(Physics.CreateVector((Single)3.0, (Single)3.0));
            ball1.Mass = 1;
            ball2.Mass = 1;

            ball1.xPos = 0;
            ball1.yPos = 0;
            ball2.xPos = Core.ScreenWidth() - Graphics.CurrentWidth(ball2);
            ball2.yPos = Core.ScreenHeight() - Graphics.CurrentHeight(ball2);
            ball1.UsePixelCollision = true;
            ball2.UsePixelCollision = true;

            for (int i = 0; i < 4200; i++)
            {
                Graphics.ClearScreen();
                Graphics.DrawSprite(ball1);
                Graphics.DrawSprite(ball2);

                if (Physics.HaveSpritesCollided(ball1, ball2))
                {
                    Console.WriteLine("b1: {0},{1}", ball1.xPos, ball1.yPos);
                    Console.WriteLine("b2: {0},{1}", ball2.xPos, ball2.yPos);
                    Physics.VectorCollision(ball1, ball2);
                }

                MoveBallUsingVector(ref ball1);
                MoveBallUsingVector(ref ball2);

                Overlay.DrawOverlay("Vector Collision Example");
                Core.ProcessEvents();
                Core.RefreshScreen();

                if (Core.WindowCloseRequested())
                {
                    break;
                }
            }
        }

        public static void MoveBallUsingVector(ref Sprite ball)
        {
            Graphics.MoveSprite(ball, ball.Movement);

            if (ball.xPos > Core.ScreenWidth() - Graphics.CurrentWidth(ball))
            {
                ball.Movement.X= ball.Movement.X * -1;
                ball.xPos = Core.ScreenWidth() - Graphics.CurrentWidth(ball);
            }

            if (ball.yPos > Core.ScreenHeight() - Graphics.CurrentHeight(ball))
            {
                ball.Movement.Y = ball.Movement.Y * -1;
                ball.yPos = Core.ScreenHeight() - Graphics.CurrentHeight(ball);
            }

            if (ball.xPos < 0)
            {
                ball.Movement.X = ball.Movement.X * -1;
                ball.xPos = 0;
            }

            if (ball.yPos < 0)
            {
                ball.Movement.Y = ball.Movement.Y * -1;
                ball.yPos = 0;
            }
        }

    }
}