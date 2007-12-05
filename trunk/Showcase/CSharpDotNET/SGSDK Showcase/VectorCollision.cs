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

        private static PhysicsData ball1 = new PhysicsData();
        private static PhysicsData ball2 = new PhysicsData();

        public static void Run()
        {
            Graphics.ClearScreen();
            ball1.Movement = Physics.CreateVector((Single)3.0, (Single)3.0);
            ball2.Movement = Physics.CreateVector((Single)3.0, (Single)3.0);
            ball1.Mass = 1;
            ball2.Mass = 1;
            ball1.Sprite = Graphics.CreateSprite(Graphics.LoadBitmap(Core.GetPathToResource("ball.png", ResourceKind.ImageResource)));
            ball2.Sprite = Graphics.CreateSprite(Graphics.LoadBitmap(Core.GetPathToResource("ball2.png", ResourceKind.ImageResource)));

            ball1.Sprite.xPos = 0;
            ball1.Sprite.yPos = 0;
            ball2.Sprite.xPos = Core.ScreenWidth() - Graphics.CurrentWidth(ball2.Sprite);
            ball2.Sprite.yPos = Core.ScreenHeight() - Graphics.CurrentHeight(ball2.Sprite);
            ball1.Sprite.UsePixelCollision = true;
            ball2.Sprite.UsePixelCollision = true;

            for (int i = 0; i < 4200; i++)
            {
                Graphics.ClearScreen();
                Graphics.DrawSprite(ball1.Sprite);
                Graphics.DrawSprite(ball2.Sprite);

                if (Physics.HaveSpritesCollided(ball1.Sprite, ball2.Sprite))
                {
                    Physics.VectorCollision(ref ball1, ref ball2);
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

        public static void MoveBallUsingVector(ref PhysicsData ball)
        {
            Graphics.MoveSprite(ball.Sprite, ball.Movement);

            if (ball.Sprite.xPos > Core.ScreenWidth() - Graphics.CurrentWidth(ball.Sprite))
            {
                ball.Movement.x = ball.Movement.x * -1;
                ball.Sprite.xPos = Core.ScreenWidth() - Graphics.CurrentWidth(ball.Sprite);
            }

            if (ball.Sprite.yPos > Core.ScreenHeight() - Graphics.CurrentHeight(ball.Sprite))
            {
                ball.Movement.y = ball.Movement.y * -1;
                ball.Sprite.yPos = Core.ScreenHeight() - Graphics.CurrentHeight(ball.Sprite);
            }

            if (ball.Sprite.xPos < 0)
            {
                ball.Movement.x = ball.Movement.x * -1;
                ball.Sprite.xPos = 0;
            }

            if (ball.Sprite.yPos < 0)
            {
                ball.Movement.y = ball.Movement.y * -1;
                ball.Sprite.yPos = 0;
            }
        }

    }
}
