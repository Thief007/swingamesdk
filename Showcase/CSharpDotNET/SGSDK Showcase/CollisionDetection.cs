using System;
using System.Collections.Generic;
using System.Windows.Forms;
using System.Drawing;

using SwinGame;
using Graphics = SwinGame.Graphics;
using Bitmap = SwinGame.Bitmap;
using Font = SwinGame.Font;
using FontStyle = SwinGame.FontStyle;
namespace SGSDK_Showcase
{
    public static class CollisionDetection
    {
        private static Sprite ball1;
        private static Sprite ball2;
        private static int xSpeed1;
        private static int xSpeed2;
        private static int ySpeed1;
        private static int ySpeed2;

        public static Font _Font = GameResources.GameFont("Courier");

        public static void Run()
        {
            Graphics.ClearScreen();

            xSpeed1 = 3;
            xSpeed2 = 3;
            ySpeed1 = 3;
            ySpeed2 = 3;

            ball1 = Graphics.CreateSprite(GameResources.GameImage("BallImage1"));
            ball2 = Graphics.CreateSprite(GameResources.GameImage("BallImage2"));

            ball1.xPos = 0;
            ball1.yPos = 0;

            ball2.xPos = Core.ScreenWidth() - Graphics.CurrentWidth(ball2);
            ball2.yPos = Core.ScreenHeight() - Graphics.CurrentHeight(ball2);

            ball1.UsePixelCollision = true;
            ball2.UsePixelCollision = true;

            do
            {
                Graphics.ClearScreen();
                Graphics.DrawSprite(ball1);
                Graphics.DrawSprite(ball2);

                if (Physics.HaveSpritesCollided(ball1, ball2))
                {
                    Text.DrawText("Collided!", Color.White, _Font, Core.ScreenWidth() - 90, Core.ScreenHeight() - 20); 
                }

                MoveBall(ref ball1, ref xSpeed1, ref ySpeed1);
                MoveBall(ref ball2, ref xSpeed2, ref ySpeed2);

                Overlay.DrawOverlay("Collision Detection Example");
                Core.ProcessEvents();
                Core.RefreshScreen();

                if (Core.WindowCloseRequested())
                {
                    break;
                }
            } while (!Input.IsKeyPressed(SwinGame.Keys.VK_RETURN));
            Core.Sleep(500);
        }

        public static void MoveBall(ref Sprite ball, ref int xSpeed, ref int ySpeed)
        {
            ball.xPos = ball.xPos + xSpeed;
            ball.yPos = ball.yPos + ySpeed;

            if (ball.xPos > Core.ScreenWidth() - Graphics.CurrentWidth(ball))
            {
                ball.xPos = Core.ScreenWidth() - Graphics.CurrentWidth(ball);
                xSpeed = -1 * xSpeed;
            }

            if (ball.yPos > Core.ScreenHeight() - Graphics.CurrentHeight(ball))
            {
			    ball.yPos = Core.ScreenHeight() - Graphics.CurrentHeight(ball);
			    ySpeed = -1 * ySpeed;
            }

            if (ball.xPos < 0)
            {
                ball.xPos = 0;
                xSpeed = -1 * xSpeed;
            }

            if (ball.yPos < 0)
            {
                ball.yPos = 0;
                ySpeed = -1 * ySpeed;
            }
        }

    }
}
