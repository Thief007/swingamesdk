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
    public static class MoveSpriteWithInput
    {
        private static Sprite ball;

        private static int xSpeed;
        private static int ySpeed;

        public static void Run()
        {
            ball = Graphics.CreateSprite(Graphics.LoadBitmap(Core.GetPathToResource("ball.png", ResourceKind.ImageResource)));
            ball.xPos = 400;
            ball.yPos = 300;

            do
            {
                xSpeed = 0;
                ySpeed = 0;

                if (Input.IsKeyPressed(SwinGame.Keys.VK_UP))
                {
                    ySpeed = -1;
                }

                if (Input.IsKeyPressed(SwinGame.Keys.VK_DOWN))
                {
                    ySpeed = 1;
                }

                if (Input.IsKeyPressed(SwinGame.Keys.VK_LEFT))
                {
                    xSpeed = -1;
                }

                if (Input.IsKeyPressed(SwinGame.Keys.VK_RIGHT))
                {
                    xSpeed = 1;
                }

                Graphics.DrawSprite(ball);
                MoveBall(ref ball, ref xSpeed, ref ySpeed);

                Overlay.DrawOverlay("Move Sprite with Arrow Keys Example");
                Core.ProcessEvents();
                Core.RefreshScreen();
                Graphics.ClearScreen();

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
        }

    }
}
