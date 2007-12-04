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
            ball.X = 400;
            ball.Y = 300;

            for (int i = 0; i < 1500; i++)
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
            }
        }

        public static void MoveBall(ref Sprite ball, ref int xSpeed, ref int ySpeed)
        {
            ball.X = ball.X + xSpeed;
            ball.Y = ball.Y + ySpeed;
        }

    }
}
