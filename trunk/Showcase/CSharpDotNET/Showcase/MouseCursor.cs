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
    public static class MouseCursor
    {                    
        private static Sprite ball = Graphics.CreateSprite(GameResources.GameImage("BallImage1"));
        private static Point2D position;

        public static void Run()
        {

            do
            {
                position = Input.GetMousePosition();

                Graphics.DrawHorizontalLine(Color.White, (int)position.Y, 0, 800);
                Graphics.DrawVerticalLine(Color.White, (int)position.X, 0, 600);

                if (Input.MouseWasClicked(MouseButton.LeftButton))
                {
                    ball.xPos = position.X - (Graphics.CurrentWidth(ball) / 2);
                    ball.yPos = position.Y - (Graphics.CurrentHeight(ball) / 2);
                    Graphics.DrawSprite(ball);
                }

                Overlay.DrawOverlay("Mouse Cursor Example");
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
    }
}
