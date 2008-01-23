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
    public static class DrawCircleWithLine
    {
        public static void Run()
        {
            Graphics.ClearScreen();

            do
            {
                if (Input.IsKeyPressed(SwinGame.Keys.VK_RIGHT))
                {
                    Camera.MoveVisualArea(4, 0);
                }
                
                if (Input.IsKeyPressed(SwinGame.Keys.VK_DOWN))
                {
                    Camera.MoveVisualArea(0, 4);
                }

                if (Input.IsKeyPressed(SwinGame.Keys.VK_UP))
                {
                    Camera.MoveVisualArea(0, -4);
                }

                if (Input.IsKeyPressed(SwinGame.Keys.VK_LEFT))
                {
                    Camera.MoveVisualArea(-4, 0);
                }

                Graphics.ClearScreen();

                Overlay.DrawOverlay("Drawing Circles Example");

                Graphics.FillCircle(Color.Green, Core.ScreenWidth() / 2, Core.ScreenHeight() / 2, 200);
			    Graphics.DrawCircle(Color.White, Core.ScreenWidth() / 2, Core.ScreenHeight() / 2, 200);
			    Graphics.FillCircleOnScreen(Color.Red, Core.ScreenWidth() / 2, Core.ScreenHeight() / 2, 100);
			    Graphics.DrawCircleOnScreen(Color.White, Core.ScreenWidth() / 2, Core.ScreenHeight() / 2, 100);
			    Graphics.DrawHorizontalLine(Color.White, Core.ScreenHeight() / 2, Core.ScreenWidth(), 0);
			    Graphics.DrawVerticalLine(Color.White, Core.ScreenWidth() / 2, Core.ScreenHeight(), 0);
			    Overlay.DrawOverlay("Camera Example");

                Core.ProcessEvents();
                Core.RefreshScreen();

                if (Core.WindowCloseRequested())
                {
                    break;
                }
            } while (!Input.IsKeyPressed(SwinGame.Keys.VK_RETURN));
            Camera.SetScreenOffset(0, 0);
            Core.Sleep(500);
        }
    }
}
