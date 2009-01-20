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
    public static class DrawBitmaps
    {
        private static Bitmap tempBitmap;
        private static Bitmap tempBitmap2;

        public static void Run()
        {
            Graphics.ClearScreen();

            tempBitmap = GameResources.GameImage("BallImage1");
            tempBitmap2 = GameResources.GameImage("BallImage2");
            int i = 0;


            do
            {
                i = i + 1;

                Graphics.ClearScreen();
                Graphics.DrawBitmap(tempBitmap, (int)Math.Round(Core.Sin(i) * 100) + 250, (int)Math.Round(Core.Cos(i) * 100) + 200);
                Graphics.DrawBitmap(tempBitmap2, (int)Math.Round(Core.Cos(i) * 100) + 250, (int)Math.Round(Core.Sin(i) * 100) + 200);

                Overlay.DrawOverlay("Drawing Bitmap Example");
                Core.ProcessEvents();
                Core.RefreshScreen();

                if (Core.WindowCloseRequested())
                {
                    break;
                }

            
            } while (!Input.IsKeyPressed(SwinGame.Keys.VK_RETURN));
            Core.Sleep(500);
        }
    }
}
