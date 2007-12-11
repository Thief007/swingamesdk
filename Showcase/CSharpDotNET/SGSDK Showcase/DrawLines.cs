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
    public static class DrawLines
    {
        public static void Run()
        {
            Graphics.ClearScreen();

            for (int i = 0; i < 1000; i++)
            {
                Graphics.DrawLine(Randoms.GetRandomColor(), Randoms.GetRandomNumber(800), Randoms.GetRandomNumber(800), Randoms.GetRandomNumber(800), Randoms.GetRandomNumber(800));

                Overlay.DrawOverlay("Drawing Lines Example");

                Core.ProcessEvents();
                Core.RefreshScreen(60);

                if (Core.WindowCloseRequested())
                {
                    break;
                }
            }

            Graphics.ClearScreen();
        }
    }
}
