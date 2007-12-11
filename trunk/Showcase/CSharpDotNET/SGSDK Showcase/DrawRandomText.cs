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
    public static class DrawRandomText
    {
        private static Font _Font = Text.LoadFont(Core.GetPathToResource("cour.ttf", ResourceKind.FontResource), 18);
      

        public static void Run()
        {
            Graphics.ClearScreen();

            for (int i = 0; i < 500; i++)
            {
                Text.SetFontStyle(_Font, Randoms.GetRandomFontStyle());
                Text.DrawText("SwinGameSDK!", Randoms.GetRandomColor(), _Font, Randoms.GetRandomNumber(800), Randoms.GetRandomNumber(600));
                Text.SetFontStyle(_Font, Randoms.GetRandomFontStyle());
                Text.DrawText("SwinGameSDK!", Randoms.GetRandomColor(), _Font, Randoms.GetRandomNumber(800), Randoms.GetRandomNumber(600));

                Text.SetFontStyle(_Font, Randoms.GetRandomFontStyle());
                Overlay.DrawOverlay("Drawing Random Texts");
                Core.Sleep(10);
                Core.RefreshScreen(60);
                Core.ProcessEvents();

                if (Core.WindowCloseRequested())
                {
                    break;
                }
            }
        }
    }
}
