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
        private static Font _Font = GameResources.GameFont("Courier");
      

        public static void Run()
        {
            Graphics.ClearScreen();

            do
            {
                Text.SetFontStyle(_Font, Randoms.GetRandomFontStyle());
                Text.DrawText("SwinGameSDK!", Randoms.GetRandomColor(), _Font, Randoms.GetRandomNumber(800), Randoms.GetRandomNumber(600));
                Text.SetFontStyle(_Font, Randoms.GetRandomFontStyle());
                Text.DrawText("SwinGameSDK!", Randoms.GetRandomColor(), _Font, Randoms.GetRandomNumber(800), Randoms.GetRandomNumber(600));

                Text.SetFontStyle(_Font, Randoms.GetRandomFontStyle());
                Overlay.DrawOverlay("Drawing Random Texts");
                Core.Sleep(10);
                Core.RefreshScreen();
                Core.ProcessEvents();

                if (Core.WindowCloseRequested())
                {
                    break;
                }
            } while (!Input.IsKeyPressed(SwinGame.Keys.VK_RETURN));
            Core.Sleep(500);
        }
    }
}
