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
    public static class KeyInput
    {
        public static Font _Font = Text.LoadFont(Core.GetPathToResource("cour.ttf", ResourceKind.FontResource), 18);
        private static String msg = "";

        public static void Run()
        {
            for (int i = 0; i < 2000; i++)
            {
                Text.DrawText("Hit a key a,s d, or f", Color.White, _Font, 280, 300);
                Text.DrawText(msg, Color.White, _Font, 280, 330);

                if (Input.IsKeyPressed(SwinGame.Keys.VK_A))
                {
                    msg = "You hit the A Key";
                }

                if (Input.IsKeyPressed(SwinGame.Keys.VK_S))
                {
                    msg = "You hit the S Key";
                }

                if (Input.IsKeyPressed(SwinGame.Keys.VK_D))
                {
                    msg = "You hit the D Key";
                }

                if (Input.IsKeyPressed(SwinGame.Keys.VK_F))
                {
                    msg = "You hit the F Key";
                }

                Overlay.DrawOverlay("Key Input Example");
                Core.ProcessEvents();
                Core.RefreshScreen(60);
                Graphics.ClearScreen();

                if (Core.WindowCloseRequested())
                {
                    break;
                }

            }
        }

    }
}
