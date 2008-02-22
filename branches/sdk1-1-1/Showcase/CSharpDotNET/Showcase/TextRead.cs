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
    public static class TextRead
    {
        private static Font _Font = Text.LoadFont(Core.GetPathToResource("cour.ttf", ResourceKind.FontResource), 18);
        private static String msg = "";

        public static void Run()
        {
            Input.StartReadingText(Color.Green, 50, _Font, 0, 65);

            while (Input.IsReadingText())
            {
                Graphics.ClearScreen();
                Text.DrawText("Please enter a message:", Color.Green, _Font, 0, 50);
                Overlay.DrawOverlay("Text Reading Example");
                Text.DrawFramerate(0, 0, _Font);
                Core.RefreshScreen();
                Core.ProcessEvents();
                if (Core.WindowCloseRequested())
                {
                    break;
                }
            }

            Text.DrawText("You have entered " + Input.TextReadAsASCII(), Color.Green, _Font, 0, 80);
            Core.RefreshScreen();

            do
            {
                Core.Sleep(20);
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
