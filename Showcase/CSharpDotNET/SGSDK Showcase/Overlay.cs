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
    class Overlay
    {
        private static Font _Font = Text.LoadFont(Core.GetPathToResource("cour.ttf", ResourceKind.FontResource), 18);

        public static void DrawOverlay(String title)
        {
            Graphics.FillRectangle(Color.Black, 0, 0, 800, 50);
            Text.DrawText(title, Color.White, _Font, (Core.ScreenWidth() / 2) - ((title.Length / 2) * 10), 20);
        }
    }
}
