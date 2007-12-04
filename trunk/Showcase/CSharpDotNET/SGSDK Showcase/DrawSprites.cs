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
    public static class DrawSprites
    {
        private static Sprite loopSprite;
        private static Sprite reverseSprite;

        private static Bitmap[] tempBitmaps;
        private static int[] tempIntegers;
        private static string tempString;

        public static void Run()
        {
            Graphics.ClearScreen();
            Array.Resize(ref tempBitmaps, 15);

            for (int i = 0; i < 15; i++)
            {
                tempString = Convert.ToString(i);
                tempBitmaps[i] = Graphics.LoadBitmap(Core.GetPathToResource("run" + tempString + ".png", ResourceKind.ImageResource));
            }

            Array.Resize(ref tempIntegers, 15);
            for (int i = 0; i < 15; i++)
            {
                tempIntegers[i] = 2;
            }

            loopSprite = Graphics.CreateSprite(tempBitmaps, tempIntegers, SpriteEndingAction.Loop);
            reverseSprite = Graphics.CreateSprite(tempBitmaps, tempIntegers, SpriteEndingAction.ReverseLoop);

            loopSprite.X = 100;
            loopSprite.Y = 200;

            reverseSprite.X = 300;
            reverseSprite.Y = 200;

            for (int i = 0; i < 16; i++)
            {
                Graphics.ClearScreen();

                Graphics.DrawSprite(loopSprite);
                Graphics.DrawSprite(reverseSprite);

                Graphics.UpdateSprite(loopSprite);
                Graphics.UpdateSprite(reverseSprite);

                Overlay.DrawOverlay("Drawing Sprite Example");

                Core.ProcessEvents();
                Core.RefreshScreen();

                Core.Sleep(50);
            }
        }
    }
}
