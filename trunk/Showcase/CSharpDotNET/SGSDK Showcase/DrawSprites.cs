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
        private static Sprite stopSprite;
        private static Sprite reverseOnceSprite;

        private static Bitmap[] tempBitmaps;
        private static int[] tempIntegers;
        private static string tempString;

        public static Font _Font = Text.LoadFont(Core.GetPathToResource("cour.ttf", ResourceKind.FontResource),18);

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
                tempIntegers[i] = 1;
            }

            loopSprite = Graphics.CreateSprite(tempBitmaps, tempIntegers, SpriteEndingAction.Loop);
            reverseSprite = Graphics.CreateSprite(tempBitmaps, tempIntegers, SpriteEndingAction.ReverseLoop);
            stopSprite = Graphics.CreateSprite(tempBitmaps, tempIntegers, SpriteEndingAction.Stop);
            reverseOnceSprite = Graphics.CreateSprite(tempBitmaps, tempIntegers, SpriteEndingAction.ReverseOnce);

            loopSprite.X = 50;
		    loopSprite.Y = 200;
		    reverseSprite.X = 150;
		    reverseSprite.Y = 200;
		    stopSprite.X = 350;
		    stopSprite.Y = 200;
		    reverseOnceSprite.X = 450;
		    reverseOnceSprite.Y = 200;

            for (int i = 0; i < 151; i++)
            {
                Graphics.ClearScreen();

                Graphics.DrawSprite(loopSprite);
                Graphics.DrawSprite(reverseSprite);
                Graphics.DrawSprite(stopSprite);
                Graphics.DrawSprite(reverseOnceSprite);

                Graphics.UpdateSprite(loopSprite);
                Graphics.UpdateSprite(reverseSprite);
                Graphics.UpdateSprite(stopSprite);
                Graphics.UpdateSprite(reverseOnceSprite);

                DrawSpriteCaption(loopSprite, "Loop");
			    DrawSpriteCaption(reverseSprite, "ReverseLoop");
			    DrawSpriteCaption(stopSprite, "Stop");
			    DrawSpriteCaption(reverseOnceSprite, "ReverseOnce");

                Overlay.DrawOverlay("Drawing Sprite Example");

                Core.ProcessEvents();
                Core.RefreshScreen();

                Core.Sleep(50);
            }
        }

        public static void DrawSpriteCaption(Sprite sprite, String caption)
        {
            Text.DrawText(caption, Color.White, _Font, (int)Math.Round(sprite.X + Graphics.CurrentWidth(sprite) / 2) - ((caption.Length / 2) * 10), (int)Math.Round(sprite.Y + Graphics.CurrentHeight(sprite)));
        
        }
    }
}
