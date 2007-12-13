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

            loopSprite.xPos = 50;
		    loopSprite.yPos = 200;
		    reverseSprite.xPos = 150;
		    reverseSprite.yPos = 200;
		    stopSprite.xPos = 350;
		    stopSprite.yPos = 200;
		    reverseOnceSprite.xPos = 450;
		    reverseOnceSprite.yPos = 200;

            do
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

                if (Core.WindowCloseRequested())
                {
                    break;
                }
            } while (!Input.IsKeyPressed(SwinGame.Keys.VK_RETURN));
            Core.Sleep(500);
        }

        public static void DrawSpriteCaption(Sprite sprite, String caption)
        {
            Text.DrawText(caption, Color.White, _Font, (int)Math.Round(sprite.xPos + sprite.Width / 2) - ((caption.Length / 2) * 10), (int)Math.Round(sprite.yPos + sprite.Height));
        }
    }
}
