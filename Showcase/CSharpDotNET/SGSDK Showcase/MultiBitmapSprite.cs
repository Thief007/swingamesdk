using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;
using System.Drawing;

using SwinGame;
using Graphics = SwinGame.Graphics;
using Bitmap = SwinGame.Bitmap;
using Font = SwinGame.Font;
using FontStyle = SwinGame.FontStyle;
using Sprite = SwinGame.Sprite;

namespace SGSDK_Showcase
{
    public static class MultiBitmapSprite
    {
        private static Bitmap b1;
        private static Bitmap b2;
        private static Sprite sp1;
        private static Sprite sp2;
        private static Font _Font = Text.LoadFont(Core.GetPathToResource("cour.ttf", ResourceKind.FontResource), 18);
        
        public static void Run()
        {

            b1 = Graphics.LoadBitmap(Core.GetPathToResource("Explosion.png", ResourceKind.ImageResource));
            b2 = Graphics.LoadBitmap(Core.GetPathToResource("ship.png", ResourceKind.ImageResource));
            sp1 = Graphics.CreateSprite(b1, 5,15,38, 38);
            sp2 = Graphics.CreateSprite(b2, 3,2,40,43);

          

            sp1.xPos = 70;
            sp1.yPos = 100;
            sp2.xPos = 80;
            sp2.yPos = 110;

            while (!Input.IsKeyPressed(SwinGame.Keys.VK_N))
            {
                Core.ProcessEvents();
                Graphics.ClearScreen();
                Graphics.DrawSprite(sp1);
                Graphics.DrawSprite(sp2);
                Graphics.UpdateSprite(sp1);
                Graphics.UpdateSprite(sp2);

                Graphics.DrawBitmapOnScreen(b1, 70, 170);
                Text.DrawText("Explosion Bitmap", Color.White, _Font, 90 + b1.Width, 190);

                Graphics.DrawBitmapOnScreen(b2, 70, 250);
                Text.DrawText("Ship Bitmap", Color.White, _Font, 90 + b2.Width, 260); 
            
                if (Physics.HaveSpritesCollided(sp1, sp2))
                {
                    Text.DrawText("Collided...", Color.White, _Font, 125, 120);
                }

                Overlay.DrawOverlay("Multi-Bitmap Collision Detection");

                Core.RefreshScreen();
            }
        
        }

    }
}


