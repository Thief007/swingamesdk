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
    public static class FollowExample
    {
        private static Bitmap sea;
        private static Bitmap b2;
        private static Sprite ship;
        private static Font _Font = Text.LoadFont(Core.GetPathToResource("cour.ttf", ResourceKind.FontResource), 18);
        
        public static void Run()
        {

            sea = Graphics.LoadBitmap(Core.GetPathToResource("sea.png", ResourceKind.ImageResource));
            b2 = Graphics.LoadBitmap(Core.GetPathToResource("ship.png", ResourceKind.ImageResource));
            ship = Graphics.CreateSprite(b2, 3,2,40,43);

            ship.xPos = 0;
            ship.yPos = 0;

            while (!Input.IsKeyPressed(SwinGame.Keys.VK_RETURN))
            {
                Core.ProcessEvents();

    
                if (Input.IsKeyPressed(SwinGame.Keys.VK_RIGHT))
                {
                    ship.xPos = ship.xPos + 3;
                }

                if (Input.IsKeyPressed(SwinGame.Keys.VK_LEFT))
                {
                    ship.xPos = ship.xPos - 3;
                }

                if (Input.IsKeyPressed(SwinGame.Keys.VK_UP))
                {
                    ship.yPos = ship.yPos - 3;
                }

                if (Input.IsKeyPressed(SwinGame.Keys.VK_DOWN))
                {
                    ship.yPos = ship.yPos + 3;
                }
 
                Camera.FollowSprite(ship, 0, -150);

                Graphics.ClearScreen();
                Graphics.DrawBitmap(sea, 0, 0);
                Graphics.DrawSprite(ship);
                Graphics.UpdateSprite(ship);

                Overlay.DrawOverlay("Follow Sprite Example");
                Core.RefreshScreen(60);
            }
        
        }

    }
}


