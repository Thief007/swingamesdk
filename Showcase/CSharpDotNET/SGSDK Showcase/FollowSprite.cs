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
        private static Font _Font = GameResources.GameFont("Courier");
        
        public static void Run()
        {

            sea = GameResources.GameImage("Sea");
            b2 = GameResources.GameImage("Ship");
            ship = Graphics.CreateSprite(b2, 3,2,40,43);

            ship.xPos = 0;
            ship.yPos = 0;

            do
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
                Core.RefreshScreen();

                if (Core.WindowCloseRequested())
                {
                    break;
                }
            } while (!Input.IsKeyPressed(SwinGame.Keys.VK_RETURN));
            Core.Sleep(500);
            Camera.SetScreenOffset(0, 0);
        
        }

    }
}


