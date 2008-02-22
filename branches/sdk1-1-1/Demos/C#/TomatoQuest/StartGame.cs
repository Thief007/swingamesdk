using System;
using System.Drawing;
using System.Collections.Generic;

using SwinGame;
using Graphics = SwinGame.Graphics;
using Bitmap = SwinGame.Bitmap;
using Font = SwinGame.Font;
using FontStyle = SwinGame.FontStyle;
using Event = SwinGame.Event;
using CollisionSide = SwinGame.CollisionSide;
using Sprite = SwinGame.Sprite;

using GameResources;

namespace TomatoQuest
{
    public static class StartGame
    {
        public static void DisplayStartGameMessage()
        {
            while (!Input.WasKeyTyped(Keys.VK_RETURN) && !Core.WindowCloseRequested())
            {
                Graphics.ClearScreen();

                Graphics.DrawBitmapOnScreen(Resources.GameImage("Logo"), 40, 90);

                Text.DrawTextOnScreen("Press Enter to Start", Color.White, Resources.GameFont("Arial"), 230, 400);

                Core.ProcessEvents();
                Core.RefreshScreen();
            }
        }
    }
}
