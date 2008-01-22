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
    public static class EndGame
    {
        private static bool _GameEnded;
        private static bool _Victory;

        public static bool HasGameEnded
        {
            get { return _GameEnded; }
            set { _GameEnded = value; }
        }

        public static bool Victory
        {
            get { return _Victory; }
            set { _Victory = value; }
        }

        public static void DisplayEndGameMessage()
        {
            while (!Input.WasKeyTyped(Keys.VK_RETURN) && !Core.WindowCloseRequested())
            {
                Graphics.ClearScreen();

                if (Victory)
                {
                    Text.DrawTextLinesOnScreen("... and so the Hero had \r\n found the tomato, only to find \r\n the tomato was rotten... \r\n The End.",
                                Color.White, Color.Black, Resources.GameFont("Arial"), FontAlignment.AlignCenter, 150, 200, 360, 400);
                }
                else
                {
                    Text.DrawTextLinesOnScreen("... and the Hero was never \r\n heard from again...",
                                Color.White, Color.Black, Resources.GameFont("Arial"), FontAlignment.AlignCenter, 150, 200, 360, 400);
                }

                Text.DrawTextOnScreen("Press Enter to Continue", Color.White, Resources.GameFont("Arial"), 230,400);

                Core.ProcessEvents();
                Core.RefreshScreen();
            }
        }
    }
}
