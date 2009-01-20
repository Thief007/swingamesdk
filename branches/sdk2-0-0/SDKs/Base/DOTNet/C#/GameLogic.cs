using System;
using System.Drawing;

using SwinGame;
using Graphics = SwinGame.Graphics;
using Bitmap = SwinGame.Bitmap;
using Font = SwinGame.Font;
using FontStyle = SwinGame.FontStyle;
using Event = SwinGame.Event;
using CollisionSide = SwinGame.CollisionSide;
using Sprite = SwinGame.Sprite;

namespace GameProject
{
    public static class GameLogic
    {
        public static void RunGame()
        {
            //Open a new Graphics Window
            Core.OpenGraphicsWindow("Game", 800, 600);
            //Open Audio Device
            Audio.OpenAudio();
            //Load Resources
            Resources.LoadResources();

            //Game Loop
            do
            {
                //Clears the Screen to Black
                Graphics.ClearScreen();

                Graphics.FillRectangle(Color.Red, 20, 200, 200, 100);
                Graphics.FillRectangle(Color.Green, 220, 200, 200, 100);
                Graphics.FillRectangle(Color.Blue, 420, 200, 200, 100);

                Text.DrawText("Hello World", Color.Red, Resources.GameFont("Courier"), 20, 310);
                Text.DrawText("Hello World", Color.Green, Resources.GameFont("Courier"), 220, 310);
                Text.DrawText("Hello World", Color.Blue, Resources.GameFont("Courier"), 420, 310);

                Text.DrawFramerate(0, 0, Resources.GameFont("Courier"));
                Text.DrawText("Hello World", Color.White, Resources.GameFont("ArialLarge"), 50, 50);

                //Refreshes the Screen and Processes Input Events
                Core.RefreshScreen();
                Core.ProcessEvents();
            } while (!Core.WindowCloseRequested());

            //Free Resources and Close Audio, to end the program.
            Resources.FreeResources();
            Audio.CloseAudio();
        }
    }
}
