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
    public static class GameLogic
    {
        public static void RunGame()
        {
            //Open a new Graphics Window
            Core.OpenGraphicsWindow("The Legend of the Tomato Quest", 640, 480);
            //Open Audio Device
            Audio.OpenAudio();
            //Load Resources
				
            Resources.LoadResources();

				Audio.PlayMusic(Resources.GameMusic("Theme"));

				Core.ToggleFullScreen();

            //Creates the Game Object
            Game _Game = new Game();

            StartGame.DisplayStartGameMessage();

            //Game Loop
            do
            {
                //Clears the Screen to Black
                Graphics.ClearScreen();

                //Runs the Game
                _Game.Run();

                //Refreshes the Screen and Processes Input Events
                Core.RefreshScreen(65);
                Core.ProcessEvents();

            } while (!Core.WindowCloseRequested() && !EndGame.HasGameEnded);

            EndGame.DisplayEndGameMessage();

            //Free Resources and Close Audio, to end the program.
            Resources.FreeResources();
            Audio.CloseAudio();
        }
    }
}
