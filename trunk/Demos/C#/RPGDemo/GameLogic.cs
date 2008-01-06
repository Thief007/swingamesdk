using System;
using System.Drawing;
using System.Collections.Generic;
using System.Windows.Forms;
using System.Text;

using SwinGame;
using Graphics = SwinGame.Graphics;
using Bitmap = SwinGame.Bitmap;
using Font = SwinGame.Font;
using FontStyle = SwinGame.FontStyle;
using Event = SwinGame.Event;
using CollisionSide = SwinGame.CollisionSide;
using Sprite = SwinGame.Sprite;

using GameResources;



namespace GameProject
{

    public static class GameLogic
    {
        const Event PLAYERSPAWN = Event.Event1; 

        public static void MainGame()
        {
            //The Map
            Map _Map = Resources.GameMap("Level1");
            Character _Player = Characters.NewCharacter("Hero", MappyLoader.EventPositionX(_Map, PLAYERSPAWN, 0), MappyLoader.EventPositionY(_Map, PLAYERSPAWN, 0));
            
            //Game Loop
            do
            {
                //Clears the Screen to Black
                Graphics.ClearScreen();

                //Draw Map
                MappyLoader.DrawMap(_Map);

                Controller.UpdatePlayer(ref _Player, _Map);

                Camera.FollowSprite(_Player.Sprite, 0, 0);

                Graphics.DrawSprite(_Player.Sprite);

                //Draws the FrameRate
                Text.DrawFramerate(550, 0, Resources.GameFont("Courier"));

                //Refreshes the Screen and Processes Input Events
                Core.RefreshScreen();
                Core.ProcessEvents();

            } while (!Core.WindowCloseRequested());
        }

        public static void RunGame()
        {
            //Open a new Graphics Window
            Core.OpenGraphicsWindow("The Legend of the Tomato Quest", 800, 600);
            //Open Audio Device
            Audio.OpenAudio();
            //Load Resources
            Resources.LoadResources();
            
            //Game Loop
            do
            {
                //Clears the Screen to Black
                Graphics.ClearScreen();

                MainGame();

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
