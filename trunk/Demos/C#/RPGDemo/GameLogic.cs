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

        public static void AddRange(Character[] contentArray, ref Character[] toBeCopiedTo)
        {
            Array.Resize(ref toBeCopiedTo, toBeCopiedTo.Length + contentArray.Length);

            if (contentArray.Length > 0)
            {
                for (int i = 0; i < contentArray.Length; i++)
                {
                    toBeCopiedTo[toBeCopiedTo.Length - contentArray.Length + i] = contentArray[i];
                }
            }

        }

        /// <summary>
        /// Main Game Routine
        /// </summary>
        public static void MainGame()
        {
            //The Map
            Map _Map = Resources.GameMap("Level1");
            Character _Player = Characters.NewCharacter("Hero", MappyLoader.EventPositionX(_Map, PLAYERSPAWN, 0), MappyLoader.EventPositionY(_Map, PLAYERSPAWN, 0),5,5,5, true, true, true);
            Character[] _Healers = Healers.NewHealers("Healer", _Map, 1, 1, 1);
            Character[] _Critters = Enemy.NewEnemies("Critter", 1, _Map, 4, 5, 4, 20);
            Character[] _Thieves = Enemy.NewEnemies("Thief", 2, _Map, 10, 6, 8, 40);
            Character[] _Leader = Enemy.NewEnemies("ThiefLeader", 3, _Map, 25, 15, 20, 400);

            Character[] _TotalAI = new Character[0];

            AddRange(_Healers, ref _TotalAI);
            AddRange(_Critters, ref _TotalAI);
            AddRange(_Thieves, ref _TotalAI);
            AddRange(_Leader, ref _TotalAI);

            /*
            //Resizes the Array so that it can include the Healers
            Array.Resize(ref _TotalAI, _Healers.Length);
            //Add the Healers
            _Healers.CopyTo(_TotalAI, 0);

            //Resizes the Array so that it can include the Critters
            Array.Resize(ref _TotalAI, _Healers.Length + _Critters.Length);
            //Add the Critters
            _Critters.CopyTo(_TotalAI, _Healers.Length - 1);

            //Resizes the Array so that it can include the Thieves
            Array.Resize(ref _TotalAI, _Healers.Length + _Critters.Length + _Thieves.Length);
            //Add the Thieves
            _Thieves.CopyTo(_TotalAI, _Healers.Length + _Critters.Length - 1);

            //Resizes the Array so that it can include the Leader
            Array.Resize(ref _TotalAI, _Healers.Length + _Critters.Length + _Thieves.Length + _Leader.Length);
            //Add the Leader
            _Leader.CopyTo(_TotalAI, _Healers.Length + _Critters.Length + _Thieves.Length - 1);
            
            */
             
            //Game Loop
            do
            {
                //Clears the Screen to Black
                Graphics.ClearScreen();

                //Draw Map
                MappyLoader.DrawMap(_Map);

                //Update Player
                Controller.UpdatePlayer(ref _Player, _Map);

                //Follow the Player 
                Camera.FollowSprite(_Player.Sprite, 0, 0);

                //Updates the AI
                AIController.UpdateAI(ref _TotalAI, _Player, _Map);

                // If Player Collides with the AI, move them back.
                AIController.PlayerCollideWithAI(ref _Player, _TotalAI);

                //Draw the Player
                Graphics.DrawSprite(_Player.Sprite);

                //Refreshes the Characters Stats
                Characters.RefreshCharacterStats(ref _Player);

                //Runs the User Interface
                UserInterface.RunUI(ref _Player);

                //Draws the Help Text
                Text.DrawTextOnScreen("Hit S to Open and Close the Stat Page", Color.White, Resources.GameFont("Courier"), 420, 580);

                //Draws the FrameRate
                Text.DrawFramerate(550, 0, Resources.GameFont("Courier"));


                //Refreshes the Screen and Processes Input Events
                Core.RefreshScreen();
                Core.ProcessEvents();

            } while (!Core.WindowCloseRequested());
        }

        /// <summary>
        /// Main Loop
        /// </summary>
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
