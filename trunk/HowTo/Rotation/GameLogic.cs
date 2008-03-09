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
    public class GameLogic
    {
        public const int CHANGE_COUNT = 120; // how many cycles before it changes direction (auto)
        public const int SCREEN_WIDTH = 800;
        public const int SCREEN_HEIGHT = 600;

        private static Aircraft _Aircraft;

        public static void Initialise()
        {
            _Aircraft = new Aircraft();    
        }

        public static void Draw()
        {
            //Draw screen
            Graphics.ClearScreen(Color.Black);
            Graphics.DrawBitmap(GameResources.GameImage("Background"), 0, 0);

            _Aircraft.Draw();

            Core.RefreshScreen(65);
        }
        
        public static void HandleInput()
        {
            int angle = 0;
            if (_Aircraft.Manual)
            {
              if (Input.WasKeyTyped(Keys.VK_LEFT)) 
                angle = -30;
              if (Input.WasKeyTyped(Keys.VK_RIGHT))
                angle = angle + 30;

              if (angle != 0)
                _Aircraft.ChangeDirection(angle);
            }

            if (Input.WasKeyTyped(Keys.VK_T))
              _Aircraft.Manual =  ! _Aircraft.Manual;
        }
        
        public static void Update()
        {
            _Aircraft.Update();
        }
      
        public static void RunGame()
        {
            //Open a new Graphics Window
            Core.OpenGraphicsWindow("Rotation", 800, 600);
            //Open Audio Device
            Audio.OpenAudio();
            //Load Resources
            GameResources.LoadResources();
            Initialise();
            //Game Loop
            do
            {
        		Core.ProcessEvents();
                HandleInput();
                Update();
                Draw();

        		Core.RefreshScreen();
            } while (!Core.WindowCloseRequested());

            //Free Resources and Close Audio, to end the program.
            GameResources.FreeResources();
            Audio.CloseAudio();
        }
    }
}
