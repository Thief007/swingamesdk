using System;
using System.Text;
using System.Drawing;
using System.Collections;
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

namespace GameProject
{
    public static class GameLogic
    {
        public static void RunGame()
        {
            //Open a new Graphics Window
            Core.OpenGraphicsWindow("Game", 800, 800);
            //Open Audio Device
            Audio.OpenAudio();
            //Load Resources
            Resources.LoadResources();

            int n = 40; //number of cities...
            int m = 10;
            RandomMap rm = new RandomMap(n);
            //Ant a = new Ant(rm.Cities, Ant.Rand.Next(n));
            Colony c = new Colony(m, rm.Cities);
            int i = 0;

            for(i = 0; i < n; i++)
            {
                c.Step();
            }
            
            //Game Loop
            do
            {
                Graphics.ClearScreen();
                
                for(i = 0; i < n + 1; i++)
                {
                    c.Step();
                }
                
                rm.Draw();
                c.Draw();
                                
                Core.RefreshScreen();                
                Core.ProcessEvents();                
            } while (!Core.WindowCloseRequested());

            //Free Resources and Close Audio, to end the program.
            Resources.FreeResources();
            Audio.CloseAudio();
        }
    }
}
