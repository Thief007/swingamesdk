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
            
            Board b = new Board(1000000);
            int i = 0;
            
            //Game Loop
            do
            {
                if(i++ % 100 == 0)
                {
                    i = 0;
                    b.Draw();
                    //Text.DrawFramerate(0, 0, Resources.GameFont("Courier"));
                    Core.RefreshScreen();
                }
                
                Core.ProcessEvents();
                
                //b.StepToSolutionGradient();
                //b.StepToSolution();
                b.StepToSolutionBestSwap();
            } while (!Core.WindowCloseRequested());

            //Free Resources and Close Audio, to end the program.
            Resources.FreeResources();
            Audio.CloseAudio();
        }
    }
}
