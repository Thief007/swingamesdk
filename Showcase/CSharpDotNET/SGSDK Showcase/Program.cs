using System;
using System.Collections.Generic;
using System.Windows.Forms;

using SwinGame;

namespace SGSDK_Showcase
{
    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        static void Main()
        {
            //Opens the Graphics Window
            Core.OpenGraphicsWindow("SwinGameSDK Showcase", 800, 600);

            //Opens the Audio System
            Audio.OpenAudio();

            //Plays the SwinGameSDK Intro

            //Intro.LoadResources();
           
            //Run Examples

            if (!Core.WindowCloseRequested())
            {
                DrawLines.Run();
                DrawRectangles.Run();
                DrawCircles.Run();
                DrawEllipses.Run();
                DrawBitmaps.Run();
                DrawSprites.Run();
                CollisionDetection.Run();
                MoveSpriteWithInput.Run();
                MouseCursor.Run();
                VectorCollision.Run();
                SoundInput.Run();
                KeyInput.Run();
                TextRead.Run();
                DrawRandomText.Run();
            }


            //Closes the Audio System
            Audio.CloseAudio();
        }
    }
}