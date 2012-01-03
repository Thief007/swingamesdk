using System;
using System.Collections.Generic;
using Color = System.Drawing.Color;
using SwinGame;

namespace HowToDrawPrimitiveShape.src
{
    public class HowToDrawPrimitiveShape
    {        
        public static void Main()
        {
            //Start the audio system so sound can be played
            Audio.OpenAudio();

            //Open the game window
            Graphics.OpenGraphicsWindow("Primitive Shapes", 800, 600);

            //Run the game loop
            while (false == Input.WindowCloseRequested())
            {
                //Fetch the next batch of UI interaction
                Input.ProcessEvents();

                //Clear the screen and draw the framerate
                Graphics.ClearScreen();
                Text.DrawFramerate(0, 0);                               
                
                Graphics.DrawRectangle(Color.Green, 25, 100, 200, 100);
                Graphics.FillRectangle(Color.Green, 25, 400, 200, 100);

                Graphics.DrawTriangle(Color.Yellow, 275, 200, 475, 200, 375, 100);
                Graphics.FillTriangle(Color.Yellow, 275, 500, 475, 500, 375, 400);

                Graphics.DrawCircle(Color.White, 625, 150, 100);

                //Draw onto the screen
                Graphics.RefreshScreen();
            }

            //End the audio
            Audio.CloseAudio();

            //Close any resources we were using
            Resources.ReleaseAllResources();
        }        
    }    
}


