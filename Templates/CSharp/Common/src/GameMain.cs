using System;
using System.Reflection;
using SwinGame;
using Color = System.Drawing.Color;

namespace MyGame
{
    public class GameMain
    {
        public static void Main()
        {
            //Start the audio system so sound can be played
            Audio.OpenAudio();
            
            //Open the game window
            Core.OpenGraphicsWindow("GameMain", 800, 600);
            
            //Run the game loop
            while(false == Core.WindowCloseRequested())
            {
                //Fetch the next batch of UI interaction
                Core.ProcessEvents();
                
                //Clear the screen and draw the framerate
                Graphics.ClearScreen();
                Text.DrawFramerate(0,0);
                
                //Draw onto the screen
                Core.RefreshScreen();
            }
            
            //End the audio
            Audio.CloseAudio();
            
            //Close any resources we were using
            Resources.ReleaseAllResources();
        }
    }
}