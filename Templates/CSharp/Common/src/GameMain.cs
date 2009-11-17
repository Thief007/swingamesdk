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
            //Set the path to the application so the resource manager can
            //find the files it needs to load.
            Resources.SetAppPath(Assembly.GetExecutingAssembly().Location, true);
            
            //Start the audio system.
            Audio.OpenAudio();
            
            Core.OpenGraphicsWindow("GameMain", 800, 600);
            
            //Run the game loop
            while(false == Core.WindowCloseRequested())
            {
                //Fetch the next batch of UI interaction
                Core.ProcessEvents();
                
                Graphics.ClearScreen();
                Text.DrawFramerate(0,0); //Draw framerate top left
                
                //Draw onto the screen
                Core.RefreshScreen();
            }
            
            //End the audio
            Audio.CloseAudio();
            Resources.ReleaseAllResources();
        }
    }
}