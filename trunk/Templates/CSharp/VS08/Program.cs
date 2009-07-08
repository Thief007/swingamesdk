using System;
using System.Reflection;
using System.Collections.Generic;
using System.Text;

using Color = System.Drawing.Color;
using SwinGame;

namespace MyGame
{
    class Program
    {
        static void Main(string[] args)
        {
            //Set the path to the application so the resource manager can
            //find the files it needs to load.
            ResourceManager.SetAppPath(Assembly.GetExecutingAssembly().Location, true);

            Audio.OpenAudio();

            Core.OpenGraphicsWindow("Hello World");

            Bitmap splashBack = ResourceManager.GetBitmap("SplashBack");

            while (false == Core.WindowCloseRequested())
            {
                Core.ProcessEvents();

                
                splashBack.DrawOnScreen(0, 0);

                Text.DrawFramerate(0, 0);
                Core.RefreshScreen();
            }

            ResourceManager.ReleaseAllResources();
            Audio.CloseAudio();
        }
    }
}
