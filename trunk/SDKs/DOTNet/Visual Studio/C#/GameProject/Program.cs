using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Drawing;
using SwinGame;

namespace GameProject
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("here");
           //Console.WriteLine(Core.GetColor(255, 0, 0, 255).ToString());
            Core.OpenGraphicsWindow("Hi", 800, 600);
            Core.OpenAudio();
            IntPtr sound = Core.LoadSoundEffect("SwinGameStart.ogg");
            Core.PlaySoundEffect(sound);

            while (Core.WindowCloseRequested() != true)
            {
           
                //Console.WriteLine(Core.GetFramerate());
                
                


                //Core.ToggleFullScreen();
                //Core.RefreshScreen();
                Core.ProcessEvents();
            }
            Core.FreeSoundEffect(ref sound);
            Core.CloseAudio();
        }
    }
}
