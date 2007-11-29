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
            Core.OpenGraphicsWindow("Hi", 800, 600);


            SwinGame.Bitmap test = Core.LoadBitmap("SwinGameAni.png");
            Console.WriteLine(test.Width);
            Console.WriteLine(test.Height);
            while (Core.WindowCloseRequested() != true)
            {
           
                //Console.WriteLine(Core.GetFramerate());
                
                


                //Core.ToggleFullScreen();
                //Core.RefreshScreen();
                Core.ProcessEvents();
            }
      
        }
    }
}
