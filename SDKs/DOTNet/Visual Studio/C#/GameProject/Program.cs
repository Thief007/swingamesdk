using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace GameProject
{
    class Program
    {
        static void Main(string[] args)
        {
            SwinGame.OpenGraphicsWindow("Hi", 800, 600);

            while (SwinGame.WindowCloseRequested() != true)
            {
                System.Threading.Thread.Sleep(100);
                //SwinGame.ProcessEvents();
            }
         
        }
    }
}
