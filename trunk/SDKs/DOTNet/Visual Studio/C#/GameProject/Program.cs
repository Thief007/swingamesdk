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
