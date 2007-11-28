using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace Test
{
    class Program
    {
        [DllImport("HelloWorld.dll")]
        static extern void SayHello();
        [DllImport("HelloWorld.dll")]
        static extern String MakeHello();

        static void Main(string[] args)
        {
            SayHello();
            String hello = "";
            Console.ReadLine();

            hello = MakeHello();
            System.GC.Collect();
   
            Console.WriteLine(hello);
            Console.ReadLine();
        }
    }
}
