using System;
using System.Collections.Generic;
using System.Text;

using TestSpace;

namespace PChar_Test
{
    class Program
    {
        static void Main(string[] args)
        {
            TestClass.SetString("This is a test");
            TestClass.WriteString();
            Console.WriteLine(TestClass.GetString());
            Console.Write("Enter to quit");
            Console.ReadLine();
        }
    }
}
