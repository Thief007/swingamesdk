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


            int[] arr = new int[10];

            for (int i = 0; i < 10; i++)
            {
                arr[i] = i+1;
            }

            TestClass.PrintArray(arr.Length,arr);

            Console.Write("Enter to quit");
            Console.ReadLine();
        }
    }
}
