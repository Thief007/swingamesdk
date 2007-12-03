using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace TestSpace
{
    class TestClass
    {
        [DllImport("TestLibrary.dll")]
        public static extern void WriteString();

        [DllImport("TestLibrary.dll")]
        public static extern String GetString();

        //[DllImport("TestLibrary.dll", CharSet=CharSet.Ansi, CallingConvention=CallingConvention.Cdecl)]
        //[DllImport("TestLibrary.dll", CallingConvention = CallingConvention.Cdecl)]
        //[DllImport("TestLibrary.dll", CharSet=CharSet.Ansi)]
        [DllImport("TestLibrary.dll")]
        public static extern void SetString(String word);
    }
}
