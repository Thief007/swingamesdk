using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace CSharpLibrary
{
    [ClassInterface(ClassInterfaceType.None)]
    [ComVisible(false)]
    internal class Library
    {
        [DllImport("TestLibrary.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern int SendInt();

        [DllImport("TestLibrary.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern string SendString();

        [DllImport("TestLibrary.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ReciveInt(int number);

        [DllImport("TestLibrary.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern string ReciveString(string word);
    }
}
