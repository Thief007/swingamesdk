using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace CSharpLibrary
{
    [Guid("8857917C-A2D3-44ef-AF4B-D18AA3F322EE")]
    [ComVisible(true)]
    public interface Istructtest
    {
         string getTemp();
         void serTemp(string vlaue);
        
    }

    [ClassInterface(ClassInterfaceType.None)]
    [Guid("303068DE-1FE3-4d07-B25E-4743B1292D3B")]
    [ComVisible(true)]
    public class structtest : Istructtest
    {
        /*public struct Words
        {
            private string temp;
            public string getTemp()
            {
                return temp;
            }
            public void serTemp(string vlaue)
            {
                temp = vlaue;
            }
        }*/
        private Words word;

        internal Words Word()
        {
            return word;
        }

        //private string temp;
        public string getTemp()
        {
            return word.Hi;
        }
        public void serTemp(string vlaue)
        {
            word.Hi = vlaue;
        }
        //internal Words convert()
        //{
        //    Words thing = new Words();
        //    thing.Hi = temp;
        //    return thing;
        //}
    }

    [StructLayout(LayoutKind.Sequential)]
    [ComVisible(false)]
    public struct Words
    {
        private string hi;

        public string Hi
        {
            get { return hi; }
            set { hi = value; }
        }
    }
        
    [ClassInterface(ClassInterfaceType.None)]
    [ComVisible(false)]
    internal class Library
    {
        [DllImport("TestLibrary.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern string ReciveWords(Words word);
        public static string ReciveWords(structtest word)
        {
            return ReciveWords(word.Word());
        }

        //public static string ReciveWords(Words word)
        //{
        //    return ReciveWords(word.
        //}

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
