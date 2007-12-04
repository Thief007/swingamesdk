using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;


namespace CSharpLibrary
{
    [Guid("E349377B-D7FA-434e-A25D-EC0EEE80C589")]
    [ComVisible(true)]
    public interface IInterface
    {
        int SendInt();
        string SendString();
        int ReciveInt(int number);
        string ReciveString(string word);
    }

    [ClassInterface(ClassInterfaceType.None)]
    [Guid("A4BD9C8F-062C-4d0e-B78D-9DAB426CFBF3")]
    [ComVisible(true)]
    public class VBTestClass : IInterface
    {
        public int SendInt()
        {
            return Library.SendInt();
        }
        public string SendString()
        {
            return Library.SendString();
        }
        public int ReciveInt(int number)
        {
            return Library.ReciveInt(number);
        }

        public string ReciveString(string word)
        {
            return Library.ReciveString(word);
        }
    }
}
