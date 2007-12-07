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
        string ReciveWords(ref structtest word);
        int SendInt();
        string SendString();
        int ReciveInt(int number);
        string ReciveString(string word);
        void Mess(ref int number);
        string the(thing me);
        int testing(int asd);
        int testing2();
    }

    [ComVisible(true)]
    public enum thing
    {
        thing1,
        thing2
    }

    

    [ClassInterface(ClassInterfaceType.None)]
    [Guid("A4BD9C8F-062C-4d0e-B78D-9DAB426CFBF3")]
    [ComVisible(true)]
    public class VBTestClass : IInterface
    {
        public int testing2()
        {
            UInt32 temp = 4294967295;
            return (int)temp;
        }
        public int testing(int asd)
        {
            UInt32 temp = (UInt32)asd;
            System.Windows.Forms.MessageBox.Show(temp +"");
            return (int)asd;
        }
        public string the(thing me)
        {
            if (me == thing.thing1)
            {
                return "this was thing1";
            }
            else
            {
                return "this was thing2";
            }
        }
        public const int things = 12;
        public void Mess(ref int number)
        {
            number = (number * number);
        }
        public string ReciveWords(ref structtest word)
        {
            //Words temp = new Words();
            //temp = word.convert();
            return Library.ReciveWords(word);
        }
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
