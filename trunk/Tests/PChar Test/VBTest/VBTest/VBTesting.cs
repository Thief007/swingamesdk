using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace VBTest
{
    [Guid("51D9777B-3341-4026-8126-B98F3FB5A4FC")]
    [ComVisible(true)]
    public interface IInterface
    {
        void WriteString();
        string GetString();
        void SetString(String word);
    }

    [ClassInterface(ClassInterfaceType.None)]
    [Guid("A5271D34-7683-4060-8FEB-74AAA3A34528")]
    public class VBTestClass : IInterface
    {
        public VBTestClass()
        {
        }
        public void WriteString()
        {
            TestClass.WriteString();
        }

        public String GetString()
        {
            return TestClass.GetString();
        }

        public void SetString(String word)
        {
            TestClass.SetString(word);
            //SetString(word);
        }
    }
}
