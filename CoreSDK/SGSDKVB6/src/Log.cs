using System;
using System.Collections.Generic;
using System.Text;
using SwinGame;
using System.Runtime.InteropServices;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
namespace SwinGameVB

{
    [ClassInterface(ClassInterfaceType.None)]
    [Guid("E13C55B0-B303-4520-881D-D21CCFE421C4")]
    [ComVisible(true)]
    public class Log :ILog
    {
        private StreamWriter writer;
        public void WriteLn(string line)
        {
                if (File.Exists(SwinGame.Core.GetPathToResource("../DebugLog.txt")))
                {
                writer = File.AppendText(SwinGame.Core.GetPathToResource("../DebugLog.txt"));
                }
                else
                {
                    writer = File.CreateText(SwinGame.Core.GetPathToResource("../DebugLog.txt"));
                }
                writer.WriteLine(line);
                writer.Close();
        }
        public void WriteLine(string line)
        {
            if (File.Exists(SwinGame.Core.GetPathToResource("../DebugLog.txt")))
            {
                writer = File.AppendText(SwinGame.Core.GetPathToResource("../DebugLog.txt"));
            }
            else
            {
                writer = File.CreateText(SwinGame.Core.GetPathToResource("../DebugLog.txt"));
            }
            writer.WriteLine(line);
            writer.Close();
        }
        public void Write(string line)
        {
            if (File.Exists(SwinGame.Core.GetPathToResource("../DebugLog.txt")))
            {
                writer = File.AppendText(SwinGame.Core.GetPathToResource("../DebugLog.txt"));
            }
            else
            {
                writer = File.CreateText(SwinGame.Core.GetPathToResource("../DebugLog.txt"));
            }
            writer.Write(line);
            writer.Close();
        }
        
    }

    [Guid("6D044702-3C97-482d-AEF9-28593CCF2EB0")]
    [ComVisible(true)]
    public interface ILog
    {
        void WriteLn(string line);
        void WriteLine(string line);
        void Write(string line);
    }
}
