using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace SwinGame
{
    class Audio
    {
        /// <summary>
        /// 
        /// </summary>
        /// <param name="effect"></param>
        [DllImport("SGSDK.dll")]
        public static extern void PlaySoundEffect(IntPtr effect);

        [DllImport("SGSDK.dll")]
        public static extern IntPtr LoadSoundEffect(String path);

        [DllImport("SGSDK.dll")]
        public static extern void OpenAudio();

        [DllImport("SGSDK.dll")]
        public static extern void CloseAudio();
        [DllImport("SGSDK.dll")]
        public static extern void FreeSoundEffect(ref IntPtr effect);

    }
}
