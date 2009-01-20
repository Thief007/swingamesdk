//-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
// 					Music
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// Change History:
//
// Version 2.0:
// - 2009-01-20: Andrew: Added version histroy 
//                       to newly created classes
//
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\

using System;
using System.Collections.Generic;
using System.Text;

namespace SwinGame
{
    /// <summary>
    /// The Music structure is used to hold music that can then be
    /// played and stopped by SwinGame. You need to ensure that you free
    /// all music at the end of the program (or when you no longer need it)
    /// </summary>
    public class Music: IDisposable
    {
        //internal IntPtr Pointer;
        readonly internal SwinGamePointer Pointer;

        internal Music(IntPtr devPtr)
        {
            Pointer = new SwinGamePointer(devPtr, PtrKind.Music);
        }

        /// <summary>
        /// Loads a music file from a give file path.
        /// </summary>
        /// <param name="path">The path to the music file to load.</param>
        public Music(string path) : this(SGSDK.LoadMusic(path)) { }

        /// <summary>
        /// Start playing the music, the music will loop.
        /// </summary>
        public void Play()
        {
            SGSDK.PlayMusic(this, -1);
        }

        /// <summary>
        /// Play the music a number of times
        /// </summary>
        /// <param name="loops">the number of times to play for effect</param>
        public void Play(int loops)
        {
            SGSDK.PlayMusic(this, loops);
        }

        /// <summary>
        /// Casts the SoundEffect to its native pointer.
        /// </summary>
        /// <param name="effect">the effect to cast</param>
        /// <returns>the native pointer</returns>
        public static implicit operator IntPtr(Music effect)
        {
            return effect.Pointer.Pointer;
        }

        #region IDisposable Members

        /// <summary>
        /// Clear the system resources used for this music.
        /// </summary>
        public void Dispose()
        {
            if (Pointer != null) Pointer.Free();
        }

        #endregion

        /// <summary>
        /// Is used to determine if the given music is currently playing.
        /// </summary>
        /// <returns>true if the music is playing</returns>
        public static bool IsPlaying()
        {
            return SGSDK.IsMusicPlaying() == -1;
        }

        /// <summary>
        /// Stops the music playing.
        /// </summary>
        /// <returns></returns>
        public static void Stop()
        {
            SGSDK.StopMusic();
        }

        /// <summary>
        /// The volume of the music playback.
        /// </summary>
        public static float Volume
        {
            get
            {
                return SGSDK.MusicVolume();
            }
            set
            {
                SGSDK.SetMusicVolume(value);
            }
        }
    }
}
