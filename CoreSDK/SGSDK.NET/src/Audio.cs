//-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
// 					Audio
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// The Audio unit is responsible for managing SDL audio
// this includes initialisation, loading, freeing, 
// playing, and checking if playing.
//
// Change History:
//
// Version 1.1:
// - 2008-01-30: Andrew: Fixed String Marshalling and Free
// - 2008-01-29: Andrew: Removed ref from Free
// - 2008-01-23: Andrew: Fixed exceptions
//               Added changes for 1.1 compatibility
//               Refactored some methods, to limit routines exposed by DLL
//               Fixed StopSoundEffect.
//               Added extra comments, and fixed code layout and line endings.
//               Using SwinGamePointer
// Version 1.0:
// - Various

using System;
using System.Runtime.InteropServices;

namespace SwinGame
{
    /// <summary>
    /// Sound Effect Structure
    /// </summary>
    public struct SoundEffect
    {
        //internal IntPtr Pointer;
        internal SwinGamePointer Pointer;
    }

    /// <summary>
    /// The Music structure is used to hold music that can then be
    /// played and stopped by SwinGame. You need to ensure that you free
    /// all music at the end of the program (or when you no longer need it)
    /// </summary>
    public struct Music
    {
        //internal IntPtr Pointer;
        internal SwinGamePointer Pointer;
    }

    /// <summary>
    /// The Audio class contains the code to play sound effects and music.
    /// </summary>
    public class Audio
    {
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "OpenAudio")]
        private static extern void DLL_OpenAudio();
        /// <summary>
        /// Opens the Audio System for SwinGameSDK. This must be called before any sound
        /// can be played. Usually this is at the start of your program.
        /// </summary>
        public static void OpenAudio()
        {
            try
            {
                DLL_OpenAudio();
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }

            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CloseAudio")]
        private static extern void DLL_CloseAudio();
        /// <summary>
        /// Closes the Audio system. This should be done at the end of your program to 
        /// ensure that it closes safely.
        /// </summary>
        public static void CloseAudio()
        {
            try
            {
                DLL_CloseAudio();
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }

            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "PlaySoundEffectLoop")]
        private static extern void DLL_PlaySoundEffectLoop(IntPtr effect, int loops);
        /// <summary>
        /// Play the indicated sound effect a number of times.
        /// </summary>
        /// <param name="effect">The Sound Effect to play</param>
        /// <param name="loops">The number of times to play it. -1 plays it infinitely</param>
        public static void PlaySoundEffect(SoundEffect effect, int loops)
        {
            try
            {
                DLL_PlaySoundEffectLoop(effect.Pointer, loops);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }

            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
        }
        /// <summary>
        /// Play the indicated sound effect once.
        /// </summary>
        /// <param name="effect">The Sound Effect to play</param>
        public static void PlaySoundEffect(SoundEffect effect)
        {
            PlaySoundEffect(effect, 0);
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "LoadSoundEffect", CharSet=CharSet.Ansi)]
        private static extern IntPtr DLL_LoadSoundEffect([MarshalAs(UnmanagedType.LPStr)]String path);
        /// <summary>
        /// Loads a SoundEffect from file. Use the GetPathToResource methods from Core to
        /// ensure that you load the file in a platform neutral way, enabling your game
        /// to run on Windows, Mac, and Linux.
        /// </summary>
        /// <param name="path">Path to the Sound Effect file</param>
        /// <returns>A SoundEffect</returns>
        public static SoundEffect LoadSoundEffect(string path)
        {
            SoundEffect effect;

            try
            {
                effect.Pointer = new SwinGamePointer(DLL_LoadSoundEffect(path), PtrKind.Sound);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }

            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }

            return effect;
        }

        /// <summary>
        /// Frees a Sound Effect From Memory. You need to ensure that all sound effects
        /// that you load are freed by the end of the game. This is usually done
        /// when the program exits.
        /// </summary>
        /// <param name="effect">The effect to be freed from memory</param>
        public static void FreeSoundEffect(SoundEffect effect)
        {
            effect.Pointer.Free();
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "LoadMusic", CharSet=CharSet.Ansi)]
        private static extern IntPtr DLL_LoadMusic([MarshalAs(UnmanagedType.LPStr)]String path);
        /// <summary>
        /// Load music to play from the file system. Music can be in the form of a
        ///	wav, ogg, or mp3 file. Use the GetPathToResource methods from Core to
        /// ensure that you load the file in a platform neutral way, enabling your game
        /// to run on Windows, Mac, and Linux.
        /// </summary>
        /// <param name="Path">Path to Music file</param>
        /// <returns>Music</returns>
        public static Music LoadMusic(String Path)
        {
            Music music;

            try
            {
                music.Pointer = new SwinGamePointer(DLL_LoadMusic(Path), PtrKind.Music);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return music;
        }

        /// <summary>
        /// Free a music value. All loaded music values need to be freed by the
        /// end of the program. This is usually done when the program exits.
        /// </summary>
        /// <param name="music">Music to be freed</param>
        public static void FreeMusic(Music music)
        {
            music.Pointer.Free();
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "PlayMusic")]
        private static extern void DLL_PlayMusic(IntPtr music, int loops);
        /// <summary>
        /// Play the indicated music effect a number of times
        /// The loops paramater can use anything starting from -1, 
        /// -1 meaning that the sound plays forever
        /// </summary>
        /// <param name="music">The Music to play</param>
        /// <param name="loops">The number of times to play it</param>
        public static void PlayMusic(Music music, int loops)
        {
            try
            {
                DLL_PlayMusic(music.Pointer, loops);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }

            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
        }

        /// <summary>
        /// Play the indicated music until stopped. This will cause
        /// the music to loop.
        /// </summary>
        /// <param name="music">The Music to play</param>
        public static void PlayMusic(Music music)
        {
            PlayMusic(music, -1);
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "IsSoundEffectPlaying")]
        private static extern bool DLL_IsSoundEffectPlaying(IntPtr effect);
        /// <summary>
        /// This function checks whether a sound is playing. 
        /// </summary>
        /// <param name="effect">The sound effect to check if it is playing</param>
        /// <returns>True if it is playing</returns>
        public static bool IsSoundEffectPlaying(SoundEffect effect)
        {
            bool temp;
            try
            {
                temp = DLL_IsSoundEffectPlaying(effect.Pointer);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "IsMusicPlaying")]
        private static extern bool DLL_IsMusicPlaying(IntPtr music);
        /// <summary>
        /// This function checks whether music is playing
        /// </summary>
        /// <param name="music">The music to check if it is playing</param>
        /// <returns>True if it is playing</returns>
        public static bool IsMusicPlaying(Music music)
        {
            bool temp;
            try
            {
                temp = DLL_IsMusicPlaying(music.Pointer);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "StopMusic")]
        private static extern void DLL_StopMusic();
        /// <summary>
        /// Stops the current music from playing.
        /// </summary>
        public static void StopMusic()
        {
            try
            {
                DLL_StopMusic();
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }

            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "StopSoundEffect")]
        private static extern void DLL_StopSoundEffect(IntPtr effect);
        /// <summary>
        /// Stop playing the indicated sound effect.
        /// </summary>
        /// <param name="effect">The effect to stop playing.</param>
        public static void StopSoundEffect(SoundEffect effect)
        {
            try
            {
                DLL_StopSoundEffect(effect.Pointer);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }

            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
        }

    }
}
