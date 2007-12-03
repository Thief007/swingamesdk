using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.IO;

namespace SwinGame
{
    /// <summary>
    /// Sound Effect Structure
    /// </summary>
    public struct SoundEffect
    {
        internal IntPtr Pointer;
    }

    /// <summary>
    /// Music Structure
    /// </summary>
    public struct Music
    {
        internal IntPtr Pointer;
    }

    public class Audio
    {

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern void OpenAudio();
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern void CloseAudio();

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "PlaySoundEffect")]
        private static extern void DLL_PlaySoundEffect(IntPtr effect);
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "PlaySoundEffectLoop")]
        private static extern void DLL_PlaySoundEffectLoop(IntPtr effect, int loops );
        /// <summary>
        /// Play the indicated sound effect a number of times
        /// </summary>
        /// <param name="effect">The Sound Effect to play</param>
        /// <param name="loops">The number of times to play it</param>
        public static void PlaySoundEffect(SoundEffect effect, int loops)
        {
            DLL_PlaySoundEffectLoop(effect.Pointer, loops);  
        }

        /// <summary>
        /// Play the indicated sound effect a number of times
        /// </summary>
        /// <param name="effect">The Sound Effect to play</param>
        public static void PlaySoundEffect(SoundEffect effect)
        {
            DLL_PlaySoundEffect(effect.Pointer);  
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "LoadSoundEffect")]
        private static extern IntPtr DLL_LoadSoundEffect(String path);
        /// <summary>
        /// Loads a SoundEffect
        /// </summary>
        /// <param name="path">Path to the Sound Effect file</param>
        /// <returns>A SoundEffect</returns>
        public static SoundEffect LoadSoundEffect(String path)
        {
            SoundEffect effect;
            effect.Pointer = DLL_LoadSoundEffect(path);
            return effect;
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "FreeSoundEffect")]
        private static extern void DLL_FreeSoundEffect(ref IntPtr effect);
        /// <summary>
        /// Frees a Sound Effect From Memory
        /// </summary>
        /// <param name="effect">The effect to be freed from memory</param>
        public static void FreeSoundEffect(ref SoundEffect effect)
        {
            DLL_FreeSoundEffect(ref effect.Pointer);
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "LoadMusic")]
        private static extern IntPtr DLL_LoadMusic(String path);
        /// <summary>
        /// Load music to play from the file system. Music can be in the form of a
        ///	wav, ogg, or mp3 file.
        /// </summary>
        /// <param name="Path">Path to Music file</param>
        /// <returns>Music</returns>
        public static Music LoadMusic(String Path)
        {
            Music music;
            music.Pointer = DLL_LoadMusic(Path);
            return music;
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "FreeMusic")]
        private static extern void DLL_FreeMusic(ref IntPtr effect);
        /// <summary>
        /// Free a music value. All loaded music values need to be freed.
        /// </summary>
        /// <param name="music">Music to be freed</param>
        public static void FreeMusic(ref Music music)
        {
            DLL_FreeMusic(ref music.Pointer);
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "PlayMusic")]
        private static extern void DLL_PlayMusic(IntPtr music, int loops);
        /// <summary>
        /// Play the indicated music effect a number of times
        /// The loops paramater can use anything starting from -1, 
        /// -1 meaning that the sound plays forever
        /// </summary>
        /// <param name="effect">The Music to play</param>
        /// <param name="loops">The number of times to play it</param>
        public static void PlayMusic(Music music, int loops)
        {
            DLL_PlayMusic(music.Pointer, loops);  
        }

        /// <summary>
        /// Play the indicated music effect a number of times
        /// </summary>
        /// <param name="effect">The Music to play</param>
        public static void PlayMusic(Music music)
        {
            DLL_PlayMusic(music.Pointer,-1);  
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "IsSoundEffectPlaying")]
        private static extern bool DLL_IsSoundEffectPlaying(IntPtr effect);
        /// <summary>
        /// This function checks whether a sound is playing
        /// </summary>
        /// <param name="effect">The sound effect to check if it is playing</param>
        /// <returns>True if it is playing</returns>
        public static bool IsSoundEffectPlaying(SoundEffect effect)
        {
            return DLL_IsSoundEffectPlaying(effect.Pointer);
          
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "IsMusicPlaying")]
        private static extern bool DLL_IsMusicPlaying(IntPtr music);
        /// <summary>
        /// This function checks whether music is playing
        /// </summary>
        /// <param name="effect">The music to check if it is playing</param>
        /// <returns>True if it is playing</returns>
        public static bool IsMusicPlaying(Music music)
        {
            return DLL_IsMusicPlaying(music.Pointer);
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "StopMusic")]
        /// Stops music from playing
        public static extern void StopMusic();

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "StopSoundEffect")]
        /// Stop playing sound effects
        public static extern void StopSoundEffect();


    }
}
