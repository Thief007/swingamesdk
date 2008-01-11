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

    /// <summary>
    /// Sound and Music Class
    /// </summary>
    public class Audio
    {
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="OpenAudio")]
        private static extern void DLL_OpenAudio();
        /// <summary>
        /// Opens the Audio System for SwinGameSDK
        /// </summary>
        public static void OpenAudio()
        {
            try
            {
                DLL_OpenAudio();
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
            }
            catch (Exception)
            {
                //if (Core.ExceptionOccured())
                throw new SwinGameException(Core.GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="CloseAudio")]
        private static extern void DLL_CloseAudio();
        /// <summary>
        /// Closes the Audio System for SwinGameSDK
        /// </summary>
        public static void CloseAudio()
        {
            try
            {
                DLL_CloseAudio();
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
            }
            catch (Exception)
            {
                //if (Core.ExceptionOccured())
                throw new SwinGameException(Core.GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "PlaySoundEffect")]
        private static extern void DLL_PlaySoundEffect(IntPtr effect);
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "PlaySoundEffectLoop")]
        private static extern void DLL_PlaySoundEffectLoop(IntPtr effect, int loops );
        /// <summary>
        /// Play the indicated sound effect a number of times
        /// </summary>
        /// <param name="effect">The Sound Effect to play</param>
        /// <param name="loops">The number of times to play it</param>
        public static void PlaySoundEffect(SoundEffect effect, int loops)
        {
            try
            {
                DLL_PlaySoundEffectLoop(effect.Pointer, loops);
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
            }
            catch (Exception)
            {
                //if (Core.ExceptionOccured())
                throw new SwinGameException(Core.GetExceptionMessage());
            }  
        }
        /// <summary>
        /// Play the indicated sound effect a number of times
        /// </summary>
        /// <param name="effect">The Sound Effect to play</param>
        public static void PlaySoundEffect(SoundEffect effect)
        {
            try
            {
                DLL_PlaySoundEffect(effect.Pointer);
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
            }
            catch (Exception)
            {
                //if (Core.ExceptionOccured())
                throw new SwinGameException(Core.GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "LoadSoundEffect")]
        private static extern IntPtr DLL_LoadSoundEffect(String path);
        /// <summary>
        /// Loads a SoundEffect
        /// </summary>
        /// <param name="path">Path to the Sound Effect file</param>
        /// <returns>A SoundEffect</returns>
        public static SoundEffect LoadSoundEffect(String path)
        {
            try
            {
                SoundEffect effect;
                effect.Pointer = DLL_LoadSoundEffect(path);
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
                return effect;
            }
            catch (Exception)
            {
                //if (Core.ExceptionOccured())
                throw new SwinGameException(Core.GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "FreeSoundEffect")]
        private static extern void DLL_FreeSoundEffect(ref IntPtr effect);
        /// <summary>
        /// Frees a Sound Effect From Memory
        /// </summary>
        /// <param name="effect">The effect to be freed from memory</param>
        public static void FreeSoundEffect(ref SoundEffect effect)
        {
            try
            {
                DLL_FreeSoundEffect(ref effect.Pointer);
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
            }
            catch (Exception)
            {
                //if (Core.ExceptionOccured())
                throw new SwinGameException(Core.GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "LoadMusic")]
        private static extern IntPtr DLL_LoadMusic(String path);
        /// <summary>
        /// Load music to play from the file system. Music can be in the form of a
        ///	wav, ogg, or mp3 file.
        /// </summary>
        /// <param name="Path">Path to Music file</param>
        /// <returns>Music</returns>
        public static Music LoadMusic(String Path)
        {
            try
            {
                Music music;
                music.Pointer = DLL_LoadMusic(Path);
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
                return music;
            }
            catch (Exception)
            {
                //if (Core.ExceptionOccured())
                throw new SwinGameException(Core.GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "FreeMusic")]
        private static extern void DLL_FreeMusic(ref IntPtr effect);
        /// <summary>
        /// Free a music value. All loaded music values need to be freed.
        /// </summary>
        /// <param name="music">Music to be freed</param>
        public static void FreeMusic(ref Music music)
        {
            try
            {
                DLL_FreeMusic(ref music.Pointer);
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
            }
            catch (Exception)
            {
                //if (Core.ExceptionOccured())
                throw new SwinGameException(Core.GetExceptionMessage());
            }  
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
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
            }
            catch (Exception)
            {
                //if (Core.ExceptionOccured())
                throw new SwinGameException(Core.GetExceptionMessage());
            }  
        }

        /// <summary>
        /// Play the indicated music effect a number of times
        /// </summary>
        /// <param name="music">The Music to play</param>
        public static void PlayMusic(Music music)
        {
            try
            {
                DLL_PlayMusic(music.Pointer, -1);
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
            }
            catch (Exception)
            {
                //if (Core.ExceptionOccured())
                throw new SwinGameException(Core.GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "IsSoundEffectPlaying")]
        private static extern bool DLL_IsSoundEffectPlaying(IntPtr effect);
        /// <summary>
        /// This function checks whether a sound is playing
        /// </summary>
        /// <param name="effect">The sound effect to check if it is playing</param>
        /// <returns>True if it is playing</returns>
        public static bool IsSoundEffectPlaying(SoundEffect effect)
        {
            try
            {
                bool temp = DLL_IsSoundEffectPlaying(effect.Pointer);
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (Core.ExceptionOccured())
                throw new SwinGameException(Core.GetExceptionMessage());
            }  
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
            try
            {
                bool temp = DLL_IsMusicPlaying(music.Pointer);
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (Core.ExceptionOccured())
                throw new SwinGameException(Core.GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "StopMusic")]
        private static extern void DLL_StopMusic();
        /// <summary>
        /// Stops music from playing
        /// </summary>
        public static void StopMusic()
        {
            try
            {
                DLL_StopMusic();
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
            }
            catch (Exception)
            {
                //if (Core.ExceptionOccured())
                throw new SwinGameException(Core.GetExceptionMessage());
            }  
        }
        
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "StopSoundEffect")]
        private static extern void DLL_StopSoundEffect();
        /// <summary>
        /// Stop playing sound effects
        /// </summary>
        public static void StopSoundEffect()
        {
            try
            {
                DLL_StopSoundEffect();
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
            }
            catch (Exception)
            {
                //if (Core.ExceptionOccured())
                throw new SwinGameException(Core.GetExceptionMessage());
            }  
        }

    }
}