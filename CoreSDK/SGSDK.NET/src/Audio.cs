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
// Version 2:
// - 2008-12-17: Andrew: Moved out other types
//                       Moved to SGSDK
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
    /// The Audio class contains the code to play sound effects and music.
    /// </summary>
    public class Audio
    {
        /// <summary>
        /// Opens the Audio System for SwinGameSDK. This must be called before any sound
        /// can be played. Usually this is at the start of your program.
        /// </summary>
        public static void OpenAudio()
        {
            SGSDK.OpenAudio();
        }

        /// <summary>
        /// Closes the Audio system. This should be done at the end of your program to 
        /// ensure that it closes safely.
        /// </summary>
        public static void CloseAudio()
        {
            //ensure that all music is freed before closing audio
            Core.ProcessEvents();
            SGSDK.CloseAudio();
        }

        /// <summary>
        /// Play the indicated sound effect a number of times.
        /// </summary>
        /// <param name="effect">The Sound Effect to play</param>
        /// <param name="loops">The number of times to play it. -1 plays it infinitely</param>
        public static void PlaySoundEffect(SoundEffect effect, int loops)
        {
            effect.Play(loops);
        }

        /// <summary>
        /// Play the indicated sound effect a number of times.
        /// </summary>
        /// <param name="effect">The Sound Effect to play</param>
        /// <param name="loops">The number of times to play it. -1 plays it infinitely</param>
        /// <param name="vol">The volume to play the effect at, 1 = 100%</param>
        public static void PlaySoundEffect(SoundEffect effect, int loops, float vol)
        {
            effect.Play(loops, vol);
        }

        /// <summary>
        /// Play the indicated sound effect at a given volume.
        /// </summary>
        /// <param name="effect">The Sound Effect to play</param>
        /// <param name="vol">The volume to play the effect at, 1 = 100%</param>
        public static void PlaySoundEffect(SoundEffect effect, float vol)
        {
            effect.Play(vol);
        }
        
        /// <summary>
        /// Play the indicated sound effect once.
        /// </summary>
        /// <param name="effect">The Sound Effect to play</param>
        public static void PlaySoundEffect(SoundEffect effect)
        {
            effect.Play();
        }

        /// <summary>
        /// Loads a SoundEffect from file. Use the GetPathToResource methods from Core to
        /// ensure that you load the file in a platform neutral way, enabling your game
        /// to run on Windows, Mac, and Linux.
        /// </summary>
        /// <param name="path">Path to the Sound Effect file</param>
        /// <returns>A SoundEffect</returns>
        public static SoundEffect LoadSoundEffect(string path)
        {
            return new SoundEffect(SGSDK.LoadSoundEffect(path));
        }

        /// <summary>
        /// Frees a Sound Effect From Memory. You need to ensure that all sound effects
        /// that you load are freed by the end of the game. This is usually done
        /// when the program exits.
        /// </summary>
        /// <param name="effect">The effect to be freed from memory</param>
        public static void FreeSoundEffect(SoundEffect effect)
        {
            effect.Dispose();
        }

        /// <summary>
        /// Load music to play from the file system. Music can be in the form of a
        ///	wav, ogg, or mp3 file. Use the GetPathToResource methods from Core to
        /// ensure that you load the file in a platform neutral way, enabling your game
        /// to run on Windows, Mac, and Linux.
        /// </summary>
        /// <param name="path">Path to Music file</param>
        /// <returns>Music</returns>
        public static Music LoadMusic(String path)
        {
            return new Music(path);
        }

        /// <summary>
        /// Free a music value. All loaded music values need to be freed by the
        /// end of the program. This is usually done when the program exits.
        /// </summary>
        /// <param name="music">Music to be freed</param>
        public static void FreeMusic(Music music)
        {
            music.Dispose();
        }

        /// <summary>
        /// Play the indicated music effect a number of times
        /// The loops paramater can use anything starting from -1, 
        /// -1 meaning that the sound plays forever
        /// </summary>
        /// <param name="music">The Music to play</param>
        /// <param name="loops">The number of times to play it</param>
        public static void PlayMusic(Music music, int loops)
        {
            music.Play(loops);
        }

        /// <summary>
        /// Play the indicated music until stopped. This will cause
        /// the music to loop.
        /// </summary>
        /// <param name="music">The Music to play</param>
        public static void PlayMusic(Music music)
        {
            music.Play();
        }

        /// <summary>
        /// This function checks whether a sound is playing. 
        /// </summary>
        /// <param name="effect">The sound effect to check if it is playing</param>
        /// <returns>True if it is playing</returns>
        public static bool IsSoundEffectPlaying(SoundEffect effect)
        {
            return effect.IsPlaying();
        }

        /// <summary>
        /// This function checks whether music is playing
        /// </summary>
        /// <returns>True if it is playing</returns>
        public static bool IsMusicPlaying()
        {
            return Music.IsPlaying();
        }

        /// <summary>
        /// Stops the current music from playing.
        /// </summary>
        public static void StopMusic()
        {
            SGSDK.StopMusic();
        }

        /// <summary>
        /// Stop playing the indicated sound effect.
        /// </summary>
        /// <param name="effect">The effect to stop playing.</param>
        public static void StopSoundEffect(SoundEffect effect)
        {
            effect.Stop();
        }

        /// <summary>
        /// Returns the current playback volume of the music.
        /// </summary>
        /// <returns>A value between 0 and 1.0 that represents the percentage volume of the music</returns>
        public static float MusicVolume()
        {
            return Music.Volume;
        }

        /// <summary>
        /// Sets the playback volume of the music.
        /// </summary>
        /// <param name="level">the level of the music as a percentage. 0 for no music, 1 for 100% volume.</param>
        public static void SetMusicVolume(float level)
        {
            Music.Volume = level;
        }
    }
}
