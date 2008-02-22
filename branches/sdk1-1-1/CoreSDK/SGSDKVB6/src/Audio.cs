using System;
using System.Collections.Generic;
using System.Text;
using SwinGame;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace SwinGameVB
{
    /// <summary>
    /// Sound Effect Structure
    /// </summary>
    [ClassInterface(ClassInterfaceType.None)]
    [Guid("72A680BD-E34C-457c-8731-6E2F28195839")]
    [ComVisible(true)]
    public class SoundEffect :ISoundEffect
    {
        private SwinGame.SoundEffect soundeffect;
        internal void Free()
        {
            SwinGame.Audio.FreeSoundEffect( soundeffect);
        }
        internal SwinGame.SoundEffect result
        {
            get
            {
                return soundeffect;
            }
            set
            {
                soundeffect = value;
            }
        }
    }

    [Guid("1CCC8692-C108-487c-B3B3-50637A026CC3")]
    [ComVisible(true)]
    public interface ISoundEffect
    {
    }

    /// <summary>
    /// Music Structure
    /// </summary>
    [ClassInterface(ClassInterfaceType.None)]
    [Guid("71F9C2D2-D0F7-434f-9D15-C4D6DC471247")]
    [ComVisible(true)]
    public class Music :IMusic
    {
        private SwinGame.Music music;
        internal void Free()
        {
            SwinGame.Audio.FreeMusic( music);
        }
        internal SwinGame.Music result
        {
            get
            {
                return music;
            }
            set
            {
                music = value;
            }
        }
    }

    [Guid("74A96D07-B80B-4585-8F0B-31AC2F70B373")]
    [ComVisible(true)]
    public interface IMusic
    {
    }

    [ClassInterface(ClassInterfaceType.None)]
    [Guid("5A3A1837-A7B4-4490-83ED-D18522B36651")]
    [ComVisible(true)]
    public class Audio : IAudio
    {

        public void OpenAudio()
        {
            
            SwinGame.Audio.OpenAudio();
        }
        public void CloseAudio()
        {
            SwinGame.Audio.CloseAudio();
        }

        /// <summary>
        /// Play the indicated sound effect a number of times
        /// </summary>
        /// <param name="effect">The Sound Effect to play</param>
        /// <param name="loops">The number of times to play it</param>
        public string PlaySoundEffect_Loop(SoundEffect effect, int loops)
        {
            SwinGame.Audio.PlaySoundEffect(effect.result, loops);
            return "";
        }

        /// <summary>
        /// Play the indicated sound effect a number of times
        /// </summary>
        /// <param name="effect">The Sound Effect to play</param>
        public void PlaySoundEffect(SoundEffect effect)
        {
            SwinGame.Audio.PlaySoundEffect(effect.result);   
        }

        /// <summary>
        /// Loads a SoundEffect
        /// </summary>
        /// <param name="path">Path to the Sound Effect file</param>
        /// <returns>A SoundEffect</returns>
        public SoundEffect LoadSoundEffect(String path)
        {
            SoundEffect effect = new SoundEffect();
            effect.result = SwinGame.Audio.LoadSoundEffect(path);
            return effect;
        }

        /// <summary>
        /// Frees a Sound Effect From Memory
        /// </summary>
        /// <param name="effect">The effect to be freed from memory</param>
        public void FreeSoundEffect(SoundEffect effect)
        {
            effect.Free();
        }

        /// <summary>
        /// Load music to play from the file system. Music can be in the form of a
        ///	wav, ogg, or mp3 file.
        /// </summary>
        /// <param name="Path">Path to Music file</param>
        /// <returns>Music</returns>
        public Music LoadMusic(String Path)
        {
            Music music = new Music();
            music.result = SwinGame.Audio.LoadMusic(Path);
            return music;
        }

        /// <summary>
        /// Free a music value. All loaded music values need to be freed.
        /// </summary>
        /// <param name="music">Music to be freed</param>
        public void FreeMusic(Music music)
        {
            music.Free();
        }

        /// <summary>
        /// Play the indicated music effect a number of times
        /// The loops paramater can use anything starting from -1, 
        /// -1 meaning that the sound plays forever
        /// </summary>
        /// <param name="effect">The Music to play</param>
        /// <param name="loops">The number of times to play it</param>
        public void PlayMusic_Loop(Music music, int loops)
        {
            SwinGame.Audio.PlayMusic(music.result, loops);  
        }

        /// <summary>
        /// Play the indicated music effect a number of times
        /// </summary>
        /// <param name="effect">The Music to play</param>
        public void PlayMusic(Music music)
        {
            SwinGame.Audio.PlayMusic(music.result, -1);  
        }

        /// <summary>
        /// This function checks whether a sound is playing
        /// </summary>
        /// <param name="effect">The sound effect to check if it is playing</param>
        /// <returns>True if it is playing</returns>
        public bool IsSoundEffectPlaying(SoundEffect effect)
        {
            return SwinGame.Audio.IsSoundEffectPlaying(effect.result);
          
        }

        /// <summary>
        /// This function checks whether music is playing
        /// </summary>
        /// <param name="effect">The music to check if it is playing</param>
        /// <returns>True if it is playing</returns>
        public bool IsMusicPlaying(Music music)
        {
            return SwinGame.Audio.IsMusicPlaying(music.result);
        }

        /// Stops music from playing
        public void StopMusic()
        {
            SwinGame.Audio.StopMusic();
        }

        /// Stop playing sound effects
        public void StopSoundEffect(SoundEffect effect)
        {
            SwinGame.Audio.StopSoundEffect(effect.result);
        }
    }

    [Guid("E2F38A8B-B51E-4779-A5CA-92AD78198B78")]
    [ComVisible(true)]
    public interface IAudio
    {
        void OpenAudio();
        void CloseAudio();
        string PlaySoundEffect_Loop(SoundEffect effect, int loops);

        void PlaySoundEffect(SoundEffect effect);

        SoundEffect LoadSoundEffect(String path);

        void FreeSoundEffect(SoundEffect effect);

        Music LoadMusic(String Path);
        void FreeMusic(Music music);
        void PlayMusic_Loop(Music music, int loops);
        void PlayMusic(Music music);
        bool IsSoundEffectPlaying(SoundEffect effect);
        bool IsMusicPlaying(Music music);
        void StopMusic();
        void StopSoundEffect(SoundEffect effect);
    }
}
