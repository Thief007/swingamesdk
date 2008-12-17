using System;
using System.Collections.Generic;
using System.Text;

namespace SwinGame
{
    /// <summary>
    /// A sound effect object can be used to play 
    /// sounds using SwinGame.
    /// </summary>
    public class SoundEffect : IDisposable
    {
        //internal IntPtr Pointer;
        readonly internal SwinGamePointer Pointer;

        internal SoundEffect(IntPtr devPtr)
        {
            Pointer = new SwinGamePointer(devPtr, PtrKind.Sound);
        }

        public SoundEffect(string path) : this(SGSDK.LoadSoundEffect(path)) {}

        /// <summary>
        /// Play the sound effect once at 100% volume.
        /// </summary>
        public void Play()
        {
            SGSDK.PlaySoundEffect(this);
        }

        /// <summary>
        /// Play the sound effect a number of times, at 100% volume
        /// </summary>
        /// <param name="loops">the number of times to play for effect</param>
        public void Play(int loops)
        {
            SGSDK.PlaySoundEffectLoop(this, loops);
        }

        /// <summary>
        /// Play the sound effect at a given percentage volume.
        /// </summary>
        /// <param name="volume">the % volume, must be between 0 and 1, with 1.0 = 100% volume</param>
        public void Play(float volume)
        {
            SGSDK.PlaySoundEffectLoopVolume(this, 0, volume);
        }

        /// <summary>
        /// Play the sound effect a number of times at a given percentage volume.
        /// </summary>
        /// <param name="loops">the number of times to repeat the sound effect (0 plays once)</param>
        /// <param name="volume">the % volume, must be between 0 and 1, with 1.0 = 100% volume</param>
        public void Play(int loops, float volume)
        {
            SGSDK.PlaySoundEffectLoopVolume(this, loops, volume);
        }

        /// <summary>
        /// Determines if the sound effect is playing.
        /// </summary>
        /// <returns>true if the sound effect is playing</returns>
        public bool IsPlaying()
        {
            return SGSDK.IsSoundEffectPlaying(this) == -1;
        }

        /// <summary>
        /// Casts the SoundEffect to its native pointer.
        /// </summary>
        /// <param name="effect">the effect to cast</param>
        /// <returns>the native pointer</returns>
        public static implicit operator IntPtr(SoundEffect effect)
        {
            return effect.Pointer.Pointer;
        }

        #region IDisposable Members

        /// <summary>
        /// Clean up the native resources used by this sound effect.
        /// </summary>
        public void Dispose()
        {
            if(Pointer != null) Pointer.Free();
        }

        #endregion

        /// <summary>
        /// Stops the sound effect playing.
        /// </summary>
        public void Stop()
        {
            SGSDK.StopSoundEffect(this);
        }
    }
}
