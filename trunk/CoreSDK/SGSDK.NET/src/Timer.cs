//-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
// 					Timer
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
    /// Timers can be used to perform time based actions. You can
    /// create a number of timers, and each can be started, stopped,
    /// paused, etc. You can then read the number of ticks that have
    /// occurred since the timer was started, when paused this wont increase
    /// until it is unpaused. Ticks are measured in milliseconds, 1/1000 of
    /// a second.
    /// </summary>
    public class Timer: IDisposable
    {
        //internal IntPtr Pointer;
        internal readonly SwinGamePointer pointer;

        internal Timer(IntPtr devPtr, bool isCopy)
        {
            if(isCopy)
                pointer = new SwinGamePointer(devPtr, PtrKind.Copy);
            else
                pointer = new SwinGamePointer(devPtr, PtrKind.Timer);
        }

        /// <summary>
        /// Cast the bitmap to its native pointer.
        /// </summary>
        /// <param name="tmr">the timer</param>
        /// <returns>its native pointer</returns>
        public static implicit operator IntPtr(Timer tmr)
        {
            return tmr.pointer.Pointer;
        }

        #region IDisposable Members

        /// <summary>
        /// Clean up the associated system resources.
        /// </summary>
        public void Dispose()
        {
            pointer.Free();
        }

        #endregion

        /// <summary>
        /// Creates a new Timer that you can start, stop, read, etc. Timers
        /// are useful for implementing time based movement rather than framerate
        /// based movement. You must free the timer when you are finished with
        /// it.
        /// </summary>
        public Timer()
        {
            pointer = new SwinGamePointer(SGSDK.CreateTimer(), PtrKind.Timer);
        }

        /// <summary>
        /// Start the timer ticking. The timer's value will now increase with time.
        /// </summary>
        public void Start()
        {
            SGSDK.StartTimer(this);
        }

        /// <summary>
        /// Stop a timer. If the timer is restarted it will now reset back
        /// to 0 ticks.
        /// </summary>
        public void Stop()
        {
            SGSDK.StopTimer(this);
        }

        /// <summary>
        /// Pause the timer. Reading the timer will now give the same value until
        /// the timer is unpaused, at which point it will continue from where
        /// it is up to.
        /// </summary>
        public void Pause()
        {
            SGSDK.PauseTimer(this);
        }

        /// <summary>
        /// Resume a paused timer. The timer will continue from where it was up to
        /// when it was paused.
        /// </summary>
        public void Resume()
        {
            SGSDK.UnpauseTimer(this);
        }

        /// <summary>
        /// Get the number of ticks (milliseconds) that have passed since
        /// the timer was started. When paused this will return the same 
        /// value until unpaused.
        /// </summary>
        public UInt32 Ticks
        {
            get
            {
                return SGSDK.GetTimerTicks(this);
            }
        }
    }
}
