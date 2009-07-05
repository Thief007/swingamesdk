//----------------------------------------------------------------------------
// PointerWrapper.cs
//----------------------------------------------------------------------------
//
//  Contains code used by the SwinGame resources. used by SGWrapperGen
//
//----------------------------------------------------------------------------
using System;
using System.Collections.Generic;
using System.Text;

namespace SwinGame
{
    //internal delegate void FreeDelegate(IntPtr toFree);
    internal enum PtrKind
    {
        Bitmap,
        Font,
        SoundEffect,
        Music,
        Map,
        Sprite,
        Timer,
        Copy
    }
    
    internal delegate PointerWrapper Creater(IntPtr pointer);
    
    /// <summary>
    /// Wraps a pointer to a SwinGame resource
    /// </summary>
    public abstract class PointerWrapper : IDisposable
    {
        /// <summary>
        /// The ptrRegistry is responsible for maintaining copies of all wrapped SwinGame pointers.
        /// </summary>
        protected static readonly Dictionary<IntPtr, WeakReference> _ptrRegister = new Dictionary<IntPtr, WeakReference>();
        
        internal static void Remove(IntPtr ptr)
        {
            _ptrRegister.Remove(ptr);
        }
        
        private static List<PointerWrapper> _ToFree = new List<PointerWrapper>();

        /// <summary>
        /// Registers the pointer with SwinGame to have the memory freed.
        /// This is done to ensure freeing is done on a single thread.
        /// </summary>
        /// <param name="ptr">the pointer to register</param>
        /// <param name="kind">the type of the pointer</param>
        private static void RegisterDelete(PointerWrapper pw)
        {
            lock (_ToFree)
            {
                _ToFree.Add(pw);
            }
        }
        
        /// <summary>
        /// Free all pointers awaiting deletion.
        /// </summary>
        internal static void FreeAnythingToFree()
        {
            lock (_ToFree)
            {
                foreach (PointerWrapper pw in _ToFree)
                {
                    pw.DoFree();
                }
                _ToFree.Clear();
            }
        }
        
        private PtrKind _Kind;
        protected internal IntPtr Pointer;
        
        protected internal abstract void DoFree();
        
        [System.Diagnostics.DebuggerNonUserCode(), System.Diagnostics.DebuggerStepThrough()]
        internal PointerWrapper(IntPtr ptr, PtrKind kind)
        {
            if (PointerWrapper._ptrRegister.ContainsKey(ptr)) throw new SwinGameException("Error managing resources.");
            PointerWrapper._ptrRegister[ptr] = new WeakReference(this);
            Pointer = ptr;
            _Kind = kind;
            
            if (_Kind == PtrKind.Copy) GC.SuppressFinalize(this);
        }
        
        [System.Diagnostics.DebuggerNonUserCode(), System.Diagnostics.DebuggerStepThrough()]
        ~PointerWrapper()
        {
            PointerWrapper.RegisterDelete(this);
        }
        
        [System.Diagnostics.DebuggerNonUserCode(), System.Diagnostics.DebuggerStepThrough()]
        public static implicit operator IntPtr(PointerWrapper p)
        {
            return p.Pointer;
        }
        
        #region IDisposable Members
        
        /// <summary>
        /// Clean up the native resources used by this resource.
        /// </summary>
        [System.Diagnostics.DebuggerNonUserCode(), System.Diagnostics.DebuggerStepThrough()]
        public void Dispose()
        {
            if (_Kind != PtrKind.Copy)
            {
              GC.SuppressFinalize(this);
              DoFree();
            }
        }
        
        #endregion
        
        [System.Diagnostics.DebuggerNonUserCode(), System.Diagnostics.DebuggerStepThrough()]
        public override String ToString()
        {
            return String.Format("%s (%x)", _Kind, Pointer);
        }
        
        /// <summary>
        /// Determines if the PointerWrappers is equal to the passed in object.
        /// </summary>
        [System.Diagnostics.DebuggerNonUserCode(), System.Diagnostics.DebuggerStepThrough()]
        public override bool Equals(object other)
        {
            if (other is PointerWrapper) return this.Pointer == ((PointerWrapper)other).Pointer;
            else if (other is IntPtr) return this.Pointer == ((IntPtr)other);
            else return false;
        }
        
        /// <summary>
        /// Returns the hash code of the PointerWrapper based on what it points to.
        /// </summary>
        [System.Diagnostics.DebuggerNonUserCode(), System.Diagnostics.DebuggerStepThrough()]
        public override int GetHashCode()
        {
            return this.Pointer.GetHashCode();
        }
        
        /// <summary>
        /// Determines if two PointerWrappers are equal.
        /// </summary>
        [System.Diagnostics.DebuggerNonUserCode(), System.Diagnostics.DebuggerStepThrough()]
        public static bool operator ==(PointerWrapper pw1, PointerWrapper pw2)
        {
            return pw1.Pointer == pw2.Pointer;
        }
        
        /// <summary>
        /// Determines if two PointerWrappers are not equal.
        /// </summary>
        [System.Diagnostics.DebuggerNonUserCode(), System.Diagnostics.DebuggerStepThrough()]
        public static bool operator !=(PointerWrapper pw1, PointerWrapper pw2)
        {
            return pw1.Pointer != pw2.Pointer;
        }
        
    }
}
