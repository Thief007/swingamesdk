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
using System.Runtime.InteropServices;

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
    
    [UnmanagedFunctionPointerAttribute(CallingConvention.Cdecl)]
    internal delegate PointerWrapper Creater(IntPtr pointer);
    
    /// <summary>
    /// Wraps a pointer to a SwinGame resource
    /// </summary>
    public abstract class PointerWrapper : IDisposable
    {
        /// <summary>
        /// The ptrRegistry is responsible for maintaining copies of all wrapped SwinGame pointers.
        /// </summary>
        protected static readonly Dictionary<IntPtr, PointerWrapper> _ptrRegister;
        
        internal static void Remove(IntPtr ptr)
        {
            if (_ptrRegister.ContainsKey(ptr))
            {
                _ptrRegister.Remove(ptr);
            }
        }
        
        static PointerWrapper()
        {
            //Register Remove with SwinGame
            //Console.WriteLine("Registering");
            _ptrRegister = new Dictionary<IntPtr, PointerWrapper>();
            sgLibrary.sg_Resources_RegisterFreeNotifier(PointerWrapper.Remove);
        }
        
        /// <summary>
        /// "Super Dodgy" (but correct) work around for the fact that C# has no unload methods for classes.
        /// </summary>
        internal class ReleaserClass
        {
            ~ReleaserClass()
            {
                Console.WriteLine("Deregistering");
                sgLibrary.sg_Resources_RegisterFreeNotifier(null);                
            }
        }
        
        internal static ReleaserClass releaser = new ReleaserClass();
        
        private PtrKind _Kind;
        protected internal IntPtr Pointer;
        
        protected internal abstract void DoFree();
        
        [System.Diagnostics.DebuggerNonUserCode(), System.Diagnostics.DebuggerStepThrough()]
        internal PointerWrapper(IntPtr ptr, PtrKind kind)
        {
            if (PointerWrapper._ptrRegister.ContainsKey(ptr)) throw new SwinGameException("Error managing resources.");
            PointerWrapper._ptrRegister[ptr] = this;
            Pointer = ptr;
            _Kind = kind;
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
