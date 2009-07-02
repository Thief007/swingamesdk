//-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
// 					SwinGameException
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// Change History:
//
// Version 3.0:
// - 2009-06-30: Andrew: Added class
//
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\

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
    
    internal delegate T Creater<T>(IntPtr pointer);
    
    /// <summary>
    /// Wraps a pointer to a SwinGame resource
    /// </summary>
    public abstract class PointerWrapper : IDisposable
    {
        /// <summary>
        /// The ptrRegistry is responsible for maintaining copies of all wrapped SwinGame pointers.
        /// </summary>
        private static readonly Dictionary<IntPtr, WeakReference> _ptrRegister = new Dictionary<IntPtr, WeakReference>();
        
        internal static T Create<T>(IntPtr ptr, Creater<T> create) where T : PointerWrapper
        {
            if (_ptrRegister.ContainsKey(ptr)) return _ptrRegister[ptr].Target as T;
            else return create(ptr); //call via delegate
        }
        
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
        
        internal PointerWrapper(IntPtr ptr, PtrKind kind)
        {
            if (PointerWrapper._ptrRegister.ContainsKey(ptr)) throw new SwinGameException("Error managing resources.");
            PointerWrapper._ptrRegister[ptr] = new WeakReference(this);
            Pointer = ptr;
            _Kind = kind;
            
            if (_Kind == PtrKind.Copy) GC.SuppressFinalize(this);
        }
        
        ~PointerWrapper()
        {
            PointerWrapper.RegisterDelete(this);
        }
        
        public static implicit operator IntPtr(PointerWrapper p)
        {
            return p.Pointer;
        }
        
        #region IDisposable Members
        
        /// <summary>
        /// Clean up the native resources used by this resource.
        /// </summary>
        public void Dispose()
        {
            if (_Kind != PtrKind.Copy)
            {
              GC.SuppressFinalize(this);
              DoFree();
            }
        }
        
        #endregion
        
        public override String ToString()
        {
            return String.Format("%s (%x)", _Kind, Pointer);
        }
    }
}
