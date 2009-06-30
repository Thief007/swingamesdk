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
    
    /// <summary>
    /// Wraps a pointer to a SwinGame resource
    /// </summary>
    public abstract class PointerWrapper : IDisposable
    {
        private PtrKind _Kind;
        internal IntPtr Pointer;
        
        protected abstract internal void DoFree();
        
        internal void Create(IntPtr ptr, PtrKind kind)
        {
            Pointer = ptr;
            _Kind = kind;
        }
        
        internal void Create(IntPtr ptr)
        {
            Create(ptr, PtrKind.Copy);
            GC.SuppressFinalize(this);
        }
        
        ~PointerWrapper()
        {
            Core.RegisterDelete(Pointer, _Kind);
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
