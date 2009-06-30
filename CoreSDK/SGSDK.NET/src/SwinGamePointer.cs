//-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
// 					SwinGamePointer
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
    internal class SwinGamePointer
    {
        //private FreeDelegate _ToFree;
        private PtrKind _Kind;
        //private bool _Freed;
        internal IntPtr Pointer;

        internal SwinGamePointer(IntPtr ptr, PtrKind kind)
        {
            Pointer = ptr;
            _Kind = kind;
        }

        internal SwinGamePointer(IntPtr ptr) : this(ptr, PtrKind.Copy)
        {
            GC.SuppressFinalize(this);
        }

        ~SwinGamePointer()
        {
            Core.RegisterDelete(Pointer, _Kind);
        }

        public static implicit operator IntPtr(SwinGamePointer p)
        {
            return p.Pointer;
        }

        internal void Free()
        {
            //Core.DoFree(Pointer, _Kind);
            Core.RegisterDelete(Pointer, _Kind);
            GC.SuppressFinalize(this);
        }
        
        #region IDisposable Members

        /// <summary>
        /// Clean up the native resources used by this resource.
        /// </summary>
        public void Dispose()
        {
            if (_Kind != PtrKind.Copy) Free();
        }

        #endregion

    }
}
