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
            //_Freed = (ptr == IntPtr.Zero);
        }

        internal SwinGamePointer(IntPtr ptr)
            : this(ptr, PtrKind.Copy)
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
            Core.DoFree(Pointer, _Kind);
            GC.SuppressFinalize(this);
        }
    }
}
