using System;
using System.Collections.Generic;
using System.Text;

namespace SwinGame
{
    /// <summary>
    /// This record is used to represent transformations that can be
    /// used to apply these changes to vectors.
    /// </summary>
    //[StructLayout(LayoutKind.Sequential)]
    public class Matrix2D
    {
        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetMatrix2DElement")]
        //private static extern Single GetMaxtrix2DElement(IntPtr maxtrix2d, int r, int c);

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SetMatrix2DElement")]
        //private static extern void SetMaxtrix2DElement(IntPtr maxtrix2d, int r, int c, Single val);

        //private IntPtr Pointer;
        private SwinGamePointer Pointer;

        internal Matrix2D(IntPtr ptr)
        {
            if (ptr == IntPtr.Zero) throw new SwinGameException("Error Creating Matrix2D");
            Pointer = new SwinGamePointer(ptr, PtrKind.Matrix);
        }

        /// <summary>
        /// Gets an element from the Matrix2D
        /// </summary>
        /// <param name="r">Row</param>
        /// <param name="c">Column</param>
        /// <returns>Element</returns>
        public float this[int r, int c]
        {
            get
            {
                return SGSDK.GetMatrix2DElement(this.Pointer, r, c);
            }
            set
            {
                SGSDK.SetMatrix2DElement(this.Pointer, r, c, value);
            }
        }

        /// <summary>
        /// A Matrix2D can be treated as an integer pointer for the purpose
        /// of interacting with the SwinGame DLL.
        /// </summary>
        /// <param name="m">The matrix to get the pointer of</param>
        /// <returns>an IntPtr pointing to the memory where the Matrix resides</returns>
        public static implicit operator IntPtr(Matrix2D m)
        {
            return m.Pointer;
        }

        /// <summary>
        /// Multiplies two matrixs 
        /// </summary>
        /// <param name="m1">The first Matrix</param>
        /// <param name="m2">The second Matrix</param>
        /// <returns>The combined Matrixes</returns>
        public static Matrix2D operator *(Matrix2D m1, Matrix2D m2)
        {
            return new Matrix2D(SGSDK.MultiplyMatrix2D(m1, m2));
        }

        /// <summary>
        /// Multiplies 1 Vector and 1 Matrix2D
        /// </summary>
        /// <param name="m">The Matrix2D</param>
        /// <param name="v">The Vector</param>
        /// <returns>The resulting Matrix2D</returns>
        public static Vector operator * (Matrix2D m, Vector v)
        {
            return SGSDK.MultiplyMatrix2DAndVector(m, v);
        }

    }
}
