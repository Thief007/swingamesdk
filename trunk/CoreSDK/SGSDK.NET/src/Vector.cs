using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace SwinGame
{
    /// <summary>
    /// A vector is used to represent movement. Each vector has an
    /// X and Y value indicating the components of the movement.
    /// </summary>
    [StructLayout(LayoutKind.Sequential)]
    public struct Vector
    {
        private float _X;
        private float _Y;
        //private Single _W;

        /// <summary>
        /// The X Component of the Vector, i.e. the movement 
        /// in the X direction
        /// </summary>
        public float X
        {
            get { return _X; }
            set { _X = value; }
        }

        /// <summary>
        /// The Y Component of the Vector, i.e. the movement
        /// in the Y direction
        /// </summary>
        public float Y
        {
            get { return _Y; }
            set { _Y = value; }
        }
    }
}
