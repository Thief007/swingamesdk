using System;
using System.Collections.Generic;
using System.Text;
using Rectangle = System.Drawing.Rectangle;
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
        /// Create a Vector with a give X and Y value.
        /// </summary>
        /// <param name="x">the x value of the vector</param>
        /// <param name="y">the y value of the vector</param>
        public Vector(float x, float y)
        {
            _X = x;
            _Y = y;
        }

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

        /// <summary>
        /// Adds one vector to another.
        /// </summary>
        /// <param name="l">l + r</param>
        /// <param name="r">l + r</param>
        /// <returns>Returns a new vectro from l + r</returns>
        public static Vector operator +(Vector l, Vector r)
        {
            Vector result = new Vector();
            result.X = l.X + r.X;
            result.Y = l.Y + r.Y;
            return result;
        }

        /// <summary>
        /// Subtracts the Vector v1 and the Vector v2.
        /// </summary>
        /// <param name="v1">The vectors to work with</param>
        /// <param name="v2">The vectors to work with</param>
        /// <returns>v1 - v2</returns>
        public static Vector operator -(Vector v1, Vector v2)
        {
            Vector result = new Vector();
            result.X = v1.X - v2.X;
            result.Y = v1.Y - v2.Y;
            return result;
        }

        /// <summary>
        /// Multiplies a Vector by a number
        /// </summary>
        /// <param name="v1">The vector to multiply</param>
        /// <param name="s1">The number to multiply the vector by</param>
        /// <returns>The multiplyed vector</returns>
        public static Vector operator *(Vector v1, Single s1)
        {
            v1.X *= s1;
            v1.Y *= s1;

            return v1;
        }

        /// <summary>
        /// The Angle between two vectors
        /// </summary>
        /// <param name="v2">The Second Vector</param>
        /// <returns>The angle</returns>
        public Single DotProduct(Vector v2)
        {
            return (X * v2.X) + (Y * v2.Y);
        }

        /// <summary>
        /// VectorNormal returns the normal of the specified vector.
        /// </summary>
        /// <returns>Normal of the Vector specified</returns>
        public Vector Normal
        {
            get
            {
                float sqrY, sqrX;
                sqrX = X * X;
                sqrY = Y * Y;

                return new Vector(-Y / (float)Math.Sqrt(sqrY + sqrX), X / (float)Math.Sqrt(sqrY + sqrX));
            }
        }

        /// <summary>
        /// Inverts the vector. Changes the direction of the vector's x and y.
        /// </summary>
        /// <returns>A new inverted vector</returns>
        public Vector Inverse
        {
            get
            {
                Vector result = new Vector();
                result.X = -X;
                result.Y = -Y;
                return result;
            }
        }

        /// <summary>
        /// Limit the magnitude of a vector.
        /// </summary>
        /// <param name="maxMagnitude">The maximum magnitude of the vector.</param>
        /// <returns>A new vector with the same direction as theVector,
        ///	with a maximum magnitude of maxMagnitude</returns>
        public Vector LimitMagnitude(Single maxMagnitude)
        {
            return SGSDK.LimitMagnitude(this, maxMagnitude);
        }

        /// <summary>
        /// Gets the unit vector of the passed in vector. The unit vector has a
        ///	magnitude of 1, resulting in a vector that indicates the direction of
        ///	the original vector.
        /// </summary>
        /// <returns>The unit vector from the passed in vector</returns>
        public Vector UnitVector
        {
            get
            {
                return SGSDK.GetUnitVector(this);
            }
        }

        /// <summary>
        /// Indicates if the magnitude of the vector is 0.
        /// </summary>
        /// <returns>True if the vector has values 0, 0</returns>
        public bool IsZeroVector
        {
            get
            {
                return X == 0 && Y == 0;
            }
        }

        /// <summary>
        /// Returns the magnitude of a vector. The magnitude represents the length of
        ///	the vector.
        /// </summary>
        /// <returns>The magnitude of the vector</returns>
        public Single Magnitude
        {
            get
            {
                return (float)Math.Sqrt((X * X) + (Y * Y));
            }
        }

        /// <summary>
        /// VectorIsWithinRect checks if the specified vector ends at the rectangle specified. 
        /// The routine assumes that the vector starts from 0, 0.
        /// </summary>
        /// <param name="x">X Coordinate of the Rectangle</param>
        /// <param name="y">Y Coordinate of the Rectangle</param>
        /// <param name="width">Width of the Rectangle</param>
        /// <param name="height">Height of the Rectangle</param>
        /// <returns>True if the Vector ends at the rectangle</returns>
        public bool IsWithinRect(float x, float y, int width, int height)
        {
            if (this.X < x) return false;
            else if (this.X > x + width) return false;
            else if (this.Y < y) return false;
            else if (this.Y > y + height) return false;
            else return true;
        }

        /// <summary>
        /// VectorIsWithinRect checks if the specified vector ends at the rectangle specified. 
        /// The routine assumes that the vector starts from 0, 0.
        /// </summary>
        /// <param name="rectangle">Rectangle</param>
        /// <returns>True if the Vector ends at the rectangle</returns>
        public bool IsWithinRect(Rectangle rectangle)
        {
            return IsWithinRect(rectangle.X, rectangle.Y, rectangle.Width, rectangle.Height);
        }
    }
}
