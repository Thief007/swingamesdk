using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace SwinGame
{
    /// <summary>
    /// A Tile structure, contains the index in which the tile occurs in the map, and also the 4 points that create
    /// the tiles shape, Square/Rectangular or Isometric
    /// </summary>
    [StructLayout(LayoutKind.Sequential)]
    public struct Tile
    {
        /// <summary>
        /// X Index of the tile, this indicates what row the tile exists on
        /// </summary>
        public int xIndex;
        /// <summary>
        /// Y Index of the tile, this indicates what column the tile exists on
        /// </summary>
        public int yIndex;
        /// <summary>
        /// The top right hand coordinate of the Tile
        /// </summary>
        public Point2D topCorner;
        /// <summary>
        /// The first point of the tile
        /// </summary>
        public Point2D pointA;
        /// <summary>
        /// The second point of the tile
        /// </summary>
        public Point2D pointB;
        /// <summary>
        /// The third point of the tile
        /// </summary>
        public Point2D pointC;
        /// <summary>
        /// The fourth point of the tile
        /// </summary>
        public Point2D pointD;
    }
}
