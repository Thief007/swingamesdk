///-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
// 					MappyLoader
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// The MappyLoader unit contains the Map related SwinGame
// routines and the Map data type.
//
// Change History:
//
// Version 1.1:
// - 2008-01-23: Stephen: Fixed Exceptions
//                        Added changes for 1.1 compatibility
//                        Added extra comments, and fixed code layout and line endings.
//                        Added SwinGamePointer for safer management of pointers.
//                        Added MoveSpriteOutOfTile
//                        Added WillCollideOnSide
//                        Added SpriteHasCollidedWithMapTile (2 overloads)
// Version 1.0:
// - Various

using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Drawing;
using System.IO;

namespace SwinGame
{
    /// <summary>
    /// MappyLoader Events
    /// </summary>
    public enum Event
    {
        /// <summary>
        /// Event 1
        /// </summary>
        Event1 = 0, 
        /// <summary>
        /// Event 2
        /// </summary>
        Event2 = 1,
        /// <summary>
        /// Event 3
        /// </summary>
        Event3 = 2,
        /// <summary>
        /// Event 4
        /// </summary>
        Event4 = 3,
        /// <summary>
        /// Event 5
        /// </summary>
        Event5 = 4,
        /// <summary>
        /// Event 6
        /// </summary>
        Event6 = 5,
        /// <summary>
        /// Event 7
        /// </summary>
        Event7 = 6,
        /// <summary>
        /// Event 8
        /// </summary>
        Event8 = 7,
        /// <summary>
        /// Event 9
        /// </summary>
        Event9 = 8,
        /// <summary>
        /// Event 10
        /// </summary>
        Event10 = 9,
        /// <summary>
        /// Event 11
        /// </summary>
        Event11 = 10,
        /// <summary>
        /// Event 12
        /// </summary>
        Event12 = 11,
        /// <summary>
        /// Event 13
        /// </summary>
        Event13 = 12,
        /// <summary>
        /// Event 14
        /// </summary>
        Event14 = 13,
        /// <summary>
        /// Event 15
        /// </summary>
        Event15 = 14,
        /// <summary>
        /// Evemnt 16
        /// </summary>
        Event16 = 15,
        /// <summary>
        /// Event 17
        /// </summary>
        Event17 = 16,
        /// <summary>
        /// Event 18
        /// </summary>
        Event18 = 17,
        /// <summary>
        /// Event 19
        /// </summary>
        Event19 = 18,
        /// <summary>
        /// Event 20
        /// </summary>
        Event20 = 19,
        /// <summary>
        /// Event 21
        /// </summary>
        Event21 = 20,
        /// <summary>
        /// Event 22
        /// </summary>
        Event22 = 21,
        /// <summary>
        /// Event 23
        /// </summary>
        Event23 = 22,
        /// <summary>
        /// Event 24
        /// </summary>
        Event24 = 23
    }

    /// <summary>
    /// CollisionSides
    /// </summary>
    public enum CollisionSide
    {
        /// <summary>
        /// Top
        /// </summary>
        Top,
        /// <summary>
        /// Bottom
        /// </summary>
        Bottom,
        /// <summary>
        /// Left
        /// </summary>
        Left,
        /// <summary>
        /// Right
        /// </summary>
        Right,
        /// <summary>
        /// TopLeft
        /// </summary>
        TopLeft,
        /// <summary>
        /// TopRight
        /// </summary>
        TopRight,
        /// <summary>
        /// BottomLeft
        /// </summary>
        BottomLeft,
        /// <summary>
        /// BottomRight
        /// </summary>
        BottomRight,
        /// <summary>
        /// LeftRight
        /// </summary>
        LeftRight,
        /// <summary>
        /// TopBottom
        /// </summary>
        TopBottom,
        /// <summary>
        /// None
        /// </summary>
        None
    }

    /// <summary>
    /// A Map is a tile based collection of sprites, that form a complete map. Map's can be animated,
    /// and maps that are animated, animate automatically. Collisions between sprite and maps can also be
    /// done, so your sprites do not go into collidable areas of the map.
    /// </summary>
    public struct Map
    {
        internal SwinGamePointer Pointer;
    }

    /// <summary>
    /// MappyLoader Class
    /// </summary>
    public class MappyLoader
    {
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="LoadMap")]
        private static extern IntPtr DLL_LoadMap([MarshalAs(UnmanagedType.LPStr)]string mapFile, [MarshalAs(UnmanagedType.LPStr)]string imgFile);
   	    /// <summary>
        /// Loads a Map, using mapFile to indicate which map file to load.
        /// </summary>
        /// <param name="mapName">Name of the map</param>
        /// <returns>Map</returns>
        public static Map LoadMap(String mapName)
        {
            Map temp;
            try
            {
                string mapFile = Core.GetPathToResource(mapName + ".sga", ResourceKind.MapResource);
			    string imgFile = Core.GetPathToResource(mapName + ".png", ResourceKind.MapResource);
					
                temp.Pointer = new SwinGamePointer(DLL_LoadMap(mapFile, imgFile), DLL_FreeMap);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawMap")]
        private static extern void DLL_DrawMap(IntPtr map);
        /// <summary>
        /// Draws the specified Map
        /// </summary>
        /// <param name="map">Map to be drawn to the screen</param>
        public static void DrawMap(Map map)
        {
            try
            {
                DLL_DrawMap(map.Pointer);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }  
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CollisionWithMapVector")]
        private static extern CollisionSide DLL_CollisionWithMapVector(IntPtr map, IntPtr spr, Vector vec);
        /// <summary>
        /// This function checks if the specified sprite has collided with the specified map.
        /// This function will also move the sprite outside of the map automatically.
        /// Useful for checking collisions, getting which side the collision has occured on, and for
        /// moving the sprite out of the collidable areas of the map.
        /// This overload allows you to pass in a vector, which describes the movement of the sprite, if the sprites movement vector has
        /// not been used.
        /// </summary>
        /// <param name="map">Map to be checked for collisions</param>
        /// <param name="spr">Sprite to be checked for collisions</param>
        /// <param name="vec">Vector to be used</param>
        /// <returns>Collision Side</returns>
        public static CollisionSide CollisionWithMap(Map map, Sprite spr, Vector vec)
        {
            CollisionSide temp;

            try
            {
                temp = DLL_CollisionWithMapVector(map.Pointer, spr.Pointer, vec);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }
        
        /// <summary>
        /// This function checks if the specified sprite has collided with the specified map.
        /// This function will also move the sprite outside of the map automatically.
        /// Useful for checking collisions, getting which side the collision has occured on, and for
        /// moving the sprite out of the collidable areas of the map.
        /// </summary>
        /// <param name="map">Map to be checked for collisions</param>
        /// <param name="spr">Sprite to be checked for collisions</param>
        /// <returns>Collision Side</returns>
        public static CollisionSide CollisionWithMap(Map map, Sprite spr)
        {
            CollisionSide temp;
            try
            {
                temp = DLL_CollisionWithMapVector(map.Pointer, spr.Pointer, spr.Movement);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "EventCount")]
        private static extern int DLL_EventCount(IntPtr map, Event evnt);
        /// <summary>
        /// This function returns the number of the specified Event type found in the Map specified.
        /// </summary>
        /// <param name="map">Map to be checked</param>
        /// <param name="evnt">Event Type to be checked</param>
        /// <returns>Number of events of Event Type specified</returns>
        public static int EventCount(Map map, Event evnt)
        {
            int temp;
            try
            {
                temp = DLL_EventCount(map.Pointer, evnt);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "EventPositionX")]
        private static extern int DLL_EventPositionX(IntPtr map, Event evnt, int eventnumber);
        /// <summary>
        /// Gets the X Coordinate of the Event specified. evnt is the Event Type that is to be searched
        /// for, and eventnumber is the index of the event, since there can be more then 1 of each Event
        /// Type.
        /// </summary>
        /// <param name="map">Map containing the Event</param>
        /// <param name="evnt">Event Type</param>
        /// <param name="eventnumber">Event Index</param>
        /// <returns>Game X Coordinate</returns>
        public static int EventPositionX(Map map, Event evnt, int eventnumber)
        {
            int temp;

            try
            {
                temp = DLL_EventPositionX(map.Pointer, evnt, eventnumber);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "EventPositionY")]
        private static extern int DLL_EventPositionY(IntPtr map, Event evnt, int eventnumber);
        /// <summary>
        /// Gets the Y Coordinate of the Event specified. evnt is the Event Type that is to be searched
        /// for, and eventnumber is the index of the event, since there can be more then 1 of each Event
        /// Type.
        /// </summary>
        /// <param name="map">Map containing the event</param>
        /// <param name="evnt">Event Type</param>
        /// <param name="eventnumber">Event Index</param>
        /// <returns>Game Y Coordinate</returns>
        public static int EventPositionY(Map map, Event evnt, int eventnumber)
        {
            int temp;

            try
            {
                temp = DLL_EventPositionY(map.Pointer, evnt, eventnumber);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "FreeMap")]
        private static extern void DLL_FreeMap(ref IntPtr map);
        /// <summary>
        /// Free a loaded map. This ensures that the resources used by the Map are returned to the system.
		/// This must be called once you have finished using the Map.
        /// </summary>
        /// <param name="map">The Map to Free</param>
        public static void FreeMap(Map map)
        {
            map.Pointer.Free();
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SpriteHasCollidedWithMapTile")]
        private static extern bool DLL_SpriteHasCollidedWithMapTile(IntPtr map, IntPtr spr, out int collidedX, out int collidedY);
        /// <summary>
        /// Checks whether the specified Sprite has collided with a map tile within the specified map.
        /// If so, this method returns true, else false. This command can be used to determine whether
        /// a Sprite has collided with the map, but also to find the x and y coordinate of the tile within
        /// the map it collided with.
        /// </summary>
        /// <param name="map">The Map to be checked against for collisions</param>
        /// <param name="spr">The Sprite to be checked against the map</param>
        /// <param name="collidedX">The X Coordinate of the Tile that Sprite has collided with</param>
        /// <param name="collidedY">The Y Coordinate of the Tile that Sprite has collided with</param>
        /// <returns></returns>
        public static bool SpriteHasCollidedWithMapTile(Map map, Sprite spr, out int collidedX, out int collidedY)
        {
            bool temp;

            try
            {
                temp = DLL_SpriteHasCollidedWithMapTile(map.Pointer, spr.Pointer, out collidedX, out collidedY);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }
        /// <summary>
        /// Checks whether the specified Sprite has collided with a map tile within the specified map.
        /// If so, this method returns true, else false. This command can be used to determine whether
        /// a Sprite has collided with the map.
        /// </summary>
        /// <param name="map">The Map to be checked against for collisions</param>
        /// <param name="spr">The Sprite to be checked against the map</param>
        /// <returns></returns>
        public static bool SpriteHasCollidedWithMapTile(Map map, Sprite spr)
        {
            bool temp;
            int collidedX;
            int collidedY;

            try
            {
                temp = DLL_SpriteHasCollidedWithMapTile(map.Pointer, spr.Pointer, out collidedX, out collidedY);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "WillCollideOnSide")]
        private static extern CollisionSide DLL_WillCollideOnSide(IntPtr map, IntPtr spr);
        /// <summary>
        /// This function with find which side the Collision between a Sprite and Map has occurred on.
        /// This can be used to make different choices depending on which side of the Map the Sprite has
        /// hit.
        /// </summary>
        /// <param name="map">The map to be checked against collisions</param>
        /// <param name="spr">The sprite to be checke against collisions</param>
        /// <returns></returns>
        public static CollisionSide WillCollideOnSide(Map map, Sprite spr)
        {
            CollisionSide temp;

            try
            {
                temp = DLL_WillCollideOnSide(map.Pointer, spr.Pointer);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "MoveSpriteOutOfTile")]
        private static extern void DLL_MoveSpriteOutOfTile(IntPtr map, IntPtr spr, int x, int y);
        /// <summary>
        /// This routine with move a specified sprite out the map's tile that is specified. This routine
        /// moves the sprite so that is just outside of the tile in question.
        /// </summary>
        /// <param name="map">Map containing the tile that is to be checked against</param>
        /// <param name="spr">Sprite to be moved out</param>
        /// <param name="x">X Coordinate of the tile</param>
        /// <param name="y">Y Coordinate of the tile</param>
        /// <returns></returns>
        public static void MoveSpriteOutOfTile(Map map, Sprite spr, int x, int y)
        {
            try
            {
                DLL_MoveSpriteOutOfTile(map.Pointer, spr.Pointer, x, y);
            }
            catch (Exception exc)
            {
                throw new SwinGameException(exc.Message);
            }
            if (Core.ExceptionOccured())
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
        }
    }
}
