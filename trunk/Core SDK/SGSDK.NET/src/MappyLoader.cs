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
    /// Map Structure
    /// </summary>
    public struct Map
    {
        internal IntPtr Pointer;
    }

    /// <summary>
    /// MappyLoader Class
    /// </summary>
    public class MappyLoader
    {
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ExceptionOccured")]
        private static extern bool ExceptionOccured();
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetExceptionMessage")]
        private static extern String GetExceptionMessage();

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="LoadMap")]
        private static extern IntPtr DLL_Loadmap(String fileName);
        /// <summary>
        /// Loads a Map
        /// </summary>
        /// <param name="fileName">Name of the map</param>
        /// <returns>Map</returns>
        public static Map Loadmap(String fileName)
        {
            Map temp;
            temp.Pointer = DLL_Loadmap(fileName);
            if (ExceptionOccured())
            {
                throw new SwinGameException(GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "DrawMap")]
        private static extern void DLL_Drawmap(IntPtr map);
        /// <summary>
        /// Draws the Map
        /// </summary>
        /// <param name="map">Map</param>
        public static void Drawmap(Map map)
        {
            DLL_Drawmap(map.Pointer);
            if (ExceptionOccured())
            {
                throw new SwinGameException(GetExceptionMessage());
            }
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CollisionWithMapVector")]
        private static extern CollisionSide DLL_CollisionWithMapVector(IntPtr map, IntPtr spr, Vector vec);
        /// <summary>
        /// Checks if a sprite collides with the map, and if so returns the side of the sprite that collided with the map.
        /// This overload allows you to pass in a vector, which describes the movement of the sprite, if the sprites movement vector has
        /// not been used.
        /// </summary>
        /// <param name="map">Map</param>
        /// <param name="spr">Sprite</param>
        /// <param name="vec">Vector</param>
        /// <returns>Collision Side</returns>
        public static CollisionSide CollisionWithMap(Map map, Sprite spr, Vector vec)
        {
            CollisionSide temp = DLL_CollisionWithMapVector(map.Pointer, spr.Pointer, vec);
            if (ExceptionOccured())
            {
                throw new SwinGameException(GetExceptionMessage());
            }
            return temp;
        }
        
        /// <summary>
        /// Checks if a sprite collides with the map, and if so returns the side of the sprite that collided with the map
        /// </summary>
        /// <param name="map">Map</param>
        /// <param name="spr">Sprite</param>
        /// <returns>Collision Side</returns>
        public static CollisionSide CollisionWithMap(Map map, Sprite spr)
        {
            CollisionSide temp = DLL_CollisionWithMapVector(map.Pointer, spr.Pointer, spr.Movement);
            if (ExceptionOccured())
            {
                throw new SwinGameException(GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "EventCount")]
        private static extern int DLL_EventCount(IntPtr map, Event evnt);
        /// <summary>
        /// Gets the Number Events of an Event Type on a Map
        /// </summary>
        /// <param name="map">Map</param>
        /// <param name="evnt">Event Type</param>
        /// <returns>Number of Events</returns>
        public static int EventCount(Map map, Event evnt)
        {
            int temp = DLL_EventCount(map.Pointer, evnt);
            if (ExceptionOccured())
            {
                throw new SwinGameException(GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "EventPositionX")]
        private static extern int DLL_EventPositionX(IntPtr map, Event evnt, int eventnumber);
        /// <summary>
        /// Gets the Game X Coordinate of the Event
        /// </summary>
        /// <param name="map">Map</param>
        /// <param name="evnt">Event Type</param>
        /// <param name="eventnumber">Event Index</param>
        /// <returns>Game X Coordinate</returns>
        public static int EventPositionX(Map map, Event evnt, int eventnumber)
        {
            int temp = DLL_EventPositionX(map.Pointer, evnt, eventnumber);
            if (ExceptionOccured())
            {
                throw new SwinGameException(GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "EventPositionY")]
        private static extern int DLL_EventPositionY(IntPtr map, Event evnt, int eventnumber);
        /// <summary>
        /// Gets the Game Y Coordinate of the Event
        /// </summary>
        /// <param name="map">Map</param>
        /// <param name="evnt">Event Type</param>
        /// <param name="eventnumber">Event Index</param>
        /// <returns>Game Y Coordinate</returns>
        public static int EventPositionY(Map map, Event evnt, int eventnumber)
        {
            int temp = DLL_EventPositionY(map.Pointer, evnt, eventnumber);
            if (ExceptionOccured())
            {
                throw new SwinGameException(GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "FreeMap")]
        private static extern void DLL_FreeMap(IntPtr map);

        public static void FreeMap(Map map)
        {
            DLL_FreeMap(map.Pointer);
            if (ExceptionOccured())
            {
                throw new SwinGameException(GetExceptionMessage());
            }
        }
    }
}
