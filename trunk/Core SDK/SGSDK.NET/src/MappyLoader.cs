using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Drawing;
using System.IO;

namespace SwinGame
{
    public enum Event
    {
        Event1 = 0, Event2 = 1, Event3 = 2, Event4 = 3, Event5 = 4,
        Event6 = 5, Event7 = 6, Event8 = 7, Event9 = 8, Event10 = 9,
        Event11 = 10, Event12 = 11, Event13 = 12, Event14 = 13, Event15 = 14,
        Event16 = 15, Event17 = 16, Event18 = 17, Event19 = 18, Event20 = 19,
        Event21 = 20, Event22 = 21, Event23 = 22, Event24 = 23
    }

    public enum CollisionSide
    {
        Top,
        Bottom,
        Left,
        Right,
        TopLeft,
        TopRight,
        BottomLeft,
        BottomRight,
        LeftRight,
        TopBottom,
        None
    }

    public struct Map
    {
        internal IntPtr Pointer;
    }

    public class MappyLoader
    {

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="Loadmap")]
        private static extern IntPtr DLL_Loadmap(String fileName);

        public static Map Loadmap(String fileName)
        {
            Map temp;
            temp.Pointer = DLL_Loadmap(fileName);
            return temp;
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "Drawmap")]
        private static extern void DLL_Drawmap(IntPtr map);

        public static void Drawmap(Map map)
        {
            DLL_Drawmap(map.Pointer);
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "CollisionWithMapVector")]
        private static extern CollisionSide DLL_CollisionWithMapVector(IntPtr map, IntPtr spr, Vector vec);

        public static CollisionSide CollisionWithMap(Map map, Sprite spr, Vector vec)
        {
            return DLL_CollisionWithMapVector(map.Pointer, spr.Pointer, vec);
        }
        
        public static CollisionSide CollisionWithMap(Map map, Sprite spr)
        {
            return DLL_CollisionWithMapVector(map.Pointer, spr.Pointer, spr.Movement);
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "EventCount")]
        private static extern int DLL_EventCount(IntPtr map, Event evnt);

        public static int EventCount(Map map, Event evnt)
        {
            return DLL_EventCount(map.Pointer, evnt);
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "EventPositionX")]
        private static extern int DLL_EventPositionX(IntPtr map, Event evnt, int eventnumber);

        public static int EventPositionX(Map map, Event evnt, int eventnumber)
        {
            return DLL_EventPositionX(map.Pointer, evnt, eventnumber);
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "EventPositionY")]
        private static extern int DLL_EventPositionY(IntPtr map, Event evnt, int eventnumber);

        public static int EventPositionY(Map map, Event evnt, int eventnumber)
        {
            return DLL_EventPositionY(map.Pointer, evnt, eventnumber);
        }
    }
}
