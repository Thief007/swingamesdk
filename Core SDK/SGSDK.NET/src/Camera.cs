using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Drawing;
using System.IO;

namespace SwinGame
{
    public class Camera
    {
        // Screen ViewPort Functions

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern int XOffset();

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern int YOffset();

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ScreenX(float x);

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern int ScreenY(float y);

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern float GameX(int x);

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern float GameY(int y);

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern float ToGameCoordinates(Vector screenVector);

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern void DLL_MoveVisualAreaWithVector(Vector v);
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern void DLL_MoveVisualArea(float dx, float dy);

        public static void MoveVisualArea(Vector v)
        {
            DLL_MoveVisualAreaWithVector(v);
        }

        public static void MoveVisualArea(float dx, float dy)
        {
            DLL_MoveVisualArea(dx, dy);
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl)]
        public static extern void SetScreenOffset(float dx, float dy);

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "FollowSprite")]
        private static extern void DLL_FollowSprite(IntPtr sprite, int xOffset, int yOffset);

        public static void FollowSprite(Sprite sprite, int xOffset, int yOffset)
        {
            DLL_FollowSprite(sprite.Pointer, xOffset, yOffset);
        }
    }
}
