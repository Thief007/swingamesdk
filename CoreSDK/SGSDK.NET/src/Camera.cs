using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Drawing;
using System.IO;

namespace SwinGame
{
    /// <summary>
    /// Camera Class
    /// </summary>
    public class Camera
    {
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ExceptionOccured")]
        private static extern bool ExceptionOccured();
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetExceptionMessage")]
        private static extern String GetExceptionMessage();

        // Screen ViewPort Functions

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "XOffset")]
        private static extern int DLL_XOffset();
        /// <summary>
        /// Gets the YOffset of the Screen
        /// </summary>
        /// <returns>YOffset</returns>
        public static int XOffset()
        {
            int temp = DLL_XOffset();
            if (ExceptionOccured())
            {
                throw new SwinGameException(GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="YOffset")]
        private static extern int DLL_YOffset();
        /// <summary>
        /// Gets the YOffset of the Screen
        /// </summary>
        /// <returns>YOffset</returns>
        public static int YOffset()
        {
            int temp = DLL_YOffset();
            if (ExceptionOccured())
            {
                throw new SwinGameException(GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="ScreenX")]
        private static extern int DLL_ScreenX(float x);
        /// <summary>
        /// Gets the Screen X Coordinate
        /// </summary>
        /// <param name="x">Game X Coordinate</param>
        /// <returns>Screen X Coordinate</returns>
        public static int ScreenX(float x)
        {
            int temp = DLL_ScreenX(x);
            if (ExceptionOccured())
            {
                throw new SwinGameException(GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ScreenY")]
        private static extern int DLL_ScreenY(float y);
        /// <summary>
        /// Gets the Screen Y Coordinate
        /// </summary>
        /// <param name="y">Game Y Coordinate</param>
        /// <returns>Screen Y Coordinate</returns>
        public static int ScreenY(float y)
        {
            int temp = DLL_ScreenY(y);
            if (ExceptionOccured())
            {
                throw new SwinGameException(GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GameX")]
        private static extern float DLL_GameX(int x);
        /// <summary>
        /// Gets the Game X Coordinate
        /// </summary>
        /// <param name="x">Screen X Coordinate</param>
        /// <returns>Game X Coordinate</returns>
        public static float GameX(int x)
        {
            float temp = DLL_GameY(x);
            if (ExceptionOccured())
            {
                throw new SwinGameException(GetExceptionMessage());
            }
            return temp;
        }



        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="GameY")]
        private static extern float DLL_GameY(int y);
        /// <summary>
        /// Gets the Game Y Coordinate
        /// </summary>
        /// <param name="y">Screen Y Coordinate</param>
        /// <returns>Game Y Coordinate</returns>
        public static float GameY(int y)
        {
            float temp = DLL_GameY(y);
            if (ExceptionOccured())
            {
                throw new SwinGameException(GetExceptionMessage());
            }
            return temp;
        }


        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="ToGameCoordinates")]
        private static extern Vector DLL_ToGameCoordinates(Vector screenVector);
        /// <summary>
        /// Converts the Vector to Game Coordinates
        /// </summary>
        /// <param name="screenVector">Screen Vector</param>
        /// <returns>Game Vector</returns>
        public static Vector ToGameCoordinates(Vector screenVector)
        {
            Vector temp = DLL_ToGameCoordinates(screenVector);
            if (ExceptionOccured())
            {
                throw new SwinGameException(GetExceptionMessage());
            }
            return temp;
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "MoveVisualAreaWithVector")]
        private static extern void DLL_MoveVisualAreaWithVector(Vector v);
        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="MoveVisualArea")]
        private static extern void DLL_MoveVisualArea(float dx, float dy);
        /// <summary>
        /// Moves the Camera's Visual Area
        /// </summary>
        /// <param name="v">Vector</param>
        public static void MoveVisualArea(Vector v)
        {
            DLL_MoveVisualAreaWithVector(v);
            if (ExceptionOccured())
            {
                throw new SwinGameException(GetExceptionMessage());
            }
        }
        /// <summary>
        /// Moves the Camera's Visual Area
        /// </summary>
        /// <param name="dx">X Movement</param>
        /// <param name="dy">Y Movement</param>
        public static void MoveVisualArea(float dx, float dy)
        {
            DLL_MoveVisualArea(dx, dy);
            if (ExceptionOccured())
            {
                throw new SwinGameException(GetExceptionMessage());
            }
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="SetScreenOffset")]
        private static extern void DLL_SetScreenOffset(float dx, float dy);
        /// <summary>
        /// Sets the Screen Offset
        /// </summary>
        /// <param name="dx">X Offset</param>
        /// <param name="dy">Y Offset</param>
        public static void SetScreenOffset(float dx, float dy)
        {
            DLL_SetScreenOffset(dx, dy);
            if (ExceptionOccured())
            {
                throw new SwinGameException(GetExceptionMessage());
            }
        }

        [DllImport("lib/SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "FollowSprite")]
        private static extern void DLL_FollowSprite(IntPtr sprite, int xOffset, int yOffset);
        /// <summary>
        /// Makes the Camera Follow the Sprite
        /// </summary>
        /// <param name="sprite">Sprite to Follow</param>
        /// <param name="xOffset">X Offset</param>
        /// <param name="yOffset">Y Offset</param>
        public static void FollowSprite(Sprite sprite, int xOffset, int yOffset)
        {
            DLL_FollowSprite(sprite.Pointer, xOffset, yOffset);
            if (ExceptionOccured())
            {
                throw new SwinGameException(GetExceptionMessage());
            }
        }
    }
}
