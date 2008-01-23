///-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
// 					Camera
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// The Camera unit is used to change the view port (ie 
// the camera location.)
//
// Change History:
//
// Version 1.1:
// - 2008-01-23: Andrew: Fixed exceptions
//               Added changes for 1.1 compatibility
//               Refactored some methods, to limit routines exposed by DLL
//               Added extra comments, and fixed code layout and line endings.
//               Changed XOffset and YOffset to properties
// Version 1.0:
// - Various

using System;
using System.Runtime.InteropServices;

namespace SwinGame
{
    /// <summary>
    /// The Camera class is used to position the screen within your
    /// game world. This is useful for games that allow the player
    /// to move around, such as scrolling shooters or RPG.
    /// </summary>
    public class Camera
    {
        // Screen ViewPort Functions

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "XOffset")]
        private static extern int DLL_XOffset();
        /// <summary>
        /// Gets the XOffset of the Screen, the distance that the camera has 
        /// moved on the X axis.
        /// </summary>
        /// <returns>Distance the camera has moved on the X axis</returns>
        public static int XOffset
        {
            get
            {
                int temp;
                try
                {
                    temp = DLL_XOffset();
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
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "YOffset")]
        private static extern int DLL_YOffset();
        /// <summary>
        /// Gets the YOffset of the Screen, the distance that the camera has 
        /// moved on the y axis
        /// </summary>
        /// <returns>Distance the camera has moved on the Y axis</returns>
        public static int YOffset
        {
            get
            {
                int temp;
                try
                {
                    temp = DLL_YOffset();
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
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ScreenX")]
        private static extern int DLL_ScreenX(float x);
        /// <summary>
        /// Converts a game x coordinate, to a screen coordinate. This is useful for
        /// determining where a game element is appearing on the screen.
        /// </summary>
        /// <param name="x">Game x coordinate</param>
        /// <returns>Screen x coordinate that matches the requested game x</returns>
        public static int ScreenX(float x)
        {
            int temp;
            try
            {
                temp = DLL_ScreenX(x);
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

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ScreenY")]
        private static extern int DLL_ScreenY(float y);
        /// <summary>
        /// Converts a game y coordinate, to a screen coordinate. This is useful for
        /// determining where a game element is appearing on the screen.
        /// </summary>
        /// <param name="x">Game y coordinate</param>
        /// <returns>Screen y coordinate that matches the requested game y</returns>
        public static int ScreenY(float y)
        {
            int temp;
            try
            {
                temp = DLL_ScreenY(y);
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

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GameX")]
        private static extern float DLL_GameX(int x);
        /// <summary>
        /// This converts a screen x coordinate to a game x coordinate. This is
        /// useful for determining what is at a given point on the screen, for example
        /// finding what game coordinate the user has clicked on.
        /// </summary>
        /// <param name="x">Screen X Coordinate</param>
        /// <returns>Game x coordinate that matches the screen coordinate</returns>
        public static float GameX(int x)
        {
            float temp;
            try
            {
                temp = DLL_GameX(x);
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

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GameY")]
        private static extern float DLL_GameY(int y);
        /// <summary>
        /// This converts a screen y coordinate to a game y coordinate. This is
        /// useful for determining what is at a given point on the screen, for example
        /// finding what game coordinate the user has clicked on.
        /// </summary>
        /// <param name="x">Screen Y coordinate</param>
        /// <returns>Game y coordinate that matches the screen coordinate</returns>
        public static float GameY(int y)
        {
            float temp;
            try
            {
                temp = DLL_GameY(y);
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


        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ToGameCoordinates")]
        private static extern Point2D DLL_ToGameCoordinates(Point2D screenPoint);
        /// <summary>
        /// Converts the indicated point from screen coordinates to game Coordinates.
        /// This is useful for determining what is at a given point on the screen, 
        /// for example finding what game coordinate the user has clicked on.
        /// </summary>
        /// <param name="screenPoint">Point on the screen you want to convert</param>
        /// <returns>The point in the game that is currently drawn on the screen at screenPoint</returns>
        public static Point2D ToGameCoordinates(Point2D screenPoint)
        {
            Point2D temp;
            try
            {
                temp = DLL_ToGameCoordinates(screenPoint);
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

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "MoveVisualArea")]
        private static extern void DLL_MoveVisualArea(float dx, float dy);
        /// <summary>
        /// Moves the Camera's visual area by the amount in the vector.
        /// </summary>
        /// <param name="v">The amount to move the camera</param>
        public static void MoveVisualArea(Vector v)
        {
            MoveVisualArea(v.X, v.Y);
        }
        /// <summary>
        /// Moves the Camera's visual area by the amounts specified.
        /// </summary>
        /// <param name="dx">The amount to move the camera on the X axis</param>
        /// <param name="dy">The amount to move the camera on the Y axis</param>
        public static void MoveVisualArea(float dx, float dy)
        {
            try
            {
                DLL_MoveVisualArea(dx, dy);
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

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "SetScreenOffset")]
        private static extern void DLL_SetScreenOffset(float dx, float dy);
        /// <summary>
        /// Moves the top left of the screen to a given x, y location in game
        /// coordinates. Use this to move the camera to a given point.
        /// </summary>
        /// <param name="x">new x position of camera (top left)</param>
        /// <param name="y">new y position of camera (top left)</param>
        public static void SetScreenOffset(float x, float y)
        {
            try
            {
                DLL_SetScreenOffset(x, y);
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

        /// <summary>
        /// Moves the top left of the screen to a given x, y location in game
        /// coordinates. Use this to move the camera to a given point.
        /// </summary>
        /// <param name="pt">The point to use for the new top left</param>
        public static void SetScreenOffset(Point2D pt)
        {
            SetScreenOffset(pt.X, pt.Y);
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "FollowSprite")]
        private static extern void DLL_FollowSprite(IntPtr sprite, int xOffset, int yOffset);
        /// <summary>
        /// Move the Camera to center on the Sprite. This must be called each time
        /// you move the sprite if you want the camera to follow that sprite. The offsets
        /// allow you to move the sprite from direct center, for example if yOffset is set
        /// to +10 the sprite will appear 10 pixels below center.
        /// </summary>
        /// <param name="sprite">The sprite you want to move the camera to view.</param>
        /// <param name="xOffset">The x offset from center</param>
        /// <param name="yOffset">The y offset from center</param>
        public static void FollowSprite(Sprite sprite, int xOffset, int yOffset)
        {
            try
            {
                DLL_FollowSprite(sprite.Pointer, xOffset, yOffset);
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

        /// <summary>
        /// Move the Camera to center on the Sprite. This must be called each time
        /// you move the sprite if you want the camera to follow that sprite. The offsets
        /// allow you to move the sprite from direct center, for example if yOffset is set
        /// to +10 the sprite will appear 10 pixels below center.
        /// </summary>
        /// <param name="sprite">The sprite you want to move the camera to view.</param>
        /// <param name="offset">A vector containing the offset from center.</param>
        public static void FollowSprite(Sprite sprite, Vector offset)
        {
            FollowSprite(sprite, (int)offset.X, (int)offset.Y);
        }
    }
}
