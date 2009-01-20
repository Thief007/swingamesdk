//-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
// 					Input
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// The Input unit contains all the routines that deal with
// the Mouse and Keyboard Inputs.
//
// Change History:
//
// Version 2:
// - 2009-01-20: Andrew: Moved to using new Font class.
// - 2008-12-18: Andrew: Moved to SGSDK
//
// Version 1.1:
// - 2008-04-02: Andrew: Added extra characters to key input
// - 2008-02-22: Andrew: Changed to GetMouseXY
// - 2008-01-30: Andrew: Fixed String Marshalling and TextRead
// - 2008-01-23: Fixed Exceptions
//               Added changes for 1.1 compatibility
//               Added extra comments, and fixed code layout and line endings.
//               Added GetMousePosition - Point2D version.
//               
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
    /// The Input class handles all Mouse and Keyboard Input related routines. Capable of detecting if a
    /// key or mouse button has been typed, is being held down, is currently up, this set of routines
    /// are extremely useful for user interaction within your games.
    /// </summary>
    public class Input
    {
        //[DllImport("SGSDK.dll", CallingConvention=CallingConvention.Cdecl, EntryPoint = "GetMouseXY")]
        //private static extern void DLL_GetMouseXY(out float x, out float y);

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="GetMousePositionAsVector")]
        //private static extern Vector DLL_GetMousePositionAsVector();

        /// <summary>
        /// Gets the current Mouse Position as a Vector, useful for finding out things such as if the mouse
        /// is hovering over a particular area of the screen.
        /// </summary>
        /// <returns>Vector representing the position of the mouse</returns>
        public static Vector GetMousePositionAsVector()
        {
            Vector temp = new Vector();
            float x, y;
            SGSDK.GetMouseXY(out x, out y);
            temp.X = x;
            temp.Y = y;
            return temp;
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetMousePosition")]
        //private static extern Point2D DLL_GetMousePosition();

        /// <summary>
        /// Gets the current Mouse Position as a Point2D, useful for finding out things such as if the mouse
        /// is hovering over a particular area of the screen.
        /// </summary>
        /// <returns>Point2D representing the Mouse Coordinates on the screen</returns>
        public static Point2D GetMousePosition()
        {
            Point2D temp = new Point2D();
            SGSDK.GetMouseXY(out temp.X, out temp.Y);
            return temp;
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "GetMouseMovement")]
        //private static extern Vector DLL_GetMouseMovement();

        /// <summary>
        /// Gets the Mouse Movement, useful for moving objects on the screen based on the movement of the
        /// mouse.
        /// </summary>
        /// <returns>Vector representing the movement of the mouse</returns>
        public static Vector GetMouseMovement()
        {
            return SGSDK.GetMouseMovement();
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "IsMouseDown")]
        //private static extern int DLL_IsMouseDown(MouseButton button);

        /// <summary>
        /// This function checks if the specified mouse button is being clicked, this is useful for
        /// ingame buttons, you could use this function to find if the user had hit the left mouse button.
        /// </summary>
        /// <param name="button">Mouse button to check</param>
        /// <returns>True if the mouse button is being clicked</returns>
        public static bool IsMouseDown(MouseButton button)
        {
            return SGSDK.IsMouseDown(button) == -1;
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "IsMouseUp")]
        //private static extern int DLL_IsMouseUp(MouseButton button);

        /// <summary>
        /// This function checks if the specified mouse button is not
        /// being clicked.
        /// </summary>
        /// <param name="button">Mouse button to check</param>
        /// <returns>True if the mouse button is not being clicked</returns>
        public static bool IsMouseUp(MouseButton button)
        {
            return SGSDK.IsMouseUp(button) == -1;
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "MouseWasClicked")]
        //private static extern int DLL_MouseWasClicked(MouseButton button);

        /// <summary>
        /// This functions checks if the mouse button specified has
        /// been clicked.
        /// </summary>
        /// <param name="button">Mouse button to check</param>
        /// <returns>True if the mouse button has been clicked</returns>
        public static bool MouseWasClicked(MouseButton button)
        {
            return SGSDK.MouseWasClicked(button) == -1;
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "StartReadingText")]
        //private static extern void DLL_StartReadingText(uint color, int maxLength, IntPtr theFont, int x, int y);

        /// <summary>
        /// StartReadingText start the API reading a string values from the user.
        ///	Entry is completed when the user presses enter, and aborted with escape.
        ///	If the user aborts entry the result is an empty string. Text entry is
        ///	updated as part of ProcessEvents, and is drawn to the screen as part of
        ///	the RefreshScreen call.
        /// </summary>
        /// <param name="toColour">The color of the text entered by the user</param>
        /// <param name="maxLength">The maximum length of the string the user can enter</param>
        /// <param name="theFont">The font used to draw the text entered</param>
        /// <param name="x">The X location at which to draw the text entered</param>
        /// <param name="y">The Y location at which to draw the text entered</param>
        public static void StartReadingText(Color toColour, int maxLength, Font theFont, int x, int y)
        {
            int color = toColour.ToArgb();
            SGSDK.StartReadingText((uint)color, maxLength, theFont, x, y);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "IsReadingText")]
        //private static extern int DLL_IsReadingText();

        /// <summary>
        /// IsReadingText indicates if the API is currently reading text from the
        ///	user. Calling StartReadingText will set this to true, and it becomes
        ///	false when the user presses enter or escape. At this point you can
        ///	read the string entered as either ASCII or Unicode.
        /// </summary>
        /// <returns>True while the API is reading text from the user</returns>
        public static bool IsReadingText()
        {
            return SGSDK.IsReadingText() == -1;
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "EndReadingText", CharSet = CharSet.Ansi)]
        //private static extern void DLL_EndReadingText([Out, MarshalAs(UnmanagedType.LPStr)] System.Text.StringBuilder result);

        /// <summary>
        /// Terminates the reading of strings from the user and returns the string they entered. 
        /// This string can still be read using TextReadAsASCII.
        /// </summary>
        /// <returns>The string entered by the user</returns>
        public static String EndReadingText()
        {
            System.Text.StringBuilder sb = new System.Text.StringBuilder(2048);
            SGSDK.EndReadingText(sb);
            return sb.ToString();
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "TextReadAsASCII", CharSet = CharSet.Ansi)]
        //private static extern void DLL_TextReadAsASCII([Out, MarshalAs(UnmanagedType.LPStr)] System.Text.StringBuilder result);

        /// <summary>
        /// TextReadAsASCII allows you to read the value of the string entered by the
        ///	user as ASCII. See TextReasAsUNICODE, StartReadingText and IsReadingText
        ///	for more details.
        /// </summary>
        /// <returns>The string entered by the user</returns>
        public static String TextReadAsASCII()
        {

            System.Text.StringBuilder sb = new System.Text.StringBuilder(2048);
            SGSDK.TextReadAsASCII(sb);
            return sb.ToString();
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "IsKeyPressed")]
        //private static extern int DLL_IsKeyPressed(int key);

        /// <summary>
        /// Returns true when the key requested is being held down. This is updated
        ///	as part of the ProcessEvents call. Use the key codes from the KeyCodes
        ///	unit.
        /// </summary>
        /// <param name="key">Key</param>
        /// <returns>True if the key is currently being held down</returns>
        public static bool IsKeyPressed(Keys key)
        {
            return SGSDK.IsKeyPressed((int)key) == -1;
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "WasKeyTyped")]
        //private static extern int DLL_WasKeyTyped(int key);

        /// <summary>
        /// Returns true when a key is typed. This occurs when the key is pressed on the 
        /// keyboard, and will not reoccur until it is released and pressed again. This
        /// needs to be checked each ProcessEvents loop.
        /// </summary>
        /// <param name="key">Key code to check</param>
        /// <returns>True if the key is pressed</returns>
        public static bool WasKeyTyped(Keys key)
        {
            return SGSDK.WasKeyTyped((int)key) == -1;
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "MoveMouse")]
        //private static extern void DLL_MoveMouse(UInt16 x, UInt16 y);

        /// <summary>
        /// This will move the mouse to the given x and y location
        /// </summary>
        /// <param name="x">The x pos to move the mouse to</param>
        /// <param name="y">The y pos to move the mouse to</param>
        public static void MoveMouse(UInt16 x, UInt16 y)
        {
            SGSDK.MoveMouse(x, y);
        }

        /// <summary>
        /// This will move the mouse to the given Point
        /// </summary>
        /// <param name="point">The Point containing the position to move the mouse to</param>
        public static void MoveMouse(Point2D point)
        {
            SGSDK.MoveMouse((ushort)point.X, (ushort)point.Y);
        }

        /// <summary>
        /// This routine will Hide the mouse
        /// </summary>
        public static void HideMouse()
        {
            ShowMouse(false);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ShowMouse")]
        //private static extern void DLL_ShowMouse(int show);

        /// <summary>
        /// This routine Shows or Hides the mouse
        /// </summary>
        /// <param name="show">if it is true it will show the mouse</param>
        public static void ShowMouse(bool show)
        {
            SGSDK.ShowMouse(show ? -1 : 0);
        }
        /// <summary>
        /// This routine will Show the Mouse
        /// </summary>
        public static void ShowMouse()
        {
            ShowMouse(true);
        }

        //[DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "IsMouseShown")]
        //private static extern int DLL_IsMouseShown();

        /// <summary>
        /// Checks to see if the mouse is shown
        /// </summary>
        /// <returns>returns true if the mouse is shown</returns>
        public static bool IsMouseShown()
        {
            return SGSDK.IsMouseShown() == -1;
        }

        /// <summary>
        /// Indicates if any key was pressed.
        /// </summary>
        /// <returns>true if a key was pressed.</returns>
        public bool AKeyWasPressed()
        {
            return SGSDK.AKeyWasPressed() == -1;
        }

        /// <summary>
        /// StartReadingText start the API reading a string values from the user.
        ///	Entry is completed when the user presses enter, and aborted with escape.
        ///	If the user aborts entry the result is an empty string. Text entry is
        ///	updated as part of ProcessEvents, and is drawn to the screen as part of
        ///	the RefreshScreen call.
        /// </summary>
        /// <param name="text">The initial text that will appear in the input area</param>
        /// <param name="toColour">The color of the text entered by the user</param>
        /// <param name="maxLength">The maximum length of the string the user can enter</param>
        /// <param name="theFont">The font used to draw the text entered</param>
        /// <param name="x">The X location at which to draw the text entered</param>
        /// <param name="y">The Y location at which to draw the text entered</param>
        public void StartReadingTextWithText(string text, Color toColour, int maxLength, Font theFont, int x, int y)
        {
            int color = toColour.ToArgb();
            SGSDK.StartReadingTextWithText(text, (uint)color, maxLength, theFont, x, y);
        }
    }
}
