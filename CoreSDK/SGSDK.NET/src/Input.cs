using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Drawing;
using System.IO;

namespace SwinGame
{

    /// <summary>
    /// Mouse Buttons
    /// </summary>
    public enum MouseButton
    {
        /// <summary>
        /// Left Mouse Button
        /// </summary>
        LeftButton = 1,
        /// <summary>
        /// Middle Mouse Button
        /// </summary>
        MiddleButton = 2,
        /// <summary>
        /// Right Mouse Button
        /// </summary>
        RightButton = 3,
        /// <summary>
        /// Mouse Wheel Up
        /// </summary>
        MouseWheelUp = 4,
        /// <summary>
        /// Mouse Wheel Down
        /// </summary>
        MouseWheelDown = 5
    }
    
    /// <summary>
    /// Keys that can be used on the Keyboard
    /// </summary>
    public enum Keys
    {
        /// <summary>
        /// Backspace
        /// </summary>
        VK_BACK = 8,
        /// <summary>
        /// Tab
        /// </summary>
	    VK_TAB = 9,
        /// <summary>
        /// Clear
        /// </summary>
	    VK_CLEAR = 12,
        /// <summary>
        /// Return
        /// </summary>
	    VK_RETURN = 13,
        /// <summary>
        /// Shift
        /// </summary>
	    VK_SHIFT = 304,
        /// <summary>
        /// Control
        /// </summary>
	    VK_CONTROL = 306,
        /// <summary>
        /// Menu
        /// </summary>
	    VK_MENU = 319,
        /// <summary>
        /// Alt
        /// </summary>
	    VK_ALT = 308,
        /// <summary>
        /// Pause
        /// </summary>
	    VK_PAUSE = 19,
        /// <summary>
        /// Capital
        /// </summary>
	    VK_CAPITAL = 301,
        /// <summary>
        /// Escape
        /// </summary>
	    VK_ESCAPE = 27,
        /// <summary>
        /// Space
        /// </summary>
	    VK_SPACE = 32,
        /// <summary>
        /// Page Up
        /// </summary>
	    VK_PAGE_UP = 280,
        /// <summary>
        /// Page Down
        /// </summary>
	    VK_PAGE_DOWN = 281,
        /// <summary>
        /// End
        /// </summary>
	    VK_END = 279,
        /// <summary>
        /// Home
        /// </summary>
	    VK_HOME = 278,
        /// <summary>
        /// Left
        /// </summary>
	    VK_LEFT = 276,
        /// <summary>
        /// Up
        /// </summary>
	    VK_UP = 273,
        /// <summary>
        /// Right
        /// </summary>
	    VK_RIGHT = 275,
        /// <summary>
        /// Down
        /// </summary>
	    VK_DOWN = 274,
        /// <summary>
        /// Print
        /// </summary>
	    VK_PRINT = 316,
        /// <summary>
        /// Insert
        /// </summary>
	    VK_INSERT = 277,
        /// <summary>
        /// Delete
        /// </summary>
	    VK_DELETE = 127,
        /// <summary>
        /// Help
        /// </summary>
	    VK_HELP = 315,
        /// <summary>
        /// 0
        /// </summary>
	    VK_0 = 48,
        /// <summary>
        /// 1
        /// </summary>
	    VK_1 = 49,
        /// <summary>
        /// 2
        /// </summary>
	    VK_2 = 50,
        /// <summary>
        /// 3
        /// </summary>
	    VK_3 = 51,
        /// <summary>
        /// 4
        /// </summary>
	    VK_4 = 52,
        /// <summary>
        /// 5
        /// </summary>
	    VK_5 = 53,
        /// <summary>
        /// 6
        /// </summary>
	    VK_6 = 54,
        /// <summary>
        /// 7
        /// </summary>
	    VK_7 = 55,
        /// <summary>
        /// 8
        /// </summary>
	    VK_8 = 56,
        /// <summary>
        /// 9
        /// </summary>
	    VK_9 = 57,
        /// <summary>
        /// A
        /// </summary>
	    VK_A = 97,
        /// <summary>
        /// B
        /// </summary>
	    VK_B = 98,
        /// <summary>
        /// C
        /// </summary>
	    VK_C = 99,
        /// <summary>
        /// D
        /// </summary>
	    VK_D = 100,
        /// <summary>
        /// E
        /// </summary>
	    VK_E = 101,
        /// <summary>
        /// F
        /// </summary>
	    VK_F = 102,
        /// <summary>
        /// G
        /// </summary>
        VK_G = 103,
        /// <summary>
        /// H
        /// </summary>
	    VK_H = 104,
        /// <summary>
        /// I
        /// </summary>
	    VK_I = 105,
        /// <summary>
        /// J
        /// </summary>
	    VK_J = 106,
        /// <summary>
        /// K
        /// </summary>
	    VK_K = 107,
        /// <summary>
        /// L
        /// </summary>
	    VK_L = 108,
        /// <summary>
        /// M
        /// </summary>
	    VK_M = 109,
        /// <summary>
        /// N
        /// </summary>
	    VK_N = 110,
        /// <summary>
        /// O
        /// </summary>
	    VK_O = 111,
        /// <summary>
        /// P
        /// </summary>
	    VK_P = 112,
        /// <summary>
        /// Q
        /// </summary>
	    VK_Q = 113,
        /// <summary>
        /// R
        /// </summary>
	    VK_R = 114,
        /// <summary>
        /// S
        /// </summary>
	    VK_S = 115,
        /// <summary>
        /// T
        /// </summary>
	    VK_T = 116,
        /// <summary>
        /// U
        /// </summary>
	    VK_U = 117,
        /// <summary>
        /// V
        /// </summary>
	    VK_V = 118,
        /// <summary>
        /// W
        /// </summary>
	    VK_W = 119,
        /// <summary>
        /// X
        /// </summary>
	    VK_X = 120,
        /// <summary>
        /// Y
        /// </summary>
	    VK_Y = 121,
        /// <summary>
        /// Z
        /// </summary>
	    VK_Z = 122,
        /// <summary>
        /// Left Win
        /// </summary>
	    VK_LWIN = 311,
        /// <summary>
        /// Right Win
        /// </summary>
	    VK_RWIN = 312,
        /// <summary>
        /// Apps
        /// </summary>
	    VK_APPS = 319,
        /// <summary>
        /// Sleep
        /// </summary>
	    VK_SLEEP = 320,
        /// <summary>
        /// Numpad 0
        /// </summary>
	    VK_NUMPAD0 = 256,
        /// <summary>
        /// Numpad 1
        /// </summary>
	    VK_NUMPAD1 = 257,
        /// <summary>
        /// Numpad 2
        /// </summary>
	    VK_NUMPAD2 = 258,
        /// <summary>
        /// Numpad 3
        /// </summary>
	    VK_NUMPAD3 = 259,
        /// <summary>
        /// Numpad 4
        /// </summary>
	    VK_NUMPAD4 = 260,
        /// <summary>
        /// Numpad 5
        /// </summary>
	    VK_NUMPAD5 = 261,
        /// <summary>
        /// Numpad 6
        /// </summary>
	    VK_NUMPAD6 = 262,
        /// <summary>
        /// Numpad 7
        /// </summary>
	    VK_NUMPAD7 = 263,
        /// <summary>
        /// Numpad 8
        /// </summary>
	    VK_NUMPAD8 = 264,
        /// <summary>
        /// Numpad 9
        /// </summary>
	    VK_NUMPAD9 = 265,
        /// <summary>
        /// Multiply
        /// </summary>
	    VK_MULTIPLY = 268,
        /// <summary>
        /// Add
        /// </summary>
	    VK_ADD = 270,
        /// <summary>
        /// Subtract
        /// </summary>
	    VK_SUBTRACT = 269,
        /// <summary>
        /// Decimal
        /// </summary>
	    VK_DECIMAL =  266,
        /// <summary>
        /// Divide
        /// </summary>
	    VK_DIVIDE = 267,
        /// <summary>
        /// F1
        /// </summary>
	    VK_F1 = 282,
        /// <summary>
        /// F2
        /// </summary>
	    VK_F2 = 283,
        /// <summary>
        /// F3
        /// </summary>
  	    VK_F3 = 284,
        /// <summary>
        /// F4
        /// </summary>
	    VK_F4 = 285,
        /// <summary>
        /// F5
        /// </summary>
	    VK_F5 = 286,
        /// <summary>
        /// F6
        /// </summary>
	    VK_F6 = 287,
        /// <summary>
        /// F7
        /// </summary>
	    VK_F7 = 288,
        /// <summary>
        /// F8
        /// </summary>
	    VK_F8 = 289,
        /// <summary>
        /// F9
        /// </summary>
	    VK_F9 = 290,
        /// <summary>
        /// F10
        /// </summary>
	    VK_F10 = 291,
        /// <summary>
        /// F11
        /// </summary>
	    VK_F11 = 292,
        /// <summary>
        /// F12
        /// </summary>
	    VK_F12 = 293,
        /// <summary>
        /// F13
        /// </summary>
	    VK_F13 = 294,
        /// <summary>
        /// F14
        /// </summary>
	    VK_F14 = 295,
        /// <summary>
        /// F15
        /// </summary>
	    VK_F15 = 296,
        /// <summary>
        /// Numlock
        /// </summary>
	    VK_NUMLOCK = 300,
        /// <summary>
        /// Scroll
        /// </summary>
	    VK_SCROLL = 302,
        /// <summary>
        /// Left Shift
        /// </summary>
	    VK_LSHIFT = 304,
        /// <summary>
        /// Right Shift
        /// </summary>
	    VK_RSHIFT = 303,
        /// <summary>
        /// Left Control
        /// </summary>
	    VK_LCONTROL = 305,
        /// <summary>
        /// Right Control
        /// </summary>
	    VK_RCONTROL = 306,
        /// <summary>
        /// Left Menu
        /// </summary>
	    VK_LMENU = 310,
        /// <summary>
        /// Left Alt
        /// </summary>
	    VK_LALT = 308,
        /// <summary>
        /// Right Menu
        /// </summary>
	    VK_RMENU = 309,
        /// <summary>
        /// Right Alt
        /// </summary>
	    VK_RALT = 307,
        /// <summary>
        /// Equals
        /// </summary>
	    VK_EQUALS = 61
    }
    
    /// <summary>
    /// Input Class
    ///
    /// Handles all the Mouse and Keyboard input functions
    /// </summary>
    public class Input
    {
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="GetMousePosition")]
        private static extern Vector DLL_GetMousePosition();
        /// <summary>
        /// Gets the Mouse Position
        /// </summary>
        /// <returns>Vector representing the position of the mouse</returns>
        public static Vector GetMousePosition()
        {
            try
            {
                Vector temp = DLL_GetMousePosition();
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (Core.ExceptionOccured())
                throw new SwinGameException(Core.GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="GetMouseMovement")]
        private static extern Vector DLL_GetMouseMovement();
        /// <summary>
        /// Gets the Movement of the mouse
        /// </summary>
        /// <returns>Vector representing the movement of the mouse</returns>
        public static Vector GetMouseMovement()
        {
            try
            {
                Vector temp = DLL_GetMouseMovement();
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (Core.ExceptionOccured())
                throw new SwinGameException(Core.GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="IsMouseDown")]
        private static extern bool DLL_IsMouseDown(MouseButton button);
        /// <summary>
        /// This function checks if the specified mouse button is
        /// being clicked
        /// </summary>
        /// <param name="button">Mouse button to check</param>
        /// <returns>True if the mouse button is being clicked</returns>
        public static bool IsMouseDown(MouseButton button)
        {
            try
            {
                bool temp = DLL_IsMouseDown(button);
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (Core.ExceptionOccured())
                throw new SwinGameException(Core.GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="IsMouseUp")]
        private static extern bool DLL_IsMouseUp(MouseButton button);
        /// <summary>
        /// This function checks if the specified mouse button is not
        /// being clicked
        /// </summary>
        /// <param name="button">Mouse button to check</param>
        /// <returns>True if the mouse button is not being clicked</returns>
        public static bool IsMouseUp(MouseButton button)
        {
            try
            {
                bool temp = DLL_IsMouseUp(button);
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (Core.ExceptionOccured())
                throw new SwinGameException(Core.GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="MouseWasClicked")]
        private static extern bool DLL_MouseWasClicked(MouseButton button);
        /// <summary>
        /// This functions checks if the mouse button specified has
        /// been clicked.
        /// </summary>
        /// <param name="button">Mouse button to check</param>
        /// <returns>True if the mouse button has been clicked</returns>
        public static bool MouseWasClicked(MouseButton button)
        {
            try
            {
                bool temp = DLL_MouseWasClicked(button);
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (Core.ExceptionOccured())
                throw new SwinGameException(Core.GetExceptionMessage());
            }  
        }


        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="StartReadingText")]
        private static extern void DLL_StartReadingText(uint color, int maxLength, Font theFont, int x, int y);
        /// StartReadingText start the API reading a string values from the user.
	    ///	Entry is completed when the user presses enter, and aborted with escape.
	    ///	If the user aborts entry the result is an empty string. Text entry is
	    ///	updated as part of ProcessEvents, and is drawn to the screen as part of
	    ///	the RefreshScreen call.
	    ///
	    ///	@param textColor:	The color of the text entered by the user
	    ///	@param maxLength:	The maximum length of the string the user can enter
	    ///	@param theFont:		The font used to draw the text entered
	    ///	@param x, y:			 The location at which to draw the text entered
        public static void StartReadingText(Color toColour, int maxLength, Font theFont, int x, int y)
        {
            try
            {
                int color = toColour.ToArgb();
                DLL_StartReadingText((uint)color, maxLength, theFont, x, y);
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
            }
            catch (Exception)
            {
                //if (Core.ExceptionOccured())
                throw new SwinGameException(Core.GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="IsReadingText")]  
	    private static extern bool DLL_IsReadingText();
        /// <summary>
        /// IsReadingText indicates if the API is currently reading text from the
        ///	user. Calling StartReadingText will set this to true, and it becomes
        ///	false when the user presses enter or escape. At this point you can
        ///	read the string entered as either ASCII or Unicode.
        /// </summary>
        /// <returns>True while the API is reading text from the user</returns>
        public static bool IsReadingText()
        {
            try
            {
                bool temp = DLL_IsReadingText();
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (Core.ExceptionOccured())
                throw new SwinGameException(Core.GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="TextReadAsASCII")]
        private static extern String DLL_TextReadAsASCII();
        /// <summary>
        /// TextReadAsASCII allows you to read the value of the string entered by the
        ///	user as ASCII. See TextReasAsUNICODE, StartReadingText and IsReadingText
        ///	for more details.
        /// </summary>
        /// <returns>The string entered by the user</returns>
        public static String TextReadAsASCII()
        {
            try
            {
                String temp = DLL_TextReadAsASCII();
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (Core.ExceptionOccured())
                throw new SwinGameException(Core.GetExceptionMessage());
            }  
        }

        /*
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="TextReadAsUNICODE")]
        private static extern String DLL_TextReadAsUNICODE();
        /// <summary>
        /// TextReadAsUNICODE returns the string entered by the user as UNICODE. See
        ///	TextReadAsASCII, StartReadingText, and IsReadingText for more details.
        /// </summary>
        /// <returns>The string entered by the user</returns>
        public static String TextReadAsUNICODE()
        {
            try
            {
                String temp = DLL_TextReadAsUNICODE();
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (Core.ExceptionOccured())
                throw new SwinGameException(Core.GetExceptionMessage());
            }  
        }
        */

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="IsKeyPressed")]
        private static extern bool DLL_IsKeyPressed(Keys key);
        /// <summary>
        /// Returns true when the key requested is being held down. This is updated
        ///	as part of the ProcessEvents call. Use the key codes from the KeyCodes
        ///	unit.
        /// </summary>
        /// <param name="key">Key</param>
        /// <returns>True if the key is currently being held down</returns>
        public static bool IsKeyPressed(Keys key)
        {
            try
            {
                bool temp = DLL_IsKeyPressed(key);
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (Core.ExceptionOccured())
                throw new SwinGameException(Core.GetExceptionMessage());
            }  
        }

        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint="WasKeyTyped")]
        private static extern bool DLL_WasKeyTyped(Keys key);
        /// <summary>
        /// Returns true when a key is typed. This occurs when the key is pressed on the 
        /// keyboard, and will not reoccur until it is released and pressed again. This
        /// needs to be checked each ProcessEvents loop.
        /// </summary>
        /// <param name="key">Key code to check</param>
        /// <returns>True if the key is pressed</returns>
        public static bool WasKeyTyped(Keys key)
        {
            try
            {
                bool temp = DLL_WasKeyTyped(key);
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
                return temp;
            }
            catch (Exception)
            {
                //if (Core.ExceptionOccured())
                throw new SwinGameException(Core.GetExceptionMessage());
            }  
        }




        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "MoveMouse")]
        private static extern void DLL_MoveMouse(UInt16 x, UInt16 y);
        /// <summary>
        /// This will move the mouse to the given x and y location
        /// </summary>
        /// <param name="x">The x pos to move the mouse to</param>
        /// <param name="y">The y pos to move the mouse to</param>
        public static void MoveMouse(UInt16 x, UInt16 y)
        {
            try
            {
                Console.WriteLine("C#: " + x + ":" + y);
                DLL_MoveMouse( x, y);
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
            }
            catch (Exception)
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
        }

        /// <summary>
        /// This will hide the mouse 
        /// </summary>
        public static void HideMouse()
        {
            ShowMouse(false);
        }


        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ShowMouse")]
        private static extern void DLL_ShowMouse(int show);
      /// <summary>
      /// Shows or hides the mouse
      /// </summary>
      /// <param name="show">if it is true it will show the mouse</param>
        public static void ShowMouse(bool show)
        {
            try
            {
                DLL_ShowMouse(show?-1:0);
                if (Core.ExceptionOccured())
                {
                    throw new SwinGameException(Core.GetExceptionMessage());
                }
            }
            catch (Exception)
            {
                throw new SwinGameException(Core.GetExceptionMessage());
            }
        }
        /// <summary>
        /// This will show the mouse 
        /// </summary>
        public static void ShowMouse()
        {
            ShowMouse(true);
        }
        [DllImport("SGSDK.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "IsMouseShown")]
        private static extern int DLL_IsMouseShown();
        /// <summary>
        /// Checks to see if the mouse is shown
        /// </summary>
        /// <returns>returns true if the mouse is shown</returns>
        public static bool IsMouseShown()
        {
            return DLL_IsMouseShown() == -1;
        }
    }
}
