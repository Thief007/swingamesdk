using System;
using System.Collections.Generic;
using System.Text;
using SwinGame;
using System.Runtime.InteropServices;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
namespace SwinGameVB
{
    /// <summary>
    /// Mouse Buttons
    /// </summary>
    [ComVisible(true)]
    public enum MouseButton
    {
        LeftButton = 1,
        MiddleButton = 2,
        RightButton = 3,
        MouseWheelUp = 4,
        MouseWheelDown = 5,
				MouseX1Button = 6,
				MouseX2Button = 7
    }

    /// <summary>
    /// Keys that can be used on the Keyboard
    /// </summary>
    [ComVisible(true)]
    public enum Keys
    {
        VK_BACK = 8,
        VK_TAB = 9,
        VK_CLEAR = 12,
        VK_RETURN = 13,
        VK_SHIFT = 304,
        VK_CONTROL = 306,
        VK_MENU = 319,
        VK_ALT = 308,
        VK_PAUSE = 19,
        VK_CAPITAL = 301,
        VK_ESCAPE = 27,
        VK_SPACE = 32,
        VK_PAGE_UP = 280,
        VK_PAGE_DOWN = 281,
        VK_END = 279,
        VK_HOME = 278,
        VK_LEFT = 276,
        VK_UP = 273,
        VK_RIGHT = 275,
        VK_DOWN = 274,
        VK_PRINT = 316,
        VK_INSERT = 277,
        VK_DELETE = 127,
        VK_HELP = 315,
        VK_0 = 48,
        VK_1 = 49,
        VK_2 = 50,
        VK_3 = 51,
        VK_4 = 52,
        VK_5 = 53,
        VK_6 = 54,
        VK_7 = 55,
        VK_8 = 56,
        VK_9 = 57,
        VK_A = 97,
        VK_B = 98,
        VK_C = 99,
        VK_D = 100,
        VK_E = 101,
        VK_F = 102,
        VK_G = 103,
        VK_H = 104,
        VK_I = 105,
        VK_J = 106,
        VK_K = 107,
        VK_L = 108,
        VK_M = 109,
        VK_N = 110,
        VK_O = 111,
        VK_P = 112,
        VK_Q = 113,
        VK_R = 114,
        VK_S = 115,
        VK_T = 116,
        VK_U = 117,
        VK_V = 118,
        VK_W = 119,
        VK_X = 120,
        VK_Y = 121,
        VK_Z = 122,
        VK_LWIN = 311,
        VK_RWIN = 312,
        VK_APPS = 319,
        VK_SLEEP = 320,
        VK_NUMPAD0 = 256,
        VK_NUMPAD1 = 257,
        VK_NUMPAD2 = 258,
        VK_NUMPAD3 = 259,
        VK_NUMPAD4 = 260,
        VK_NUMPAD5 = 261,
        VK_NUMPAD6 = 262,
        VK_NUMPAD7 = 263,
        VK_NUMPAD8 = 264,
        VK_NUMPAD9 = 265,
        VK_MULTIPLY = 268,
        VK_ADD = 270,
        VK_SUBTRACT = 269,
        VK_DECIMAL = 266,
        VK_DIVIDE = 267,
        VK_F1 = 282,
        VK_F2 = 283,
        VK_F3 = 284,
        VK_F4 = 285,
        VK_F5 = 286,
        VK_F6 = 287,
        VK_F7 = 288,
        VK_F8 = 289,
        VK_F9 = 290,
        VK_F10 = 291,
        VK_F11 = 292,
        VK_F12 = 293,
        VK_F13 = 294,
        VK_F14 = 295,
        VK_F15 = 296,
        VK_NUMLOCK = 300,
        VK_SCROLL = 302,
        VK_LSHIFT = 304,
        VK_RSHIFT = 303,
        VK_LCONTROL = 305,
        VK_RCONTROL = 306,
        VK_LMENU = 310,
        VK_LALT = 308,
        VK_RMENU = 309,
        VK_RALT = 307,
        VK_EQUALS = 61,
				 VK_COLON = 58,
				 VK_SEMICOLON = 59,
				 VK_LESS = 60,
				 VK_GREATER = 62,
				 VK_QUESTION = 63,
				 VK_AT = 64,
				 VK_COMMA = 44,
				 VK_PERIOD = 46,
				 VK_SLASH =47
    }

    /// <summary>
    /// Input Class
    ///
    /// Handles all the Mouse and Keyboard input functions
    /// </summary>
    [ClassInterface(ClassInterfaceType.None)]
    [Guid("61146386-9F07-497b-ACC4-D53D8A0A39D9")]
    [ComVisible(true)]
    public class Input:IInput
    {
        /// <summary>
        /// Gets the Mouse Position
        /// </summary>
        /// <returns>Vector representing the position of the mouse</returns>
        public Point2D GetMousePosition()
        {
            Point2D vector = new Point2D();
            vector.SetX(SwinGame.Input.GetMousePosition().X);
            vector.SetY(SwinGame.Input.GetMousePosition().Y);
            return vector;
        }

        /// <summary>
        /// Gets the Movement of the mouse
        /// </summary>
        /// <returns>Vector representing the movement of the mouse</returns>
        public Vector GetMouseMovement()
        {
            SwinGame.Point2D temp = new SwinGame.Point2D();
            temp = SwinGame.Input.GetMousePosition();

            Vector vector = new Vector();
            vector.setX(temp.X - Core._LastMousePos.X);//SwinGame.Input.GetMouseMovement().x);
            vector.setY(temp.Y - Core._LastMousePos.Y);//SwinGame.Input.GetMouseMovement().y);
            Core._LastMousePos = SwinGame.Input.GetMousePosition();
            return vector;

        }


        /// <summary>
        /// This function checks if the specified mouse button is
        /// being clicked
        /// </summary>
        /// <param name="button">Mouse button to check</param>
        /// <returns>True if the mouse button is being clicked</returns>
        public bool IsMouseDown(MouseButton button)
        {
            return SwinGame.Input.IsMouseDown((SwinGame.MouseButton)button);
        }

        /// <summary>
        /// This function checks if the specified mouse button is not
        /// being clicked
        /// </summary>
        /// <param name="button">Mouse button to check</param>
        /// <returns>True if the mouse button is not being clicked</returns>
        public bool IsMouseUp(MouseButton button)
        {
            return SwinGame.Input.IsMouseUp((SwinGame.MouseButton)button);
        }

        /// <summary>
        /// This functions checks if the mouse button specified has
        /// been clicked.
        /// </summary>
        /// <param name="button">Mouse button to check</param>
        /// <returns>True if the mouse button has been clicked</returns>
        public  bool MouseWasClicked(MouseButton button)
        {
            return SwinGame.Input.MouseWasClicked((SwinGame.MouseButton)button);
        }

				/// <summary>
        /// StartReadingText start the API reading a string values from the user.
        ///	Entry is completed when the user presses enter, and aborted with escape.
        ///	If the user aborts entry the result is an empty string. Text entry is
        ///	updated as part of ProcessEvents, and is drawn to the screen as part of
        ///	the RefreshScreen call.
				/// </summary>
        ///
        ///	@param textColor:	The color of the text entered by the user
        ///	@param maxLength:	The maximum length of the string the user can enter
        ///	@param theFont:		The font used to draw the text entered
        ///	@param x, y:			 The location at which to draw the text entered
        public void StartReadingText(int toColour, int maxLength, Fonts theFont, int x, int y)
        {
            Color color = Color.FromArgb(toColour);
            SwinGame.Input.StartReadingText(color, maxLength, theFont.result, x, y);
        }

				/// <summary>
        /// Ends reading text from the user and returns the string entered.
				/// </summary>
        /// <returns>The String entered by the user</returns>
				public string EndReadingText()
				{
						return SwinGame.Input.EndReadingText();
				}

        /// <summary>
        /// IsReadingText indicates if the API is currently reading text from the
        ///	user. Calling StartReadingText will set this to true, and it becomes
        ///	false when the user presses enter or escape. At this point you can
        ///	read the string entered as either ASCII or Unicode.
        /// </summary>
        /// <returns>True while the API is reading text from the user</returns>
        public bool IsReadingText()
        {
            return SwinGame.Input.IsReadingText();
        }

        /// <summary>
        /// TextReadAsASCII allows you to read the value of the string entered by the
        ///	user as ASCII. See TextReasAsUNICODE, StartReadingText and IsReadingText
        ///	for more details.
        /// </summary>
        /// <returns>The string entered by the user</returns>
        public String TextReadAsASCII()
        {
            return SwinGame.Input.TextReadAsASCII();
        }

        /// <summary>
        /// Returns true when the key requested is being held down. This is updated
        ///	as part of the ProcessEvents call. Use the key codes from the KeyCodes
        ///	unit.
        /// </summary>
        /// <param name="key">Key</param>
        /// <returns>True if the key is currently being held down</returns>
        public bool IsKeyPressed(Keys key)
        {
            return SwinGame.Input.IsKeyPressed((SwinGame.Keys)key);
        }

        /// <summary>
        /// Returns true when a key is typed. This occurs when the key is pressed on the 
        /// keyboard, and will not reoccur until it is released and pressed again. This
        /// needs to be checked each ProcessEvents loop.
        /// </summary>
        /// <param name="key">Key code to check</param>
        /// <returns>True if the key is pressed</returns>
        public bool WasKeyTyped(Keys key)
        {
            return SwinGame.Input.WasKeyTyped((SwinGame.Keys)key);
        }

        public void ShowMouse()
        {
            SwinGame.Input.ShowMouse();
        }
        public void HideMouse()
        {
            SwinGame.Input.HideMouse();
        }
        public void MoveMouse(short x, short y)
        {
            SwinGame.Input.MoveMouse((UInt16)x, (UInt16)y);
            Core._LastMousePos = SwinGame.Input.GetMousePosition();
        }
        public void ShowHideMouse(bool Show)
        {
            SwinGame.Input.ShowMouse(Show);
        }
        public bool IsMouseShown()
        {
            return SwinGame.Input.IsMouseShown();
        }
        public Vector GetMousePositionAsVector()
        {
            Vector temp = new Vector();
            temp.result = SwinGame.Input.GetMousePositionAsVector();
            return temp;
        }
    }

    [Guid("AF53AFA1-8B04-46fb-8014-904BEF21D0EE")]
    [ComVisible(true)]
    public interface IInput
    {
        Point2D GetMousePosition();
        Vector GetMouseMovement();

        void ShowMouse();
        void HideMouse();
        void MoveMouse(short x, short y);
        void ShowHideMouse(bool show);
        bool IsMouseShown();

        bool IsMouseDown(MouseButton button);
        bool IsMouseUp(MouseButton button);
        bool MouseWasClicked(MouseButton button);
        void StartReadingText(int toColour, int maxLength, Fonts theFont, int x, int y);
        bool IsReadingText();
        String TextReadAsASCII();
        bool IsKeyPressed(Keys key);
        bool WasKeyTyped(Keys key);
        Vector GetMousePositionAsVector();
    }

}
