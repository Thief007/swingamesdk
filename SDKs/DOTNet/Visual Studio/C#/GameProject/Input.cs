using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace SwinGame
{

    public enum MouseButton
    {
        LeftButton,
        MiddleButton,
        RightButton,
        MouseWheelUp,
        MouseWheelDown
    }
    
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
	    VK_DECIMAL =  266,
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
	    VK_EQUALS = 61
    }
    
    public class Input
    {
        [DllImport("SGSDK.dll")]
        public static extern Vector GetMousePosition();

        [DllImport("SGSDK.dll")]
        public static extern Vector GetMouseMovement();

        [DllImport("SGSDK.dll")]
        public static extern bool IsMouseDown(MouseButton button);

        [DllImport("SGSDK.dll")]
        public static extern bool IsMouseUp(MouseButton button);

        [DllImport("SGSDK.dll")]
        public static extern bool MouseWasClicked(MouseButton button);

        //[DllImport("SGSDK.dll")]
        //public static extern void StartReadingText(Color color, int maxLength, Font theFont, int x, int y);

        [DllImport("SGSDK.dll")]
        public static extern String TextReadAsASCII();

        [DllImport("SGSDK.dll")]
        public static extern String TextReadAsUNICODE();

        [DllImport("SGSDK.dll")]
        public static extern bool IsKeyPressed(Keys key);

        [DllImport("SGSDK.dll")]
        public static extern bool WasKeyTyped(Keys key);
    }
}
