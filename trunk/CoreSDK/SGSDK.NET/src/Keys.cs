//-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
// 					Keys
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// Change History:
//
// Version 2.0:
// - 2009-01-20: Andrew: Added version histroy 
//                       to newly created classes
//
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\

using System;
using System.Collections.Generic;
using System.Text;

namespace SwinGame
{
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
        VK_DECIMAL = 266,
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
        VK_EQUALS = 61,

        /// <summary>
        /// Colon
        /// </summary>
        VK_COLON = 58,
        /// <summary>
        /// Semicolon
        /// </summary>
        VK_SEMICOLON = 59,
        /// <summary>
        /// Less than
        /// </summary>
        VK_LESS = 60,
        /// <summary>
        /// Greater than
        /// </summary>
        VK_GREATER = 62,
        /// <summary>
        /// Question ?
        /// </summary>
        VK_QUESTION = 63,
        /// <summary>
        /// At @
        /// </summary>
        VK_AT = 64,

        /// <summary>
        /// Comma ,
        /// </summary>			
        VK_COMMA = 44,
        /// <summary>
        /// Period .
        /// </summary>
        VK_PERIOD = 46,
        /// <summary>
        /// Slash /
        /// </summary>
        VK_SLASH = 47
    }
}
