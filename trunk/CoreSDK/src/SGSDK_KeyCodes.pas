//---------------------------------------------------/
//+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+/+
//          SGSDK_KeyCodes.pas
//+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+
//\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\
//
// The KeyCodes unit contains the constants for the key
// codes.
//
// Change History:
//
// Version 1.1:
// - 2008-04-02: Andrew: Added extra key codes
// - 2008-01-17: Aki + Andrew: Refactor
//  
// Version 1.0:
// - Various

/// @module Keys
unit SGSDK_KeyCodes;

interface
uses SDL;

  type
    /// @enum KeyCode
    KeyCode = (
  vk_Unknown = 0,
  vk_BACKSPACE = 8,
  vk_TAB = 9,
  vk_CLEAR = 12,
  vk_RETURN = 13,
  vk_PAUSE = 19,
  vk_ESCAPE = 27,
  vk_SPACE = 32,
  vk_EXCLAIM = 33,
  vk_QUOTEDBL = 34,
  vk_HASH = 35,
  vk_DOLLAR = 36,
  vk_AMPERSAND = 38,
  vk_QUOTE = 39,
  vk_LEFTPAREN = 40,
  vk_RIGHTPAREN = 41,
  vk_ASTERISK = 42,
  vk_PLUS = 43,
  vk_COMMA = 44,
  vk_MINUS = 45,
  vk_PERIOD = 46,
  vk_SLASH = 47,
  vk_0 = 48,
  vk_1 = 49,
  vk_2 = 50,
  vk_3 = 51,
  vk_4 = 52,
  vk_5 = 53,
  vk_6 = 54,
  vk_7 = 55,
  vk_8 = 56,
  vk_9 = 57,
  vk_COLON = 58,
  vk_SEMICOLON = 59,
  vk_LESS = 60,
  vk_EQUALS = 61,
  vk_GREATER = 62,
  vk_QUESTION = 63,
  vk_AT = 64,

  { Skip uppercase letters }

  vk_LEFTBRACKET = 91,
  vk_BACKSLASH = 92,
  vk_RIGHTBRACKET = 93,
  vk_CARET = 94,
  vk_UNDERSCORE = 95,
  vk_BACKQUOTE = 96,
  vk_a = 97,
  vk_b = 98,
  vk_c = 99,
  vk_d = 100,
  vk_e = 101,
  vk_f = 102,
  vk_g = 103,
  vk_h = 104,
  vk_i = 105,
  vk_j = 106,
  vk_k = 107,
  vk_l = 108,
  vk_m = 109,
  vk_n = 110,
  vk_o = 111,
  vk_p = 112,
  vk_q = 113,
  vk_r = 114,
  vk_s = 115,
  vk_t = 116,
  vk_u = 117,
  vk_v = 118,
  vk_w = 119,
  vk_x = 120,
  vk_y = 121,
  vk_z = 122,
  vk_DELETE = 127,
  // End of ASCII mapped keysyms

  // International keyboard syms
  vk_WORLD_0 = 160, // 0xA0
  vk_WORLD_1 = 161,
  vk_WORLD_2 = 162,
  vk_WORLD_3 = 163,
  vk_WORLD_4 = 164,
  vk_WORLD_5 = 165,
  vk_WORLD_6 = 166,
  vk_WORLD_7 = 167,
  vk_WORLD_8 = 168,
  vk_WORLD_9 = 169,
  vk_WORLD_10 = 170,
  vk_WORLD_11 = 171,
  vk_WORLD_12 = 172,
  vk_WORLD_13 = 173,
  vk_WORLD_14 = 174,
  vk_WORLD_15 = 175,
  vk_WORLD_16 = 176,
  vk_WORLD_17 = 177,
  vk_WORLD_18 = 178,
  vk_WORLD_19 = 179,
  vk_WORLD_20 = 180,
  vk_WORLD_21 = 181,
  vk_WORLD_22 = 182,
  vk_WORLD_23 = 183,
  vk_WORLD_24 = 184,
  vk_WORLD_25 = 185,
  vk_WORLD_26 = 186,
  vk_WORLD_27 = 187,
  vk_WORLD_28 = 188,
  vk_WORLD_29 = 189,
  vk_WORLD_30 = 190,
  vk_WORLD_31 = 191,
  vk_WORLD_32 = 192,
  vk_WORLD_33 = 193,
  vk_WORLD_34 = 194,
  vk_WORLD_35 = 195,
  vk_WORLD_36 = 196,
  vk_WORLD_37 = 197,
  vk_WORLD_38 = 198,
  vk_WORLD_39 = 199,
  vk_WORLD_40 = 200,
  vk_WORLD_41 = 201,
  vk_WORLD_42 = 202,
  vk_WORLD_43 = 203,
  vk_WORLD_44 = 204,
  vk_WORLD_45 = 205,
  vk_WORLD_46 = 206,
  vk_WORLD_47 = 207,
  vk_WORLD_48 = 208,
  vk_WORLD_49 = 209,
  vk_WORLD_50 = 210,
  vk_WORLD_51 = 211,
  vk_WORLD_52 = 212,
  vk_WORLD_53 = 213,
  vk_WORLD_54 = 214,
  vk_WORLD_55 = 215,
  vk_WORLD_56 = 216,
  vk_WORLD_57 = 217,
  vk_WORLD_58 = 218,
  vk_WORLD_59 = 219,
  vk_WORLD_60 = 220,
  vk_WORLD_61 = 221,
  vk_WORLD_62 = 222,
  vk_WORLD_63 = 223,
  vk_WORLD_64 = 224,
  vk_WORLD_65 = 225,
  vk_WORLD_66 = 226,
  vk_WORLD_67 = 227,
  vk_WORLD_68 = 228,
  vk_WORLD_69 = 229,
  vk_WORLD_70 = 230,
  vk_WORLD_71 = 231,
  vk_WORLD_72 = 232,
  vk_WORLD_73 = 233,
  vk_WORLD_74 = 234,
  vk_WORLD_75 = 235,
  vk_WORLD_76 = 236,
  vk_WORLD_77 = 237,
  vk_WORLD_78 = 238,
  vk_WORLD_79 = 239,
  vk_WORLD_80 = 240,
  vk_WORLD_81 = 241,
  vk_WORLD_82 = 242,
  vk_WORLD_83 = 243,
  vk_WORLD_84 = 244,
  vk_WORLD_85 = 245,
  vk_WORLD_86 = 246,
  vk_WORLD_87 = 247,
  vk_WORLD_88 = 248,
  vk_WORLD_89 = 249,
  vk_WORLD_90 = 250,
  vk_WORLD_91 = 251,
  vk_WORLD_92 = 252,
  vk_WORLD_93 = 253,
  vk_WORLD_94 = 254,
  vk_WORLD_95 = 255, // 0xFF

  // Numeric keypad
  vk_KP0 = 256,
  vk_KP1 = 257,
  vk_KP2 = 258,
  vk_KP3 = 259,
  vk_KP4 = 260,
  vk_KP5 = 261,
  vk_KP6 = 262,
  vk_KP7 = 263,
  vk_KP8 = 264,
  vk_KP9 = 265,
  vk_KP_PERIOD = 266,
  vk_KP_DIVIDE = 267,
  vk_KP_MULTIPLY = 268,
  vk_KP_MINUS = 269,
  vk_KP_PLUS = 270,
  vk_KP_ENTER = 271,
  vk_KP_EQUALS = 272,

  // Arrows + Home/End pad
  vk_UP = 273,
  vk_DOWN = 274,
  vk_RIGHT = 275,
  vk_LEFT = 276,
  vk_INSERT = 277,
  vk_HOME = 278,
  vk_END = 279,
  vk_PAGEUP = 280,
  vk_PAGEDOWN = 281,

  // Function keys
  vk_F1 = 282,
  vk_F2 = 283,
  vk_F3 = 284,
  vk_F4 = 285,
  vk_F5 = 286,
  vk_F6 = 287,
  vk_F7 = 288,
  vk_F8 = 289,
  vk_F9 = 290,
  vk_F10 = 291,
  vk_F11 = 292,
  vk_F12 = 293,
  vk_F13 = 294,
  vk_F14 = 295,
  vk_F15 = 296,

  // Key state modifier keys
  vk_NUMLOCK = 300,
  vk_CAPSLOCK = 301,
  vk_SCROLLOCK = 302,
  vk_RSHIFT = 303,
  vk_LSHIFT = 304,
  vk_RCTRL = 305,
  vk_LCTRL = 306,
  vk_RALT = 307,
  vk_LALT = 308,
  vk_RMETA = 309,
  vk_LMETA = 310,
  vk_LSUPER = 311, // Left "Windows" key
  vk_RSUPER = 312, // Right "Windows" key
  vk_MODE = 313, // "Alt Gr" key
  vk_COMPOSE = 314, // Multi-key compose key
  // Miscellaneous function keys
  vk_HELP = 315,
  vk_PRINT = 316,
  vk_SYSREQ = 317,
  vk_BREAK = 318,
  vk_MENU = 319,
  vk_POWER = 320, // Power Macintosh power key
  vk_EURO = 321 // Some european keyboards
);

{const




  VK_BACK = SDLK_BACKSPACE;
  VK_TAB = SDLK_TAB;
  VK_CLEAR = SDLK_CLEAR;
  VK_RETURN = SDLK_RETURN;
  VK_SHIFT = SDLK_LSHIFT;
  VK_CONTROL = SDLK_LCTRL;
  VK_MENU = SDLK_MENU;
  VK_ALT = SDLK_LALT;
  VK_PAUSE = SDLK_PAUSE;
  VK_CAPITAL = SDLK_CAPSLOCK;
  VK_ESCAPE = SDLK_ESCAPE;
  VK_SPACE = SDLK_SPACE;
  VK_PAGE_UP = SDLK_PAGEUP;
  VK_PAGE_DOWN = SDLK_PAGEDOWN;
  VK_END = SDLK_END;
  VK_HOME = SDLK_HOME;
  VK_LEFT = SDLK_LEFT;
  VK_UP = SDLK_UP;
  VK_RIGHT = SDLK_RIGHT;
  VK_DOWN = SDLK_DOWN;
  VK_PRINT = SDLK_PRINT;
  VK_INSERT = SDLK_INSERT;
  VK_DELETE = SDLK_DELETE;
  VK_HELP = SDLK_HELP;
  VK_0 = SDLK_0;
  VK_1 = SDLK_1;
  VK_2 = SDLK_2;
  VK_3 = SDLK_3;
  VK_4 = SDLK_4;
  VK_5 = SDLK_5;
  VK_6 = SDLK_6;
  VK_7 = SDLK_7;
  VK_8 = SDLK_8;
  VK_9 = SDLK_9;
  VK_A = SDLK_A;
  VK_B = SDLK_B;
  VK_C = SDLK_C;
  VK_D = SDLK_D;
  VK_E = SDLK_E;
  VK_F = SDLK_F;
  VK_G = SDLK_G;
  VK_H = SDLK_H;
  VK_I = SDLK_I;
  VK_J = SDLK_J;
  VK_K = SDLK_K;
  VK_L = SDLK_L;
  VK_M = SDLK_M;
  VK_N = SDLK_N;
  VK_O = SDLK_O;
  VK_P = SDLK_P;
  VK_Q = SDLK_Q;
  VK_R = SDLK_R;
  VK_S = SDLK_S;
  VK_T = SDLK_T;
  VK_U = SDLK_U;
  VK_V = SDLK_V;
  VK_W = SDLK_W;
  VK_X = SDLK_X;
  VK_Y = SDLK_Y;
  VK_Z = SDLK_Z;
  VK_LWIN = SDLK_LSUPER;
  VK_RWIN = SDLK_RSUPER;
  VK_APPS = SDLK_MENU;
  VK_SLEEP = SDLK_POWER;
  VK_NUMPAD0 = SDLK_KP0;
  VK_NUMPAD1 = SDLK_KP1;
  VK_NUMPAD2 = SDLK_KP2;
  VK_NUMPAD3 = SDLK_KP3;
  VK_NUMPAD4 = SDLK_KP4;
  VK_NUMPAD5 = SDLK_KP5;
  VK_NUMPAD6 = SDLK_KP6;
  VK_NUMPAD7 = SDLK_KP7;
  VK_NUMPAD8 = SDLK_KP8;
  VK_NUMPAD9 = SDLK_KP9;
  VK_MULTIPLY = SDLK_KP_MULTIPLY;
  VK_ADD = SDLK_KP_PLUS;
  VK_SUBTRACT = SDLK_MINUS;
  VK_DECIMAL = SDLK_KP_PERIOD;
  VK_PERIOD = SDLK_PERIOD;
  VK_DIVIDE = SDLK_KP_DIVIDE;
  VK_F1 = SDLK_F1;
  VK_F2 = SDLK_F2;
  VK_F3 = SDLK_F3;
  VK_F4 = SDLK_F4;
  VK_F5 = SDLK_F5;
  VK_F6 = SDLK_F6;
  VK_F7 = SDLK_F7;
  VK_F8 = SDLK_F8;
  VK_F9 = SDLK_F9;
  VK_F10 = SDLK_F10;
  VK_F11 = SDLK_F11;
  VK_F12 = SDLK_F12;
  VK_F13 = SDLK_F13;
  VK_F14 = SDLK_F14;
  VK_F15 = SDLK_F15;
  VK_NUMLOCK = SDLK_NUMLOCK;
  VK_SCROLL = SDLK_SCROLLOCK;
  VK_LSHIFT = SDLK_LSHIFT;
  VK_RSHIFT = SDLK_RSHIFT;
  VK_LCONTROL = SDLK_LCTRL;
  VK_RCONTROL = SDLK_RCTRL;
  VK_LMENU = SDLK_LMETA;
  VK_LALT = SDLK_LALT;
  VK_RMENU = SDLK_RMETA;
  VK_RALT = SDLK_RALT;
  VK_EQUALS = SDLK_EQUALS;
  VK_COLON =  SDLK_COLON;
  VK_SEMICOLON =  SDLK_SEMICOLON;
  VK_LESS =  SDLK_LESS;
  VK_GREATER =  SDLK_GREATER;
  VK_QUESTION =  SDLK_QUESTION;
  VK_AT =  SDLK_AT;
  VK_COMMA = SDLK_COMMA;
  VK_SLASH = SDLK_SLASH;
}  
implementation

end.
