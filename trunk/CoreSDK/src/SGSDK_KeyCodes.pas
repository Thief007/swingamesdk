//=============================================================================
//          SGSDK_KeyCodes.pas
//=============================================================================
//
// The KeyCodes unit contains the constants for the key codes.
//
// Change History:
// Version 3.0
// - 2009-06-16: Clinton: Formatting/comment tweaks
// - 2009-06-15: Andrew: Updated key codes/format and function+meta comments
//
// Version 1.1:
// - 2008-04-02: Andrew: Added extra key codes
// - 2008-01-17: Aki + Andrew: Refactor
//  
// Version 1.0:
// - Various
//=============================================================================

/// @module Keys
unit SGSDK_KeyCodes;

//=============================================================================
interface
//=============================================================================

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

      // Skip uppercase letters 

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



  /// The KeyName function returns a string name for a given `KeyCode`. For 
  /// example, vk_Comma returns the string 'Comma'. This function could be used
  /// to display more meaningful key names for configuring game controls, etc.
  ///
  /// @param key The enumerated key code value used to identify a specific key
  /// @returns a string version of the key name
  /// @lib
  function KeyName(key: KeyCode): String;
  
//=============================================================================
implementation
//=============================================================================

function KeyName(key: KeyCode): String;
begin
  case key of
    vk_Unknown: result := 'Unknown';
    vk_BACKSPACE: result := 'Backspace';
    vk_TAB: result := 'Tab';
    vk_CLEAR: result := 'Clear';
    vk_RETURN: result := 'Return';
    vk_PAUSE: result := 'Pause';
    vk_ESCAPE: result := 'Escape';
    vk_SPACE: result := 'Space';
    vk_EXCLAIM: result := 'Exclaim';
    vk_QUOTEDBL: result := 'Double Quote';
    vk_HASH: result := 'Hash';
    vk_DOLLAR: result := 'Dollar';
    vk_AMPERSAND: result := 'Ampersand';
    vk_QUOTE: result := 'Quote';
    vk_LEFTPAREN: result := 'Left Parenthesis';
    vk_RIGHTPAREN: result := 'Right Parenthesis';
    vk_ASTERISK: result := 'Asterisk';
    vk_PLUS: result := 'Plus';
    vk_COMMA: result := 'Comma';
    vk_MINUS: result := 'Minus';
    vk_PERIOD: result := 'Period';
    vk_SLASH: result := 'Slash';
    vk_0: result := '0';
    vk_1: result := '1';
    vk_2: result := '2';
    vk_3: result := '3';
    vk_4: result := '4';
    vk_5: result := '5';
    vk_6: result := '6';
    vk_7: result := '7';
    vk_8: result := '8';
    vk_9: result := '9';
    vk_COLON: result := 'Colon';
    vk_SEMICOLON: result := 'Semicolon';
    vk_LESS: result := 'Less';
    vk_EQUALS: result := 'Equals';
    vk_GREATER: result := 'Greater';
    vk_QUESTION: result := 'Question';
    vk_AT: result := 'At';

    // Skip uppercase letters 

    vk_LEFTBRACKET: result := 'Left Bracket';
    vk_BACKSLASH: result := 'Backslash';
    vk_RIGHTBRACKET: result := 'Right Bracket';
    vk_CARET: result := 'Caret';
    vk_UNDERSCORE: result := 'Underscore';
    vk_BACKQUOTE: result := 'Back Quote';
    vk_a: result := 'a';
    vk_b: result := 'b';
    vk_c: result := 'c';
    vk_d: result := 'd';
    vk_e: result := 'e';
    vk_f: result := 'f';
    vk_g: result := 'g';
    vk_h: result := 'h';
    vk_i: result := 'i';
    vk_j: result := 'j';
    vk_k: result := 'k';
    vk_l: result := 'l';
    vk_m: result := 'm';
    vk_n: result := 'n';
    vk_o: result := 'o';
    vk_p: result := 'p';
    vk_q: result := 'q';
    vk_r: result := 'r';
    vk_s: result := 's';
    vk_t: result := 't';
    vk_u: result := 'u';
    vk_v: result := 'v';
    vk_w: result := 'w';
    vk_x: result := 'x';
    vk_y: result := 'y';
    vk_z: result := 'z';
    vk_DELETE: result := 'Delete';
    // End of ASCII mapped keysyms

    // International keyboard syms
    vk_WORLD_0: result := 'World 0';
    vk_WORLD_1: result := 'World 1';
    vk_WORLD_2: result := 'World 2';
    vk_WORLD_3: result := 'World 3';
    vk_WORLD_4: result := 'World 4';
    vk_WORLD_5: result := 'World 5';
    vk_WORLD_6: result := 'World 6';
    vk_WORLD_7: result := 'World 7';
    vk_WORLD_8: result := 'World 8';
    vk_WORLD_9: result := 'World 9';
    vk_WORLD_10: result := 'World 10';
    vk_WORLD_11: result := 'World 11';
    vk_WORLD_12: result := 'World 12';
    vk_WORLD_13: result := 'World 13';
    vk_WORLD_14: result := 'World 14';
    vk_WORLD_15: result := 'World 15';
    vk_WORLD_16: result := 'World 16';
    vk_WORLD_17: result := 'World 17';
    vk_WORLD_18: result := 'World 18';
    vk_WORLD_19: result := 'World 19';
    vk_WORLD_20: result := 'World 20';
    vk_WORLD_21: result := 'World 21';
    vk_WORLD_22: result := 'World 22';
    vk_WORLD_23: result := 'World 23';
    vk_WORLD_24: result := 'World 24';
    vk_WORLD_25: result := 'World 25';
    vk_WORLD_26: result := 'World 26';
    vk_WORLD_27: result := 'World 27';
    vk_WORLD_28: result := 'World 28';
    vk_WORLD_29: result := 'World 29';
    vk_WORLD_30: result := 'World 30';
    vk_WORLD_31: result := 'World 31';
    vk_WORLD_32: result := 'World 32';
    vk_WORLD_33: result := 'World 33';
    vk_WORLD_34: result := 'World 34';
    vk_WORLD_35: result := 'World 35';
    vk_WORLD_36: result := 'World 36';
    vk_WORLD_37: result := 'World 37';
    vk_WORLD_38: result := 'World 38';
    vk_WORLD_39: result := 'World 39';
    vk_WORLD_40: result := 'World 40';
    vk_WORLD_41: result := 'World 41';
    vk_WORLD_42: result := 'World 42';
    vk_WORLD_43: result := 'World 43';
    vk_WORLD_44: result := 'World 44';
    vk_WORLD_45: result := 'World 45';
    vk_WORLD_46: result := 'World 46';
    vk_WORLD_47: result := 'World 47';
    vk_WORLD_48: result := 'World 48';
    vk_WORLD_49: result := 'World 49';
    vk_WORLD_50: result := 'World 50';
    vk_WORLD_51: result := 'World 51';
    vk_WORLD_52: result := 'World 52';
    vk_WORLD_53: result := 'World 53';
    vk_WORLD_54: result := 'World 54';
    vk_WORLD_55: result := 'World 55';
    vk_WORLD_56: result := 'World 56';
    vk_WORLD_57: result := 'World 57';
    vk_WORLD_58: result := 'World 58';
    vk_WORLD_59: result := 'World 59';
    vk_WORLD_60: result := 'World 60';
    vk_WORLD_61: result := 'World 61';
    vk_WORLD_62: result := 'World 62';
    vk_WORLD_63: result := 'World 63';
    vk_WORLD_64: result := 'World 64';
    vk_WORLD_65: result := 'World 65';
    vk_WORLD_66: result := 'World 66';
    vk_WORLD_67: result := 'World 67';
    vk_WORLD_68: result := 'World 68';
    vk_WORLD_69: result := 'World 69';
    vk_WORLD_70: result := 'World 70';
    vk_WORLD_71: result := 'World 71';
    vk_WORLD_72: result := 'World 72';
    vk_WORLD_73: result := 'World 73';
    vk_WORLD_74: result := 'World 74';
    vk_WORLD_75: result := 'World 75';
    vk_WORLD_76: result := 'World 76';
    vk_WORLD_77: result := 'World 77';
    vk_WORLD_78: result := 'World 78';
    vk_WORLD_79: result := 'World 79';
    vk_WORLD_80: result := 'World 80';
    vk_WORLD_81: result := 'World 81';
    vk_WORLD_82: result := 'World 82';
    vk_WORLD_83: result := 'World 83';
    vk_WORLD_84: result := 'World 84';
    vk_WORLD_85: result := 'World 85';
    vk_WORLD_86: result := 'World 86';
    vk_WORLD_87: result := 'World 87';
    vk_WORLD_88: result := 'World 88';
    vk_WORLD_89: result := 'World 89';
    vk_WORLD_90: result := 'World 90';
    vk_WORLD_91: result := 'World 91';
    vk_WORLD_92: result := 'World 92';
    vk_WORLD_93: result := 'World 93';
    vk_WORLD_94: result := 'World 94';
    vk_WORLD_95: result := 'World 95';

    // Numeric keypad
    vk_KP0: result := 'Keypad 0';
    vk_KP1: result := 'Keypad 1';
    vk_KP2: result := 'Keypad 2';
    vk_KP3: result := 'Keypad 3';
    vk_KP4: result := 'Keypad 4';
    vk_KP5: result := 'Keypad 5';
    vk_KP6: result := 'Keypad 6';
    vk_KP7: result := 'Keypad 7';
    vk_KP8: result := 'Keypad 8';
    vk_KP9: result := 'Keypad 9';
    vk_KP_PERIOD: result := 'Keypad Period';
    vk_KP_DIVIDE: result := 'Keypad Divide';
    vk_KP_MULTIPLY: result := 'Keypad Multiply';
    vk_KP_MINUS: result := 'Keypad Minus';
    vk_KP_PLUS: result := 'Keypad Plus';
    vk_KP_ENTER: result := 'Keypad Enter';
    vk_KP_EQUALS: result := 'Keypad Equals';

    // Arrows + Home/End pad
    vk_UP: result := 'Up';
    vk_DOWN: result := 'Down';
    vk_RIGHT: result := 'Right';
    vk_LEFT: result := 'Left';
    vk_INSERT: result := 'Insert';
    vk_HOME: result := 'Home';
    vk_END: result := 'End';
    vk_PAGEUP: result := 'Page Up';
    vk_PAGEDOWN: result := 'Page Down';

    // Function keys
    vk_F1: result := 'F1';
    vk_F2: result := 'F2';
    vk_F3: result := 'F3';
    vk_F4: result := 'F4';
    vk_F5: result := 'F5';
    vk_F6: result := 'F6';
    vk_F7: result := 'F7';
    vk_F8: result := 'F8';
    vk_F9: result := 'F9';
    vk_F10: result := 'F10';
    vk_F11: result := 'F11';
    vk_F12: result := 'F12';
    vk_F13: result := 'F13';
    vk_F14: result := 'F14';
    vk_F15: result := 'F15';

    // Key state modifier keys
    vk_NUMLOCK: result := 'Numlock';
    vk_CAPSLOCK: result := 'Caps lock';
    vk_SCROLLOCK: result := 'Scroll Lock';
    vk_RSHIFT: result := 'Right Shift';
    vk_LSHIFT: result := 'Left Shift';
    vk_RCTRL: result := 'Right Ctrl';
    vk_LCTRL: result := 'Left Ctrl';
    vk_RALT: result := 'Right Alt';
    vk_LALT: result := 'Left Alt';
    vk_RMETA: result := 'Right Meta';
    vk_LMETA: result := 'Left Meta';
    vk_LSUPER: result := 'Left Super';
    vk_RSUPER: result := 'Right Super';
    vk_MODE: result := 'Mode';
    vk_COMPOSE: result := 'Compose';
    // Miscellaneous function keys
    vk_HELP: result := 'Help';
    vk_PRINT: result := 'Print';
    vk_SYSREQ: result := 'Sys Req';
    vk_BREAK: result := 'Break';
    vk_MENU: result := 'Menu';
    vk_POWER: result := 'Power';
    vk_EURO: result := 'Euro';
  end;
  
end;

end.
