from ctypes import *
from math import sqrt, cos, sin

options = {'debug':False}

sgTrue = c_int(-1)
sgFalse = c_int(0)

#-- thoughts --
# need sgsdk.
#  - SetSpritePos(x,y) # two in one



#------------------------------------------------------------------------------
# simple array types
#------------------------------------------------------------------------------

##Colour = c_byte * 4 # RGBA or c_unit32
#class Color(Structure):
#    _fields_ = ('b',c_byte), ('g',c_byte),('r',c_byte),('a',c_byte)
#    
##Color = c_uint32
#Colour = Color       
#------------------------------------------------------------------------------
# enumeration types
#------------------------------------------------------------------------------

CollisionDetectionRange = c_int 
(cdrEquals, cdrGreaterThan, cdrLessThan) = map(c_int,xrange(3))

CollisionSide = c_int 
(csTop, csBottom, csLeft, csRight, 
 csTopLeft, csTopRight, csBottomLeft, csBottomRight, 
 csNone) = map(c_int, xrange(9))

EventKind = c_int #mappy event 0..23
# can't be bother enumerating all these...

FontAlignment = c_int 
(faLeft, faCenter, faRight) = map(c_int,(1,2,4))

FontStyle = c_int #
(fsNormal, fsBold, fsItalic, fsUnderline) = map(c_int, xrange(4))

MouseButton = c_int # 
(mbLeft, mbMiddle, mbRight) = map(c_int,xrange(3))

ResourceKind = c_int #
(rkFont, rkImage, rkSound, rkMap) = map(c_int, xrange(4))

SpriteEndingAction = c_int # 
(seaLoop, seaReverseLoop, seaReverseOnce, seaStop) = map(c_int, xrange(4))

SpriteKind = c_int # 
(skStatic, skAnimArray, skAnimMulti) = map(c_int, xrange(3))

#------------------------------------------------------------------------------
# Record to Structure types
#------------------------------------------------------------------------------

class Point2D(Structure): # record; x,y single
    _fields_ = ('x',c_float), ('y',c_float)
    
class Rectangle(Structure): # record; x,y (single), width,heigh(integer)
    _fields = [('x', c_int), ('y', c_int), 
               ('width', c_int), ('height', c_int)]
               
class Vector(Structure):  #record x,y,w: single
    _fields_ = ('x',c_float), ('y',c_float), ('w',c_float)
     
    
class LineSegment(Structure):
    _fields_ = ("startPoint", Point2D), ("endPoint", Point2D)


class Tile(Structure):
    _fields_ = ('xIndex', c_int), ('yIndex', c_int), ('topCorner', Point2D), ('pointA', Point2D), ('pointB', Point2D), ('pointC', Point2D), ('pointD', Point2D)

#------------------------------------------------------------------------------
# Vector Methods (not in sgsdk.. atm)

def VectorMultiply(v, s):
    # s is a scalar
    return Vector(v.x*s, v.y*s, v.w*s )

def VectorAdd(v1,v2):
    return Vector(v1.x+v2.x, v1.y+v2.y, v1.w+v2.w)

def VectorMagnitude(v):
    return sqrt(v.x**2 + v.y**2)

def VectorOfAngle(angle, magnitude):
    return Vector(magnitude * cos(angle), magnitude * sin(angle), 1)

#------------------------------------------------------------------------------
# Mapping of Pascal Types (dll types) to C# Types
#------------------------------------------------------------------------------

sgsdk_cs_mods = {
    'out': '[Out] out ',
    'var': 'ref ',
    '': ''
}

# define a dictionary mapping between swingame types and output types
sgsdk_types = {
    'Bitmap': 'Bitmap', # ^BitmapData (pointer)
    'BitmapPtr': 'IntPtr',
    'Byte': 'byte',
    'CollisionDetectionRange': 'CollisionDetectionRange', #enum
    'CollisionSide': 'CollisionSide', #enum
    'Colour': 'uint', # RGBA c_byte*4
    'Color': 'uint', # same as Colour
    'Event': 'EventKind', 
    'Font': 'IntPtr', # PTTF_Font
    'FontAlignment': 'FontAlignment', #enum (specified values)
    'FontStyle': 'FontStyle', #enum
    #'Integer': 'int',
    #'integer': 'int',
    'LongInt': 'int',
    'IntPtr': 'IntPtr', # ^Integer (pointer)
    'LineSegment': 'LineSegment', # record
    'LineSegPtr': 'IntPtr',
    'Map': 'IntPtr', # ^MapRecord (pointer)
    'Matrix2DPtr': 'IntPtr',
    'MouseButton': 'MouseButton', #enum
    'Music': 'IntPtr', # PMix_Music
    'PChar': '[MarshalAs(UnmanagedType.LPStr)]string', # null terminated string
    'Point2D': 'Point2D', # record
    'Pointer': 'IntPtr',
    'ResourceKind': 'ResourceKind', #enum
    'Rectangle': 'Rectangle', # record
    'SoundEffect': 'IntPtr', #PMix_Chunk               
    'SpriteEndingAction': 'SpriteEndingAction', #enum
    'Sprite': 'IntPtr', # ^SpriteData (pointer)
    'SpriteKind': 'SpriteKind', #enum
    'Single': 'float',
    'Timer': 'IntPtr', # ^TimerData (pointer)
    'TSDL_Color': 'uint',
    'UInt16': 'c_uint16',
    'UInt32': 'uint',
    'Vector': 'Vector', #record
    'Point2DPtr': 'IntPtr', #pointer to Point2D data
    'Tile': 'Tile', #record
    'None': 'void'
 }
 
sgsdk_special_types = {
     'o': { 'PChar': '[MarshalAs(UnmanagedType.LPStr), Out] StringBuffer'},
     '_': sgsdk_types
 }
 

class _Keys(object):
    def __init__(self): 
        self.BACK = 8 # SDLK_BACKSPACE;
        self.TAB = 9 # SDLK_TAB;
        self.CLEAR = 12 # SDLK_CLEAR;
        self.RETURN = 13 # SDLK_RETURN;
        self.SHIFT = 304 # SDLK_LSHIFT;
        self.CONTROL = 306 # SDLK_LCTRL;
        self.MENU = 319 # SDLK_MENU;
        self.ALT = 308 # SDLK_LALT;
        self.PAUSE = 19 # SDLK_PAUSE;
        self.CAPITAL = 301 # SDLK_CAPSLOCK;
        self.ESCAPE = 27 # SDLK_ESCAPE;
        self.SPACE = 32 # SDLK_SPACE;
        self.PAGE_UP = 280 # SDLK_PAGEUP;
        self.PAGE_DOWN = 281 # SDLK_PAGEDOWN;
        self.END = 279 # SDLK_END;
        self.HOME = 278 # SDLK_HOME;
        self.LEFT = 276 # SDLK_LEFT;
        self.UP = 273 # SDLK_UP;
        self.RIGHT = 275 # SDLK_RIGHT;
        self.DOWN = 274 # SDLK_DOWN;
        self.PRINT = 316 # SDLK_PRINT;
        self.INSERT = 277 # SDLK_INSERT;
        self.DELETE = 127 # SDLK_DELETE;
        self.HELP = 315 # SDLK_HELP;
        self._0 = 48 # SDLK_0; 
        self._1 = 49 # SDLK_1; 
        self._2 = 50 # SDLK_2; 
        self._3 = 51 # SDLK_3; 
        self._4 = 52 # SDLK_4; 
        self._5 = 53 # SDLK_5; 
        self._6 = 54 # SDLK_6; 
        self._7 = 55 # SDLK_7; 
        self._8 = 56 # SDLK_8; 
        self._9 = 57 # SDLK_9; 
        self.A = 97 # SDLK_A; 
        self.B = 98 # SDLK_B;
        self.C = 99 # SDLK_C;
        self.D = 100 # SDLK_D;
        self.E = 101 # SDLK_E;
        self.F = 102  # SDLK_F;
        self.G = 103 # SDLK_G;
        self.H = 104 # SDLK_H;
        self.I = 105 # SDLK_I;
        self.J = 106 # SDLK_J;
        self.K = 107 # SDLK_K;
        self.L = 108 # SDLK_L;
        self.M = 109 # SDLK_M;
        self.N = 110 # SDLK_N;
        self.O = 111 # SDLK_O;
        self.P = 112 # SDLK_P;
        self.Q = 113 # SDLK_Q;
        self.R = 114 # SDLK_R;
        self.S = 115 # SDLK_S;
        self.T = 116 # SDLK_T;
        self.U = 117 # SDLK_U;
        self.V = 118 # SDLK_V;
        self.W = 119 # SDLK_W;
        self.X = 120 # SDLK_X;
        self.Y = 121 # SDLK_Y;
        self.Z = 122 # SDLK_Z;
        self.LWIN = 311 # SDLK_LSUPER;
        self.RWIN = 312 # SDLK_RSUPER;
        self.APPS = 319 # SDLK_MENU;
        self.SLEEP = 320 # SDLK_POWER;
        self.NUMPAD0 = 256 # SDLK_KP0;
        self.NUMPAD1 = 257 # SDLK_KP1;
        self.NUMPAD2 = 258 # SDLK_KP2;
        self.NUMPAD3 = 259 # SDLK_KP3;
        self.NUMPAD4 = 260 # SDLK_KP4;
        self.NUMPAD5 = 261 # SDLK_KP5;
        self.NUMPAD6 = 262 # SDLK_KP6;
        self.NUMPAD7 = 263 # SDLK_KP7;
        self.NUMPAD8 = 264 # SDLK_KP8;
        self.NUMPAD9 = 265 #SDLK_KP9;
        self.MULTIPLY = 268 # SDLK_KP_MULTIPLY;
        self.ADD = 270 # SDLK_KP_PLUS;
        self.SUBTRACT = 269 # SDLK_MINUS;
        self.DECIMAL = 266 # SDLK_KP_PERIOD;
        self.PERIOD = 46 # SDLK_PERIOD;
        self.DIVIDE = 267 # SDLK_KP_DIVIDE;
        self.F1 = 282 # SDLK_F1; 
        self.F2 = 283 # SDLK_F2; 
        self.F3 = 284 # SDLK_F3;  
        self.F4 = 285 # SDLK_F4; 
        self.F5 = 286 # SDLK_F5; 
        self.F6 = 287 # SDLK_F6; 
        self.F7 = 288 # SDLK_F7; 
        self.F8 = 289 # SDLK_F8; 
        self.F9 = 290 # SDLK_F9; 
        self.F10 = 291 # SDLK_F10; 
        self.F11 = 292 # SDLK_F11; 
        self.F12 = 293 # SDLK_F12; 
        self.F13 = 294 # SDLK_F13; 
        self.F14 = 295 # SDLK_F14; 
        self.F15 = 296 # SDLK_F15; 
        self.NUMLOCK = 300 # SDLK_NUMLOCK; 
        self.SCROLL = 302 # SDLK_SCROLLOCK; 
        self.LSHIFT = 304 # SDLK_LSHIFT; 
        self.RSHIFT = 303 # SDLK_RSHIFT; 
        self.LCONTROL = 306 # SDLK_LCTRL;
        self.RCONTROL = 305 # SDLK_RCTRL;
        self.LMENU = 310 # SDLK_LMETA; 
        self.LALT = 308 # SDLK_LALT; 
        self.RMENU = 309 # SDLK_RMETA;
        self.RALT = 307 # SDLK_RALT; 
        self.EQUALS = 61 # SDLK_EQUALS; 
        self.COLON = 58 # SDLK_COLON;
        self.SEMICOLON = 59 # SDLK_SEMICOLON;
        self.LESS = 60 # SDLK_LESS;
        self.GREATER = 62 # SDLK_GREATER;
        self.QUESTION = 63#  SDLK_QUESTION;
        self.AT = 64 # SDLK_AT;
        self.COMMA = 44 # SDLK_COMMA;

Keys = _Keys() # easy singleton

