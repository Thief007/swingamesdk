#!/usr/bin/env python
# encoding: utf-8
"""
create_csharp_lib.py

Created by Andrew Cain on 2009-06-02.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import logging
import sys

from sg import parser_runner, wrapper_helper
from sg.sg_cache import logger, find_or_add_file, find_or_add_type, find_or_add_class
from sg.print_writer import PrintWriter
from sg.file_writer import FileWriter
from sg.sg_type import SGType
from sg.sg_parameter import SGParameter

_out_path="../../Templates/CSharp/Library"

#templates for adapter
_header = ''
_method_wrapper = ''

#templates for modules
_module_method = ''
_module_header = ''
_module_footer = ''

_class_header = ''
_class_footer = ''

_array_property = ''

_property_class = ''
_property_class_property = ''
_property_class_field = ''
_property_class_indexer = ''

_pointer_wrapper_class_header = ''

_type_switcher = {
    None : {    
        #Pascal type: what it maps to
        
        #Primitives
        'single': 'float %s',
        'longint': 'int %s',
        'string': 'string %s',
        'boolean': 'bool %s',
        'byte': 'byte %s',
        'color': 'Color %s',
        
        #Resources
        'animationtemplate':    'AnimationTemplate %s',
        'animation':            'Animation %s',
        'character':            'Character %s',
        'soundeffect':          'SoundEffect %s',
        'music':                'Music %s',
        'shapeprototype':       'ShapePrototype %s',
        'timer':                'Timer %s',
        'shape':                'Shape %s',
        'sprite':               'Sprite %s',
        
        'resourcekind': 'ResourceKind %s',
        'uint32': 'uint %s',
        'bitmap': 'Bitmap %s',
        'pointer': 'IntPtr %s',
        'single[0..2][0..2]': 'float %s[3][3]',
        # '^bitmapdata': 'BitmapData *%s',
        # '^spritedata': 'SpriteData *%s',
        # '^timerdata': 'TimerData *%s',
        'psdl_surface': 'IntPtr %s',
        'boolean[0..n - 1][0..n - 1]': 'bool[][] %s',
        'bitmap[0..n - 1]': 'Bitmap[] %s',
        'spritekind': 'SpriteKind %s',
        'longint[0..n - 1]': 'int[] %s',
        'vector': 'Vector %s',
        'spriteendingaction': 'SpriteEndingAction %s',
        'point2d': 'Point2D %s',
        'point2d[0..2]': 'Point2D %s[3]',
        'point2d[0..n - 1]': 'Point2D[] %s',
        'linesegment': 'LineSegment %s',
        
        'rectangle': 'Rectangle %s',
        'triangle': 'Triangle %s',
        'linesarray': 'LineSegment[] %s',
        'font': 'Font %s',
        'uint16': 'ushort %s',
        'keycode': 'KeyCode %s',
        'collisionside': 'CollisionSide %s',
        'longint[0..n - 1][0..n - 1]': 'int[][] %s',
        'mapdata': 'MapData %s',
        'animationdata[0..n - 1]': 'AnimationData[] %s',
        'layerdata[0..n - 1]': 'LayerData[] %s',
        'collisiondata': 'CollisionData %s',
        'maptagdetails[0..n - 1][0..23]': 'MapTagDetails[][24] %s',
        'map': 'Map %s',
        'maptag': 'MapTag %s',
        'maptile': 'MapTile %s',
        'circle': 'Circle %s',
        'point2darray': 'Point2D[] %s',
        
        #Enums
        'shapekind': 'ShapeKind %s',
        'collisiontestkind': 'CollisionTestKind %s',
        'fontalignment': 'FontAlignment %s',
        'fontstyle': 'FontStyle %s',
        'mousebutton': 'MouseButton %s',
        
        #Functions
        'freenotifier': 'FreeNotifier %s',
    },
    'const' : {
        #Data
        'point2d':      'Point2D %s',
        'linesegment':  'LineSegment %s',
        'rectangle':    'Rectangle %s',
        'matrix2d':     'Matrix2D %s',
        'vector':       'Vector %s',
        'circle':       'Circle %s',
        'bitmapcell':   'BitmapCell %s',
        
        #Arrays
        'linesarray':   'LineSegment[] %s',
        'triangle':     'Triangle %s',
        'bitmaparray':  'Bitmap[] %s',
        'longintarray': 'int[] %s',
        'point2darray': 'Point2D[] %s',
        'stringarray':  'string[] %s',
        'trianglearray':'Triangle[] %s',

    },
    'var' : {
        'animation':            'Animation %s',
        'animationtemplate':    'AnimationTemplate %s',
        'character':            'Character %s',
        'soundeffect':          'SoundEffect %s',
        'music':                'Music %s',
        'timer':                'Timer %s',
        'shapeprototype':       'ShapePrototype %s',
        'shape':                'Shape %s',
        'font':                 'Font %s',
        'bitmap':               'Bitmap %s',
        'sprite':               'Sprite %s',
        'map':                  'Map %s',
        
        'string':       'ref string %s',
        'single':       'ref float %s',
        'rectangle':    'ref Rectangle %s',
        
        'triangle': 'Triangle %s',
        'matrix2d': 'Matrix2D %s',
        
        'linesarray': 'LineSegment[] %s',
        'point2darray': 'Point2D[] %s',
    },
    'out' : {
        'string':       'out string %s',
        'byte':         'out byte %s',
        'color':        'out Color %s',
        'timer':        'out Timer %s',
        'point2d':      'out Point2D %s',
        'longint':      'out int %s',
        'single':       'out float %s',
        'linesegment':  'out LineSegment %s',
        'linesarray':   'out LineSegment[] %s',
        'matrix2d':     'out Matrix2D %s',
        'point2darray': 'out Point2D[] %s',
        'triangle':     'out Triangle %s',
    },
    'return' : {
        None: 'void %s',
        'boolean': 'bool %s',
        'single': 'float %s',
        'longint': 'int %s',
        'byte': 'byte %s',
        'color': 'System.Drawing.Color %s',
        'uint32': 'uint %s',
        'string': 'String %s',
        
        #Data
        'point2d':          'Point2D %s',
        'vector':           'Vector %s',
        'circle':           'Circle %s',
        'rectangle':        'Rectangle %s',
        'linesegment':      'LineSegment %s',
        'matrix2d':         'Matrix2D %s',
        'bitmapcell':       'BitmapCell %s',
        'directionangles':  'DirectionAngles %s',
        
        #Resources
        'music':        'Music %s',
        'soundeffect':  'SoundEffect %s',
        'timer':        'Timer %s',
        'bitmap':       'Bitmap %s',
        'shapeprototype': 'ShapePrototype %s',
        'shape':        'Shape %s',
        'font':         'Font %s',
        'map':          'Map %s',
        'sprite':       'Sprite %s',
        'animationtemplate': 'AnimationTemplate %s',
        'animation':    'Animation %s',
        'character':    'Character %s',
        
        #Enum
        'collisionside':        'CollisionSide %s',
        'fontstyle':            'FontStyle %s',
        'maptag':               'MapTag %s',
        'maptile':              'MapTile %s',
        'spriteendingaction':   'SpriteEndingAction %s',
        'spritekind':           'SpriteKind %s',
        'shapekind':            'ShapeKind %s',
        'collisiontestkind':    'CollisionTestKind %s',
        'fontalignment':        'FontAlignment %s',
        
        #Arrays
        'linesarray':   'LineSegment[] %s',
        'point2darray': 'Point2D[] %s',
        'triangle':     'Triangle %s',
        'longintarray': 'int[] %s',
        'trianglearray':'Triangle[] %s',
        'bitmaparray':  'Bitmap[] %s',
        'stringarray':  'string[] %s',
    }
}

_data_switcher = {
    'temp_return' :
    {
        'string':       '%s.ToString()',
        'linesarray':   '%s',
        'matrix2d':     'Utils.MatrixFromArray(%s)',
        'point2darray': '%s',
        'triangle':     'Utils.TriangleFromArray(%s)',
        'longint':      '%s',
        'longintarray': '%s',
        'trianglearray':'Utils.TriangleArrayFrom(%s)',
        'bitmaparray':  'Utils.BitmapArrayFrom(%s)',
        'stringarray':  'Utils.StringArrayFrom(%s)',
    },
    'return_val' : 
    {
        #Pascal type: what values of this type switch to %s = data value
        'boolean': '%s != 0',
        
        'music':                'Music.Create(%s)',
        'soundeffect':          'SoundEffect.Create(%s)',
        'shape':                'Shape.Create(%s)',
        'shapeprototype':       'ShapePrototype.Create(%s)',
        'bitmap':               'Bitmap.Create(%s)',
        'font':                 'Font.Create(%s)',
        'timer':                'Timer.Create(%s)',
        'map':                  'Map.Create(%s)',
        'sprite':               'Sprite.Create(%s)',
        'animation':            'Animation.Create(%s)',
        'animationtemplate':    'AnimationTemplate.Create(%s)',
        'character':            'Character.Create(%s)',
        
        'color': 'System.Drawing.Color.FromArgb(%s)',
        
        'keycode':              '(KeyCode)%s',
        'mousebutton':          '(MouseButton)%s',
        'spriteendingaction':   '(SpriteEndingAction)%s',
        'spritekind':           '(SpriteKind)%s',
        'maptag':               '(MapTag)%s',
        'collisionside':        '(CollisionSide)%s',
        'fontalignment':        '(FontAlignment)%s',
        'fontstyle':            '(FontStyle)%s',
    },
    #Argument with a parameter value
    'arg_val' : 
    {
        #Pascal type: what values of this type switch to %s = data value
        'boolean': '(%s ? 1 : 0)',
        'color': '%s.ToArgb()',
        'keycode': '(int)%s',
        'mousebutton': '(int)%s',
        'spriteendingaction': '(int)%s',
        'maptag': '(int)%s',
        'collisionside': '(int)%s',
        'resourcekind': '(int)%s',
        'fontalignment': '(int)%s',
        'fontstyle': '(int)%s',
        
        'bitmaparray': 'Utils.BitmapArrToIntPtrArr(%s)'
    },
    #Argument with a literal value
    'arg_lit_val' : 
    {
        #Pascal type: what values of this type switch to %s = data value
        'single': '%sf',
        'self.pointer': 'this.Pointer',
        'self.data': 'this._data',
        'self': 'this',
        'true': '1',
        'false': '0',
        'self': 'this',
    },
}

_adapter_type_switcher = {
    None: {
        #Primitives
        'single': 'float %s',
        'longint': 'int %s',
        'string': 'string %s',
        'boolean': 'int %s',
        'byte': 'byte %s',
        'color': 'int %s',
        
        #Resources
        'soundeffect':          'IntPtr %s',
        'music':                'IntPtr %s',
        'shapeprototype':       'IntPtr %s',
        'timer':                'IntPtr %s',
        'shape':                'IntPtr %s',
        'animation':            'IntPtr %s',
        'animationtemplate':    'IntPtr %s',
        'character':            'IntPtr %s',
        'sprite':               'IntPtr %s',
        'bitmap':               'IntPtr %s',
        
        'uint32': 'uint %s',
        'rectangle': 'Rectangle %s',
        'linesegment': 'LineSegment %s',
        'triangle': 'Point2D[] %s',
        'point2d': 'Point2D %s',
        'linesarray': 'LineSegment[] %s',
        'font': 'IntPtr %s',
        'fontalignment': 'int %s',
        'fontstyle': 'int %s',
        'mousebutton': 'int %s',
        'uint16': 'ushort %s',
        'vector': 'Vector %s',
        'spriteendingaction': 'int %s',
        'keycode': 'int %s',
        'matrix2d': 'Matrix2D %s',
        'collisionside': 'int %s',
        'map': 'IntPtr %s',
        'maptag': 'int %s',
        'maptile': 'MapTile %s',
        'circle': 'Circle %s',
        
        #Arrays
        'point2darray': 'Point2D[] %s',
        'longintarray': 'int[] %s',
        
        #Enums
        'collisiontestkind':    'CollisionTestKind %s',
        'resourcekind': 'int %s',
        'spritekind': 'SpriteKind %s',
        'shapekind': 'ShapeKind %s',
        
        #Functions
        'freenotifier': 'FreeNotifier %s',
    },
    #No modifier and for the actual library call
    'lib_': {
        #primitive
        'single': 'float %s',
        'longint': 'int %s',
        'string': '[MarshalAs(UnmanagedType.LPStr)] string %s',
        'boolean': 'int %s',
        'byte': 'byte %s',
        'color': 'int %s',
        
        #Resources
        'soundeffect':          'IntPtr %s',
        'music':                'IntPtr %s',
        'shapeprototype':       'IntPtr %s',
        'timer':                'IntPtr %s',
        'shape':                'IntPtr %s',
        'animation':            'IntPtr %s',
        'animationtemplate':    'IntPtr %s',
        'character':            'IntPtr %s',
        'sprite':               'IntPtr %s',
        
        'resourcekind': 'int %s',
        'uint32': 'uint %s',
        'bitmap': 'IntPtr %s',
        'rectangle': 'Rectangle %s',
        'linesegment': 'LineSegment %s',
        'triangle': '[MarshalAs(UnmanagedType.LPArray, SizeConst=3)] Point2D[] %s',
        'point2d': 'Point2D %s',
        'linesarray': '[MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s)] LineSegment[] %s',
        'font': 'IntPtr %s',
        'fontalignment': 'int %s',
        'fontstyle': 'int %s',
        'mousebutton': 'int %s',
        'uint16': 'ushort %s',
        'vector': 'Vector %s',
        'spriteendingaction': 'int %s',
        'keycode': 'int %s',
        'matrix2d': 'Matrix2D %s',
        'collisionside': 'int %s',
        'map': 'IntPtr %s',
        'maptag': 'int %s',
        'maptile': 'MapTile %s',
        'circle': 'Circle %s',
        'point2darray': '[MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s)] Point2D[] %s',
        'longintarray': '[MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s)] int[] %s',
        
        #Enums
        'collisiontestkind':    'CollisionTestKind %s',        
        'spritekind':           'SpriteKind %s',
        'shapekind':            'ShapeKind %s',
        
        #Functions
        'freenotifier': 'FreeNotifier %s',
    },
    'const' : {
        'point2d':      'ref Point2D %s',
        'linesegment':  'ref LineSegment %s',
        'rectangle':    'ref Rectangle %s',
        'circle':       'ref Circle %s',
        'bitmapcell':   'ref BitmapCell %s',
        
        # Arrays are passed by reference already
        'matrix2d':     'float[,] %s',
        'triangle':     'Point2D[] %s',
        'trianglearray':'Point2D[] %s',
        'vector':       'ref Vector %s',
        'linesarray':   'LineSegment[] %s',
        'longintarray': 'int[] %s',
        'bitmaparray':  'IntPtr[] %s',
        'point2darray': 'Point2D[] %s',
        
        'stringarray':  'String[] %s',
    },
    'lib_const' : {
        'point2d':      'ref Point2D %s',
        'linesegment':  'ref LineSegment %s',
        'rectangle':    'ref Rectangle %s',
        'circle':       'ref Circle %s',
        'vector':       'ref Vector %s',
        'bitmapcell':   'ref BitmapCell %s',
        
        # Arrays are passed by reference already
        'matrix2d':     '[MarshalAs(UnmanagedType.LPArray, SizeConst=9), In] float[,] %s',
        'triangle':     '[MarshalAs(UnmanagedType.LPArray, SizeConst=3), In] Point2D[] %s',
        'trianglearray':'[MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s), In] Point2D[] %s',
        'linesarray':   '[MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s), In] LineSegment[] %s',
        'longintarray': '[MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s), In] int[] %s',
        'bitmaparray':  '[MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s), In] IntPtr[] %s',
        'point2darray': '[MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s), In] Point2D[] %s',
        
        'stringarray':  '[MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s), In] String[] %s',
    },
    'var': {
        'animation':            'ref IntPtr %s',
        'animationtemplate':    'ref IntPtr %s',
        'soundeffect':          'ref IntPtr %s',
        'music':                'ref IntPtr %s',
        'timer':                'ref IntPtr %s',
        'font':                 'ref IntPtr %s',
        'bitmap':               'ref IntPtr %s',
        'sprite':               'ref IntPtr %s',
        'map':                  'ref IntPtr %s',
        'shapeprototype':       'ref IntPtr %s',
        'shape':                'ref IntPtr %s',
        'character':            'ref IntPtr %s',
        
        'string':               'StringBuilder %s',
        
        'single':               'ref float %s',
        'byte':                 'ref byte %s',
        
        'triangle':             'Point2D[] %s',
        'rectangle':            'ref Rectangle %s',
    },
    
    'lib_var': {
        'animation':            'ref IntPtr %s',
        'animationtemplate':    'ref IntPtr %s',
        'character':            'ref IntPtr %s',
        'soundeffect':          'ref IntPtr %s',
        'music':                'ref IntPtr %s',
        'timer':                'ref IntPtr %s',
        'byte':                 'ref byte %s',
        'font':                 'ref IntPtr %s',
        'bitmap':               'ref IntPtr %s',
        'sprite':               'ref IntPtr %s',
        'map':                  'ref IntPtr %s',
        'shapeprototype':       'ref IntPtr %s',
        'shape':                'ref IntPtr %s',
        
        'single':               'ref float %s',
        
        'string':               '[MarshalAs(UnmanagedType.LPStr), In, Out] StringBuilder %s',
        
        'triangle':             '[MarshalAs(UnmanagedType.LPArray, SizeConst=3), In, Out] Point2D[] %s',
        
        'rectangle':            'ref Rectangle %s',
    },
    'out': {
        'string':       'StringBuilder %s',
        'byte':         'out byte %s',
        'color':        'out int %s',
        'single':       'out float %s',
        'point2d':      'out Point2D %s',
        'longint':      'out int %s',
        'linesegment':  'out LineSegment %s',
        # 'linesarray':   'out LineSegment[] %s',
    },
    'lib_out': {
        'string':       '[MarshalAs(UnmanagedType.LPStr), Out] StringBuilder %s',
        'byte':         '[Out] out byte %s',
        'color':        '[Out] out int %s',
        'single':       '[Out] out float %s',
        'point2d':      '[Out] out Point2D %s',
        'longint':      '[Out] out int %s',
        'linesegment':  '[Out] out LineSegment %s',
    },
    'result': {
        'string':           'StringBuilder %s',
        'stringarray':      'StringBuilder[] %s',
        'linesarray':       'LineSegment[] %s',
        'matrix2d':         'float[,] %s',
        'point2darray':     'Point2D[] %s',
        'triangle':         'Point2D[] %s',
        'trianglearray':    'Point2D[] %s',
        'longintarray':     'int[] %s',
        'bitmaparray':      'IntPtr[] %s',
    },
    'lib_result': {
        'string':       '[MarshalAs(UnmanagedType.LPStr), Out] StringBuilder %s',
        'stringarray':  '[MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s), Out] StringBuilder[] %s',
        
        'linesarray':   '[MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s), Out] LineSegment[] %s',
        'matrix2d':     '[MarshalAs(UnmanagedType.LPArray, SizeConst=9), Out] float[,] %s',
        'point2darray': '[MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s), Out] Point2D[] %s',
        'triangle':     '[MarshalAs(UnmanagedType.LPArray, SizeConst=3), Out] Point2D[] %s',
        'longintarray': '[MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s), Out] int[] %s',
        'trianglearray':'[MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s), Out] Point2D[] %s',
        
        'bitmaparray':  '[MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s), Out] IntPtr[] %s',
    },
    'return' : {
        None: 'void %s',
        'boolean': 'int %s',
        'single': 'float %s',
        'point2d': 'Point2D %s',
        'longint': 'int %s',
        
        #Resources
        'animation':            'IntPtr %s',
        'animationtemplate':    'IntPtr %s',
        'music':                'IntPtr %s',
        'soundeffect':          'IntPtr %s',
        'shapeprototype':       'IntPtr %s',
        'shape':                'IntPtr %s',
        'timer':                'IntPtr %s',
        'bitmap':               'IntPtr %s',
        'font':                 'IntPtr %s',
        'map':                  'IntPtr %s',
        'sprite':               'IntPtr %s',
        'character':            'IntPtr %s',
        
        'maptile':              'MapTile %s',
        
        'byte':     'byte %s',
        'color':    'int %s',
        'uint32':   'uint %s',
        
        # Structs
        'bitmapcell':       'BitmapCell %s',
        'vector':           'Vector %s',
        'circle':           'Circle %s',
        'rectangle':        'Rectangle %s',
        'linesegment':      'LineSegment %s',
        'directionangles':  'DirectionAngles %s',
        
        # Enums
        'fontalignment':        'FontAlignment %s',
        'spriteendingaction':   'int %s',
        'spritekind':           'int %s',
        'shapekind':            'ShapeKind %s',
        'collisiontestkind':    'CollisionTestKind %s',
        'collisionside':        'int %s',
        'fontstyle':            'int %s',
        'maptag':               'int %s',
    }
}

# mapping for local variables
_local_type_switcher = {
    'string':       'StringBuilder %s = new StringBuilder(2048);',
    'color':        'int %s;',
    'matrix2d':     'float[,] %s = new float[3,3];',
    'triangle':     'Point2D[] %s = new Point2D[3];',
    'linesarray':   'LineSegment[] %s;',
    'longintarray': 'int[] %s;',
    'bitmaparray' : 'IntPtr[] %s;',
    'point2darray': 'Point2D[] %s;',
    'longint':      'int %s;',
    'trianglearray':'Point2D[] %s;',
    'stringarray':  'string[] %s;',
}

# mapping for struct fields
_struct_type_switcher = {
    #Resource types
    'sprite': 'internal Sprite _%s',
    'character': 'internal Character _%s',
    
    #Pascal type: what it maps to
    'single': 'internal float _%s',
    'longint': 'internal int _%s',
    'soundeffect': 'internal SoundEffect _%s',
    'music': 'internal Music _%s',
    'string': 'internal string _%s',
    'boolean': 'internal bool _%s',
    'byte': 'internal byte _%s',
    'timer': 'internal Timer _%s',
    'color': 'internal Color _%s',
    'resourcekind': 'internal ResourceKind _%s',
    'uint32': 'internal uint _%s',
    'bitmap': 'internal Bitmap _%s',
    'single[0..2][0..2]': '[ MarshalAs( UnmanagedType.ByValArray, SizeConst=9 )]\ninternal float[,] _%s',
    'vector': 'internal Vector _%s',
    'point2d': 'internal Point2D _%s',
    'point2d[0..2]': '[ MarshalAs( UnmanagedType.ByValArray, SizeConst=3 )]\ninternal Point2D[] _%s',
    'linesegment': 'internal LineSegment _%s',

    'rectangle': 'internal Rectangle _%s',
    'linesarray': 'internal LineSegment[] _%s',
    'font': 'internal Font _%s',
    'fontalignment': 'internal FontAlignment _%s',
    'fontstyle': 'internal FontStyle _%s',
    'mousebutton': 'internal MouseButton _%s',
    'uint16': 'internal ushort _%s',
    'keycode': 'internal KeyCode _%s',
    'collisionside': 'internal CollisionSide _%s',
    'longint[0..n - 1]': 'internal int[] _%s',
    'longint[0..n - 1][0..n - 1]': 'internal int[][] _%s',
    'mapdata': 'internal MapData _%s',
    'animationdata[0..n - 1]': 'internal AnimationData[] _%s',
    'layerdata[0..n - 1]': 'internal LayerData[] _%s',
    'collisiondata': 'internal CollisionData _%s',
    'map': 'internal Map _%s',
    'maptag': 'internal MapTag _%s',
    'maptile': 'internal MapTile _%s',
    'circle': 'internal Circle _%s',
    'point2darray': 'internal Point2D[] _%s',
}


_names = []

def load_data():
    global _header, _method_wrapper
    global _module_method, _module_header, _module_footer
    global _class_header, _class_footer
    global _pointer_wrapper_class_header
    global _array_property
    global _property_class, _property_class_property, _property_class_field
    global _property_class_indexer
    global _struct_property
    
    f = open('./cs_lib/lib_header.txt')
    _header = f.read()
    f.close()
    
    f = open('./cs_lib/lib_method_wrap.txt')
    _method_wrapper = f.read()
    f.close()
    
    f = open('./cs_lib/module_method.txt')
    _module_method = f.read()
    f.close()
    
    f = open('./cs_lib/module_header.txt')
    _module_header = f.read()
    f.close()
    
    f = open('./cs_lib/module_footer.txt')
    _module_footer = f.read()
    f.close()
    
    f = open('./cs_lib/class_header.txt')
    _class_header = f.read()
    f.close()
    
    f = open('./cs_lib/class_footer.txt')
    _class_footer = f.read()
    f.close()
    
    f = open('./cs_lib/array_property.txt')
    _array_property = f.read()
    f.close()
    
    f = open('./cs_lib/pointer_wrapper_class_header.txt')
    _pointer_wrapper_class_header = f.read()
    f.close()
    
    f = open('./cs_lib/property_class.txt')
    _property_class = f.read()
    f.close()
    
    f = open('./cs_lib/property_class_property.txt')
    _property_class_property = f.read()
    f.close()
    
    f = open('./cs_lib/property_class_field.txt')
    _property_class_field = f.read()
    f.close()
    
    f = open('./cs_lib/property_class_indexer.txt')
    _property_class_indexer = f.read()
    f.close()
    
    f = open('./cs_lib/struct_property.txt')
    _struct_property = f.read()
    f.close()
    

def doc_transform(the_docs):
    docLines = the_docs.splitlines(True)
    return ''.join([line if line == docLines[0] else '/// ' + line for line in docLines])

def arg_visitor(arg_str, the_arg, for_param):
    '''Called for each argument in a call, performs required mappings. the_arg has the argument, for_param has
    the parameter being mapped to'''
    
    if not isinstance(for_param, SGParameter):
        print arg_str, the_arg, for_param
        assert False
    
    if isinstance(the_arg, SGParameter): #uses parameter as value
        data_key = 'arg_val'
    else:
        data_key = 'arg_lit_val'
    
    the_type = for_param.data_type
    
    #check for pointer wrapper param
    if the_type.pointer_wrapper and not '.pointer' in arg_str.lower():
        if for_param.modifier != 'out' and for_param.modifier != 'var':
            arg_str = '(' + arg_str + ' == null ? IntPtr.Zero : ' + arg_str + '.Pointer)'
        else:
            arg_str = arg_str + '.Pointer'
    
    # Change True to true for example...
    if for_param.modifier != 'out' and arg_str.lower() in _data_switcher[data_key]:
        data = _data_switcher[data_key][arg_str.lower()] 
        if '%s' in data:
            arg_str = _data_switcher[data_key][arg_str.lower()] % arg_str
        else:
            arg_str = data
    
    if for_param.modifier != 'out' and the_type.name.lower() in _data_switcher[data_key]:
        #convert data using pattern from _data_switcher
        result = _data_switcher[data_key][the_type.name.lower()] % arg_str
    else:
        result = arg_str
    
    #check var/out/const
    if the_type.array_wrapper or the_type.fixed_array_wrapper:
        return result
    elif (for_param.modifier == 'var' or for_param.modifier == 'const'):
        result = 'ref ' + result
    elif (for_param.modifier == 'out') and the_type.name.lower() != "string": #TODO: check for array types...
        result = 'out ' + result
    
    return result

def arg_cs_dll_visitor(arg_str, the_arg, for_param):
    the_type = for_param.data_type
    result = arg_str
    
    #check var/out/const
    if the_type.array_wrapper or the_type.fixed_array_wrapper:
        return result
    if (for_param.modifier == 'var' or for_param.modifier == 'const'): # and not (the_type.array_wrapper or the_type.fixed_array_wrapper):
        result = 'ref ' + result
    elif (for_param.modifier == 'out') and the_type.name.lower() != "string":
        result = 'out ' + result
    
    return result

def adapter_type_visitor(the_type, modifier = None):
    '''switch types for the c SwinGame adapter (links to DLL)'''
    key = the_type.name.lower() if the_type != None else None
    if key not in _adapter_type_switcher[modifier]:
        logger.error('CREATE Cs : Error changing adapter type %s - %s', modifier, the_type)
        assert False
    
    return _adapter_type_switcher[modifier][key]

def adapter_param_visitor(the_param, last):
    return delegated_param_visitor(the_param, last, False)

def lib_adapter_param_visitor(the_param, last):
    return delegated_param_visitor(the_param, last, True)
    
def delegated_param_visitor(the_param, last, was_lib):
    key = the_param.data_type.name.lower()
    modifier = the_param.modifier
    
    if was_lib:
        modifier = "lib_" + (modifier if not modifier == None else "")
    
    if key not in _adapter_type_switcher[modifier]:
        logger.error('CREATE Cs : Error changing adapter parameter %s - %s', modifier, the_param.data_type.name)
        assert(False)
    
    if the_param.data_type.array_wrapper and (not modifier == "result"):
        #print _adapter_type_switcher[modifier][key], modifier, key
        if was_lib and the_param.data_type.array_wrapper:
            #if for a library result, arrays need to know the index of the length parameter
            return '%s%s' % ( _adapter_type_switcher[modifier][key] % (the_param.length_idx, the_param.name), ', ' if not last else '')
        else:
            return '%s%s' % ( _adapter_type_switcher[modifier][key] % (the_param.name), ', ' if not last else '')
    else:
        # map to name + type
        return '%s%s' % ( _adapter_type_switcher[modifier][key] % the_param.name, ', ' if not last else '')


#
# Type conversion for the Classes, Modules, and Structs
#

def type_visitor(the_type, modifier = None):
    '''switch types for the c SwinGame library'''
    key = the_type.name.lower() if the_type != None else None
    
    if modifier == 'result': modifier = 'return'
    
    if key not in _type_switcher[modifier]:
        logger.error('CREATE Cs : Error changing model type %s - %s', modifier, the_type)
    return _type_switcher[modifier][key]

def struct_type_visitor(the_type):
    '''switch types for the fields of a struct'''
    logger.debug('CREATE Cs : Changing model type %s', the_type)
    return _struct_type_switcher[the_type.name.lower()]


def param_visitor(the_param, last):
    return '%s%s' % (
        type_visitor(the_param.data_type, the_param.modifier) % the_param.name,
        ', ' if not last else '')

def create_cs_call(details, the_method):
    """Create a c# call for the passed in details dictionary/method"""
    
    if the_method.is_constructor:
        details['pre_call'] =''
        details['return_type'] = '%s'
        details['returns'] = ''
        details['returns_end'] = '' # ', PtrKind.%s)' % details['in_class']
        details['public'] = 'public '
        details['base_const'] = ': base(%(calls.class)s.%(calls.name)s(%(calls.args)s), PtrKind.%(in_class)s)%(returns_end)s' % details
        result = ''
    elif the_method.is_destructor:
        details['pre_call'] ='' # now done via callback 'PointerWrapper.Remove(this);\n    '
        details['return_type'] = 'void DoFree'
        details['returns_end'] = ''
        details['public'] = 'protected internal override '
        details['base_const'] = ''
        result = '%(calls.class)s.%(calls.name)s(%(calls.args)s)%(returns_end)s' % details
    else:
        if the_method.name in ['WindowCloseRequested', 'CloseAudio']:
            details['pre_call'] = '' # no longer needed 'PointerWrapper.FreeAnythingToFree();\n    '
        elif the_method.mimic_destructor:
            details['pre_call'] = '' # now done via callback 'PointerWrapper.Remove(%s.Pointer);\n    ' % the_method.params[0].name
        else: details['pre_call'] =''
        details['returns_end'] = ''
        details['public'] = 'public '
        details['base_const'] = ''
        result = '%(calls.class)s.%(calls.name)s(%(calls.args)s)%(returns_end)s' % details
        
        if the_method.return_type != None:
            if the_method.return_type.name.lower() in _data_switcher['return_val']:
                result = _data_switcher['return_val'][the_method.return_type.name.lower()] % result
    
    # if the_method.name in ['ProcessEvents']:
    #     details['post_call'] = '\n    PointerWrapper.FreeAnythingToFree();'
    # else: 
    details['post_call'] =''
    
    return ('%(returns)s' + result) % details

def create_cs_dll_call(details, the_method):
    """Create a c# call for the passed in details dictionary/method, this call is from the SGSDK.cs code
    to the native library..."""
    
    details['pre_call'] =''
    details['post_call'] =''
    # details['public'] = 'internal '
    result = ''
    
    return '%(returns)sDLL_%(name)s(%(args)s)' % details

def method_visitor(the_method, other, as_accessor_name = None):
    writer = other['file writer']
    
    if other['lib method']: 
        details = the_method.to_keyed_dict(lib_adapter_param_visitor, other['type visitor'], other['arg visitor'], doc_transform, other['call_creater'])
        #write out the library versions...
        writer.writeln('[DllImport("sgsdk.dll", CallingConvention=CallingConvention.Cdecl, EntryPoint="%s", CharSet=CharSet.Ansi)]' % details['name'])
        writer.write('private static extern %(return_type)s' % details % 'DLL_' + details['name'])
        writer.write('(%(params)s);' % details) 
        writer.writeln('')
        
        #write out the wrapped version...
        details = the_method.to_keyed_dict(other['param visitor'], other['type visitor'], other['arg visitor'], doc_transform, other['call_creater'])
        writer.writeln((_method_wrapper % details).replace('%s', details['name']) )
        writer.writeln('')
    else:
        details = the_method.to_keyed_dict(other['param visitor'], other['type visitor'], other['arg visitor'], doc_transform, other['call_creater'])
        
        if len(the_method.local_vars) > 0:
            temp = '\n'
            temp_process_params = details['pre_call']
            temp_process_result = details['post_call'] + '\n'
            
            #process all local variables
            for local_var in the_method.local_vars:
                # print '----'
                # print local_var.data_type.name, local_var.maps_result
                
                #declare variables
                temp += '    %s\n' % _local_type_switcher[local_var.data_type.name.lower()] % local_var.name
                
                #
                # Check if this local variable needs pre-call data manipulation
                #
                if isinstance(local_var, SGParameter) and local_var.maps_result:
                    #setup the size of a return array
                    if the_method.fixed_result_size > 0:
                        assert local_var.data_type.name in ['LinesArray', 'Point2DArray']
                        temp_process_params += '%s = new %s[%s];\n    ' % (
                                local_var.name,
                                'LineSegment' if local_var.data_type.name == 'LinesArray' else 'Point2D',
                                the_method.fixed_result_size
                            )
                    elif the_method.length_call != None:
                        # print the_method.name, local_var.data_type.name, ' = ', details['length_call']
                        #FIX THIS...
                        assert local_var.data_type.name in ['LongIntArray','Point2DArray','TriangleArray','StringArray','LinesArray','BitmapArray']
                        
                        if local_var.data_type.name == 'StringArray':
                            temp_process_params = '%s = Utils.ResultStringArray(%s);' % (local_var.name, details['length_call'].replace('return ', '') )
                            # also fix local variable type of string arrays that map results :(
                            temp = temp.replace('string', 'StringBuilder')
                        else:
                            temp_process_params = '%s = new %s[%s];\n    ' % (
                                local_var.name,
                                'int' if local_var.data_type.name in ['LongIntArray'] \
                                else 'Point2D' if local_var.data_type.name in ['Point2DArray', 'TriangleArray'] \
                                else 'LineSegment' if local_var.data_type.name in ['LinesArray'] \
                                else 'IntPtr' if local_var.data_type.name in ['BitmapArray'] \
                                else 'StringBuilder',
                                details['length_call'].replace('return ', '') + (' * 3' if local_var.data_type.name in ['TriangleArray'] else '')
                            )
                        
                    # skip initialisation of result
                    continue                
                    
                type_name = local_var.data_type.name.lower()
                
                if type_name == 'string':
                    assert local_var.modifier == 'out' or local_var.maps_result
                    temp_process_result += '\n    %s = %s.ToString();' % (local_var.name[:-5], local_var.name)
                elif type_name == 'color':
                    temp_process_result += '\n    %s = System.Drawing.Color.FromArgb(%s);' % (local_var.name[:-5], local_var.name)
                
                if local_var.is_length_param:
                    temp_process_params += '%s = %s.Length;\n    ' % (
                            local_var.name,
                            local_var.length_of.name if not local_var.length_of.has_field else local_var.length_of.name + '._' + local_var.field_name
                        )
                elif type_name == 'trianglearray' and not local_var.maps_result:
                    temp_process_params += '%s = Utils.TriangleArrToPoint2DArr(%s);\n    ' % (
                            local_var.name,
                            local_var.name[:-5] if not local_var.has_field else local_var.name[:-5] + '._' + local_var.field_name
                        )                    
                elif type_name == 'bitmaparray' and not local_var.maps_result:
                    temp_process_params += '%s = Utils.BitmapArrToIntPtrArr(%s);\n    ' % (
                            local_var.name,
                            local_var.name[:-5] if not local_var.has_field else local_var.name[:-5] + '._' + local_var.field_name
                        )                    
                elif local_var.modifier != 'out':
                    # copy in value
                    temp_process_params += '%s = %s;\n    ' % (
                            local_var.name,
                            local_var.name[:-5] if not local_var.has_field else local_var.name[:-5] + '._' + local_var.field_name
                        )
                
            details['vars'] = temp
            details['post_call'] = temp_process_result
            details['pre_call'] = temp_process_params
        else:
            details['vars'] = ''
        
        #process return types...
        if the_method.method_called.was_function:
            #get the return parameter
            result_param = the_method.method_called.params[-1]
            if not result_param.maps_result: #in case of returning var length array
                result_param = the_method.method_called.params[-2]
            details['post_call'] += '\n    return %s;' % _data_switcher['temp_return'][result_param.data_type.name.lower()] % result_param.name;
            details['the_call'] = details['the_call'][7:] #remove return...
        
        if the_method.name.lower() in ['tostring']:
            details['override'] = 'override '
        else:
            details['override'] = ''
            
        if as_accessor_name != None:
            #change for property support
            details['return_type'] = as_accessor_name
            details['public'] = ''
            details['static'] = ''
            details['params'] = ''
        else:
            details['params'] = '(%s)' % details['params']
        
        if the_method.is_operator:
            details['name'] = details['name'].replace('operator =', 'operator ==').replace('operator <>', 'operator !=')
        #     writer.writeln((_operator_overload % details).replace('%s', details['name']) )
        # else:
        
        #
        # Check parameters 
        #
        for param in the_method.params:
            if param.data_type.pointer_wrapper and param.modifier == 'var':
                details['pre_call'] = 'if (' + param.name + ' == null) return;\n    ' + details['pre_call']
                print 'Checks for null var parameter: ', the_method.name
        
        # Morph fn is a function used to alter the output for this method.
        # It is passed the string output and changes it as required.
        # This is used by properties to alter some details.
        morph_fn = other['morph_method_fn']
        
        if morph_fn == None:
            writer.writeln((_module_method % details).replace('%s', details['name']) )
        else:
            writer.writeln(morph_fn((_module_method % details).replace('%s', details['name']), details))
        
        writer.writeln()
        # if the_method.is_function:
        #     #%(calls.name)s(%(calls.args)s)
        #     details['the_call'] = other['arg visitor']('%(calls.name)s(%(calls.args)s)' % details, the_method.return_type)
        #     other['file writer'].write(_module_c_function % details % the_method.uname)
        # else:
        #     other['file writer'].write(_module_c_method % details)
    
    return other

def _write_wrapped_property(the_property, other):
    '''
    The property is a structure or array type, so write a wrapper to give access
    to elements and as a whole.
    '''
    
    writer = other['file writer']
    
    details = dict()
    details["property_name"] = the_property.name
    details["in_type"] = the_property.in_class.name
    details["struct_type"] = _adapter_type_switcher[None][the_property.data_type.name.lower()] % ''
    details["methods"] = ''
    details["properties"] = ''
    
    writer.writeln(_property_class_property % details)
    writer.writeln(_property_class % details)
    
    writer.indent(2)
    
    if the_property.data_type.is_struct:
        for field in the_property.data_type.fields:
            details["field_name"] = field.pascalName
            details["field_type"] = (_struct_type_switcher[field.data_type.name.lower()] % field.pascalName).replace("_","").replace("internal", "public")
            writer.writeln(_property_class_field % details)
        
        # In the wrapped properties we need to access the internal data before the call or after the set
        
        import re
        get_regex = re.compile('(get(.|\n)*?\{)')
        old_mfn = other['morph_method_fn']
        other['morph_method_fn'] = lambda s, details: \
            get_regex.sub(r'\1\n    this._StructData = %s;' % the_property.name, \
                s.replace('this', 'this._StructData') \
                )
        find_or_add_class(the_property.data_type.name).visit_properties(property_visitor, other)
        other['morph_method_fn'] = old_mfn
    else:
        field = the_property.data_type.fields[0]
        details["field_type"] = (_struct_type_switcher[field.data_type.nested_type.name.lower()] % 'this').replace("_", "").replace("internal", "public")
        #print details["field_type"], the_property.name, the_property.in_class.name
        writer.writeln(_property_class_indexer % details)
    

def property_visitor(the_property, other):
    writer = other['file writer']
    
    type_name = _type_switcher['return'][the_property.data_type.name.lower()]
    
    is_wrapped = the_property.in_class.is_pointer_wrapper and (the_property.data_type.is_struct or the_property.data_type.is_array) and not the_property.is_static and the_property.getter != None and the_property.setter != None
    
    if is_wrapped:
        _write_wrapped_property(the_property, other)
        writer.write('private %s\n{\n' % type_name % the_property.name)
    else:    
        # Write standard property
        writer.write('public %s%s\n{\n' % ('static ' if the_property.is_static else '', type_name) % the_property.name)
    
    writer.indent(2)

    if the_property.getter != None:
        method_visitor(the_property.getter, other, 'get');
    if the_property.setter != None:
        method_visitor(the_property.setter, other, 'set');

    writer.outdent(2)

    writer.write('}\n');
    
    if is_wrapped:
        writer.outdent(2)
        writer.write('}\n')
    
    return other

def write_cs_sgsdk_file(the_file, for_others = False):
    '''Write the c sharp library adapter - header file that matches DLL'''
    file_writer = FileWriter('%s/%s.cs'% (_out_path, the_file.name))
    file_writer.writeln(_header)
    
    #visit the methods of the library
    other = {
        'lib method': True,
        'file writer': file_writer,
        'type visitor': adapter_type_visitor,
        'param visitor': adapter_param_visitor,
        'arg visitor': arg_cs_dll_visitor,
        'call_creater': create_cs_dll_call,
        'morph_method_fn': None,
    }
    
    file_writer.indent(2);
    file_writer.writeln('internal class %s' % the_file.members[0].name);
    file_writer.writeln('{');
    
    #
    # Get the file path set when the library is loaded...
    #
    file_writer.writeln('''    static %s()
    {
        Resources.SetAppPath(System.Reflection.Assembly.GetExecutingAssembly().Location, true);
    }
    ''' % the_file.members[0].name)
    
    file_writer.indent(2);
    the_file.members[0].visit_methods(method_visitor, other)
    file_writer.outdent(2);
    
    file_writer.writeln('}');
    file_writer.outdent(2);
    
    file_writer.write('}')
    file_writer.close()

def write_cs_methods_for_module(member, other):
    '''Write out a single c# member'''
    
    writer = other['file writer']
    
    details = member.to_keyed_dict(doc_transform=doc_transform)
    
    #Support Module style access...
    details['module_attr'] = '' #'\n[Microsoft.VisualBasic.CompilerServices.StandardModuleAttribute()]' if member.is_module else ''
    
    writer.writeln(_class_header % details)
    
    writer.indent(2)
    
    member.visit_methods(method_visitor, other)
    
    writer.outdent(2)
    
    writer.writeln(_class_footer)

def write_sg_class(member, other):
    #if member.name not in ['SoundEffect', 'Music']: return
    
    file_writer = FileWriter('%s/%s.cs'% (_out_path, member.name))
    logger.info('Creating %s/%s.cs'% (_out_path, member.name))
    
    file_writer.writeln(_module_header % { 
        'name' : other['the_file'].name,
        'pascal_name' : member.name + ' from ' + other['the_file'].pascal_name
        })
    
    file_writer.indent(2)
    
    details = member.to_keyed_dict(doc_transform=doc_transform)
    
    file_writer.writeln(_pointer_wrapper_class_header % details);
    
    my_other = other.copy()
    my_other['file writer'] = file_writer
    my_other['arg visitor'] = arg_visitor
    
    file_writer.indent(2);    
    member.visit_methods(method_visitor, my_other)
    member.visit_properties(property_visitor, my_other)
    member.visit_operators(method_visitor, my_other)
    file_writer.outdent(2);
    
    file_writer.writeln(_class_footer)
    file_writer.outdent(2)
    
    file_writer.write(_module_footer)
    file_writer.close()

def write_delegate(member, other):
    '''write out a delegate data type'''
    writer = other['file writer']
    
    method = member.data_type.method
    
    writer.write('/// <summary>\n/// %s\n/// </summary>\n' % doc_transform(member.doc))
    
    details = dict()
    details['return_type'] = adapter_type_visitor(method.return_type, 'return') % member.name
    details['params'] = method.param_string(param_visitor)
    
    writer.write('[UnmanagedFunctionPointerAttribute(CallingConvention.Cdecl)]');
    writer.write('public delegate %(return_type)s(%(params)s);\n' % details)
    

def write_struct(member, other):
    '''Write out s struct data type'''
    writer = other['file writer']
    
    writer.write('/// <summary>\n/// %s\n/// </summary>\n' % doc_transform(member.doc))
    writer.write('[ StructLayout( LayoutKind.Sequential, CharSet=CharSet.Ansi )]\n')
    writer.write('public struct %s\n{\n' % member.name)
    
    writer.indent(2)
    
    for field in member.field_list:
        writer.writeln('%s;' % struct_type_visitor(field.data_type) % field.name)
        
        #hack
        prop_decl = (struct_type_visitor(field.data_type).replace("_","").replace("internal", "public") % field.pascalName)\
            .split('\n')[-1]
        
        writer.writeln(_struct_property % { 'prop_decl': prop_decl,  'field_name': field.name})
    
    if member.wraps_array:
        # add accessor methods
        main_type = member.data_type.related_type
        dim = main_type.dimensions
        details = dict()
        details['type'] = (struct_type_visitor(main_type.nested_type) % 'this').replace("_","").replace("internal", "public")
        details['params'] =  ', '.join(['int idx%s' % i for i,d in enumerate(dim)])
        details['idxs'] = ', '.join(['idx%s' % i for i,d in enumerate(dim)])
        
        writer.writeln(_array_property % details)
    
    if member.data_type.same_as != None:
        filename = './cs_lib/%s_to_%s.txt' % (member.data_type.name.lower(), member.data_type.same_as.name.lower())
        f = open(filename)
        cast_code = f.read()
        f.close()
        
        writer.writeln(cast_code)
        
    member.visit_methods(method_visitor, other)
    member.visit_operators(method_visitor, other)
    member.visit_properties(property_visitor, other)
    
    writer.outdent(2)
    
    writer.writeln('}\n')
    

def write_cs_type_for(member, other):
    '''Write out a single c member'''
    
    if member.via_pointer:
        return
    
    writer = other['file writer']
    
    if member.is_class:
        #convert to resource pointer
        if member.is_pointer_wrapper:
            assert len(member.fields) == 1
            write_sg_class(member, other)
        else:
            logger.error('CREATE Cs : Unknown class type for %s', member.uname)
            assert false
    elif member.is_struct:
        write_struct(member, other)
    elif member.data_type.is_procedure:
        write_delegate(member, other)
    elif member.is_enum:
        #enum id { list }
        writer.write('/// <summary>\n/// %s\n/// </summary>\n' % doc_transform(member.doc))
        writer.write('public enum %s\n{\n' % member.name)
        
        writer.indent(2)
        
        for val in member.values:
            writer.write('%s' % val)
            if val != member.values[-1]: writer.write(',')
            writer.write('\n')
        
        writer.outdent(2)
        
        writer.writeln('}\n')

def write_cs_lib_module(the_file):
    '''Write the header and c file to wrap the attached files detials'''
    file_writer = FileWriter('%s/%s.cs'% (_out_path, the_file.name))
    logger.info('%s/%s.cs'% (_out_path, the_file.name))
    
    file_writer.writeln(_module_header % { 
        'name' : the_file.name,
        'pascal_name' : the_file.pascal_name
        })
    
    other = {
        'file writer': file_writer,
        'type visitor': type_visitor,
        'param visitor': param_visitor,
        'arg visitor': arg_visitor,
        'lib method': False,
        'the_file': the_file,
        'call_creater': create_cs_call,
        'morph_method_fn': None,
        }
    
    file_writer.indent(2)
    
    #process all types first so they appear at the top of the header files
    for member in the_file.members:
        if member.is_module or member.is_header or (member.is_type and not member.data_type.is_procedure):
            pass
        elif member.is_class or member.is_struct or member.is_enum or member.data_type.is_procedure:
            write_cs_type_for(member, other)
        else:
            print member.module_kind, member
            assert False
    
    #process all methods
    for member in the_file.members:
        if member.is_module:
            write_cs_methods_for_module(member, other)
    
    file_writer.outdent(2)
    
    file_writer.write(_module_footer)
    file_writer.close()

def post_parse_process(the_file):
    ''' the c modules also wrap array return values and adds length parameters for arrays.'''
    if the_file.name == 'SGSDK':
        return
    
    logger.info('Post Processing %s for C# wrapper creation', the_file.name)
    
    # Step 1: Rename property parameter names...
    #
    # In C# all properties have an invisible value variable.
    # This must therefore be changed in all properties before other
    # processing occurs. Also note that properties dont really
    # have special methods in the model, so this is method is shared
    # by the associated module code.
    for member in the_file.members:
        for key, prop in member.properties.items():
            if prop.setter != None:
                #HACK: method call details created the called method. so have to change the called method as well
                prop.setter.method_called.get_parameter(prop.setter.params[0].name).name = 'value'
                #change in the class method as well...
                prop.setter.params[0].name = 'value'
    
    for member in the_file.members:
        #process all method of the file
        methods_and_operators = member.methods.items() + member.operators.items()
        
        for key, prop in member.properties.items():
            if prop.getter != None:
                _post_process_method(prop.getter)
            if prop.setter != None:
                _post_process_method(prop.setter)
        
        for key, method in methods_and_operators:
            _post_process_method(method)
        

def _post_process_method(method):
    for param in method.params:
        #
        # Check if local variables are needed to be added for this parameter...
        #
        if param.maps_result or param.data_type.wraps_array or (param.modifier in ['var', 'out'] and param.data_type.name.lower() in ['string','color']):
            
            if method.is_constructor: continue
            
            wrapper_helper.add_local_var_for_param(method, param)
            
            # logger.debug('Create cs : Adding local var of type %s to %s', param.data_type, method.uname)
            # 
            #             local_var = SGParameter(param.name + '_temp')
            #             local_var.data_type = param.data_type
            #             local_var.modifier = param.modifier
            #             local_var.maps_result = param.maps_result
            #             
            #             if param.data_type.wraps_array:
            #                 if param.data_type.is_struct:
            #                     local_var.has_field = True
            #                     local_var.field_name = param.data_type.fields[0].name
            #                 # else:
            #                 #     print 'skipping', param
            #                 #     continue
            #                 
            #             method.local_vars.append(local_var)
            #             param.maps_to_temp = True
    
    if method.method_called.was_function:
        wrapper_helper.add_local_var_for_result(method)
        
        #get the return parameter
        
        # result_param = method.method_called.params[-1]
        #         if not result_param.maps_result: #in case of returning var length array
        #             result_param = method.method_called.params[-2]
        #         
        #         assert result_param.maps_result
        #         
        #         method.local_vars.append(result_param)
        #         method.args.append(result_param)
        
    if method.method_called.has_length_params:
        wrapper_helper.add_length_args(method, '%s.Length')
        # for param in method.method_called.params:
        #     if param.is_length_param:
        #         #possibly need an extra local for this... if out
        #         if param.length_of.maps_result:
        #             # need to indicate the size of the returned array...
        #             if method.fixed_result_size > 0:
        #                 method.args.append(str(method.fixed_result_size))
        #             else:
        #                 method.args.append(param.length_of.name + '.Length')
        #         elif param.modifier == 'out':
        #             var_name = param.length_of.local_var_name() + '_length'
        # 
        #             local_var = SGParameter(var_name)
        #             local_var.data_type = find_or_add_type('LongInt')
        #             local_var.modifier = param.modifier
        # 
        #             method.local_vars.append(local_var)
        #             method.args.append(var_name)
        #         elif not param.data_type.is_struct:
        #             method.args.append(param.length_of.name + '.Length')
        #         else:
        #             method.args.append(param.length_of.local_var_name() + '.Length')
    
    # for member in the_file.members:
    #     #process all method of the file
    #     for key, method in member.methods.items():
    #         if method.method_called.was_function:
    #             #convert string return types to result parameters...
    #             result_param = SGParameter('result')
    #             result_param.data_type = method.return_type
    #             result_param.modifier = 'out'
    #             param_list = list(method.params)
    #             param_list.append(result_param)
    #             method.params = tuple(param_list)
    #             
    #             arg_list = list(method.args)
    #             arg_list.append(result_param)
    #             method.args = arg_list
    #             method.return_type = None
    #         if method.method_called.has_length_params:
    #             #add length parameters to this method
    #             for param in method.method_called.params:
    #                 if param.is_length_param:
    #                     param_list = list(method.params)
    #                     param_list.append(param)
    #                     method.params = tuple(param_list)
    #                     arg_list = list(method.args)
    #                     arg_list.append(param)
    #                     method.args = arg_list

def file_visitor(the_file, other):
    '''Called for each file read in by the parser'''
    
    
    if the_file.name == 'SGSDK':
        logger.info('Creating C# Library Adapter %s.cs', the_file.name)
        write_cs_sgsdk_file(the_file)
    else:
        logger.info('Creating C# SwinGame Module %s', the_file.name)
        post_parse_process(the_file)
        write_cs_lib_module(the_file)

def main():
    logging.basicConfig(level=logging.WARNING,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    
    load_data()
    parser_runner.run_for_all_units(file_visitor)

if __name__ == '__main__':
    main()

