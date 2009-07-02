#!/usr/bin/env python
# encoding: utf-8
"""
create_csharp_lib.py

Created by Andrew Cain on 2009-06-02.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import logging
import sys

from sg import parser_runner
from sg.sg_cache import logger, find_or_add_file, find_or_add_type
from sg.print_writer import PrintWriter
from sg.file_writer import FileWriter
from sg.sg_type import SGType
from sg.sg_parameter import SGParameter

_out_path="../../Templates/DotNET/common/lib"

#templates for adapter
_header = ''
_footer = ''
_method_wrapper = ''

#templates for modules
_module_method = ''
_module_header = ''
_module_footer = ''

_class_header = ''
_class_footer = ''

_pointer_wrapper_class_header = ''

_type_switcher = {
    None : {    
        #Pascal type: what it maps to
        'single': 'float %s',
        'longint': 'int %s',
        'soundeffect': 'SoundEffect %s',
        'music': 'Music %s',
        'string': 'string %s',
        'boolean': 'bool %s',
        'byte': 'byte %s',
        'timer': 'Timer %s',
        'color': 'Color %s',
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
        # '^point2d': 'Point2D *%s',
        'point2d[0..2]': 'Point2D %s[3]',
        'point2d[0..n - 1]': 'Point2D[] %s',
        # '^linesegment': 'LineSegment *%s',
        'linesegment': 'LineSegment %s',
        'sprite': 'Sprite %s',
        'rectangle': 'Rectangle %s',
        'triangle': 'Triangle %s',
        'linesarray': 'LineSegment[] %s',
        # 'linesegmentptr': 'LineSegment *%s',
        'font': 'Font %s',
        'fontalignment': 'FontAlignment %s',
        'fontstyle': 'FontStyle %s',
        'mousebutton': 'MouseButton %s',
        'uint16': 'ushort %s',
        # '^single': 'float *%s',
        'keycode': 'KeyCode %s',
        # 'bitmapptr': 'Bitmap *%s',
        # '^bitmap': 'Bitmap *%s',
        # 'longintptr': 'int *%s',
        # '^longint': 'int *%s',
        'collisionside': 'CollisionSide %s',
        'longint[0..n - 1][0..n - 1]': 'int[][] %s',
        'mapdata': 'MapData %s',
        'animationdata[0..n - 1]': 'AnimationData[] %s',
        'layerdata[0..n - 1]': 'LayerData[] %s',
        'collisiondata': 'CollisionData %s',
        'eventdetails[0..n - 1][0..23]': 'EventDetails[][24] %s',
        # '^maprecord': 'MapRecord *%s',
        'map': 'Map %s',
        'event': 'Event %s',
        'tile': 'Tile %s',
        'circle': 'Circle %s',
        'arrayofpoint2d': 'Point2D[] %s',
#        None: 'void %s'
    },
    'const' : {
        'point2d': 'Point2D %s',
        'linesegment': 'LineSegment %s',
        'rectangle': 'Rectangle %s',
        'matrix2d': 'Matrix2D %s',
        'vector': 'Vector %s',
        'linesarray': 'LineSegment[] %s',
        'triangle': 'Triangle %s',
        'bitmaparray': 'Bitmap[] %s',
        'longintarray': 'int[] %s',
        'circle': 'Circle %s',
    },
    'var' : {
        'soundeffect': 'SoundEffect %s',
        'music': 'Music %s',
        'timer': 'Timer %s',
        'string': 'char *%s',
        'triangle': 'Triangle %s',
        'linesarray': 'LineSegment[] %s',
        'font': 'Font %s',
        'bitmap': 'Bitmap %s',
        'sprite': 'Sprite %s',
        'matrix2d': 'Matrix2D %s',
        'map': 'Map %s',
        'arrayofpoint2d': 'Point2D[] %s',
    },
    'out' : {
        'string': 'out string %s',
        'byte': 'out byte %s',
        'color': 'out Color %s',
        'timer': 'out Timer %s',
        'point2d': 'out Point2D %s',
#        'triangle': 'Triangle *%s'
        'longint': 'out int %s',
        'linesegment': 'out LineSegment %s',
        'linesarray': 'out LineSegment[] %s',
        'matrix2d': 'out Matrix2D %s',
        'arrayofpoint2d': 'out Point2D[] %s',
        'triangle': 'out Triangle %s',
    },
    'return' : {
        None: 'void %s',
        'boolean': 'bool %s',
        'music': 'Music %s',
        'soundeffect': 'SoundEffect %s',
        'single': 'float %s',
        'point2d': 'Point2D %s',
        'longint': 'int %s',
        'timer': 'Timer %s',
        'byte': 'byte %s',
        'color': 'System.Drawing.Color %s',
        'uint32': 'uint %s',
        'vector': 'Vector %s',
        'circle': 'Circle %s',
        'rectangle': 'Rectangle %s',
        'linesegment': 'LineSegment %s',
        'bitmap': 'Bitmap %s',
        'collisionside': 'CollisionSide %s',
        'font': 'Font %s',
        'map': 'Map %s',
        'sprite': 'Sprite %s',
        'fontstyle': 'FontStyle %s',
        'event': 'Event %s',
        'tile': 'Tile %s',
        'string': 'String %s',
        'linesarray': 'LineSegment[] %s',
        'matrix2d': 'Matrix2D %s',
        'arrayofpoint2d': 'Point2D[] %s',
        'triangle': 'Triangle %s',
    }
}

_data_switcher = {
    'temp_return' :
    {
        'string': '%s.ToString()',
        'linesarray': '%s',
        'matrix2d': 'Utils.MatrixFromArray(%s)',
        'arrayofpoint2d': '%s',
        'triangle': 'Utils.TriangleFromArray(%s)',
        'longint': '%s;',
    },
    'return_val' : 
    {
        #Pascal type: what values of this type switch to %s = data value
        'boolean': '%s != 0',
        'music': 'PointerWrapper.Create<Music>(%s, Music.Create)',
        'soundeffect': 'PointerWrapper.Create<SoundEffect>(%s, SoundEffect.Create)',
        'bitmap': 'PointerWrapper.Create<Bitmap>(%s, Bitmap.Create)',
        'font': 'PointerWrapper.Create<Font>(%s, Font.Create)',
        'timer': 'PointerWrapper.Create<Timer>(%s, Timer.Create)',
        'map': 'PointerWrapper.Create<Map>(%s, Map.Create)',
        'sprite': 'PointerWrapper.Create<Sprite>(%s, Sprite.Create)',
        'color': 'System.Drawing.Color.FromArgb(%s)',
        'keycode': '(KeyCode)%s',
        'mousebutton': '(MouseButton)%s',
        'spriteendingaction': '(SpriteEndingAction)%s',
        'event': '(Event)%s',
        'collisionside': '(CollisionSide)%s',
        'fontalignment': '(FontAlignment)%s',
        'fontstyle': '(FontStyle)%s'
        # 'pointer.pointer': 'this.Pointer'
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
        'event': '(int)%s',
        'collisionside': '(int)%s',
        'resourcekind': '(int)%s',
        'fontalignment': '(int)%s',
        'fontstyle': '(int)%s'
    },
    #Argument with a literal value
    'arg_lit_val' : 
    {
        #Pascal type: what values of this type switch to %s = data value
        'single': '%sf',
        'pointer.pointer': 'this.Pointer',
        'true': '1',
        'false': '0',
    },
}

_adapter_type_switcher = {
    None: {
        'single': 'float %s',
        'longint': 'int %s',
        'soundeffect': 'IntPtr %s',
        'music': 'IntPtr %s',
        'string': '[MarshalAs(UnmanagedType.LPStr)] string %s',
        'boolean': 'int %s',
        'byte': 'byte %s',
        'color': 'int %s',
        'timer': 'IntPtr %s',
        'resourcekind': 'int %s',
        'uint32': 'uint %s',
        'bitmap': 'IntPtr %s',
        'rectangle': 'Rectangle %s',
        'linesegment': 'LineSegment %s',
        'triangle': '[MarshalAs(UnmanagedType.LPArray, SizeConst=3)] Point2D[] %s',
        'point2d': 'Point2D %s',
        'sprite': 'IntPtr %s',
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
        'event': 'int %s',
        'tile': 'Tile %s',
        'circle': 'Circle %s',
        'arrayofpoint2d': '[MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s)] Point2D[] %s',
#        None: 'void %s'
    },
    'const' : {
        'point2d':      '[MarshalAs(UnmanagedType.Struct), In] ref Point2D %s',
        'linesegment':  '[MarshalAs(UnmanagedType.Struct), In] ref LineSegment %s',
        'rectangle':    '[MarshalAs(UnmanagedType.Struct), In] ref Rectangle %s',
        'matrix2d':     '[MarshalAs(UnmanagedType.LPArray, SizeConst=9), In] float[,] %s',
        'triangle':     '[MarshalAs(UnmanagedType.LPArray, SizeConst=3), In] Point2D[] %s',
        'vector':       '[MarshalAs(UnmanagedType.Struct), In] ref Vector %s',
        'linesarray':   '[MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s), In] LineSegment[] %s',
        'longintarray': '[MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s), In] int[] %s',
        'bitmaparray':  '[MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s), In] Bitmap[] %s',
        'circle':       '[MarshalAs(UnmanagedType.Struct), In] ref Circle %s'
    },
    'var': {
        'soundeffect': 'ref IntPtr %s',
        'music': 'ref IntPtr %s',
        'timer': 'ref IntPtr %s',
        'byte': 'ref byte %s',
        'string': '[MarshalAs(UnmanagedType.LPStr), In, Out] StringBuilder %s',
        'triangle': '[MarshalAs(UnmanagedType.LPArray, SizeConst=3), In, Out] Point2D[] %s',
        #'linesarray': '[MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s), In, Out] LineSegment[] %s',
        'font': 'ref IntPtr %s',
        'bitmap': 'ref IntPtr %s',
        'sprite': 'ref IntPtr %s',
        'map': 'ref IntPtr %s'
    },
    'out': {
        'string': '[MarshalAs(UnmanagedType.LPStr), Out] StringBuilder %s',
        'byte': '[Out] out byte %s',
        'color': '[Out] out int %s',
#        'timer': '[Out] IntPtr %s',
        'point2d': '[Out] out Point2D %s',
#        'triangle': 'Triangle *%s',
        'linesarray': '[MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s), Out] LineSegment[] %s',
        'longint': '[Out] out int %s',
        'linesegment': '[Out] out LineSegment %s',
    },
    'result': {
        'string': '[MarshalAs(UnmanagedType.LPStr), Out] StringBuilder %s',
        'linesarray': '[MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s), Out] LineSegment[] %s',
        'matrix2d': '[MarshalAs(UnmanagedType.LPArray, SizeConst=9), Out] float[,] %s',
        'arrayofpoint2d': '[MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s), Out] Point2D[] %s',
        'triangle': '[MarshalAs(UnmanagedType.LPArray, SizeConst=3), Out] Point2D[] %s',
    },
    'return' : {
        None: 'void %s',
        'boolean': 'int %s',
        'music': 'IntPtr %s',
        'soundeffect': 'IntPtr %s',
        'single': 'float %s',
        'point2d': 'Point2D %s',
        'longint': 'int %s',
        'timer': 'IntPtr %s',
        'byte': 'byte %s',
        'color': 'int %s',
        'uint32': 'uint %s',
        'vector': 'Vector %s',
        'circle': 'Circle %s',
        'rectangle': 'Rectangle %s',
        'linesegment': 'LineSegment %s',
        'bitmap': 'IntPtr %s',
        'collisionside': 'int %s',
        'font': 'IntPtr %s',
        'map': 'IntPtr %s',
        'sprite': 'IntPtr %s',
        'fontstyle': 'int %s',
        'event': 'int %s',
        'tile': 'Tile %s',
    }
}

# mapping for local variables
_local_type_switcher = {
    'string': 'StringBuilder %s = new StringBuilder(2048);',
    'color': 'int %s;',
    'matrix2d': 'float[,] %s = new float[3,3];',
    'triangle': 'Point2D[] %s = new Point2D[3];',
    'linesarray': 'LineSegment[] %s;',
    'longintarray': 'int[] %s;',
    'bitmaparray' : 'Bitmap[] %s;',
    'arrayofpoint2d': 'Point2D[] %s;',
    'longint': 'int %s;',
}

# mapping for struct fields
_struct_type_switcher = {
    #Pascal type: what it maps to
    'single': 'public float %s',
    'longint': 'public int %s',
    'soundeffect': 'public SoundEffect %s',
    'music': 'public Music %s',
    'string': 'public string %s',
    'boolean': 'public bool %s',
    'byte': 'public byte %s',
    'timer': 'public Timer %s',
    'color': 'public Color %s',
    'resourcekind': 'public ResourceKind %s',
    'uint32': 'public uint %s',
    'bitmap': 'public Bitmap %s',
    #'pointer': 'public IntPtr %s',
    'single[0..2][0..2]': '[ MarshalAs( UnmanagedType.ByValArray, SizeConst=9 )]\npublic float[,] %s',
    # '^bitmapdata': 'public BitmapData *%s',
    # '^spritedata': 'public SpriteData *%s',
    # '^timerdata': 'public TimerData *%s',
    # 'psdl_surface': 'public IntPtr %s',
    # 'boolean[0..n - 1][0..n - 1]': 'public bool[,] %s',
    # 'bitmap[0..n - 1]': 'public Bitmap[] %s',
    # 'spritekind': 'public SpriteKind %s',
    # 'longint[0..n - 1]': 'public int[] %s',
    'vector': 'public Vector %s',
    # 'spriteendingaction': 'public SpriteEndingAction %s',
    'point2d': 'public Point2D %s',
    # '^point2d': 'public Point2D *%s',
    'point2d[0..2]': '[ MarshalAs( UnmanagedType.ByValArray, SizeConst=3 )]\npublic Point2D[] %s',
    # 'point2d[0..n - 1]': 'public Point2D[] %s',
    # '^linesegment': 'public LineSegment *%s',
    'linesegment': 'public LineSegment %s',
    'sprite': 'public Sprite %s',
    'rectangle': 'public Rectangle %s',
    #'triangle': 'public Triangle %s',
    'linesarray': 'public LineSegment[] %s',
    # 'linesegmentptr': 'public LineSegment *%s',
    'font': 'public Font %s',
    'fontalignment': 'public FontAlignment %s',
    'fontstyle': 'public FontStyle %s',
    'mousebutton': 'public MouseButton %s',
    'uint16': 'public ushort %s',
    # '^single': 'public float *%s',
    'keycode': 'public KeyCode %s',
    # 'bitmapptr': 'public Bitmap *%s',
    # '^bitmap': 'public Bitmap *%s',
    # 'longintptr': 'public int *%s',
    # '^longint': 'public int *%s',
    'collisionside': 'public CollisionSide %s',
    'longint[0..n - 1][0..n - 1]': 'public int[][] %s',
    'mapdata': 'public MapData %s',
    'animationdata[0..n - 1]': 'public AnimationData[] %s',
    'layerdata[0..n - 1]': 'public LayerData[] %s',
    'collisiondata': 'public CollisionData %s',
    # 'eventdetails[0..n - 1][0..23]': 'public EventDetails[][24] %s',
    # '^maprecord': 'public MapRecord *%s',
    'map': 'public Map %s',
    'event': 'public Event %s',
    'tile': 'public Tile %s',
    'circle': 'public Circle %s',
    'arrayofpoint2d': 'public Point2D[] %s',
}


_names = []

def load_data():
    global _header, _method_wrapper
    global _module_method, _module_header, _module_footer
    global _class_header, _class_footer
    global _pointer_wrapper_class_header
    
    #old
    global _module_header_header, _module_c_header, _module_c_method
    global _module_c_function, _footer, _module_header_footer
    
    f = open('./cs_lib/lib_header.txt')
    _header = f.read()
    f.close()
    
    f = open('./cs_lib/lib_method_wrap.txt')
    _method_wrapper = f.read()
    f.close()
    
    f = open('./cs_lib/lib_header_footer.txt')
    _footer = f.read()
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
    
    f = open('./cs_lib/pointer_wrapper_class_header.txt')
    _pointer_wrapper_class_header = f.read()
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
    if the_type.pointer_wrapper:
        arg_str = arg_str + '.Pointer'
    
    # Change True to true for example...
    if for_param.modifier != 'out' and arg_str.lower() in _data_switcher[data_key]:
        data = _data_switcher[data_key][arg_str.lower()] 
        if '%s' in data: #for_param.modifier not in ['out', 'var']:
            arg_str = _data_switcher[data_key][arg_str.lower()] % arg_str
        else:
            arg_str = data
    
    if for_param.modifier != 'out' and the_type.name.lower() in _data_switcher[data_key]:
        #convert data using pattern from _data_switcher
        result = _data_switcher[data_key][the_type.name.lower()] % arg_str
    else:
        result = arg_str
    
    #check var/out/const
    if (for_param.modifier == 'var' or for_param.modifier == 'const') and not (the_type.array_wrapper or the_type.fixed_array_wrapper):
        result = 'ref ' + result
    elif for_param.modifier == 'out' and the_type.name.lower() != "string":
        result = 'out ' + result
    
    return result

def arg_cs_dll_visitor(arg_str, the_arg, for_param):
    the_type = for_param.data_type
    result = arg_str
    #check var/out/const
    if (for_param.modifier == 'var' or for_param.modifier == 'const') and not (the_type.array_wrapper or the_type.fixed_array_wrapper):
        result = 'ref ' + result
    elif for_param.modifier == 'out' and the_type.name.lower() != "string":
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
    key = the_param.data_type.name.lower()
    modifier = the_param.modifier
    
    if key not in _adapter_type_switcher[modifier]:
        logger.error('CREATE Cs : Error changing adapter parameter %s - %s', the_param.modifier, the_param.data_type.name)
    
    if not the_param.data_type.array_wrapper:
        # map to name + type
        return '%s%s' % ( _adapter_type_switcher[modifier][key] % the_param.name, ', ' if not last else '')
    else:
        return '%s%s' % ( _adapter_type_switcher[modifier][key] % (the_param.length_idx, the_param.name), ', ' if not last else '')


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
        details['pre_call'] ='PointerWrapper.Remove(this);\n    '
        details['return_type'] = 'void DoFree'
        details['returns_end'] = ''
        details['public'] = 'protected internal override '
        details['base_const'] = ''
        result = '%(calls.class)s.%(calls.name)s(%(calls.args)s)%(returns_end)s' % details
    else:
        if the_method.name in ['WindowCloseRequested', 'CloseAudio']:
            details['pre_call'] = 'PointerWrapper.FreeAnythingToFree();\n    '
        elif the_method.mimic_destructor:
            details['pre_call'] ='PointerWrapper.Remove(%s.Pointer);\n    ' % the_method.params[0].name
        else: details['pre_call'] =''
        details['returns_end'] = ''
        details['public'] = 'public '
        details['base_const'] = ''
        result = '%(calls.class)s.%(calls.name)s(%(calls.args)s)%(returns_end)s' % details
        
        if the_method.return_type != None:
            if the_method.return_type.name.lower() in _data_switcher['return_val']:
                result = _data_switcher['return_val'][the_method.return_type.name.lower()] % result
    
    if the_method.name in ['ProcessEvents']:
        details['post_call'] = '\n    PointerWrapper.FreeAnythingToFree();'
    else: details['post_call'] =''
    
    return ('%(returns)s' + result) % details

def create_cs_dll_call(details, the_method):
    """Create a c# call for the passed in details dictionary/method, this call is from the SGSDK.cs code
    to the native library..."""
    
    details['pre_call'] =''
    details['post_call'] =''
    details['public'] = 'internal '
    result = ''
    
    return '%(returns)sDLL_%(name)s(%(args)s)' % details

def method_visitor(the_method, other, as_accessor_name = None):
    details = the_method.to_keyed_dict(
        other['param visitor'], other['type visitor'], other['arg visitor'], doc_transform, other['call_creater'])
    writer = other['file writer']
    
    if other['lib method']: 
        #write out the library versions...
        writer.writeln('[DllImport("SGSDK.dll", CallingConvention=CallingConvention.Cdecl, EntryPoint="%s", CharSet=CharSet.Ansi)]' % details['name'])
        writer.write('private static extern %(return_type)s' % details % 'DLL_' + details['name'])
        writer.write('(%(params)s);' % details) 
        writer.writeln('')
        
        #write out the wrapped version...
        
        writer.writeln((_method_wrapper % details).replace('%s', details['name']) )
        writer.writeln('')
    else:
        if len(the_method.local_vars) > 0:
            temp = '\n'
            temp_process_params = details['pre_call']
            temp_process_result = details['post_call'] + '\n'
            
            #process all local variables
            for local_var in the_method.local_vars:
                temp += '    %s\n' % _local_type_switcher[local_var.data_type.name.lower()] % local_var.name
                if isinstance(local_var, SGParameter) and local_var.maps_result:
                    #setup the size of a return array
                    if the_method.fixed_result_size > 0:
                        assert local_var.data_type.name in ['LinesArray', 'ArrayOfPoint2D']
                        temp_process_params = '%s = new %s[%s];\n    ' % (
                                local_var.name,
                                'LineSegment' if local_var.data_type.name == 'LinesArray' else 'Point2D',
                                the_method.fixed_result_size
                            )
                    continue                
                type_name = local_var.data_type.name.lower()
                
                if type_name == 'string':
                    assert local_var.modifier == 'out' or local_var.maps_result
                    temp_process_result += '\n    %s = %s.ToString();' % (local_var.name[:-5], local_var.name)
                elif type_name == 'color':
                    temp_process_result += '\n    %s = System.Drawing.Color.FromArgb(%s);' % (local_var.name[:-5], local_var.name)
                
                if local_var.modifier != 'out':
                    # copy in value
                    temp_process_params += '%s = %s;\n    ' % (
                            local_var.name,
                            local_var.name[:-5] if not local_var.has_field else local_var.name[:-5] + '.' + local_var.field_name
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
        
        
        if as_accessor_name != None:
            #change for property support
            details['return_type'] = as_accessor_name
            details['public'] = ''
            details['static'] = ''
            details['params'] = ''
        else:
            details['params'] = '(%s)' % details['params']
        
        writer.writeln((_module_method % details).replace('%s', details['name']) )
        writer.writeln()
        # if the_method.is_function:
        #     #%(calls.name)s(%(calls.args)s)
        #     details['the_call'] = other['arg visitor']('%(calls.name)s(%(calls.args)s)' % details, the_method.return_type)
        #     other['file writer'].write(_module_c_function % details % the_method.uname)
        # else:
        #     other['file writer'].write(_module_c_method % details)

def property_visitor(the_property, other):
    writer = other['file writer']
    
    # public string Name { getter setter }
    type_name = _type_switcher['return'][the_property.data_type.name.lower()]
    writer.write('public %s%s\n{\n' % ('static ' if the_property.is_static else '', type_name) % the_property.name)
    
    writer.indent(2)
    
    if the_property.getter != None:
        method_visitor(the_property.getter, other, 'get');
    if the_property.setter != None:
        method_visitor(the_property.setter, other, 'set');
    
    writer.outdent(2)
    
    writer.write('}\n');

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
    }
    
    file_writer.indent(2);
    file_writer.writeln('internal class %s' % the_file.members[0].name);
    file_writer.writeln('{');
    
    file_writer.indent(2);
    the_file.members[0].visit_methods(method_visitor, other)
    file_writer.outdent(2);
    
    file_writer.writeln('}');
    file_writer.outdent(2);
    
    file_writer.write(_footer)
    file_writer.close()

def write_cs_methods_for_module(member, other):
    '''Write out a single c member'''
    
    writer = other['file writer']
    
    details = member.to_keyed_dict(doc_transform=doc_transform)
    
    #Support Module style access...
    details['module_attr'] = '\n[Microsoft.VisualBasic.CompilerServices.StandardModuleAttribute()]' if member.is_module else ''
    
    writer.writeln(_class_header % details)
    
    writer.indent(2)
    
    member.visit_methods(method_visitor, other)
    
    writer.outdent(2)
    
    writer.writeln(_class_footer)

def write_sg_class(member, other):
    #if member.name not in ['SoundEffect', 'Music']: return
    
    file_writer = FileWriter('%s/%s.cs'% (_out_path, member.name))
    
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
    file_writer.outdent(2);
    
    file_writer.writeln(_class_footer)
    file_writer.outdent(2)
    
    file_writer.write(_module_footer)
    file_writer.close()

def write_c_type_for(member, other):
    '''Write out a single c member'''
    
    assert member.is_class or member.is_struct or member.is_enum
    
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
        writer.write('[ StructLayout( LayoutKind.Sequential, CharSet=CharSet.Ansi )]\n')
        writer.write('public struct %s\n{\n' % member.name)
        
        writer.indent(2)
        
        for field in member.field_list:
            writer.writeln('%s;' % struct_type_visitor(field.data_type) % field.name)
            
        writer.outdent(2)
        
        writer.writeln('}\n')
    elif member.is_enum:
        #enum id { list }
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
        }
    
    file_writer.indent(2)
    
    #process all types first so they appear at the top of the header files
    for member in the_file.members:
        if member.is_module or member.is_header or member.is_type:
            pass
        elif member.is_class or member.is_struct or member.is_enum:
            write_c_type_for(member, other)
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
    
    for member in the_file.members:
        #process all method of the file
        for key, method in member.methods.items():
            for param in method.params:
                if param.maps_result or param.data_type.wraps_array or (param.modifier in ['var', 'out'] and param.data_type.name.lower() in ['string','color']):
                    logger.debug('Create cs : Adding local var of type %s to %s', param.data_type, method.uname)

                    local_var = SGParameter(param.name + '_temp')
                    local_var.data_type = param.data_type
                    local_var.modifier = param.modifier
                    local_var.maps_result = param.maps_result

                    if param.data_type.wraps_array:
                        if param.data_type.is_struct:
                            local_var.has_field = True
                            local_var.field_name = param.data_type.fields[0].name
                        else:
                            continue
                        
                    method.local_vars.append(local_var)
                    param.maps_to_temp = True
            
            if method.method_called.was_function:
                #get the return parameter
                result_param = method.method_called.params[-1]
                if not result_param.maps_result: #in case of returning var length array
                    result_param = method.method_called.params[-2]
                method.local_vars.append(result_param)
                method.args.append(result_param)
                
            if method.method_called.has_length_params:
                for param in method.method_called.params:
                    if param.is_length_param:
                        #possibly need an extra local for this... if out
                        if param.length_of.maps_result:
                            # need to indicate the size of the returned array...
                            assert method.fixed_result_size > 0
                            method.args.append(str(method.fixed_result_size))
                        elif param.modifier == 'out':
                            var_name = param.length_of.local_var_name() + '_length'

                            local_var = SGParameter(var_name)
                            local_var.data_type = find_or_add_type('LongInt')
                            local_var.modifier = param.modifier

                            method.local_vars.append(local_var)
                            method.args.append(var_name)
                        elif not param.data_type.is_struct:
                            method.args.append(param.length_of.name + '.Length')
                        else:
                            method.args.append(param.length_of.local_var_name() + '.Length')

    
    return
    
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
    post_parse_process(the_file)
    
    if the_file.name == 'SGSDK':
        logger.info('Creating C# Library Adapter %s.cs', the_file.name)
        write_cs_sgsdk_file(the_file)
    else:
        logger.info('Creating C# SwinGame Module %s', the_file.name)
        write_cs_lib_module(the_file)

def main():
    logging.basicConfig(level=logging.WARNING,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    
    load_data()
    parser_runner.run_for_all_units(file_visitor)

if __name__ == '__main__':
    main()

