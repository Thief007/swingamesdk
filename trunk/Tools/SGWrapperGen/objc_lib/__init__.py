#!/usr/bin/env python
# encoding: utf-8
"""
__init__.py

Created by Andrew Cain on 2009-06-02.
Copyright (c) 2009 Swinburne. All rights reserved.
"""

import sys
import os

#
# The type_switcher switches types from the Pascal types in the
# model to the ObjC types used by the users of the SwinGame API.
#

_type_switcher = {
    None : {    
        #Pascal type: what it maps to
        'single':   'float',
        'longint':  'int',
        'string':   'NSString *',
        'boolean':  'BOOL',
        'byte':     'unsigned char',
        'uint32':   'uint',
        'uint16':   'unsigned short',

        'soundeffect':  'SGSoundEffect *',
        'music':        'SGMusic *',
        'sprite':       'SGSprite *',
        'font':         'SGFont *',
        'bitmap':       'SGBitmap *',
        'timer':        'SGTimer *',
        'map':          'SGMap *',
        
        # 'rectangle':    'SGRectangle',
        # 'triangle':     'SGTriangle',
        # 'circle':       'SGCircle',
        
        #elements of arrays
        'point2d':      'SGPoint2D *',
                
        'pointer': 'id',

        'color': 'color',

        'freenotifier': 'free_notifier',

        'resourcekind':         'resource_kind',
        'mousebutton':          'mouse_button',
        'keycode':              'key_code',
        'spriteendingaction':   'sprite_ending_action',
        'maptag':               'map_tag',
        'spritekind':           'sprite_kind',
        'fontalignment':        'font_alignment',
        'fontstyle':            'font_style',
    },
    'const' : {
        'point2d':      'SGPoint2D *',
        'linesegment':  'SGLineSegment *',
        'rectangle':    'SGRectangle *',
        'vector':       'SGVector *',
        'linesarray':   'NSArray *',
        
        'matrix2d':     'SGMatrix2D *',
        'triangle':     'SGTriangle *',
        
        'bitmaparray':  'NSArray *',
        'longintarray': 'NSArray *',
        'circle':       'SGCircle *',
    },
    'var' : {
        'soundeffect':  'SGSoundEffect *',
        'music':        'SGMusic *',
        'timer':        'SGTimer *',
        'bitmap':       'SGBitmap *',
        'sprite':       'SGSprite *',
        'map':          'SGMap *',
        'font':         'SGFont *',
        
        'triangle':     'SGTriangle *',
        'matrix2d':     'SGMatrix2D *',
    },
    'out' : {
        'string':       'NSString **',
        'byte':         'unsigned char *',
        'color':        'color *',
        
        'point2d':      'SGPoint2D **',
        
        'longint':          'int *',
        'single':           'float *',
        'linesegment':      'SGLineSegment **',
        'linesarray':       'NSArray *',
        'matrix2d':         'SGMatrix2D *',
        'arrayofpoint2d':   'NSArray *',
        'triangle':         'SGTriangle *',
    },
    'return' : {
        None: 'void',
        'boolean':      'BOOL',
        
        'music':        'SGMusic *',
        'soundeffect':  'SGSoundEffect *',
        'bitmap':       'SGBitmap *',
        'font':         'SGFont *',
        'map':          'SGMap *',
        'sprite':       'SGSprite *',
        'timer':        'SGTimer *',
                
        'longintarray':     'NSArray *',
        'arrayofpoint2d':   'NSArray *',
        'linesarray':       'NSArray *',
                
        'color':        'color',
        
        #basic types
        'single':       'float',
        'longint':      'int',
        'byte':         'unsigned char',
        'uint32':       'uint',
        'string':       'NSString *',
        
        #enums
        'collisionside':        'collision_side',
        'fontstyle':            'font_style',
        'maptag':               'map_tag',
        'maptile':              'map_tile',
        'spriteendingaction':   'sprite_ending_action',
        'spritekind':           'sprite_kind',
        
        #arrays + structs
        'matrix2d':     'SGMatrix2D *',
        'triangle':     'SGTriangle *',
        'point2d':      'SGPoint2D *',
        'vector':       'SGVector *',
        'circle':       'SGCircle *',
        'rectangle':    'SGRectangle *',
        'linesegment':  'SGLineSegment *',
    }
}

#
# The data switcher provides a mapping for types and literal values
# from pascal to objective c. 
#
_data_switcher = {
    #
    # The temp return is used to map local variable temporary variables
    # to appropriate values to return.
    #
    # 'temp_return' :
    # {
    #     'string':           '%s.ToString()',
    #     'linesarray':       '%s',
    #     'matrix2d':         'Utils.MatrixFromArray(%s)',
    #     'arrayofpoint2d':   '%s',
    #     'triangle':         'Utils.TriangleFromArray(%s)',
    #     'longint':          '%s',
    #     'longintarray':     '%s',
    # },

    'return_val' : 
    {
        #Pascal type: what values of this type switch to %s = data value
        'boolean':              '%s != 0',
        
        'music':        '[SGMusic createWithId:%s]',
        'soundeffect':  '[SGSoundEffect createWithId:%s]',
        'bitmap':       '[SGBitmap createWithId:%s]',
        'font':         '[SGFont createWithId:%s]',
        'timer':        '[SGTimer createWithId:%s]',
        'map':          '[SGMap createWithId:%s]',
        'sprite':       '[SGSprite createWithId:%s]',
        
        'point2d':      '[SGPoint2D point2DForData:%s]',
        'vector':       '[SGVector vectorForData:%s]',
        'circle':       '[SGCircle circleForData:%s]',
        'rectangle':    '[SGRectangle rectangleForData:%s]',
        'linesegment':  '[SGLineSegment lineSegmentForData:%s]',
        
        # 'matrix2d':     '[SGMatrix2D matrix2DForData:%s]',
        # 'triangle':     '[SGTriangle triangleForData:%s]',
        
        'keycode':              '(key_code)%s',
        'mousebutton':          '(mouse_button)%s',
        'spriteendingaction':   '(sprite_ending_action)%s',
        'spritekind':           '(sprite_kind)%s',
        'maptag':               '(map_tag)%s',
        'collisionside':        '(collision_side)%s',
        'fontalignment':        '(font_alignment)%s',
        'fontstyle':            '(font_style)%s'
    },
    #Argument with a parameter value
    'arg_val' : 
    {
        #Pascal type: what values of this type switch to %s = data value
        'boolean':              '(%s ? 1 : 0)',
        'keycode':              '(int)%s',
        'mousebutton':          '(int)%s',
        'spriteendingaction':   '(int)%s',
        'maptag':               '(int)%s',
        'collisionside':        '(int)%s',
        'resourcekind':         '(int)%s',
        'fontalignment':        '(int)%s',
        'fontstyle':            '(int)%s',
        
        'linesegment':          '%s->data',
        'vector':               '%s->data',
        'point2d':              '%s->data',
        'circle':               '%s->data',
        'rectangle':            '%s->data',
        
        'matrix2d':             '%s->data',
        'triangle':             '%s->data',
    },
    #Argument with a literal value
    'arg_lit_val' : 
    {
        #Pascal type: what values of this type switch to %s = data value
        'single': '%sf',
        'self.pointer': 'self',
        'self.data': 'self', #adds ->data
        'self': 'self',
        'true': '1',
        'false': '0',
    },
}

# mapping for local variables
local_variable_switcher = {
    # All switches are passed a dictionary with
    #  - %(var)s = local variable name
    #  - %(param)s = parameter name
    #  - %(modifier)s = const if 'const '... otherwise ''
    #  - %(size)s = size expression (literal or call)
    'declare':
    {
        'boolean':          'BOOL %(var)s;\n    ',
        'color':            'color %(var)s;\n    ',
        'longint':          'int %(var)s;\n    ',
        
        'soundeffect':      'SGSoundEffect *%(var)s;\n    ',
        'music':            'SGMusic *%(var)s;\n    ',
        'bitmap':           'SGBitmap *%(var)s;\n    ',
        
        'string':           'char %(var)s[%(size)s];\n    ',
        'matrix2d':         'matrix2d %(var)s;\n    ',
        'triangle':         'triangle %(var)s;\n    ',
        
        #arrays for structs
        'linesarray':       'line_segment %(var)s[%(size)s];\n    ',
        'longintarray':     'int %(var)s[%(size)s];\n    ',
        'bitmaparray' :     'bitmap %(var)s[%(size)s];\n    ',
        'arrayofpoint2d':   'point2d %(var)s[%(size)s];\n    ',
        
        #out structs
        'point2d':          'point2d %(var)s;\n    ',
        'linesegment':      'line_segment %(var)s;\n    ',
        'vector':           'vector %(var)s;\n    ',
        
        'longint':          'int %s;\n    ',
    },
    'length-of': #used to calculate %(size)s - is an expression
    {
        #String type
        'string':           '[%(param)s length] + 1',
        #NSArray types
        'bitmaparray':      '[%(param)s count]',
        'longintarray':     '[%(param)s count]',
        'linesarray':       '[%(param)s count]',
    },
    'initialise-param':
    {
        #Direct mappings for C based language
        'matrix2d':         '%(var)s = &%(param)s[0][0];\n    ',
        'triangle':         '%(var)s = &%(param)s[0];\n    ',
        #String type
        'string':           '[%(param)s getCString:%(var)s maxLength:[%(param)s length] + 1 encoding:NSASCIIStringEncoding];\n    ',
        #NSArray types
        'bitmaparray':      '[SGBitmap getBitmaps:%(var)s fromArray:%(param)s maxSize:%(size)s];\n    ',
        'longintarray':     '[SGUtils getIntegers:%(var)s fromArray:%(param)s maxSize:%(size)s];\n    ',
        'linesarray':       '[SGLineSegment getLineSegments:%(var)s fromArray:%(param)s maxSize:%(size)s];\n    ',
    },
    'process-out-param': 
    {
        'string':           '\n    *%(param)s = [[[NSString alloc] initWithCString:%(var)s encoding:NSASCIIStringEncoding] autorelease];',
        
        #Passed through arrays
        'triangle':         '\n    *%(param)s = [[[SGTriangle alloc] initWithTriangle:%(var)s size:3] autorelease];',
        'matrix2d':         '\n    *%(param)s = [[[SGMatrix2D alloc] initWithMatrix2D:%(var)s size:9] autorelease];',
        #NSArray types
        # 'linesarray':       '\n    %(param)s = [[NSArray alloc] todo...];',
        # 'arrayofpoint2d':   '\n    %(param)s = [[NSArray alloc] todo...];',
        # 'longintarray':     '\n    %(param)s = [SGUtils arrayOfIntegers:%(var)s size:%(size)s];',
        
        #out structs
        'point2d':          '\n    *%(param)s = [[[SGPoint2D alloc] initWithPoint2D:%(var)s] autorelease];',
        'linesegment':      '\n    *%(param)s = [[[SGLineSegment alloc] initWithLineSegment:%(var)s] autorelease];',
    },
    'process-result': 
    {
        'boolean':          '\n    return %(var)s;',
        'color':            '\n    return %(var)s;',
        'longint':          '\n    return %(var)s;',
        
        'string':           '\n    return [[[NSString alloc] initWithCString:%(var)s encoding:NSASCIIStringEncoding] autorelease];',
        
        'soundeffect':      '\n    return %(var)s;',
        'music':            '\n    return %(var)s;',
        'bitmap':           '\n    return %(var)s;',
        
        
        #Passed through arrays
        'matrix2d':         '\n    return [SGMatrix2D matrix2DForData:%(var)s];',
        'triangle':         '\n    return [SGTriangle triangleForData:%(var)s];',
        
        #NSArray types
        'linesarray':       '\n    return [SGLineSegment arrayOfLineSegments:%(var)s size:%(size)s];',
        'arrayofpoint2d':   '\n    return [SGPoint2D arrayOfPoint2Ds:%(var)s size:%(size)s];',
        'longintarray':     '\n    return [SGUtils arrayOfIntegers:%(var)s size:%(size)s];',

        #return structs
        'vector':           '\n    return [[[SGVector alloc] initWithVector:%(var)s] autorelease];',
    },
    'clean-up':
    {
        'string':           '',
    },
}

# struct_type_switcher = {
#     None:
#     {
#         'single':   'float',
#     },
# }

def main():
    (path, script_file) = os.path.split(sys.modules[__name__].__file__) 
    dirList=os.listdir(path)
    
    for f in dirList:
        if '.py' in f or f[0] == '.' : continue
    
        (dirName, fileName) = os.path.split(f)
        key = fileName.replace('.', '_')
    
        fin = open(path + '/' + f)
        data = fin.read()
        fin.close()
        
        setattr(sys.modules[__name__], key, data)

main()