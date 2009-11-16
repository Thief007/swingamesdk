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
        
        #SwinGame resources
        'soundeffect':      'SGSoundEffect *',
        'music':            'SGMusic *',
        'sprite':           'SGSprite *',
        'font':             'SGFont *',
        'bitmap':           'SGBitmap *',
        'timer':            'SGTimer *',
        'map':              'SGMap *',
        'shapeprototype':   'SGShapePrototype *',
        'shape':            'SGShape *',
        'triangle':         'SGTriangle *',
        # 'rectangle':    'SGRectangle',
        # 'circle':       'SGCircle',
        
        #elements of arrays
        'point2d':          'SGPoint2D *',
        
        #arrays from within structs
        'point2darray':     'NSArray *',
        'shapearray':       'NSArray *',
        
        'pointer': 'id',
        
        'color': 'color',
        
        #function pointers
        'freenotifier':     'free_notifier',
        'shapedrawingfn':   'shape_drawing_fn',
        
        #Enums
        'resourcekind':         'resource_kind',
        'mousebutton':          'mouse_button',
        'keycode':              'key_code',
        'spriteendingaction':   'sprite_ending_action',
        'maptag':               'map_tag',
        'spritekind':           'sprite_kind',
        'fontalignment':        'font_alignment',
        'fontstyle':            'font_style',
        'shapekind':            'shape_kind',
    },
    'const' : {
        #records
        'point2d':      'SGPoint2D *',
        'linesegment':  'SGLineSegment *',
        'rectangle':    'SGRectangle *',
        'vector':       'SGVector *',
        'matrix2d':     'SGMatrix2D *',
        'triangle':     'SGTriangle *',
        'circle':       'SGCircle *',
        
        #Arrays
        'linesarray':       'NSArray *',
        'bitmaparray':      'NSArray *',
        'longintarray':     'NSArray *',
        'point2darray':     'NSArray *',
        'shapearray':       'NSArray *',
    },
    'var' : {
        'soundeffect':      'SGSoundEffect *',
        'music':            'SGMusic *',
        'timer':            'SGTimer *',
        'bitmap':           'SGBitmap *',
        'sprite':           'SGSprite *',
        'map':              'SGMap *',
        'font':             'SGFont *',
        'shapeprototype':   'SGShapePrototype *',
        'shape':            'SGShape *',
        
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
        'point2darray':     'NSArray *',
        'triangle':         'SGTriangle *',
    },
    'return' : {
        #SwinGame resources
        'music':            'SGMusic *',
        'soundeffect':      'SGSoundEffect *',
        'bitmap':           'SGBitmap *',
        'font':             'SGFont *',
        'map':              'SGMap *',
        'sprite':           'SGSprite *',
        'timer':            'SGTimer *',
        'shapeprototype':   'SGShapePrototype *',
        'shape':            'SGShape *',
        
        #Arrays
        'longintarray':     'NSArray *',
        'point2darray':     'NSArray *',
        'linesarray':       'NSArray *',
        'shapearray':       'NSArray *',
                
        #basic types
        None:           'void',
        'boolean':      'BOOL',
        'color':        'color',
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
        'shapekind':            'shape_kind',
        
        #arrays + structs
        'matrix2d':     'SGMatrix2D *',
        'triangle':     'SGTriangle *',
        'point2d':      'SGPoint2D *',
        'vector':       'SGVector *',
        'circle':       'SGCircle *',
        'rectangle':    'SGRectangle *',
        'linesegment':  'SGLineSegment *',
        
        #function pointers
        'shapedrawingfn':   'shape_drawing_fn',
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
    #     'point2darray':   '%s',
    #     'triangle':         'Utils.TriangleFromArray(%s)',
    #     'longint':          '%s',
    #     'longintarray':     '%s',
    # },

    'return_val' : 
    {
        #Pascal type: what values of this type switch to %s = data value
        'boolean':              '%s != 0',
        
        'music':            '[SGMusic createWithId:%s]',
        'soundeffect':      '[SGSoundEffect createWithId:%s]',
        'bitmap':           '[SGBitmap createWithId:%s]',
        'font':             '[SGFont createWithId:%s]',
        'timer':            '[SGTimer createWithId:%s]',
        'map':              '[SGMap createWithId:%s]',
        'sprite':           '[SGSprite createWithId:%s]',
        'shape':            '[SGShape createWithId:%s]',
        'shapeprototype':   '[SGShapePrototype createWithId:%s]',
        
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
        'fontstyle':            '(font_style)%s',
    },
    # Argument with a parameter value
    'arg_val' : 
    {
        # Pascal type: what values of this type switch to %s = data value
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
    # Argument with a literal value
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
        'point2darray':     'point2d %(var)s[%(size)s];\n    ',
        'shapearray':       'shape %(var)s[%(size)s];\n    ',
        
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
        'point2darray':     '[%(param)s count]',
        'shapearray':       '[%(param)s count]',
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
        'point2darray':     '[SGPoint2D getPoint2Ds:%(var)s fromArray:%(param)s maxSize:%(size)s];\n    ',
        'shapearray':       '[SGShape getShapes:%(var)s fromArray:%(param)s maxSize:%(size)s];\n    ',
    },
    # -------------------------------
    # Parameter post-call processing for standard parameters (arrays only)
    # -------------------------------
    # This is used for updating NSArray objects after passing data to Pascal.
    # For example: apply_matrix(m, points) converts the NSArray for points into
    # point data and passes this to Pascal which applys the matrix to all points.
    # The resulting points then need to be moved back into the NSArray.
    'process-param': 
    {
        'point2darray':     '\n    [SGPoint2D updatePoint2DsIn:%(param)s fromDataIn:%(var)s];',
    },
    # -------------------------------
    # Parameter post-call processing for out parameters
    # -------------------------------
    # With out parameters the code needs to generate the appropriate code to copy the data passed to the Pascal code
    # (and updated there due to out parameter) back to the parameter in this language.
    'process-out-param': 
    {
        'string':           '\n    *%(param)s = [[[NSString alloc] initWithCString:%(var)s encoding:NSASCIIStringEncoding] autorelease];',
        
        #Passed through arrays
        'triangle':         '\n    *%(param)s = [[[SGTriangle alloc] initWithTriangle:%(var)s size:3] autorelease];',
        'matrix2d':         '\n    *%(param)s = [[[SGMatrix2D alloc] initWithMatrix2D:%(var)s size:9] autorelease];',
        
        #out structs
        'point2d':          '\n    *%(param)s = [[[SGPoint2D alloc] initWithPoint2D:%(var)s] autorelease];',
        'linesegment':      '\n    *%(param)s = [[[SGLineSegment alloc] initWithLineSegment:%(var)s] autorelease];',
    },
    # -------------------------------
    # Post-call processing for returning the result
    # -------------------------------
    # This provides the code that is used to return a value from a function.
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
        'longintarray':     '\n    return [SGUtils arrayOfIntegers:%(var)s size:%(size)s];',
        'linesarray':       '\n    return [SGLineSegment arrayOfLineSegments:%(var)s size:%(size)s];',
        'point2darray':     '\n    return [SGPoint2D arrayOfPoint2Ds:%(var)s size:%(size)s];',
        'shapearray':       '\n    return [SGShape arrayOfShapes:%(var)s size:%(size)s];',
        
        #return structs
        'vector':           '\n    return [[[SGVector alloc] initWithVector:%(var)s] autorelease];',
    },
    'clean-up':
    {
        'string':           '',
    },
}

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