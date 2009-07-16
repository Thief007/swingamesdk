#!/usr/bin/env python
# encoding: utf-8
"""
__init__.py

Created by Andrew Cain on 2009-06-02.
Copyright (c) 2009 Swinburne. All rights reserved.
"""

import sys
import os

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

        'color': 'Color',

        'freenotifier': 'FreeNotifier',

        'resourcekind':         'ResourceKind',
        'mousebutton':          'MouseButton',
        'keycode':              'KeyCode',
        'spriteendingaction':   'SpriteEndingAction',
        'maptag':               'MapTag',
        'spritekind':           'SpriteKind',
        'fontalignment':        'FontAlignment',
        'fontstyle':            'FontStyle',
        
        'collisionside': 'CollisionSide %s',
        
        #'maptile':              'MapTile',
        
        
        'single[0..2][0..2]': 'float %s[3][3]',
        'psdl_surface': 'IntPtr %s',
        'boolean[0..n - 1][0..n - 1]': 'bool[][] %s',
        'bitmap[0..n - 1]': 'Bitmap[] %s',
        'longint[0..n - 1]': 'int[] %s',
        'vector': 'Vector %s',
        'point2d[0..2]': 'Point2D %s[3]',
        'point2d[0..n - 1]': 'Point2D[] %s',
        'linesegment': 'LineSegment %s',
        'linesarray': 'LineSegment[] %s',
        'longint[0..n - 1][0..n - 1]': 'int[][] %s',
        'mapdata': 'MapData %s',
        'animationdata[0..n - 1]': 'AnimationData[] %s',
        'layerdata[0..n - 1]': 'LayerData[] %s',
        'collisiondata': 'CollisionData %s',
        'maptagdetails[0..n - 1][0..23]': 'MapTag[][24] %s',
        'arrayofpoint2d': 'Point2D[] %s',
    },
    'const' : {
        'point2d':      'const SGPoint2D *',
        'linesegment':  'const SGLineSegment *',
        'rectangle':    'const SGRectangle *',
        'vector':       'const SGVector *',
        'linesarray':   'const NSArray *',
        
        'matrix2d':     'const SGMatrix2D *',
        'triangle':     'const SGTriangle *',
        
        'bitmaparray':  'const NSArray *',
        'longintarray': 'const NSArray *',
        'circle':       'const SGCircle *',
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
        
        'linesarray': 'LineSegment[] %s',
        'arrayofpoint2d': 'Point2D[] %s',
    },
    'out' : {
        'string':       'out NSString *',
        'byte':         'out unsigned char *',
        'color':        'out Color *',
        
        #'timer':        'out SGTimer *',
        'point2d':      'out SGPoint2D *',
        
        'longint':      'out int *',
        'linesegment':  'out LineSegment *',
        'linesarray':   'out LineSegment[] %s',
        'matrix2d':     'out Matrix2D %s',
        'arrayofpoint2d': 'out Point2D[] %s',
        'triangle':     'out Triangle %s',
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
                
        'color':        'Color',
        
        #basic types
        'single':       'float',
        'longint':      'int',
        'byte':         'unsigned char',
        'uint32':       'uint',
        'string':       'NSString *',
        
        #enums
        'collisionside':        'CollisionSide',
        'fontstyle':            'FontStyle',
        'maptag':               'MapTag',
        'maptile':              'MapTile',
        'spriteendingaction':   'SpriteEndingAction',
        'spritekind':           'SpriteKind',
        
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
        
        'keycode':              '(KeyCode)%s',
        'mousebutton':          '(MouseButton)%s',
        'spriteendingaction':   '(SpriteEndingAction)%s',
        'spritekind':           '(SpriteKind)%s',
        'maptag':               '(MapTag)%s',
        'collisionside':        '(CollisionSide)%s',
        'fontalignment':        '(FontAlignment)%s',
        'fontstyle':            '(FontStyle)%s'
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
        'self.data': 'data',
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
        'color':            'Color %(var)s;\n    ',
        'longint':          'int %(var)s;\n    ',
        
        'soundeffect':      'SGSoundEffect *%(var)s;\n    ',
        'music':            'SGMusic *%(var)s;\n    ',
        'bitmap':           'SGBitmap *%(var)s;\n    ',
        
        'string':           'char %(var)s[%(size)s];\n    ',
        'matrix2d':         'Matrix2D %(var)s;\n    ',
        'triangle':         'Triangle %(var)s;\n    ',
        
        #arrays for structs
        'linesarray':       'LineSegment %(var)s[%(size)s];\n    ',
        'longintarray':     'int %(var)s[%(size)s];\n    ',
        'bitmaparray' :     'Bitmap %s[%(size)s];\n    ',
        'arrayofpoint2d':   'Point2D %s[%(size)s];\n    ',
        
        #out structs
        'point2d':          'Point2D %(var)s;\n    ',
        'linesegment':      'LineSegment %(var)s;\n    ',
        'vector':           'Vector %(var)s;\n    ',
        
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
        'bitmaparray':      '[%(param)s getObjects:%(var)s];\n    ',
        'longintarray':     '[%(param)s getObjects:%(var)s];\n    ',
        'linesarray':       '[SGLineSegment getLineSegments:%(var)s fromArray:%(param)s maxSize:%(size)s];\n    ',
    },
    'process-out-param': 
    {
        'string':           '\n    %(param)s = [[[NSString alloc] initWithCString:%(var)s encoding:NSASCIIStringEncoding] autorelease];',
        
        #Passed through arrays
        'triangle':         '\n    %(param)s = [[[SGTriangle alloc] initWithTriangle:%(var)s] autorelease];',
        'matrix2d':         '\n    %(param)s = [[[SGMatrix2D alloc] initWithMatrix2D:%(var)s] autorelease];',
        #NSArray types
        'linesarray':       '\n    %(param)s = [[NSArray alloc] todo...];',
        'arrayofpoint2d':   '\n    %(param)s = [[NSArray alloc] todo...];',
        'longintarray':     '\n    %(param)s = [[NSArray alloc] todo...];',
        
        #out structs
        'point2d':          '\n    %(param)s = [[[SGPoint2D alloc] initWithPoint2D:%(var)s] autorelease];',
        'linesegment':      '\n    %(param)s = [[[SGLineSegment alloc] initWithLineSegment:%(var)s] autorelease];',
    },
    'process-result': 
    {
        'boolean':          '\n    return %(var)s;',
        'color':            '\n    return %(var)s;',
        'longint':          '\n    return %(var)s;',
        
        'string':           '\n    return [[NSString alloc] initWithCString:%(var)s encoding:NSASCIIStringEncoding];',
        
        'soundeffect':      '\n    return %(var)s;',
        'music':            '\n    return %(var)s;',
        'bitmap':           '\n    return %(var)s;',
        
        
        #Passed through arrays
        'matrix2d':         '\n    return [SGMatrix2D matrix2DForData:%(var)s];',
        'triangle':         '\n    return [SGTriangle triangleForData:%(var)s];',
        
        #NSArray types
        'linesarray':       '\n    %(param)s = [[NSArray alloc] todo...];',
        'arrayofpoint2d':   '\n    %(param)s = [[NSArray alloc] todo...];',
        'longintarray':     '\n    %(param)s = [[NSArray alloc] todo...];',

        #return structs
        'vector':           '\n    %(param)s = [[[SGVector alloc] initWithVector:%(var)s] autorelease];',
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