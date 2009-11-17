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
        'single':   '%s As Single',
        'longint':  '%s As Integer',
        'string':   '%s As String',
        'boolean':  '%s As Boolean',
        'byte':     '%s As Byte',
        'uint32':   '%s As System.UInt32',
        'uint16':   '%s As System.UInt16',
        
        #SwinGame resources
        'soundeffect':      '%s As SoundEffect',
        'music':            '%s As Music',
        'sprite':           '%s As Sprite',
        'font':             '%s As Font',
        'bitmap':           '%s As Bitmap',
        'timer':            '%s As Timer',
        'map':              '%s As Map',
        'shapeprototype':   '%s As ShapePrototype',
        'shape':            '%s As Shape',
        'triangle':         '%s As Triangle',
        # 'rectangle':    'SGRectangle',
        # 'circle':       'SGCircle',
        
        #elements of arrays
        'point2d':          '%s As Point2D',
        
        #arrays from within structs
        'point2darray':     '%s() As Point2D',
        'shapearray':       '%s() As Shape',
        
        'pointer': '%s As IntPtr',
        
        'color': '%s As System.Drawing.Color',
        
        #function pointers
        'freenotifier':     '%s As FreeNotifier',
        # 'shapedrawingfn':   '%s As shape_drawing_fn',
        
        #Enums
        'resourcekind':         '%s As ResourceKind',
        'mousebutton':          '%s As MouseButton',
        'keycode':              '%s As KeyCode',
        'spriteendingaction':   '%s As SpriteEndingAction',
        'maptag':               '%s As MapTag',
        'spritekind':           '%s As SpriteKind',
        'fontalignment':        '%s As FontAlignment',
        'fontstyle':            '%s As FontStyle',
        'shapekind':            '%s As ShapeKind',
    },
    'const' : {
        #records
        'point2d':      'ByRef %s As Point2D',
        'linesegment':  'ByRef %s As LineSegment',
        'rectangle':    'ByRef %s As Rectangle',
        'vector':       'ByRef %s As Vector',
        'matrix2d':     'ByVal %s As Matrix2D',
        'triangle':     'ByVal %s As Triangle',
        'circle':       'ByRef %s As Circle',
        
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

_adapter_type_switcher = {
    'lib_' : {
        #Primitives
        'single': 'ByVal %s As Single',
        'longint': 'ByVal %s As Integer',
        'string': '<MarshalAs(UnmanagedType.LPStr), In> ByVal %s As String',
        'boolean': 'ByVal %s As Integer',
        'byte': 'ByVal %s As Byte',
        'color': 'ByVal %s As Integer',
        'uint32': 'ByVal %s As System.UInt32',
        'uint16': 'ByVal %s As System.UInt16',
        
        #structs
        'rectangle':    'ByVal %s As Rectangle',
        'point2d':      'ByVal %s As Point2D',
        'triangle':     '<MarshalAs(UnmanagedType.LPArray, SizeConst:=3), In> ByVal %s() As Point2D',
        'circle':       'ByVal %s As Circle',
        'linesegment':  'ByVal %s As LineSegment',
        'vector':       'ByVal %s As Vector',
        'maptile':      'ByVal %s As MapTile',
        
        #Enums
        'resourcekind':     'ByVal %s As Integer',
        'fontalignment':    'ByVal %s As Integer',
        'fontstyle':        'ByVal %s As Integer',
        'mousebutton':      'ByVal %s As Integer',
        'spriteendingaction': 'ByVal %s As Integer',
        'keycode':          'ByVal %s As Integer',
        'collisionside':    'ByVal %s As Integer',
        'maptag':           'ByVal %s As Integer',
        'spritekind':       'ByVal %s As Integer',
        'shapekind':        'ByVal %s As Integer',
        
        #Arrays
        'point2darray': '<MarshalAs(UnmanagedType.LPArray, SizeParamIndex:=%s), In> ByVal %s() As Point2D',
        'longintarray': '<MarshalAs(UnmanagedType.LPArray, SizeParamIndex:=%s), In> ByVal %s() As Integer',
        
        #Resources
        'music':        'ByVal %s As IntPtr',
        'soundeffect':  'ByVal %s As IntPtr',
        'music':        'ByVal %s As IntPtr',
        'shapeprototype': 'ByVal %s As IntPtr',
        'timer':        'ByVal %s As IntPtr',
        'shape':        'ByVal %s As IntPtr',
        'bitmap':       'ByVal %s As IntPtr',
        'font':         'ByVal %s As IntPtr',
        'sprite':       'ByVal %s As IntPtr',
        'map':          'ByVal %s As IntPtr',
        
        #Functions
        'freenotifier': 'ByVal %s As FreeNotifier',
    },
    'lib_var' : {
        #Resources
        'soundeffect':  'ByRef %s As IntPtr',
        'music':        'ByRef %s As IntPtr',
        'timer':        'ByRef %s As IntPtr',
        'font':         'ByRef %s As IntPtr',
        'bitmap':       'ByRef %s As IntPtr',
        'sprite':       'ByRef %s As IntPtr',
        'map':          'ByRef %s As IntPtr',
        'shapeprototype': 'ByRef %s As IntPtr',
        'shape':        'ByRef %s IntPtr',
        
        #Primitives
        'byte': 'ByRef %s As Byte',
        
        'string': '[MarshalAs(UnmanagedType.LPStr), In, Out] ByVal %s As StringBuilder',
        'triangle': '[MarshalAs(UnmanagedType.LPArray, SizeConst=3), In, Out] ByVal %s() As Point2D',
    },
    'lib_result' : {
        'string':       '<MarshalAs(UnmanagedType.LPStr), Out> ByVal %s As StringBuilder',
        'linesarray':   '<MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s), Out> ByVal %s() As LineSegment',
        'matrix2d':     '<MarshalAs(UnmanagedType.LPArray, SizeConst=9), Out> ByVal %s As Single(,)',
        'point2darray': '<MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s), Out> ByVal %s() As Point2D',
        'triangle':     '<MarshalAs(UnmanagedType.LPArray, SizeConst=3), Out> ByVal %s() As Point2D',
        'longintarray': '<MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s), Out> ByVal %s() As int',
    },
    'lib_const' : {
        'point2d':      'ByRef %s As Point2D',
        'linesegment':  'ByRef %s As LineSegment',
        'rectangle':    'ByRef %s As Rectangle',
        'circle':       'ByRef %s As Circle',
        'vector':       'ByRef %s As Vector',
    
        # Arrays are passed by reference already
        'matrix2d':     '<MarshalAs(UnmanagedType.LPArray, SizeConst=9), In> ByVal %s(,) As Single',
        'triangle':     '<MarshalAs(UnmanagedType.LPArray, SizeConst=3), In> ByVal %s() As Point2D',
        'linesarray':   '<MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s)> ByVal %s() As LineSegment',
        'longintarray': '<MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s)> ByVal %s() As Integer',
        'bitmaparray':  '<MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s)> ByVal %s() As Bitmap',
        'point2darray': '<MarshalAs(UnmanagedType.LPArray, SizeParamIndex=%s)> ByVal %s() As Point2D',
    },
    'lib_out' : {
        'string':       '<MarshalAs(UnmanagedType.LPStr), Out> ByVal %s As StringBuilder',
        'byte':         '<Out> ByRef %s As Byte',
        'color':        '<Out> ByRef %s As Integer',
        'single':       '<Out> ByRef %s As Single',
        'point2d':      '<Out> ByRef %s As Point2D',
        'longint':      '<Out> ByRef %s As Integer',
        'linesegment':  '<Out> ByRef %s As LineSegment',
    },
    None : {    
        #Pascal type: what it maps to
        'single':   'ByVal %s As Single',
        'longint':  'ByVal %s As Integer',
        'string':   'ByVal %s As String',
        'boolean':  'ByVal %s As Integer',
        'byte':     'ByVal %s As Byte',
        'uint32':   'ByVal %s As System.UInt32',
        'uint16':   'ByVal %s As System.UInt16',
        
        #SwinGame resources
        'soundeffect':      'ByVal %s As IntPtr',
        'music':            'ByVal %s As IntPtr',
        'sprite':           'ByVal %s As IntPtr',
        'font':             'ByVal %s As IntPtr',
        'bitmap':           'ByVal %s As IntPtr',
        'timer':            'ByVal %s As IntPtr',
        'map':              'ByVal %s As IntPtr',
        'shapeprototype':   'ByVal %s As IntPtr',
        'shape':            'ByVal %s As IntPtr',
        'triangle':         'ByVal %s As IntPtr',
        
        #elements of arrays
        'point2d':          'ByVal %s As Point2D',
        
        #arrays from within structs
        'point2darray':     'ByVal %s() As Point2D',
        
        #'pointer': 'id',
        'color': 'ByVal %s As System.Drawing.Color',
        
        #function pointers
        'freenotifier':     'ByVal %s As FreeNotifier',
        #'shapedrawingfn':   'ShapeDrawingFn',
        
        #Enums
        'resourcekind':         'ByVal %s As Integer',
        'mousebutton':          'ByVal %s As Integer',
        'keycode':              'ByVal %s As Integer',
        'spriteendingaction':   'ByVal %s As Integer',
        'maptag':               'ByVal %s As Integer',
        'spritekind':           'ByVal %s As Integer',
        'fontalignment':        'ByVal %s As Integer',
        'fontstyle':            'ByVal %s As Integer',
        'shapekind':            'ByVal %s As Integer',
    },
    'const' : {
        #records
        'point2d':      'ByVal %s As Point2D',
        'linesegment':  'ByVal %s As LineSegment',
        'rectangle':    'ByVal %s As Rectangle',
        'vector':       'ByVal %s As Vector',
        'matrix2d':     'ByVal %s(,) As Single',
        'triangle':     'ByVal %s() As Point2D',
        'circle':       'ByVal %s As Circle',
        
        #Arrays
        'linesarray':       'ByVal %s() As LineSegment',
        'bitmaparray':      'ByVal %s() As Bitmap',
        'longintarray':     'ByVal %s() As Integer',
        'point2darray':     'ByVal %s() As Point2D',
        'shapearray':       'ByVal %s() As Shape',
    },
    'var' : {
        'soundeffect':      'ByRef %s As IntPtr',
        'music':            'ByRef %s As IntPtr',
        'timer':            'ByRef %s As IntPtr',
        'bitmap':           'ByRef %s As IntPtr',
        'sprite':           'ByRef %s As IntPtr',
        'map':              'ByRef %s As IntPtr',
        'font':             'ByRef %s As IntPtr',
        'shapeprototype':   'ByRef %s As IntPtr',
        'shape':            'ByRef %s As IntPtr',
        
        'triangle':     'ByVal %s() As Point2D',
        'matrix2d':     'ByVal %s(,) As Single',
    },
    'out' : {
        'string':       'ByVal %s As StringBuilder',
        'byte':         'ByRef %s As Byte',
        'color':        'ByRef %s As Integer',
        
        'point2d':      'ByVal %s() Point2D',
        
        'longint':      'ByRef %s As Integer',
        'single':       'ByRef %s As Single',
        'linesegment':  'ByRef %s As LineSegment',
        'linesarray':   'ByVal %s() As LineSegment',
        'matrix2d':     'ByVal %s(,) As Single',
        'point2darray': 'ByVal %s() As Point2D',
        'triangle':     'ByVal %s() As Point2D',
    },
    'result_param' : {
        'matrix2d':     'ByVal %s(,) As Single',
        'triangle':     'ByVal %s() As Point2D',
        'string':       'ByVal %s As StringBuilder',
        'longintarray': 'ByVal %s() As Integer',
        'point2darray': 'ByVal %s() As Point2D',
        'linesarray':   'ByVal %s() As LineSegment',
        'shapearray':   'ByVal %s() As Shape',
    },
    'return' : {
        #SwinGame resources
        'music':            'IntPtr',
        'soundeffect':      'IntPtr',
        'bitmap':           'IntPtr',
        'font':             'IntPtr',
        'map':              'IntPtr',
        'sprite':           'IntPtr',
        'timer':            'IntPtr',
        'shapeprototype':   'IntPtr',
        'shape':            'IntPtr',
        
        #Arrays
        'longintarray':     'Integer()',
        'point2darray':     'Point2D()',
        'linesarray':       'LineSegment()',
        'shapearray':       'Shape()',
                
        #basic types
        None:           '',
        'boolean':      'Integer',
        'color':        'Integer',
        'single':       'Single',
        'longint':      'Integer',
        'byte':         'Byte',
        'uint32':       'System.UInt32',
        'string':       'StringBuilder',
        
        #enums
        'collisionside':        'Integer',
        'fontstyle':            'Integer',
        'maptag':               'Integer',
        'maptile':              'Integer',
        'spriteendingaction':   'Integer',
        'spritekind':           'Integer',
        'shapekind':            'Integer',
        
        #arrays + structs
        'matrix2d':     'Single(,)',
        'triangle':     'Point2D()',
        'point2d':      'Point2D',
        'vector':       'Vector',
        'circle':       'Circle',
        'rectangle':    'Rectangle',
        'linesegment':  'LineSegment',
        
        #function pointers
        # 'shapedrawingfn':   'shape_drawing_fn',
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