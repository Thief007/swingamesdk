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
        
        'pointer': 'id',

        'color': 'Color',

        'freenotifier': 'FreeNotifier',

        'resourcekind': 'ResourceKind',
        'mousebutton': 'MouseButton',
        'keycode': 'KeyCode',
        'spriteendingaction': 'SpriteEndingAction',
        
        
        'single[0..2][0..2]': 'float %s[3][3]',
        'psdl_surface': 'IntPtr %s',
        'boolean[0..n - 1][0..n - 1]': 'bool[][] %s',
        'bitmap[0..n - 1]': 'Bitmap[] %s',
        'spritekind': 'SpriteKind %s',
        'longint[0..n - 1]': 'int[] %s',
        'vector': 'Vector %s',
        'point2d': 'Point2D',
        'point2d[0..2]': 'Point2D %s[3]',
        'point2d[0..n - 1]': 'Point2D[] %s',
        'linesegment': 'LineSegment %s',
        'rectangle': 'Rectangle',
        'triangle': 'Triangle',
        'linesarray': 'LineSegment[] %s',
        'fontalignment': 'FontAlignment %s',
        'fontstyle': 'FontStyle %s',
        'collisionside': 'CollisionSide %s',
        'longint[0..n - 1][0..n - 1]': 'int[][] %s',
        'mapdata': 'MapData %s',
        'animationdata[0..n - 1]': 'AnimationData[] %s',
        'layerdata[0..n - 1]': 'LayerData[] %s',
        'collisiondata': 'CollisionData %s',
        'maptagdetails[0..n - 1][0..23]': 'MapTag[][24] %s',
        'maptag': 'MapTag',
        'maptile': 'MapTile',
        'circle': 'Circle %s',
        'arrayofpoint2d': 'Point2D[] %s',
    },
    'const' : {
        'point2d':      'const Point2D *',
        'linesegment':  'const LineSegment *',
        'rectangle':    'const Rectangle *',
        'matrix2d':     'const Matrix2D *',
        'vector':       'const Vector *',
        'linesarray':   'const NSArray *',
        'triangle':     'const Triangle *',
        'bitmaparray':  'const NSArray *',
        'longintarray': 'const NSArray *',
        'circle':       'const Circle *',
    },
    'var' : {
        'soundeffect': 'SoundEffect *',
        'music': 'Music *',
        'timer': 'Timer *',
        'string': 'char *%s',
        'triangle': 'Triangle *',
        'linesarray': 'LineSegment[] %s',
        'font': 'Font *',
        'bitmap': 'Bitmap *',
        'sprite': 'Sprite *',
        'matrix2d': 'Matrix2D',
        'map': 'Map *',
        'arrayofpoint2d': 'Point2D[] %s',
    },
    'out' : {
        'string': 'out NSString *',
        'byte': 'out unsigned char *',
        'color': 'out Color *',
        'timer': 'out Timer *',
        'point2d': 'out Point2D *',
        'longint': 'out int *',
        'linesegment': 'out LineSegment *',
        'linesarray': 'out LineSegment[] %s',
        'matrix2d': 'out Matrix2D %s',
        'arrayofpoint2d': 'out Point2D[] %s',
        'triangle': 'out Triangle %s',
    },
    'return' : {
        None: 'void',
        'boolean':      'BOOL',
        
        'music':        'SGMusic *',
        'soundeffect':  'SGSoundEffect *',
        'bitmap':       'SGBitmap *',
        'font':         'SGFont *',
        'map':          'Map *',
        'sprite':       'Sprite *',
        'timer':        'Timer *',
                
        'longintarray':     'int *',
        'arrayofpoint2d':   'Point2D *',
        'linesarray':       'LineSegment *',
                
        'color': 'Color',
        
        'single':       'float',
        'longint':      'int',
        'byte':         'unsigned char',
        'uint32':       'uint',
        'string':       'NSString *',
        
        'collisionside':'CollisionSide',
        'fontstyle':    'FontStyle',
        'maptag':       'MapTag',
        'maptile':      'MapTile',
        'spriteendingaction': 'SpriteEndingAction',
        'spritekind': 'SpriteKind',
        
        'matrix2d':     'Matrix2D',
        'triangle':     'Triangle',
        'point2d':      'Point2D',
        'vector':       'Vector',
        'circle':       'Circle',
        'rectangle':    'Rectangle',
        'linesegment':  'LineSegment',
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
    'temp_return' :
    {
        'string': '%s.ToString()',
        'linesarray': '%s',
        'matrix2d': 'Utils.MatrixFromArray(%s)',
        'arrayofpoint2d': '%s',
        'triangle': 'Utils.TriangleFromArray(%s)',
        'longint': '%s',
        'longintarray': '%s',
    },
    'return_val' : 
    {
        #Pascal type: what values of this type switch to %s = data value
        'boolean':              '%s != 0',
        
        'music':                '[SGMusic createWithId:%s]',
        'soundeffect':          '[SGSoundEffect createWithId:%s]',
        'bitmap':               '[SGBitmap createWithId:%s]',
        'font':                 '[SGFont createWithId:%s]',
        'timer':                '[SGTimer createWithId:%s]',
        'map':                  '[SGMap createWithId:%s]',
        'sprite':               '[SGSprite createWithId:%s]',
        
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