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
        'single': 'float',
        'longint': 'int',
        'soundeffect': 'SoundEffect *',
        'music': 'Music *',
        'string': 'NSString *',
        'boolean': 'BOOL',
        'byte': 'unsigned char',
        'timer': 'Timer *',
        'color': 'Color',
        'resourcekind': 'ResourceKind',
        'uint32': 'uint',
        'bitmap': 'Bitmap *',
        'pointer': 'id',
        'single[0..2][0..2]': 'float %s[3][3]',
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
        'sprite': 'Sprite %s',
        'rectangle': 'Rectangle %s',
        'triangle': 'Triangle %s',
        'linesarray': 'LineSegment[] %s',
        'font': 'Font %s',
        'fontalignment': 'FontAlignment %s',
        'fontstyle': 'FontStyle %s',
        'mousebutton': 'MouseButton %s',
        'uint16': 'ushort %s',
        'keycode': 'KeyCode %s',
        'collisionside': 'CollisionSide %s',
        'longint[0..n - 1][0..n - 1]': 'int[][] %s',
        'mapdata': 'MapData %s',
        'animationdata[0..n - 1]': 'AnimationData[] %s',
        'layerdata[0..n - 1]': 'LayerData[] %s',
        'collisiondata': 'CollisionData %s',
        'maptagdetails[0..n - 1][0..23]': 'MapTag[][24] %s',
        'map': 'Map %s',
        'maptag': 'MapTag',
        'maptile': 'MapTile',
        'circle': 'Circle %s',
        'arrayofpoint2d': 'Point2D[] %s',
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
        'string': 'out string %s',
        'byte': 'out byte %s',
        'color': 'out Color %s',
        'timer': 'out Timer %s',
        'point2d': 'out Point2D %s',
        'longint': 'out int %s',
        'linesegment': 'out LineSegment %s',
        'linesarray': 'out LineSegment[] %s',
        'matrix2d': 'out Matrix2D %s',
        'arrayofpoint2d': 'out Point2D[] %s',
        'triangle': 'out Triangle %s',
    },
    'return' : {
        None: 'void',
        'boolean': 'BOOL',
        'music': 'Music *',
        'soundeffect': 'SoundEffect *',
        'single': 'float',
        'point2d': 'Point2D',
        'longint': 'int',
        'timer': 'Timer',
        'byte': 'byte',
        'color': 'Color',
        'uint32': 'uint',
        'vector': 'Vector',
        'circle': 'Circle',
        'rectangle': 'Rectangle',
        'linesegment': 'LineSegment',
        'bitmap': 'Bitmap *',
        'collisionside': 'CollisionSide',
        'font': 'Font',
        'map': 'Map',
        'sprite': 'Sprite',
        'fontstyle': 'FontStyle',
        'maptag': 'MapTag',
        'maptile': 'MapTile',
        'string': 'String',
        'linesarray': 'LineSegment *',
        'matrix2d': 'Matrix2D',
        'arrayofpoint2d': 'Point2D *',
        'triangle': 'Triangle',
        'longintarray': 'int *',
        'spriteendingaction': 'SpriteEndingAction',
        'spritekind': 'SpriteKind',
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
        'boolean': '%s != 0',
        'music': 'Music.Create(%s)',
        'soundeffect': 'SoundEffect.Create(%s)',
        'bitmap': 'Bitmap.Create(%s)',
        'font': 'Font.Create(%s)',
        'timer': 'Timer.Create(%s)',
        'map': 'Map.Create(%s)',
        'sprite': 'Sprite.Create(%s)',
        'color': 'System.Drawing.Color.FromArgb(%s)',
        'keycode': '(KeyCode)%s',
        'mousebutton': '(MouseButton)%s',
        'spriteendingaction': '(SpriteEndingAction)%s',
        'spritekind': '(SpriteKind)%s',
        'maptag': '(MapTag)%s',
        'collisionside': '(CollisionSide)%s',
        'fontalignment': '(FontAlignment)%s',
        'fontstyle': '(FontStyle)%s'
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
    },
    #Argument with a literal value
    'arg_lit_val' : 
    {
        #Pascal type: what values of this type switch to %s = data value
        'single': '%sf',
        'self.pointer': 'Pointer',
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
        (fileBaseName, fileExtension)=os.path.splitext(fileName)
    
        fin = open(path + '/' + f)
        data = fin.read()
        fin.close()
    
        setattr(sys.modules[__name__], fileBaseName, data)

main()