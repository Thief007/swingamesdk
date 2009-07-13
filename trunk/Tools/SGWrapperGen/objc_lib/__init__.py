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
        #'pointer': 'IntPtr %s',
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
        'eventdetails[0..n - 1][0..23]': 'EventDetails[][24] %s',
        'map': 'Map %s',
        'event': 'Event %s',
        'tile': 'Tile %s',
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
        'bitmap': 'Bitmap',
        'collisionside': 'CollisionSide',
        'font': 'Font',
        'map': 'Map',
        'sprite': 'Sprite',
        'fontstyle': 'FontStyle',
        'event': 'Event',
        'tile': 'Tile',
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