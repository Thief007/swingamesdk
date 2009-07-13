#!/usr/bin/env python
# encoding: utf-8
"""
create_c_lib.py

Created by Andrew Cain on 2009-06-02.
Copyright (c) 2009 Swinburne. All rights reserved.
"""

import logging
import sys

from sg import parser_runner
from sg.sg_cache import logger, find_or_add_file
from sg.print_writer import PrintWriter
from sg.file_writer import FileWriter
from sg.sg_type import SGType
from sg.sg_parameter import SGParameter

_out_path="../../Templates/C/common/lib"

#templates for adapter
_lib_import_header = ''
_header = ''
_footer = ''

#templates for modules
_module_header_header = ''
_module_header_footer = ''
_module_c_header = ''

_footer = ''
_procedure_lines = None
_function_lines = None
_exports_header = ''
_module_c_method = ''
_module_c_function = ''

_type_switcher = {
    None : {    
        #Pascal type: what it maps to
        'single': 'float %s',
        'longint': 'int %s',
        'soundeffect': 'SoundEffect %s',
        'music': 'Music %s',
        'string': 'const char *%s',
        'boolean': 'bool %s',
        'byte': 'unsigned char %s',
        'timer': 'Timer %s',
        'color': 'Color %s',
        'resourcekind': 'ResourceKind %s',
        'uint32': 'unsigned int %s',
        'bitmap': 'Bitmap %s',
        'pointer': 'void *%s',
        'single[0..2][0..2]': 'float %s[3][3]',
        '^bitmapdata': 'BitmapData *%s',
        '^spritedata': 'SpriteData *%s',
        '^timerdata': 'TimerData *%s',
        'boolean[0..n - 1][0..n - 1]': 'bool *%s',
        'bitmap[0..n - 1]': 'Bitmap *%s',
        'spritekind': 'SpriteKind %s',
        'longint[0..n - 1]': 'int *%s',
        'vector': 'Vector %s',
        'spriteendingaction': 'SpriteEndingAction %s',
        'point2d': 'Point2D %s',
        'point2dptr': 'Point2D *%s',
        'point2d[0..2]': 'Point2D %s[3]',
        'point2d[0..n - 1]': 'Point2D *%s',
        '^linesegment': 'LineSegment *%s',
        'linesegment': 'LineSegment %s',
        'sprite': 'Sprite %s',
        'rectangle': 'Rectangle %s',
        'triangle': 'Triangle %s',
        'linesarray': 'LinesArray %s',
        'linesegmentptr': 'LineSegment *%s',
        'font': 'Font %s',
        'fontalignment': 'FontAlignment %s',
        'fontstyle': 'FontStyle %s',
        'mousebutton': 'MouseButton %s',
        'uint16': 'unsigned short %s',
        'singleptr': 'float *%s',
        'keycode': 'KeyCode %s',
        'bitmapptr': 'Bitmap *%s',
        '^bitmap': 'Bitmap *%s',
        'longintptr': 'int *%s',
        '^longint': 'int *%s',
        'collisionside': 'CollisionSide %s',
        'longint[0..n - 1][0..n - 1]': 'int *%s',
        'mapdata': 'MapData %s',
        'mapanimationdata[0..n - 1]': 'MapAnimationData *%s',
        'maplayerdata[0..n - 1]': 'MapLayerData *%s',
        'mapcollisiondata': 'MapCollisionData %s',
        'tagmapdetails[0..n - 1][0..23]': 'MapTagDetails *%s[24]',
        '^maprecord': 'MapRecord *%s',
        'map': 'Map %s',
        'maptag': 'MapTag %s',
        'maptile': 'MapTile %s',
        'circle': 'Circle %s',
        'arrayofpoint2d': 'Point2D *%s',
        'matrix2d': 'Matrix2D %s',
        None: 'void %s'
    },
    'const' : {
        'point2d': 'const Point2D *%s',
        'linesegment': 'const LineSegment *%s',
        'rectangle': 'const Rectangle *%s',
        'matrix2d': 'const Matrix2D %s',
        'vector': 'const Vector *%s',
        'linesarray': 'const LinesArray %s',
        'triangle': 'const Triangle %s',
        'bitmaparray': 'const Bitmap %s',
        'longintarray': 'const int *%s',
        'circle': 'const Circle *%s',
    },
    'var' : {
        'soundeffect': 'SoundEffect *%s',
        'music': 'Music *%s',
        'timer': 'Timer *%s',
        'string': 'char *%s',
        'triangle': 'Triangle %s',
        'linesarray': 'LinesArray %s',
        'font': 'Font *%s',
        'bitmap': 'Bitmap *%s',
        'sprite': 'Sprite *%s',
        'matrix2d': 'Matrix2D %s',
        'map': 'Map *%s',
        'arrayofpoint2d': 'Point2D *%s',
        'longintarray': 'int *%s',
    },
    'out' : {
        'string': 'char *%s',
        'byte': 'unsigned char *%s',
        'color': 'unsigned int *%s',
        'timer': 'Timer *%s',
        'point2d': 'Point2D *%s',
#        'triangle': 'Triangle *%s'
        'longint': 'int *%s',
        'linesegment': 'LineSegment *%s',
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
        'byte': 'unsigned char %s',
        'color': 'Color %s',
        'uint32': 'unsigned int %s',
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
        'maptag': 'MapTag %s',
        'maptile': 'MapTile %s',
        'string': 'String %s',
        'linesarray': 'LineSegment *%s',
        'matrix2d': 'Matrix2D %s',
        'arrayofpoint2d': 'Point2D *%s',
        'triangle': 'Triangle %s',
        'spriteendingaction': 'SpriteEndingAction %s',
        'spritekind': 'SpriteKind %s',
    },
}

_data_switcher = {
    #Pascal type: what values of this type switch to %s = data value
    'Boolean': '%s != 0'
}

_val_switcher = {
    'True': 'true',
    'False': 'false'
}

_adapter_type_switcher = {
    None: {
        'single': 'float %s',
        'longint': 'int %s',
        'soundeffect': 'void *%s',
        'music': 'void *%s',
        'string': 'const char *%s',
        'boolean': 'int %s',
        'byte': 'unsigned char %s',
        'color': 'unsigned int %s',
        'timer': 'void *%s',
        'resourcekind': 'int %s',
        'uint32': 'unsigned int %s',
        'bitmap': 'void *%s',
        'rectangle': 'Rectangle %s',
        'linesegment': 'LineSegment %s',
        'triangle': 'Triangle %s',
        'point2d': 'Point2D %s',
        'sprite': 'void *%s',
        'linesarray': 'LinesArray %s',
        'font': 'void *%s',
        'fontalignment': 'int %s',
        'fontstyle': 'int %s',
        'mousebutton': 'int %s',
        'uint16': 'unsigned short %s',
        'vector': 'Vector %s',
        'spriteendingaction': 'SpriteEndingAction %s',
        'keycode': 'KeyCode %s',
        'matrix2d': 'Matrix2D %s',
        'collisionside': 'CollisionSide %s',
        'map': 'MapRecord *%s',
        'maptag': 'MapTag %s',
        'maptile': 'MapTile %s',
        'circle': 'Circle %s',
        'arrayofpoint2d': 'Point2D *%s',
        'psdl_surface': 'void *%s',
        'boolean[0..n - 1][0..n - 1]': 'bool *%s',
        'boolean[0..n - 1]': 'bool *%s',
        'bitmap[0..n - 1]': 'void *%s',
        'spritekind': 'int %s',
        'longint[0..n - 1]': 'int *%s',
        'longintarray': 'int *%s',
        'longint[0..n - 1][0..n - 1]': 'int *%s',
        'mapdata': 'MapData %s',
        'mapanimationdata[0..n - 1]': 'MapAnimationData *%s',
        'maplayerdata[0..n - 1]': 'MapLayerData *%s',
        'mapcollisiondata': 'MapCollisionData %s',
        'maptagdetails[0..n - 1][0..23]': 'MapTagDetails *%s[24]',
        'point2dptr': 'Point2D *%s',
        'point2d[0..n - 1]': 'Point2D *%s',
        'point2d[0..2]': 'Point2D %s[3]',
        'linesegmentptr': 'LineSegment *%s',
        'single[0..2][0..2]': 'float %s[3][3]',
        'singleptr': 'float *%s',
        'longintptr': 'int *%s',
        'bitmapptr': 'void *%s',
        None: 'void %s'
    },
    'const' : {
        'point2d': 'const Point2D *%s',
        'linesegment': 'const LineSegment *%s',
        'rectangle': 'const Rectangle *%s',
        'matrix2d': 'const Matrix2D %s',
        'triangle': 'const Triangle %s',
        'vector': 'const Vector *%s',
        'linesarray': 'const LinesArray %s',
        'longintarray': 'const int *%s',
        'bitmaparray': 'const Bitmap %s',
        'circle': 'const Circle *%s'
    },
    'var': {
        'soundeffect': 'SoundEffect *%s',
        'music': 'Music *%s',
        'timer': 'Timer *%s',
        'byte': 'unsigned char *%s',
        'string': 'char *%s',
        'triangle': 'Triangle %s',
        'linesarray': 'LinesArray %s',
        'font': 'Font *%s',
        'bitmap': 'Bitmap *%s',
        'sprite': 'Sprite *%s',
        'map': 'Map *%s'
    },
    'out': {
        'string': 'char *%s',
        'byte': 'unsigned char *%s',
        'color': 'unsigned int *%s',
        'timer': 'void *%s',
        'point2d': 'Point2D *%s',
#        'triangle': 'Triangle *%s',
        'linesarray': 'LinesArray %s',
        'longint': 'int *%s',
        'linesegment': 'LineSegment *%s',
    },
    'result' : {
        'string': 'char *%s',
        'linesarray': 'LineSegment *%s',
        'matrix2d': 'Matrix2D %s',
        'arrayofpoint2d': 'Point2D *%s',
        'triangle': 'Triangle %s',
        'longintarray': 'int *%s',
    },
    # Mapping of the return type of a function
    'return' : {
        None: 'void %s',
        'boolean': 'int %s',
        'music': 'Music %s',
        'soundeffect': 'SoundEffect %s',
        'single': 'float %s',
        'point2d': 'Point2D %s',
        'longint': 'int %s',
        'timer': 'Timer %s',
        'byte': 'unsigned char %s',
        'color': 'unsigned int %s',
        'uint32': 'unsigned int %s',
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
        'maptag': 'MapTag %s',
        'maptile': 'MapTile %s',
        'spriteendingaction': 'SpriteEndingAction %s',
        'spritekind': 'SpriteKind %s',
    }

}

_names = []

def load_data():
    global _lib_import_header, _header
    global _module_header_header, _module_c_header, _module_c_method
    global _module_c_function, _footer, _module_header_footer
    
    f = open('./c_lib/lib_import_header.txt')
    _lib_import_header = f.read()
    f.close()
    
    f = open('./c_lib/lib_header.txt')
    _header = f.read()
    f.close()
    
    f = open('./c_lib/lib_header_footer.txt')
    _footer = f.read()
    f.close()
    
    f = open('./c_lib/module_header_header.txt')
    _module_header_header = f.read()
    f.close()
    
    f = open('./c_lib/module_header_footer.txt')
    _module_header_footer = f.read()
    f.close()
    
    f = open('./c_lib/module_c_header.txt')
    _module_c_header = f.read()
    f.close()
    
    f = open('./c_lib/module_c_method.txt')
    _module_c_method = f.read()
    f.close()
    
    f = open('./c_lib/module_c_function.txt')
    _module_c_function = f.read()
    f.close()

def arg_visitor(arg_str, the_arg, for_param_or_type):
    '''Called for each argument in a call, performs required mappings'''
    if isinstance(for_param_or_type, SGType):
        the_type = for_param_or_type
    else:
        the_type = for_param_or_type.data_type
    
    if arg_str in _val_switcher:
        arg_str = _val_switcher[arg_str]
        
    if the_type.name in _data_switcher:
        #convert data using pattern from _data_switcher
        return _data_switcher[the_type.name] % arg_str
    else:
        return arg_str

def _const_strip_arg_visitor(arg_str, the_arg, for_param_or_type):
    '''
    Called for each argument in a call, performs required mappings.
    This version adds in & operators for all const parameters to 
    enable the creation of a pass by value version.
    '''
    arg_str = arg_visitor(arg_str, the_arg, for_param_or_type)
    
    if isinstance(for_param_or_type, SGType):
        the_type = for_param_or_type
    else:
        the_type = for_param_or_type.data_type
    
    if the_arg != None and isinstance(the_arg, SGParameter) and the_arg.modifier == 'const' and not the_type.is_array:
        return '&' + arg_str
    else:
        return arg_str

def adapter_type_visitor(the_type, modifier = None):
    '''switch types for the c SwinGame adapter (links to DLL)'''
    if (the_type.name.lower() if the_type != None else None) not in _adapter_type_switcher[modifier]:
        logger.error('CREATE C  : Error changing adapter type %s - %s', modifier, the_type)
    
    return _adapter_type_switcher[modifier][the_type.name.lower() if the_type != None else None]

def adapter_param_visitor(the_param, last):
    if not the_param.data_type.name.lower() in _adapter_type_switcher[the_param.modifier]:
        logger.error('CREATE C  : Error visiting adapter parameter %s - %s', the_param.modifier, the_param.data_type.name)
        assert False
    
    return '%s%s' % (
        _adapter_type_switcher[the_param.modifier][the_param.data_type.name.lower()] % the_param.name,
        ', ' if not last else '')

def type_visitor(the_type, modifier = None):
    '''switch types for the c SwinGame library'''
    if not (the_type.name.lower() if the_type != None else None) in _type_switcher[modifier]:
        logger.error('CREATE C  : Error changing model type %s - %s', modifier, the_type)
        assert False
    
    return _type_switcher[modifier][the_type.name.lower() if the_type != None else None]

def param_visitor(the_param, last):
    return '%s%s' % (
        type_visitor(the_param.data_type, the_param.modifier) % the_param.name,
        ', ' if not last else '')

def _const_strip_param_visitor(the_param, last):
    '''
    Called for each parameter in the function/procedure being mapped. 
    This version strips off the const details to enable call by value.
    '''
    if the_param.modifier != 'const' or the_param.data_type.is_array:
        mod = the_param.modifier
    else: mod = None
    
    return '%s%s' % (
        type_visitor(the_param.data_type, mod) % the_param.name,
        ', ' if not last else '')

def method_visitor(the_method, other):
    details = the_method.to_keyed_dict(
        other['param visitor'], other['type visitor'], other['arg visitor'])
    
    other['header writer'].write('%(return_type)s' % details % details['uname'])
    other['header writer'].write('(%(params)s);' % details) 
    other['header writer'].writeln('')
    
    if other['c writer'] != None: 
        if the_method.is_function:
            #%(calls.name)s(%(calls.args)s)
            details['the_call'] = other['arg visitor']('%(calls.name)s(%(calls.args)s)' % details, None, the_method.return_type)
            other['c writer'].write(_module_c_function % details % details['uname'])
        else:
            other['c writer'].write(_module_c_method % details)
            
        if the_method.has_const_params():
            # Create a version with const parameters passed by value
            details = the_method.to_keyed_dict(
                _const_strip_param_visitor, other['type visitor'], _const_strip_arg_visitor)
            details['uname'] = details['uname'] + '_byval'
            details['name'] = details['uname']
            
            if the_method.is_function:    
                details['the_call'] = _const_strip_arg_visitor('%(calls.name)s(%(calls.args)s)' % details, None, the_method.return_type)
                other['c writer'].write(_module_c_function % details % details['uname'])
            else:
                other['c writer'].write(_module_c_method % details)
            
            # Also write to header
            other['header writer'].write('%(return_type)s' % details % details['uname'])
            other['header writer'].write('(%(params)s);' % details) 
            other['header writer'].writeln('')
            
    
    return other

def write_c_lib_header(the_file, for_others = False):
    '''Write the c library adapter - header file that matches DLL'''
    file_writer = FileWriter('%s/%s.h'% (_out_path, the_file.name))
    file_writer.write(_header)
    
    #visit the methods of the library
    other = {
        'header writer': file_writer,
        'c writer': None,
        'type visitor': adapter_type_visitor,
        'param visitor': adapter_param_visitor,
        'arg visitor': None
    }
    
    for a_file in the_file.uses:
        if a_file.name != None and ((not for_others) or (not a_file.has_body)):
            file_writer.writeln(_lib_import_header % {'name': a_file.name})

    file_writer.writeln('')
    
    the_file.members[0].visit_methods(method_visitor, other)
    
    file_writer.writeln(_footer)
    file_writer.close()

def write_c_methods_for(member, other):
    '''Write out a single c member'''
    
    member.visit_methods(method_visitor, other)

def write_c_type_for(member, other):
    '''Write out a single c member'''
    
    assert member.is_class or member.is_struct or member.is_enum or member.is_type
    
    if member.is_class or member.is_type or (member.is_struct and member.wraps_array):
        #convert to resource pointer
        if member.is_pointer_wrapper:
            # assert len(member.fields) == 1
            the_type = member.data_type
            other['header writer'].writeln('typedef %s;\n' % adapter_type_visitor(the_type, None) % member.name)
        elif member.is_data_wrapper:
            assert len(member.fields) == 1
            the_type = member.fields['data'].data_type
            other['header writer'].writeln('typedef %s;\n' % adapter_type_visitor(the_type) % member.name)
        elif member.wraps_array:
            assert len(member.fields) == 1
            the_type = member.fields['data'].data_type
            other['header writer'].writeln('typedef %s;\n' % adapter_type_visitor(the_type) % member.name)
        else:
            logger.error('CREATE C  : Unknown class type for %s', member.uname)
            assert False
    elif member.is_struct:
        #typedef struct %s_struct { } struct;
        writer = other['header writer']
        writer.write('typedef struct %s_struct { \n' % member.name)
        for field in member.field_list:
            writer.writeln('    %s;' % adapter_type_visitor(field.data_type) % field.name)
        writer.writeln('} %s;\n' % member.name)
    elif member.is_enum:
        #enum id { list }
        other['header writer'].write('typedef enum %s_enum { \n    ' % member.name)
        other['header writer'].write( ',\n    '.join([v for v in member.values]))
        other['header writer'].writeln('\n} %s;\n' % member.name)

def write_c_lib_module(the_file):
    '''Write the header and c file to wrap the attached files detials'''
    header_file_writer = FileWriter('%s/%s.h'% (_out_path, the_file.name))

    if the_file.has_body:
        c_file_writer = FileWriter('%s/%s.c'% (_out_path, the_file.name))
    else: c_file_writer = None
    
    header_file_writer.writeln(_module_header_header % { 
        'name' : the_file.name,
        'pascal_name' : the_file.pascal_name
        })
    
    if the_file.has_body:
        c_file_writer.writeln(_module_c_header % { 
            'name' : the_file.name,
            'pascal_name' : the_file.pascal_name
            })
    
    other = {
        'header writer': header_file_writer,
        'c writer': c_file_writer,
        'type visitor': type_visitor,
        'param visitor': param_visitor,
        'arg visitor': arg_visitor
    }
    
    for a_file in the_file.uses:
        if a_file.name != None:
            header_file_writer.writeln(_lib_import_header % {'name': a_file.name})
    header_file_writer.writeln('')
    
    #process all types first so they appear at the top of the header files
    for member in the_file.members:
        if member.is_module or member.is_header:
            pass
        elif member.is_class or member.is_struct or member.is_enum or member.is_type:
            write_c_type_for(member, other)
        else:
            assert False
    #process all methods
    for member in the_file.members:
        if member.is_module:
            write_c_methods_for(member, other)
    
    if the_file.has_body:
        #close the c file
        c_file_writer.writeln()
        c_file_writer.close()
    
    header_file_writer.writeln(_module_header_footer)    
    header_file_writer.close()

def post_parse_process(the_file):
    ''' the c modules also wrap array return values and adds length parameters for arrays.'''
    if the_file.name == 'SGSDK':
        return
    
    logger.info('Post Processing %s for C wrapper creation', the_file.name)
    
    for member in the_file.members:
        for key, method in member.methods.items():
            if method.method_called.was_function:
                #convert string return types
                result_param = SGParameter('result')
                result_param.data_type = method.return_type
                result_param.modifier = 'var'
                param_list = list(method.params)
                param_list.append(result_param)
                method.params = tuple(param_list)
                arg_list = list(method.args)
                arg_list.append(result_param)
                method.args = arg_list
                method.return_type = None
            if method.method_called.has_length_params:
                #add length parameters to this method
                for param in method.method_called.params:
                    if param.is_length_param:
                        param_list = list(method.params)
                        param_list.append(param)
                        method.params = tuple(param_list)
                        arg_list = list(method.args)
                        arg_list.append(param)
                        method.args = arg_list

def file_visitor(the_file, other):
    '''Called for each file read in by the parser'''
    post_parse_process(the_file)
    if the_file.name == 'SGSDK':
        logger.info('Creating C Library Adapter %s.h', the_file.name)
        write_c_lib_header(the_file)
    else:
        logger.info('Creating C SwinGame Module %s', the_file.name)
        write_c_lib_module(the_file)

def main():
    logging.basicConfig(level=logging.WARNING,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    
    load_data()
    parser_runner.run_for_all_units(file_visitor)

if __name__ == '__main__':
    main()

