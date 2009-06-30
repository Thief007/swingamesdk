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
from sg.sg_cache import logger, find_or_add_file
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



#old
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
        None: 'void %s'
    },
    'const' : {
        'point2d': 'Point2D %s',
        'linesegment': 'LineSegment %s',
        'rectangle': 'Rectangle %s',
        'matrix2d': 'Matrix2D %s',
        'vector': 'Vector %s',
        'linesarray': 'LinesArray %s',
        'triangle': 'Triangle %s',
        'bitmaparray': 'Bitmap %s',
        'longintarray': 'int %s',
        'circle': 'Circle %s',
    },
    'var' : {
        'soundeffect': 'SoundEffect %s',
        'music': 'Music %s',
        'timer': 'Timer %s',
        'string': 'char *%s',
        'triangle': 'Triangle %s',
        'linesarray': 'LinesArray %s',
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
    }
}

_data_switcher = {
    #Pascal type: what values of this type switch to %s = data value
    'Boolean': '%s != 0',
    'pointer.Pointer': 'this.Pointer'
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
        'color': 'uint %s',
        'timer': 'IntPtr %s',
        'resourcekind': 'int %s',
        'uint32': 'uint %s',
        'bitmap': 'IntPtr %s',
        'rectangle': 'Rectangle %s',
        'linesegment': 'LineSegment %s',
        'triangle': 'Triangle %s',
        'point2d': 'Point2D %s',
        'sprite': 'IntPtr %s',
        'linesarray': 'LinesArray %s',
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
        'arrayofpoint2d': 'Point2D *%s',
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
        'soundeffect': 'ref IntPtr %s',
        'music': 'ref IntPtr %s',
        'timer': 'ref IntPtr %s',
        'byte': 'ref byte %s',
        'string': '[MarshalAs(UnmanagedType.LPStr), Out] StringBuilder %s',
        'triangle': 'Triangle %s',
        'linesarray': 'LinesArray %s',
        'font': 'ref IntPtr %s',
        'bitmap': 'ref IntPtr %s',
        'sprite': 'ref IntPtr %s',
        'map': 'ref IntPtr %s'
    },
    'out': {
        'string': '[MarshalAs(UnmanagedType.LPStr), Out] StringBuilder %s',
        'byte': '[Out] out byte %s',
        'color': '[Out] out uint %s',
#        'timer': '[Out] IntPtr %s',
        'point2d': '[Out] out Point2D %s',
#        'triangle': 'Triangle *%s',
        'linesarray': 'LinesArray %s',
        'longint': '[Out] out int %s',
        'linesegment': '[Out] out LineSegment %s',
    },
    'result': {
        'string': '[MarshalAs(UnmanagedType.LPStr), Out] StringBuilder %s',
    }
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
    
    
    
    
    
    
    f = open('./cs_lib/module_header_header.txt')
    _module_header_header = f.read()
    f.close()
    
    f = open('./cs_lib/module_header_footer.txt')
    _module_header_footer = f.read()
    f.close()
    
    f = open('./cs_lib/module_c_header.txt')
    _module_c_header = f.read()
    f.close()
    
    f = open('./cs_lib/module_c_method.txt')
    _module_c_method = f.read()
    f.close()
    
    f = open('./cs_lib/module_c_function.txt')
    _module_c_function = f.read()
    f.close()

def doc_transform(the_docs):
    docLines = the_docs.splitlines(True)
    return ''.join([line if line == docLines[0] else '/// ' + line for line in docLines])

def arg_visitor(the_arg, for_param_or_type):
    '''Called for each argument in a call, performs required mappings'''
    if isinstance(for_param_or_type, SGType):
        the_type = for_param_or_type
    else:
        the_type = for_param_or_type.data_type
        
    if the_type.name in _data_switcher:
        #convert data using pattern from _data_switcher
        result = _data_switcher[the_type.name] % the_arg
    else:
        result = the_arg
    
    if the_type.pointer_wrapper:
        result = result + '.Pointer'
            
    if result in _data_switcher:
        result = _data_switcher[result]
    
    if isinstance(for_param_or_type, SGParameter):
        if for_param_or_type.modifier == 'var':
            result = 'ref ' + result
        elif for_param_or_type.modifier == 'out' and the_type.name.lower() != "string":
            result = 'out ' + result
    
    return result

def adapter_type_visitor(the_type, modifier = None):
    '''switch types for the c SwinGame adapter (links to DLL)'''
    logger.debug('CREATE Cs : Changing adapter type %s - %s', modifier, the_type)
    return _adapter_type_switcher[modifier][the_type.name.lower() if the_type != None else None]

def adapter_param_visitor(the_param, last):
    logger.debug('CREATE Cs : Visiting adapter parameter %s - %s', the_param.modifier, the_param.data_type.name)
    return '%s%s' % (
        _adapter_type_switcher[the_param.modifier][the_param.data_type.name.lower()] % the_param.name,
        ', ' if not last else '')

def type_visitor(the_type, modifier = None):
    '''switch types for the c SwinGame library'''
    logger.debug('CREATE Cs : Changing model type %s - %s', modifier, the_type)
    return _type_switcher[modifier][the_type.name.lower() if the_type != None else None]

def param_visitor(the_param, last):
    return '%s%s' % (
        type_visitor(the_param.data_type, the_param.modifier) % the_param.name,
        ', ' if not last else '')

def method_visitor(the_method, other):
    details = the_method.to_keyed_dict(
        other['param visitor'], other['type visitor'], other['arg visitor'], doc_transform)
    writer = other['file writer']
    
    if the_method.is_constructor:
        details['return_type'] = '%s'
        details['returns'] = 'base.Create('
        details['returns_end'] = ', PtrKind.%s)' % details['in_class']
        details['public'] = 'public '
    elif the_method.is_destructor:
        details['return_type'] = 'void DoFree'
        details['returns_end'] = ''
        details['public'] = 'protected internal override '
    else:
        details['returns_end'] = ''
        details['public'] = 'public '
    
    if other['lib method']: 
        #write out the library versions...
        writer.writeln('[DllImport("SGSDK.dll", CallingConvention=CallingConvention.Cdecl, EntryPoint="%s", CharSet=CharSet.Ansi)]' % details['name'])
        writer.write('private static extern %(return_type)s' % details % 'DLL_' + details['name'])
        writer.write('(%(params)s);' % details) 
        writer.writeln('')
        
        #write out the wrapped version...
        
        writer.writeln((_module_method % details).replace('%s', details['name']) )
        writer.writeln('')
    else:
        writer.writeln((_module_method % details).replace('%s', details['name']) )
        writer.writeln()
        # if the_method.is_function:
        #     #%(calls.name)s(%(calls.args)s)
        #     details['the_call'] = other['arg visitor']('%(calls.name)s(%(calls.args)s)' % details, the_method.return_type)
        #     other['file writer'].write(_module_c_function % details % the_method.uname)
        # else:
        #     other['file writer'].write(_module_c_method % details)

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
        'arg visitor': arg_visitor
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

def write_c_methods_for(member, other):
    '''Write out a single c member'''
    
    writer = other['file writer']
    
    details = member.to_keyed_dict(doc_transform=doc_transform)
    
    writer.writeln(_class_header % details)
    
    writer.indent(2)
    
    member.visit_methods(method_visitor, other)
    
    writer.outdent(2)
    
    writer.writeln(_class_footer)

def write_pointer_wrapper_type(member, other):
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
    file_writer.outdent(2);
    
    file_writer.writeln(_class_footer)
    file_writer.outdent(2)
    
    file_writer.write(_module_footer)
    file_writer.close()

def write_c_type_for(member, other):
    '''Write out a single c member'''
    
    assert member.is_class or member.is_struct or member.is_enum
    
    if member.is_class:
        #convert to resource pointer
        if member.is_pointer_wrapper:
            assert len(member.fields) == 1
            write_pointer_wrapper_type(member, other)
        # elif member.is_data_wrapper:
        #     assert len(member.fields) == 1
        #     the_type = member.fields['data'].data_type
        #     other['file writer'].writeln('typedef %s;\n' % type_visitor(the_type) % member.name)
        # elif member.wraps_array:
        #     assert len(member.fields) == 1
        #     the_type = member.fields['data'].data_type
        #     other['file writer'].writeln('typedef %s;\n' % type_visitor(the_type) % member.name)
        # else:
        #     logger.error('CREATE Cs : Unknown class type for %s', member.uname)
        #     assert false
    elif member.is_struct:
        #typedef struct %s_struct { } struct;
        writer = other['file writer']
        writer.write('typedef struct %s_struct { \n' % member.name)
        for field in member.field_list:
            writer.writeln('    %s;' % type_visitor(field.data_type) % field.name)
        writer.writeln('} %s;\n' % member.name)
    elif member.is_enum:
        #enum id { list }
        other['file writer'].write('typedef enum %s_enum { ' % member.name)
        for val in member.values:
            other['file writer'].write('%s' % val)
            if val != member.values[-1]: other['file writer'].write(', ')
        other['file writer'].writeln('} %s;\n' % member.name)

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
        }
    
    file_writer.indent(2)
    
    #process all types first so they appear at the top of the header files
    for member in the_file.members:
        if member.is_module or member.is_header:
            pass
        elif member.is_class or member.is_struct or member.is_enum:
            write_c_type_for(member, other)
        else:
            assert False
    
    #process all methods
    for member in the_file.members:
        if member.is_module:
            write_c_methods_for(member, other)
    
    file_writer.outdent(2)
    
    file_writer.write(_module_footer)
    file_writer.close()

def post_parse_process(the_file):
    ''' the c modules also wrap array return values and adds length parameters for arrays.'''
    if the_file.name == 'SGSDK':
        return
    
    logger.info('Post Processing %s for C# wrapper creation', the_file.name)
    
    for member in the_file.members:
        for key, method in member.methods.items():
            if method.method_called.was_function:
                #convert string return types
                result_param = SGParameter('result')
                result_param.data_type = method.return_type
                result_param.modifier = 'out'
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
        logger.info('Creating C# Library Adapter %s.cs', the_file.name)
        write_cs_sgsdk_file(the_file)
    else:
        logger.info('Creating C# SwinGame Module %s', the_file.name)
        write_cs_lib_module(the_file)

def main():
    logging.basicConfig(level=logging.INFO,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    
    load_data()
    parser_runner.run_for_all_units(file_visitor)

if __name__ == '__main__':
    main()

