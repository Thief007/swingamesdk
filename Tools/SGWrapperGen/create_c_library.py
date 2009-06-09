#!/usr/bin/env python
# encoding: utf-8
"""
create_pas_lib.py

Created by Andrew Cain on 2009-06-02.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import logging
import sys

import parser_runner
from sgcache import logger
from print_writer import PrintWriter
from file_writer import FileWriter

#my_writer = PrintWriter()
#my_writer = FileWriter('../../CoreSDK/src/sgsdk1.pas')

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
        'string': 'char *%s',
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
        'psdl_surface': 'void *%s',
        'boolean[0..n - 1][0..n - 1]': 'bool *%s',
        'bitmap[0..n - 1]': 'Bitmap *%s',
        'spritekind': 'SpriteKind %s',
        'longint[0..n - 1]': 'int *%s',
        'vector': 'Vector %s',
        'spriteendingaction': 'SpriteEndingAction %s',
        None: 'void %s'
    },
    'var' : {
        'soundeffect': 'SoundEffect *%s',
        'music': 'Music *%s',
        'timer': 'Timer *%s'
    },
    'out' : {
        'string': 'char *%s',
        'byte': 'unsigned char *%s',
        'color': 'unsigned int *%s',
        'timer': 'Timer *%s'
    }
}

_data_switcher = {
    #Pascal type: what values of this type switch to %s = data value
    'Boolean': '%s != 0'
}

_adapter_type_switcher = {
    None: {
        'single': 'float %s',
        'longint': 'int %s',
        'soundeffect': 'void *%s',
        'music': 'void *%s',
        'string': 'char *%s',
        'boolean': 'int %s',
        'byte': 'unsigned char %s',
        'color': 'unsigned int %s',
        'timer': 'void *%s',
        'resourcekind': 'int %s',
        'uint32': 'unsigned int %s',
        'bitmap': 'void *%s',
        None: 'void %s'
    },
    'var': {
        'soundeffect': 'void *%s',
        'music': 'void *%s',
        'timer': 'void *%s',
        'byte': 'unsigned char *%s'
    },
    'out': {
        'string': 'char *%s',
        'byte': 'unsigned char *%s',
        'color': 'unsigned int *%s',
        'timer': 'void *%s'    
    }
}

_names = []

def _load_data():
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

def arg_visitor(the_arg, for_param):
    '''Called for each argument in a call, performs required mappings'''
    if for_param.data_type.name in _data_switcher:
        #convert data using pattern from _data_switcher
        return _data_switcher[for_param.data_type.name] % the_arg
    else:
        return the_arg

def adapter_type_visitor(the_type, modifier = None):
    '''switch types for the c SwinGame adapter (links to DLL)'''
    return _adapter_type_switcher[modifier][the_type.name.lower() if the_type != None else None]

def adapter_param_visitor(the_param, last):
    return '%s%s' % (
        _adapter_type_switcher[the_param.modifier][the_param.data_type.name.lower()] % the_param.name,
        ', ' if not last else '')

def type_visitor(the_type, modifier = None):
    '''switch types for the c SwinGame library'''
    return _type_switcher[modifier][the_type.name.lower() if the_type != None else None]

def param_visitor(the_param, last):
    return '%s%s' % (
        type_visitor(the_param.data_type, the_param.modifier) % the_param.name,
        ', ' if not last else '')

def method_visitor(the_method, other):
    details = the_method.to_keyed_dict(
        other['param visitor'], other['type visitor'], other['arg visitor'])
    
    other['header writer'].write('%(return_type)s' % details % details['uname'])
    other['header writer'].write('(%(params)s);' % details) 
    other['header writer'].writeln('\n')
    
    if other['c writer'] != None: 
        if the_method.is_function:
            #%(calls.name)s(%(calls.args)s)
            details['the_call'] = other['arg visitor']('%(calls.name)s(%(calls.args)s)' % details, the_method.return_type)
            other['c writer'].write(_module_c_function % details % the_method.uname)
        else:
            other['c writer'].write(_module_c_method % details)

def write_c_lib_adapter(the_file):
    '''Write the c library adapter - header file that matches DLL'''
    file_writer = FileWriter('./out/%s.h'% the_file.name)
    file_writer.write(_header)
    
    #visit the methods of the library
    other = {
        'header writer': file_writer,
        'c writer': None,
        'type visitor': adapter_type_visitor,
        'param visitor': adapter_param_visitor,
        'arg visitor': None
    }
    the_file.members[0].visit_methods(method_visitor, other)
    file_writer.write(_footer)
    file_writer.close()

def write_c_methods_for(member, other):
    '''Write out a single c member'''
    
    member.visit_methods(method_visitor, other)
    
    #check nothing else...
    assert(len(member.fields) == 0, 'Error with number of fields in module')
    assert(len(member.properties) == 0, 'Error with number of properties in module')

def write_c_type_for(member, other):
    '''Write out a single c member'''
    
    assert member.is_class or member.is_struct or member.is_enum
    
    if member.is_class:
        #convert to resource pointer
        if member.is_pointer_wrapper:
            assert len(member.fields) == 1
            the_type = member.fields['pointer'].data_type
            other['header writer'].writeln('typedef %s;\n' % type_visitor(the_type, None) % member.name)
        elif member.is_data_wrapper:
            assert len(member.fields) == 1
            the_type = member.fields['data'].data_type
            other['header writer'].writeln('typedef %s;\n' % type_visitor(the_type) % member.name)
    elif member.is_struct:
        #typedef struct %s_struct { } struct;
        writer = other['header writer']
        writer.write('typedef struct %s_struct { \n' % member.name)
        for field in member.fields:
            writer.writeln('    %s;' % type_visitor(field.data_type) % field.name)
        writer.writeln('} %s;\n' % member.name)
    elif member.is_enum:
        #enum id { list }
        other['header writer'].write('typedef enum %s_enum { ' % member.name)
        for val in member.values:
            other['header writer'].write('%s' % val)
            if val != member.values[-1]: other['header writer'].write(', ')
        other['header writer'].writeln('} %s;\n' % member.name)
    #check nothing else...
    assert(len(member.fields) == 0, 'Error with number of fields in module')
    assert(len(member.properties) == 0, 'Error with number of properties in module')


def write_c_lib_module(the_file):
    '''Write the header and c file to wrap the attached files detials'''
    header_file_writer = FileWriter('./out/%s.h'% the_file.name)
    c_file_writer = FileWriter('./out/%s.c'% the_file.name)
    
    header_file_writer.writeln(_module_header_header % { 
        'name' : the_file.name,
        'pascal_name' : the_file.pascal_name
        })
    
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
        if member.is_module:
            pass
        elif member.is_class or member.is_struct or member.is_enum:
            write_c_type_for(member, other)
        else:
            assert(False, 'Got something I dont know')
    
    #process all methods
    for member in the_file.members:
        if member.is_module:
            write_c_methods_for(member, other)
    
    header_file_writer.write(_module_header_footer)
    
    c_file_writer.close()
    header_file_writer.close()

def file_visitor(the_file, other):
    '''Called for each file read in by the parser'''
    if the_file.name == 'SGSDK_Lib':
        logger.info('Creating C Library Adapter %s.h', the_file.name)
        write_c_lib_adapter(the_file)
    else:
        logger.info('Creating C SwinGame Module %s', the_file.name)
        write_c_lib_module(the_file)

def main():
    logging.basicConfig(level=logging.WARNING,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    
    _load_data()    
    parser_runner.run_for_all_units(file_visitor)


if __name__ == '__main__':
    main()

