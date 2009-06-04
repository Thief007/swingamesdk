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

#templates for modules
_module_header_header = ''
_module_c_header = ''

_footer = ''
_procedure_lines = None
_function_lines = None
_exports_header = ''
_module_c_method = ''
_module_c_function = ''

_type_switcher = {
    #Pascal type: what it maps to
    'Single': 'float ',
    'LongInt': 'int ',
    'SoundEffect': 'SoundEffect ',
    'Music': 'Music ',
    'String': 'char *',
    'Boolean': 'bool ',
    None: 'void '
}

_data_switcher = {
    #Pascal type: what values of this type switch to %s = data value
    'Boolean': '%s == -1'
}

_adapter_type_switcher = {
    'Single': 'float ',
    'LongInt': 'int ',
    'SoundEffect': 'void *',
    'Music': 'void *',
    'String': 'char *',
    'Boolean': 'int ',
    None: 'void '
}

_names = []

def _load_data():
    global _lib_import_header, _header
    global _module_header_header, _module_c_header, _module_c_method
    global _module_c_function
    
    f = open('./c_lib/lib_import_header.txt')
    _lib_import_header = f.read()
    f.close()
    
    f = open('./c_lib/lib_header.txt')
    _header = f.read()
    f.close()
    
    f = open('./c_lib/module_header_header.txt')
    _module_header_header = f.read()
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

def arg_visitor(the_arg, arg_type):
    '''Called for each argument in a call, performs required mappings'''
    if arg_type.name in _data_switcher:
        #convert data using pattern from _data_switcher
        return _data_switcher[arg_type.name] % the_arg
    else:
        return the_arg

def adapter_type_visitor(the_type):
    '''switch types for the c SwinGame adapter (links to DLL)'''
    return _adapter_type_switcher[the_type.name if the_type != None else None]

def adapter_param_visitor(the_param, last):
    return '%s%s%s%s' % (
        _adapter_type_switcher[the_param.data_type.name], 
        '*' if the_param.modifier in ['var','out'] else '',
        the_param.name,
        ', ' if not last else '')

def type_visitor(the_type):
    '''switch types for the c SwinGame library'''
    return _type_switcher[the_type.name if the_type != None else None]

def param_visitor(the_param, last):
    return '%s%s%s%s' % (
        _type_switcher[the_param.data_type.name], 
        '*' if the_param.modifier in ['var','out'] else '',
        the_param.name,
        ', ' if not last else '')

def method_visitor(the_method, other):
    details = the_method.to_keyed_dict(
        other['param visitor'], other['type visitor'], other['arg visitor'])
    
    other['header writer'].write('%(return_type)s%(uname)s(%(params)s);' % details) 
    other['header writer'].writeln('\n')
    
    if other['c writer'] != None: 
        if the_method.is_function:
            #%(calls.name)s(%(calls.args)s)
            details['the_call'] = other['arg visitor']('%(calls.name)s(%(calls.args)s)' % details, the_method.return_type)
            other['c writer'].write(_module_c_function % details)
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
    file_writer.close()

def write_c_methods_for(member, other):
    '''Write out a single c member'''
    
    member.visit_methods(method_visitor, other)
    
    #check nothing else...
    assert(len(member.fields) == 0, 'Error with number of fields in module')
    assert(len(member.properties) == 0, 'Error with number of properties in module')

def write_c_type_for(member, other):
    '''Write out a single c member'''
    
    assert(member.is_class or member.is_struct, 'type must be a class/struct...')
    
    if member.is_class:
        #convert to resource pointer
        assert(len(member.fields) == 1, 'class should have one field (the pointer)')
        other['header writer'].writeln('typedef void* %s;\n' % member.name)
    
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
        elif member.is_class:
            write_c_type_for(member, other)
        else:
            assert(False, 'Got something I dont know')
    
    #process all methods
    for member in the_file.members:
        if member.is_module:
            write_c_methods_for(member, other)
    
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
    logging.basicConfig(level=logging.INFO,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    
    _load_data()    
    parser_runner.run_for_all_units(file_visitor)


if __name__ == '__main__':
    main()

