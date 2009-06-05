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
my_writer = FileWriter('../../CoreSDK/src/sgsdk1.pas')
_header = ''
_footer = ''
_procedure_lines = None
_function_lines = None
_exports_header = ''
_type_switcher = {
    'Single': 'Single',
    'LongInt': 'LongInt',
    'SoundEffect': 'SoundEffect',
    'Music': 'Music',
    'String': 'PChar'
}

_names = []

def _load_data():
    global _header, _footer, _procedure_lines, _function_lines 
    global _exports_header, _export_line
    
    f = open('./pas_lib/header.txt')
    _header = f.read()
    f.close()
    
    f = open('./pas_lib/footer.txt')
    _footer = f.read()
    f.close()
    
    f = open('./pas_lib/exports_header.txt')
    _exports_header = f.read()
    f.close()
    
    f = open('./pas_lib/export_line.txt')
    _export_line = f.read()
    f.close()
    
    f = open('./pas_lib/procedure.txt')
    _procedure_lines = f.readlines()
    f.close()
    
    f = open('./pas_lib/function.txt')
    _function_lines = f.readlines()
    f.close()

def param_visitor(the_param, last):
    return '%s%s: %s%s' % (
        the_param.modifier + ' ' if the_param.modifier != None else '',
        the_param.name, 
        _type_switcher[the_param.data_type.name], 
        '; ' if not last else ''
        )

def method_visitor(the_method, other):
    global _procedure_lines, _function_lines, _names
    if the_method.return_type == None: 
        lines = _procedure_lines
    else: 
        lines = _function_lines
    
    _names.append(the_method.name)
    
    for line in lines:
        my_writer.write(line % the_method.to_keyed_dict(param_visitor)) 
    my_writer.writeln('\n')

def file_visitor(the_file, other):
    '''Called for each file read in by the parser'''
    global _header, _footer, _exports_header, _export_line, _names
        
    if the_file.name != 'SGSDK_Lib':
        logger.info('skipping %s', the_file.name)
        return
    
    logger.info('Creating Pascal Library')
    
    my_writer.writeln(_header % { 
        'name' : the_file.pascal_name, 
        'uses' : the_file.uses_str(lambda a_file: a_file.pascal_name), 
        'version' : the_file.members[0].version #version from library
        })
    my_writer.indent();
    
    #visit the methods of the library
    the_file.members[0].visit_methods(method_visitor, None)
    
    #write out exports
    my_writer.writeln(_exports_header)
    my_writer.indent()
    for name in _names:
        my_writer.write(_export_line % {'name': name})
    
    my_writer.outdent();
    my_writer.outdent();
    
    my_writer.writeln(_footer)
    my_writer.close()

def main():
    logging.basicConfig(level=logging.DEBUG,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    
    _load_data()    
    parser_runner.run_for_all_units(file_visitor)


if __name__ == '__main__':
    main()

