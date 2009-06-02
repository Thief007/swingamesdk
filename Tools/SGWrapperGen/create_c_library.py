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
_lib_import_header = ''
_footer = ''
_procedure_lines = None
_function_lines = None
_exports_header = ''
_type_switcher = {
    'Single': 'float',
    'LongInt': 'long',
    'SoundEffect': 'void *',
    'Music': 'void *',
    'String': 'char *',
    'Boolean': 'long',
    None: 'void'
}

_names = []

def _load_data():
    global _lib_import_header, _header
    
    f = open('./c_lib/lib_import_header.txt')
    _lib_import_header = f.read()
    f.close()
    
    f = open('./c_lib/lib_header.txt')
    _header = f.read()
    f.close()


def type_visitor(the_type):
    return _type_switcher[the_type.name if the_type != None else None]

def param_visitor(the_param, last):
    return '%s%s %s%s' % (
        _type_switcher[the_param.data_type.name], 
        '*' if the_param.modifier in ['var','out'] else '',
        the_param.name,
        ', ' if not last else '')

def method_visitor(the_method, file_writer):
    file_writer.write('%(return_type)s %(name)s(%(params)s);' % the_method.to_keyed_dict(param_visitor, type_visitor)) 
    file_writer.writeln('\n')

def file_visitor(the_file):
    '''Called for each file read in by the parser'''
    global _header, _lib_import_header
    
    #for the moment... just the library
    if the_file.name != 'SGSDK_Lib':
        logger.info('skipping %s', the_file.name)
        return
    
    file_writer = FileWriter('./out/%s.h'% the_file.name)
    file_writer.write(_header)
    
    # for a_file in the_file.uses:
    #     if a_file.name != None:
    #         file_writer.writeln(_lib_import_header % {'name': a_file.name})
    
    #visit the methods of the library
    the_file.members[0].visit_methods(lambda the_method: method_visitor(the_method, file_writer))
    
    #my_writer.writeln(_footer)
    file_writer.close()

def main():
    logging.basicConfig(level=logging.INFO,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    
    _load_data()    
    parser_runner.run_for_all_units(file_visitor)


if __name__ == '__main__':
    main()

