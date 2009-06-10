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
from SGParameter import SGParameter

#my_writer = PrintWriter()
my_writer = FileWriter('../../CoreSDK/src/sgsdk1.pas')
_header = ''
_footer = ''
_procedure_lines = None
_function_lines = None
_function_as_procedure = None
_exports_header = ''
_type_switcher = {
    'single': 'Single',
    'longint': 'LongInt',
    'soundeffect': 'SoundEffect',
    'music': 'Music',
    'string': 'PChar',
    'color': 'LongWord',
    'timer': 'Timer',
    'byte': 'Byte',
    'resourcekind': 'ResourceKind',
    'uint32': 'UInt32',
    'bitmap': 'Bitmap'
}

_names = []

def _load_data():
    global _header, _footer, _procedure_lines, _function_lines 
    global _exports_header, _export_line, _function_as_procedure
    
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
    
    f = open('./pas_lib/function_as_procedure.txt')
    _function_as_procedure = f.readlines()
    f.close()

def param_visitor(the_param, last):
    if the_param.modifier == 'out' and the_param.data_type.name.lower() == 'string':
        return 'var %s: %s%s' % (
            the_param.name, 
            _type_switcher[the_param.data_type.name.lower()], 
            '; ' if not last else ''
            )
    else:
        return '%s%s: %s%s' % (
            the_param.modifier + ' ' if the_param.modifier != None else '',
            the_param.name, 
            _type_switcher[the_param.data_type.name.lower()], 
            '; ' if not last else ''
            )

def arg_visitor(the_arg, for_param):
    '''Ensures data type consistency for all var/out parameters'''
    if for_param.modifier in ['var','out']:
        #ensure exact same type for PChar and Color
        if for_param.data_type.name.lower() in ['string', 'color']:
            return the_arg + '_temp'
        
    return the_arg

def method_visitor(the_method, other):
    data = the_method.to_keyed_dict(param_visitor, arg_visitor = arg_visitor)
    
    if the_method.was_function:
        if the_method.params[-1].data_type.name.lower() != 'string':
            logger.error('CREATE LIB: Unknown parameter return type in %s.', the_method.name)
            assert False
        lines = _function_as_procedure
        data['return_type'] = _type_switcher[the_method.params[-1].data_type.name.lower()]
    elif the_method.return_type == None: 
        lines = _procedure_lines
    else: 
        lines = _function_lines
    
    if len(the_method.local_vars) > 0:
        temp = '\n  var\n'
        temp_process_result = ''
        for local_var in the_method.local_vars:
            temp += '    %s: %s;\n' % (local_var.name, local_var.data_type)
            if local_var.data_type.name.lower() == 'string':
                temp_process_result += '\n      StrCopy(%s, PChar(%s));' % (local_var.name[:-5], local_var.name)
            elif local_var.data_type.name.lower() == 'color':
                temp_process_result += '\n      %s := %s;' % (local_var.name[:-5], local_var.name)
            else:
                logger.error('CREATE LIB: Unknow local variable type in %s', the_method.name)
        data['vars'] = temp[:-1]
        data['process_result'] = temp_process_result
    else:
        data['vars'] = ''
        data['process_result'] = ''
    
    _names.append(the_method.name)
    
    for line in lines:
        my_writer.write(line % data) 
    my_writer.write('\n')

def post_parse_process(the_lib):
    logger.info('Post Processing library for Pascal library creation')
    
    for key, method in the_lib.methods.items():
        for param in method.params:
            if param.modifier in ['var', 'out']:
                if param.data_type.name.lower() in ['string','color']:
                    local_var = SGParameter(param.name + '_temp')
                    local_var.data_type = param.data_type
                    method.local_vars.append(local_var)

def file_visitor(the_file, other):
    '''Called for each file read in by the parser'''
    if the_file.name != 'SGSDK_Lib':
        logger.info('skipping %s', the_file.name)
        return
    
    post_parse_process(the_file.members[0])
    
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
        if name == _names[-1]:
            my_writer.write((_export_line % {'name': name})[:-2])
            my_writer.write(';')
        else:
            my_writer.write(_export_line % {'name': name})
    
    my_writer.outdent();
    my_writer.outdent();
    
    my_writer.writeln(_footer)
    my_writer.close()

def main():
    logging.basicConfig(level=logging.WARNING,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    
    _load_data()    
    parser_runner.run_for_all_units(file_visitor)


if __name__ == '__main__':
    main()

