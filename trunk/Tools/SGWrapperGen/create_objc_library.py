#!/usr/bin/env python
# encoding: utf-8
"""
create_objc_lib.py

Created by Andrew Cain on 2009-06-02.
Copyright (c) 2009 Swinburne. All rights reserved.
"""

import logging
import sys

import objc_lib #import templates

from sg import parser_runner, wrapper_helper
from sg.sg_cache import logger, find_or_add_file
from sg.print_writer import PrintWriter
from sg.file_writer import FileWriter
from sg.sg_type import SGType
from sg.sg_parameter import SGParameter

_out_path="../../Templates/ObjC/common/lib"


def _create_objc_method_headers(the_method, other):
    def type_visitor(the_type, modifier = None):
        '''switch types for the c SwinGame library'''
        key = the_type.name.lower() if the_type != None else None
        
        if modifier == 'result': modifier = 'return'
        
        if key not in objc_lib._type_switcher[modifier]:
            logger.error('CREATE Cs : Error changing model type %s - %s', modifier, the_type)
            assert False
        return objc_lib._type_switcher[modifier][key]
    def param_visitor(the_param, last):
        return '(%s) %s%s' % (type_visitor(the_param.data_type, the_param.modifier), the_param.name, ',' if not last else '')
    # Start of _create_objc_method_headers
    result_details = other['details']
    
    my_details = the_method.to_keyed_dict(param_visitor, type_visitor)
    
    if the_method.is_static:
        if len(the_method.params) > 0:
            result_details['static_method_headers'] += '\n+ (%(return_type)s)%(uname)s:%(params)s;' % my_details
        else: result_details['static_method_headers'] += '\n+ (%(return_type)s)%(uname)s;' % my_details
    else:
        if len(the_method.params) > 0:
            result_details['method_headers'] += '\n- (%(return_type)s)%(uname)s:%(params)s;' % my_details
        else: result_details['method_headers'] += '\n- (%(return_type)s)%(uname)s;' % my_details
    
    return other

def _write_objc_user_module(module):
    '''Write the header and obj-c file to wrap the attached files details'''
    header_file_writer = FileWriter('%s/%s.h'% (_out_path, module.name))
    file_writer = FileWriter('%s/%s.m'% (_out_path, module.name))
    
    details = module.to_keyed_dict()
    details['method_headers'] = ''
    details['static_method_headers'] = ''
    details['fields'] = ''
    
    other = dict()
    other['details'] = details
    
    print other
    module.visit_methods(_create_objc_method_headers, other)
    
    #Write the header file
    header_file_writer.writeln(objc_lib.class_header % details)
    
    file_writer.close()
    header_file_writer.close()

def _post_parse_process(the_file):
    '''
    Post process the passed in file. 
    For Objective-C this will perform the following changes:
    
    1: Add local variables for arrays
    2: Add arguments for length parameters
    
    '''
    if the_file.name == 'SGSDK':
        return
    
    logger.info('Post Processing %s for Objective-C wrapper creation', the_file.name)
    
    for member in the_file.members:
        #process all method of the file
        methods_and_operators = member.methods.items() + member.operators.items()
        
        for key, method in methods_and_operators:
            _post_process_method(method)
        
        for key, prop in member.properties.items():
            if prop.getter != None:
                _post_process_method(prop.getter)
            if prop.setter != None:
                _post_process_method(prop.setter)

def _post_process_method(method):
    method.uname = method.uname.lower()[0] + method.uname[1:]
    for param in method.params:
        if param.maps_result or param.data_type.wraps_array or (param.modifier in ['var', 'out'] and param.data_type.name.lower() in ['string','color']):
            if method.is_constructor: continue
            wrapper_helper.add_local_var_for_param(method, param)
    if method.method_called.was_function:
        wrapper_helper.add_local_var_for_result(method)
    if method.method_called.has_length_params:
        wrapper_helper.add_length_params(method, '%s.Length')

def _file_visitor(the_file, other):
    '''Called for each file read in by the parser'''
    _post_parse_process(the_file)
    
    for member in the_file.members:
        if member.is_module:
            _write_objc_user_module(member)
    
    # if the_file.name == 'SGSDK':
    #     logger.info('Creating C# Library Adapter %s.cs', the_file.name)
    #     write_cs_sgsdk_file(the_file)
    # else:
    #     logger.info('Creating C# SwinGame Module %s', the_file.name)
    #     write_cs_lib_module(the_file)

def main():
    logging.basicConfig(level=logging.INFO,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    
    parser_runner.run_for_all_units(_file_visitor)

if __name__ == '__main__':
    main()
