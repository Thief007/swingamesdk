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
from sg.file_writer import FileWriter
from sg.sg_type import SGType
from sg.sg_parameter import SGParameter

_out_path="../../Templates/ObjC/common/lib"


def type_visitor(the_type, modifier = None):
    '''switch types for the c SwinGame library'''
    key = the_type.name.lower() if the_type != None else None
    
    if modifier == 'result': modifier = 'return'
    
    if key not in objc_lib._type_switcher[modifier]:
        logger.error('CREATE Cs : Error changing model type %s - %s', modifier, the_type)
        assert False
    return objc_lib._type_switcher[modifier][key]

def arg_visitor(arg_str, the_arg, for_param):
    '''Called for each argument in a call, performs required mappings. the_arg has the argument, for_param has
    the parameter being mapped to'''
    
    if not isinstance(for_param, SGParameter):
        print arg_str, the_arg, for_param
        assert False
        
    if isinstance(the_arg, SGParameter): #uses parameter as value
        data_key = 'arg_val'
    else:
        data_key = 'arg_lit_val'
        
    the_type = for_param.data_type
    
    #check for pointer wrapper param
    if the_type.pointer_wrapper and not '->pointer' in arg_str.lower():
        arg_str = '%s->Pointer' % arg_str
        
    # Change True to true for example...
    if for_param.modifier != 'out' and arg_str.lower() in objc_lib._data_switcher[data_key]:
        data = objc_lib._data_switcher[data_key][arg_str.lower()] 
        if '%s' in data:
            arg_str = objc_lib._data_switcher[data_key][arg_str.lower()] % arg_str
        else:
            arg_str = data
            
    if for_param.modifier != 'out' and the_type.name.lower() in objc_lib._data_switcher[data_key]:
        #convert data using pattern from _data_switcher
        result = objc_lib._data_switcher[data_key][the_type.name.lower()] % arg_str
    else:
        result = arg_str
        
    #check var/out/const
    if (for_param.modifier == 'var' or for_param.modifier == 'const'): #and not (the_type.array_wrapper or the_type.fixed_array_wrapper):
        result = '&' + result
    elif (for_param.modifier == 'out' or for_param.modifier == 'result') and the_type.name.lower() != "string":
        result = '&' + result

    return result


def _create_objc_call(details, the_method):
    """Create an objective-c call for the passed in details dictionary/method"""
    if the_method.is_constructor:
        details['pre_call'] =''
        details['return_type'] = '%s'
        details['returns'] = ''
        details['returns_end'] = '' # ', PtrKind.%s)' % details['in_class']
        details['public'] = 'public '
        details['base_const'] = ': base(%(calls.class)s.%(calls.name)s(%(calls.args)s), PtrKind.%(in_class)s)%(returns_end)s' % details
        result = ''
    elif the_method.is_destructor:
        details['pre_call'] ='PointerWrapper.Remove(this);\n    '
        details['return_type'] = 'void DoFree'
        details['returns_end'] = ''
        details['public'] = 'protected internal override '
        details['base_const'] = ''
        result = '%(calls.name)s(%(calls.args)s)%(returns_end)s' % details
    else:
        if the_method.name in ['WindowCloseRequested', 'CloseAudio']:
            details['pre_call'] = 'PointerWrapper.FreeAnythingToFree();\n    '
        elif the_method.mimic_destructor:
            details['pre_call'] ='[PointerWrapper remove: %s->Pointer];\n    ' % the_method.params[0].name
        else: details['pre_call'] =''
        details['returns_end'] = ''
        details['public'] = 'public '
        details['base_const'] = ''
        result = '%(calls.name)s(%(calls.args)s)%(returns_end)s' % details

        if the_method.return_type != None:
            if the_method.return_type.name.lower() in objc_lib._data_switcher['return_val']:
                result = objc_lib._data_switcher['return_val'][the_method.return_type.name.lower()] % result

    if the_method.name in ['ProcessEvents']:
        details['post_call'] = '\n    PointerWrapper.FreeAnythingToFree();'
    else: details['post_call'] =''

    return ('%(returns)s' + result) % details


def _create_objc_method_details(the_method, other):
    ''' This method creates the objective c method details for a user facing module.'''
    def param_visitor(the_param, last):
        return '(%s) %s%s' % (type_visitor(the_param.data_type, the_param.modifier), the_param.name, ',' if not last else '')
    # Start of _create_objc_method_headers
    result_details = other['details']
    
    my_details = the_method.to_keyed_dict(param_visitor, type_visitor, call_creater=_create_objc_call, arg_visitor=arg_visitor)
    
    if the_method.is_static:
        header = '\n+ (%(return_type)s)%(uname)s:%(params)s' % my_details
        if len(the_method.params) == 0: header = header[:-1]
        result_details['static_method_headers'] += header + ';'
        dest_key = 'static_method_bodys'
    else:
        if the_method.is_constructor:
            header = '\n- (id)init:%(params)s' % my_details
            if len(the_method.params) == 0: header = header[:-1]
            result_details['init_headers'] += header + ';'
            dest_key = 'init_bodys'
        elif the_method.is_destructor:
            header = '\n- (void)dealloc;' % my_details
            result_details['dealloc_headers'] += header + ';'
            dest_key = 'dealloc_bodys'
        else:
            header = '\n- (%(return_type)s)%(uname)s:%(params)s;' % my_details
            if len(the_method.params) == 0: header = header[:-1]
            result_details['method_headers'] += header + ';'
            dest_key = 'method_bodys'
    
    #Create the method body
    result_details[dest_key] += header
    result_details[dest_key] += '\n{'
    result_details[dest_key] += '\n    %(pre_call)s%(the_call)s;%(post_call)s' % my_details
    result_details[dest_key] += '\n}'
    
    return other

def _write_objc_user_module(module):
    '''Write the header and obj-c file to wrap the attached files details'''
    header_file_writer = FileWriter('%s/%s.h'% (_out_path, module.name))
    file_writer = FileWriter('%s/%s.m'% (_out_path, module.name))
    
    details = module.to_keyed_dict()
    details['method_headers'] = ''
    details['init_headers'] = ''
    details['dealloc_headers'] = ''
    details['static_method_headers'] = ''
    details['method_bodys'] = ''
    details['init_bodys'] = ''
    details['dealloc_bodys'] = ''
    details['static_method_bodys'] = ''    
    details['fields'] = ''
    
    if module.is_class:
        for (name, data_type) in module.fields.items():
            print name, 'type = ', data_type.name
            details['fields'] += '\n    (%s) %s;' % (type_visitor(data_type), name)
    
    other = dict()
    other['details'] = details
    
    module.visit_methods(_create_objc_method_details, other)
    
    #Write the header file
    header_file_writer.writeln(objc_lib.class_header % details)
    file_writer.writeln(objc_lib.class_body % details)
    
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
        if member.is_module or member.is_class:
            _write_objc_user_module(member)
    
    # if the_file.name == 'SGSDK':
    #     logger.info('Creating C# Library Adapter %s.cs', the_file.name)
    #     write_cs_sgsdk_file(the_file)
    # else:
    #     logger.info('Creating C# SwinGame Module %s', the_file.name)
    #     write_cs_lib_module(the_file)

def main():
    logging.basicConfig(level=logging.WARNING,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    
    parser_runner.run_for_all_units(_file_visitor)

if __name__ == '__main__':
    main()
