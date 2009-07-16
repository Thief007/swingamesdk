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
import create_c_library

from sg import parser_runner, wrapper_helper
from sg.sg_cache import logger, find_or_add_file
from sg.file_writer import FileWriter
from sg.sg_type import SGType
from sg.sg_parameter import SGParameter

_out_path="../../Templates/ObjC/common/lib"

def type_visitor(the_type, modifier = None):
    return wrapper_helper.std_type_visitor(objc_lib._type_switcher, the_type, modifier)

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
    
    # Change True to true for example...
    if for_param.modifier != 'out' and arg_str.lower() in objc_lib._data_switcher[data_key]:
        data = objc_lib._data_switcher[data_key][arg_str.lower()] 
        if '%s' in data:
            arg_str = objc_lib._data_switcher[data_key][arg_str.lower()] % arg_str
        else:
            arg_str = data
    
    #check for pointer wrapper param
    if the_type.pointer_wrapper and not '->pointer' in arg_str.lower():
        arg_str = '%s->pointer' % arg_str
    # elif the_type.is_struct and not '->data' in arg_str.lower():
    #     arg_str = '%s->data' % arg_str
    
    if the_type.pointer_wrapper and for_param.modifier == 'var':
        arg_str = '&' + arg_str
    
    if the_type.name.lower() in objc_lib._data_switcher[data_key] and for_param.modifier not in ['out', 'return', 'result']:
        #convert data using pattern from _data_switcher
        arg_str = objc_lib._data_switcher[data_key][the_type.name.lower()] % arg_str
    
    if the_type.is_struct and for_param.modifier in ['const', 'var', 'out'] and not the_type.wraps_array:
        #Get address for const, var and out parameters - excect if array as they are already pointers
        result = '&' + arg_str
    else:
        result = arg_str
    
    return result


def _create_objc_call(details, the_method):
    """Create an objective-c call for the passed in details dictionary/method"""
    if the_method.is_constructor:
        details['pre_call'] =''
        details['returns_end'] = '' # ', PtrKind.%s)' % details['in_class']
        details['base_const'] = '' # 'se(%(calls.class)s.%(calls.name)s(%(calls.args)s), PtrKind.%(in_class)s)%(returns_end)s' % details
        result = '[self initWithId: %(calls.name)s(%(calls.args)s)]' % details
    elif the_method.is_destructor:
        details['pre_call'] ='PointerWrapper.Remove(this);\n    '
        # details['return_type'] = 'void DoFree'
        details['returns_end'] = ''
        details['base_const'] = ''
        result = '%(calls.name)s(%(calls.args)s)%(returns_end)s' % details
    else:
        details['pre_call'] = ''
        details['returns_end'] = ''
        details['base_const'] = ''
        result = '%(calls.name)s(%(calls.args)s)%(returns_end)s' % details
        
        if the_method.return_type != None:
            if the_method.return_type.name.lower() in objc_lib._data_switcher['return_val']:
                result = objc_lib._data_switcher['return_val'][the_method.return_type.name.lower()] % result
    
    if the_method.method_called.was_function:
        details['returns'] = ''
    elif the_method.has_out_params and the_method.return_type != None:
        #todo: can this be done better?
        details['returns'] = 'result = '
    
    details['post_call'] =''
    
    return ('%(returns)s' + result) % details

def param_visitor(the_param, last):
    return '(%s)%s%s' % (type_visitor(the_param.data_type, the_param.modifier), the_param.name, ': ' if not last else '')

def _create_objc_method_details(the_method, other):
    ''' This method creates the objective c method details for a user facing module.'''
    #todo: check this...
    def special_visitor(the_param, last):
        return '(%s)%s%s' % (type_visitor(the_param.data_type, the_param.modifier), the_param.name, ' ' if not last else '')
    # Start of _create_objc_method_headers
    result_details = other['details']
    
    my_details = the_method.to_keyed_dict(param_visitor, type_visitor, call_creater=_create_objc_call, arg_visitor=arg_visitor,special_visitor=special_visitor)
    
    if the_method.is_static:
        header = '\n+ (%(return_type)s)%(uname)s:%(params)s' % my_details
        if len(the_method.params) == 0: header = header[:-1]
        result_details['static_method_headers'] += header + ';'
        dest_key = 'static_method_bodys'
    else:
        if the_method.is_constructor:
            header = '\n- (id)%(sn)s' % my_details
            if len(the_method.params) == 0: header = header[:-1]
            result_details['init_headers'] += header + ';'
            dest_key = 'init_bodys'
        elif the_method.is_destructor:
            header = '\n- (void)dealloc' % my_details
            result_details['dealloc_headers'] += header + ';'
            dest_key = 'dealloc_bodys'
        else:
            header = '\n- (%(return_type)s)%(uname)s:%(params)s' % my_details
            if len(the_method.params) == 0: header = header[:-1]
            result_details['method_headers'] += header + ';'
            dest_key = 'method_bodys'
    
    wrapper_helper.add_local_var_processing(the_method, my_details, objc_lib.local_variable_switcher)
    
    #Create the method body
    result_details[dest_key] += header
    result_details[dest_key] += '\n{'
    result_details[dest_key] += '\n    %(vars)s%(pre_call)s%(the_call)s;%(post_call)s' % my_details
    result_details[dest_key] += '\n}\n'
    
    return other

def _write_objc_user_module(module):
    '''Write the header and obj-c file to wrap the attached files details'''
    header_file_writer = FileWriter('%s/SG%s.h'% (_out_path, module.name))
    file_writer = FileWriter('%s/SG%s.m'% (_out_path, module.name))
    
    details = module.to_keyed_dict(type_visitor=type_visitor, array_idx_sep='][', param_visitor=param_visitor)
    details['method_headers'] = ''
    details['init_headers'] = ''
    details['dealloc_headers'] = ''
    details['static_method_headers'] = ''
    details['method_bodys'] = ''
    details['init_bodys'] = ''
    details['dealloc_bodys'] = ''
    details['static_method_bodys'] = ''    
    details['fields'] = ''
    details['imports'] = ''
    
    if module.is_class:
        for (name, data_type) in module.fields.items():
            details['fields'] += '\n    (%s) %s;' % (type_visitor(data_type), name)
    
    for f in module.in_file.uses:
        if f.name == None: continue
        details['imports'] += '#import "SG%s.h"\n' % f.name
        for cm in f.members:
            if (cm.is_class and cm != module) or (cm.is_struct and not cm.via_pointer):
                details['imports'] += '#import "SG%s.h"\n' % cm.name
    
    
    other = dict()
    other['details'] = details
    
    module.visit_methods(_create_objc_method_details, other)
    
    #Write the header file
    if module.is_pointer_wrapper:
        header_file_writer.writeln(objc_lib.pointer_wrapper_h % details)
        file_writer.writeln(objc_lib.pointer_wrapper_m % details)
    elif module.is_struct:
        if module.wraps_array:
            header_file_writer.writeln(objc_lib.array_wrapper_h % details)
            file_writer.writeln(objc_lib.array_wrapper_m % details)        
        else:
            header_file_writer.writeln(objc_lib.struct_wrapper_h % details)
            file_writer.writeln(objc_lib.struct_wrapper_m % details)        
    else:
        header_file_writer.writeln(objc_lib.class_header_txt % details)
        file_writer.writeln(objc_lib.class_body_txt % details)
    
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
    #Mangle name to lowercase first character
    method.uname = method.uname.lower()[0] + method.uname[1:]
    for param in method.params:
        if param.maps_result or param.data_type.name.lower() in ['string']:
            wrapper_helper.add_local_var_for_param(method, param)
        elif param.data_type.wraps_array and not param.data_type.name.lower() in ['triangle', 'matrix2d']:
            #also add arrays (variable length)
            wrapper_helper.add_local_var_for_param(method, param)
        elif param.modifier == 'out' and param.data_type.is_struct:
            #also add out parameters
            wrapper_helper.add_local_var_for_param(method, param)
    if method.method_called.was_function or method.has_out_params:
        wrapper_helper.add_local_var_for_result(method)
    if method.method_called.has_length_params:
        wrapper_helper.add_length_params(method, '[%s count]')

def _file_visitor(the_file, other):
    '''Called for each file read in by the parser'''
    
    _post_parse_process(the_file)
    
    if the_file.name == 'SGSDK':
        create_c_library.write_c_lib_header(the_file, True)
    elif the_file.name == 'Types':
        create_c_library.write_c_lib_module(the_file)
    
    for member in the_file.members:
        if member.is_module or member.is_class or (member.is_struct and not member.via_pointer):
            #need the classes file for #importing the class names for use in SwinGame class code.
            if member.is_module:
                classes_file_writer.writeln('@class %s;' % member.name)
            else:
                classes_file_writer.writeln('@class SG%s;' % member.name)
            
            _write_objc_user_module(member)
    
    # if the_file.name == 'SGSDK':
    #     logger.info('Creating C# Library Adapter %s.cs', the_file.name)
    #     write_cs_sgsdk_file(the_file)
    # else:
    #     logger.info('Creating C# SwinGame Module %s', the_file.name)
    #     write_cs_lib_module(the_file)

def main():
    global classes_file_writer
    classes_file_writer = FileWriter('%s/SGTypes.h'% _out_path)
    classes_file_writer.writeln('#import "Types.h"')
    
    logging.basicConfig(level=logging.WARNING,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    
    create_c_library.load_data()
    
    parser_runner.run_for_all_units(_file_visitor)
    classes_file_writer.close

if __name__ == '__main__':
    main()
