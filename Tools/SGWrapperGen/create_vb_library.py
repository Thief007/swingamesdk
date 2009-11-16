#!/usr/bin/env python
# encoding: utf-8
"""
create_vb_lib.py

Created by Andrew Cain on 2009-06-02.
Copyright (c) 2009 Swinburne. All rights reserved.
"""

import logging
import sys

import vb_lib #import templates

from sg import parser_runner, wrapper_helper
from sg.sg_cache import logger, find_or_add_file
from sg.file_writer import FileWriter
from sg.sg_type import SGType
from sg.sg_parameter import SGParameter

_out_path="../../Templates/VB/common/lib"

def type_visitor(the_type, modifier = None):
    return wrapper_helper.std_type_visitor(vb_lib._type_switcher, the_type, modifier)

def adapter_type_visitor(the_type, modifier = None):
    return wrapper_helper.std_type_visitor(vb_lib._adapter_type_switcher, the_type, modifier, '_adapter_type_switcher')

def _map_data_value(key, return_type, result):
    return wrapper_helper.map_data_value(vb_lib._data_switcher, key, return_type, result)

def param_visitor(the_param, last):
    return '(%s)%s%s' % (type_visitor(the_param.data_type, the_param.modifier), the_param.name, ': ' if not last else '')

def special_visitor(the_param, last):
    return '(%s)%s%s' % (type_visitor(the_param.data_type, the_param.modifier), the_param.name, ' ' if not last else '')


def arg_visitor(arg_str, the_arg, for_param):
    '''Called for each argument in a call, performs required mappings. the_arg has the argument, for_param has
    the parameter being mapped to'''
    
    if not isinstance(for_param, SGParameter):
        # print arg_str, the_arg, for_param
        assert False
        
    if isinstance(the_arg, SGParameter): #uses parameter as value
        data_key = 'arg_val'
    else:
        data_key = 'arg_lit_val'
        
    the_type = for_param.data_type
    
    # Change True to true for example...
    if for_param.modifier != 'out' and arg_str.lower() in vb_lib._data_switcher[data_key]:
        data = vb_lib._data_switcher[data_key][arg_str.lower()] 
        if '%s' in data:
            arg_str = vb_lib._data_switcher[data_key][arg_str.lower()] % arg_str
        else:
            arg_str = data
    
    #check for pointer wrapper param
    if the_type.pointer_wrapper and not '->pointer' in arg_str.lower():
        arg_str = '%s->pointer' % arg_str
    # elif the_type.is_struct and not '->data' in arg_str.lower():
    #     arg_str = '%s->data' % arg_str
    
    if the_type.pointer_wrapper and for_param.modifier == 'var':
        arg_str = '&' + arg_str
    
    if the_type.name.lower() in vb_lib._data_switcher[data_key] and for_param.modifier not in ['out', 'return', 'result']:
        #convert data using pattern from _data_switcher
        arg_str = vb_lib._data_switcher[data_key][the_type.name.lower()] % arg_str
    
    if the_type.is_struct and not isinstance(the_arg, SGParameter):
        arg_str = vb_lib._data_switcher['arg_val'][the_type.name.lower()] % arg_str
    
    if the_type.is_struct and for_param.modifier in ['const', 'var', 'out'] and not the_type.wraps_array:
        #Get address for const, var and out parameters - excect if array as they are already pointers
        result = '&' + arg_str
    else:
        result = arg_str
    
    return result

def _create_vb_call(details, the_method):
    """Create an objective-c call for the passed in details dictionary/method"""
    details['pre_call'] = ''
    details['returns_end'] = ''
    
    if the_method.is_constructor:
        if the_method.in_class.is_struct:
            result = '[self initWith%(name)s: %(calls.name)s(%(calls.args)s)]' % details
        else:
            result = '[self initWithId: %(calls.name)s(%(calls.args)s)]' % details
    elif the_method.is_destructor:
        result = '%(calls.name)s(%(calls.args)s)%(returns_end)s' % details
    elif the_method.is_getter and the_method.method_called == None:
        #Created property getter...
        result = 'data.%(field.name_lower)s' % details
        result = _map_data_value('return_val', the_method.return_type, result)
    elif the_method.is_setter and the_method.method_called == None:
        #Created property setter...
        details['value'] = arg_visitor('value', the_method.params[0], the_method.params[0])
        result = 'data.%(field.name_lower)s = %(value)s' % details
    else:
        result = '%(calls.name)s(%(calls.args)s)%(returns_end)s' % details
        
        if the_method.return_type != None:
            result = _map_data_value('return_val', the_method.return_type, result)
            
    if the_method.method_called != None and the_method.method_called.was_function:
        details['returns'] = ''
    elif the_method.has_out_params and the_method.return_type != None:
        #todo: can this be done better?
        details['returns'] = 'result = '
    
    details['post_call'] =''
    
    return ('%(returns)s' + result) % details

def _create_vb_method_details(the_method, other):
    ''' This method creates the objective c method details for a user facing module.'''
    result_details = other['details']
    
    if the_method.in_same_class_as_class_method():
        return other
    
    my_details = the_method.to_keyed_dict(param_visitor, type_visitor, 
        call_creater = _create_vb_call, 
        arg_visitor = arg_visitor,
        special_visitor = special_visitor)
    
    if the_method.is_static:
        #header = '\n+ (%(return_type)s)%(uname)s:%(params)s' % my_details
        header = '\n+ (%(return_type)s)%(sn)s' % my_details
        #if len(the_method.params) == 0: header = header[:-1]
        result_details['static_method_headers'] += header + ';'
        dest_key = 'static_method_bodies'
    else:
        if the_method.is_constructor:
            header = '\n- (id)%(sn)s' % my_details
            #if len(the_method.params) == 0: header = header[:-1]
            result_details['init_headers'] += header + ';'
            dest_key = 'init_bodys'
        elif the_method.is_destructor:
            header = '\n- (void)free' % my_details
            result_details['method_headers'] += header + ';'
            dest_key = 'method_bodies'
        elif the_method.is_getter or the_method.is_setter:
            #dont write in header...
            header = '\n- (%(return_type)s)%(sn)s' % my_details
            #header = '\n- (%(return_type)s)%(uname)s:%(params)s' % my_details
            #if len(the_method.params) == 0: header = header[:-1]
            result_details['method_headers'] += '\n#if OBJC_NEW_PROPERTIES != 1' + header + ';\n#endif'
            dest_key = 'method_bodies'
        else:
            #header = '\n- (%(return_type)s)%(uname)s:%(params)s' % my_details
            header = '\n- (%(return_type)s)%(sn)s' % my_details
            #if len(the_method.params) == 0: header = header[:-1]
            result_details['method_headers'] += header + ';'
            dest_key = 'method_bodies'
    
    wrapper_helper.add_local_var_processing(the_method, my_details, vb_lib.local_variable_switcher)
    
    #Create the method body
    result_details[dest_key] += header
    result_details[dest_key] += '\n{'
    result_details[dest_key] += '\n    %(vars)s%(pre_call)s%(the_call)s;%(post_call)s' % my_details
    result_details[dest_key] += '\n}\n'
    
    if the_method.is_getter:
        result_details['wrapped_property_bodies'] += header
        result_details['wrapped_property_bodies'] += '\n{'
        result_details['wrapped_property_bodies'] += '\n    [self callRead];'
        result_details['wrapped_property_bodies'] += '\n    %(vars)s%(pre_call)s%(the_call)s;%(post_call)s' % my_details
        result_details['wrapped_property_bodies'] += '\n}\n'
    elif the_method.is_setter:
        result_details['wrapped_property_bodies'] += header
        result_details['wrapped_property_bodies'] += '\n{'
        result_details['wrapped_property_bodies'] += '\n    %(vars)s%(pre_call)s%(the_call)s;%(post_call)s' % my_details
        result_details['wrapped_property_bodies'] += '\n    [self callUpdate];'
        result_details['wrapped_property_bodies'] += '\n}\n'
    
    return other

def _create_vb_property_details(the_property, other):
    '''Visited for each property, adds details to the result details'''
    result_details = other['details']
    
    type_name = wrapper_helper.std_type_visitor(vb_lib._type_switcher, the_property.data_type, 'return')
    
    is_wrapped = the_property.in_class.is_pointer_wrapper and (the_property.data_type.is_struct or the_property.data_type.is_array) and not the_property.is_static and the_property.getter != None and the_property.setter != None

    if the_property.is_static:
        #print 'static', the_property.name
        
        _create_vb_method_details(the_property.getter, other);
        if the_property.setter != None:
            the_property.setter.is_setter = False #not supported...
            _create_vb_method_details(the_property.setter, other);
        
        return other
    elif the_property.getter == None:
        print '!! write only', the_property.name
        _create_vb_method_details(the_property.setter, other);
        return other
    # elif is_wrapped:
    #     print 'wrapped', the_property.name
    #     return other
        # _write_wrapped_property(the_property, other)
        # writer.write('private %s\n{\n' % type_name % the_property.name)
    else:    
        # Write standard property
        header = '\n#if OBJC_NEW_PROPERTIES\n@property (%s, %s) %s %s;\n#endif' % (
            'assign', 
            'readonly ' if the_property.setter == None else 'readwrite', 
            type_name, 
            the_property.name)
    
    if is_wrapped and the_property.data_type.is_struct:
        #rename getter
        the_property.getter.uname = 'pri_' + the_property.getter.uname
    
    result_details['property_headers'] += header + ';'
    result_details['property_synthesizes'] += '\n@dynamic %s;' % the_property.name
    
    _create_vb_method_details(the_property.getter, other);
    if the_property.setter != None:
        _create_vb_method_details(the_property.setter, other);
    
    if is_wrapped and the_property.data_type.is_struct:
        #add the implementation of the getter...
        the_property.getter.uname = the_property.getter.uname[4:] #remove pri_
        
        my_details = the_property.getter.to_keyed_dict(param_visitor, type_visitor, 
            call_creater = _create_vb_call, 
            arg_visitor = arg_visitor,
            special_visitor = special_visitor)
        
        result_details['method_bodies'] += '\n- (%(return_type)s)%(uname)s' % my_details
        result_details['method_bodies'] += '\n{'
        result_details['method_bodies'] += '\n    SGWrapped%s *result;' % the_property.data_type.name
        result_details['method_bodies'] += '\n    result = [SGWrapped%s %sWithDelegate:self update:%s andRead: @selector(%s)];' % (
            the_property.data_type.name,
            the_property.data_type.camel_name,
            '@selector(%s:)' % the_property.setter.uname if the_property.setter != None else 'nil',
            'pri_' + the_property.getter.uname
            )
        result_details['method_bodies'] += '\n    return result;'
        result_details['method_bodies'] += '\n}\n'
        
    
    # if is_wrapped:
    #     writer.outdent(2)
    #     writer.write('}\n')

    return other

def _add_properties_for_fields(module, details):
    '''Add property code to access the fields of the passed in module (struct)'''
    
    for k, f in module.fields.items():
        prop = wrapper_helper.create_property_for_field(module, f)
        
        _post_process_method(prop.getter)
        if prop.setter != None:
            _post_process_method(prop.setter)
        
        _create_vb_property_details(prop, details)

def _write_vb_user_module(module):
    '''Write the header and obj-c file to wrap the attached files details'''
    header_file_writer = FileWriter('%s/SG%s.h'% (_out_path, module.name))
    file_writer = FileWriter('%s/SG%s.m'% (_out_path, module.name))
    
    details = module.to_keyed_dict(type_visitor = type_visitor, 
        array_idx_sep ='][', 
        param_visitor = param_visitor,
        map_data_value = _map_data_value)
    
    details['method_headers'] = ''
    details['init_headers'] = ''
    details['dealloc_headers'] = ''
    details['static_method_headers'] = ''
    details['method_bodies'] = ''
    details['init_bodys'] = ''
    details['dealloc_bodys'] = ''
    details['static_method_bodies'] = ''    
    details['fields'] = ''
    details['imports'] = ''
    details['property_synthesizes'] = ''
    details['property_headers'] = ''
    details['wrapped_property_bodies'] = ''
    
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
    
    module.visit_properties(_create_vb_property_details, other)
    module.visit_methods(_create_vb_method_details, other)
    
    #Write the header file
    if module.is_pointer_wrapper:
        header_file_writer.writeln(vb_lib.pointer_wrapper_h % details)
        file_writer.writeln(vb_lib.pointer_wrapper_m % details)
    elif module.is_struct:
        if module.wraps_array:
            header_file_writer.writeln(vb_lib.array_wrapper_h % details)
            file_writer.writeln(vb_lib.array_wrapper_m % details)        
        else:
            _add_properties_for_fields(module, other)
            header_file_writer.writeln(vb_lib.struct_wrapper_h % details)
            file_writer.writeln(vb_lib.struct_wrapper_m % details)        
    else:
        header_file_writer.writeln(vb_lib.class_header_txt % details)
        file_writer.writeln(vb_lib.class_body_txt % details)
    
    file_writer.close()
    header_file_writer.close()

def _create_vb_dll_call(details, the_method):
    """
    Create a Visual Basic call for the passed in details dictionary/method, this call is from the SGSDK.cs code
    to the native library...
    """
    
    details['pre_call'] =''
    details['post_call'] =''
    result = ''
    
    return '%(returns)sDLL_%(name)s(%(args)s)' % details

def _arg_vb_dll_visitor(arg_str, the_arg, for_param):
    the_type = for_param.data_type
    result = arg_str
    
    #check var/out/const
    if the_type.array_wrapper or the_type.fixed_array_wrapper:
        return result
    if (for_param.modifier == 'var' or for_param.modifier == 'const'): # and not (the_type.array_wrapper or the_type.fixed_array_wrapper):
        result = 'ref ' + result
    elif (for_param.modifier == 'out') and the_type.name.lower() != "string":
        result = 'out ' + result
    
    return result

def _create_vb_lib_method(the_method, other):
    '''
    Create a library method for Visual Basic. This is a method in the DLL
    facing class.
    '''
    output = other['details']  #contains the library_code key used for library output
    
    # Read the details of the method into a keyed dictionary that can be used to
    # generate the output
    my_details = the_method.to_keyed_dict(param_visitor, type_visitor, 
        call_creater = _create_vb_dll_call, 
        arg_visitor = arg_visitor,
        special_visitor = None)
    
    my_details['public'] = 'friend '
    my_details['override'] = ''
    
    if the_method.is_function:
        my_details['fn_type'] = 'Function '
        my_details['fn_return'] = ' As %(return_type)s' % my_details
    else:
        my_details['fn_type'] = 'Sub '
        my_details['fn_return'] = ''
    
    wrapper_helper.add_local_var_processing(the_method, my_details, vb_lib.local_variable_switcher)
    
    output['library_code'] += vb_lib.sgsdk_method_vb % my_details
    
    return other

def write_vb_lib(the_file):
    '''
    Write the vb library adapter - header file that matches DLL
    '''
    details = dict()
    
    details['library_code'] = ''
    
    #visit the methods of the library
    other = {
        # 'lib method': True,
        # 'type visitor': adapter_type_visitor,
        # 'param visitor': adapter_param_visitor,
        # 'arg visitor': _arg_vb_dll_visitor,
        # 'call_creater': _create_vb_dll_call,
        'details': details
    }
    
    details['library_code'] += 'Friend Class %s' % the_file.members[0].name
    the_file.members[0].visit_methods(_create_vb_lib_method, other)
    details['library_code'] += '\nEnd Class'
    
    file_writer = FileWriter('%s/%s.vb'% (_out_path, the_file.name))
    file_writer.write(vb_lib.sgsdk_vb % details)
    file_writer.close()


def _post_parse_process(the_file):
    '''
    Post process the passed in file. This loops over the methods, operators,
    and properties and processes each of these using _post_process_method which
    is where the real work is.
    '''
    if the_file.name == 'SGSDK':
        return
    
    logger.info('Post Processing %s for Visual Basic wrapper creation', the_file.name)
    
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
    '''
    For Visual Basic this will perform the following changes:
    
    1: Process the parameters of the methods adding local variables for parameters that
       are used to map results (used for returning arrays), are arrays themselves, or
       will be passed 'var' or 'out' and are strings or colours. These types of parameters
       require local processing within the method which is performed on a local variable.
       For example: colors are converted to a 32bit integer to be passed then converted back to
       an Color on return.
    2: A local variable to store the return value is created. This allows post processing
       of returned values (return or var/out param) before the actual value is returned.
    3: Arguments are added to map to any length parameters in this method using the Length
       of the array (%s.Length)
    
    '''
    for param in method.params:
        if method.is_constructor: break
        if param.maps_result or param.data_type.wraps_array or (param.modifier in ['var', 'out'] and param.data_type.name.lower() in ['string','color']):
            wrapper_helper.add_local_var_for_param(method, param)
    
    if method.method_called.was_function:
        wrapper_helper.add_local_var_for_result(method)
    if method.method_called.has_length_params:
        wrapper_helper.add_length_args(method, '%s.Length')

def _file_visitor(the_file, other):
    '''
    Called for each file read in by the parser. For each file we need to do the following:
    
    1: Perform post parse processing on the file such as adding local variables and changing
       return types for arrays etc. (you can return an array in c, hence cant return in DLL).
    2: Write out the file
    3: Write out the members that were located within the file
    
    '''
    
    _post_parse_process(the_file)
    
    if the_file.name == 'SGSDK':
        write_vb_lib(the_file)
    # elif the_file.name == 'Types':
    #     create_c_library.write_c_lib_module(the_file)
    
    return
    
    for member in the_file.members:
        if member.is_module or member.is_class or (member.is_struct and not member.via_pointer):
            #need the classes file for #importing the class names for use in SwinGame class code.
            if member.is_module:
                classes_file_writer.writeln('@class %s;' % member.name)
            else:
                classes_file_writer.writeln('@class SG%s;' % member.name)
            
            _write_vb_user_module(member)
    
    # if the_file.name == 'SGSDK':
    #     logger.info('Creating C# Library Adapter %s.cs', the_file.name)
    #     write_cs_sgsdk_file(the_file)
    # else:
    #     logger.info('Creating C# SwinGame Module %s', the_file.name)
    #     write_cs_lib_module(the_file)


def main():
    '''
    Create the Visual Basic version of the SwinGame library.
    
    This is done using a visitor that visits each file that is parsed by the SG parser.
    '''
    parser_runner.run_for_all_units(_file_visitor)

if __name__ == '__main__':
    main()
