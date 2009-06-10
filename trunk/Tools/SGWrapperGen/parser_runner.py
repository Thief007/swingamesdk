#!/usr/bin/env python
# encoding: utf-8
"""
parser_runner.py

Created by Andrew Cain on 2009-06-02.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

from sgcache import find_or_add_file, find_or_add_class, find_or_add_class, all_files, logger
from SGPasParser import SGPasParser
from SGParameter import SGParameter

def method_process_visitor(the_method, other):
    '''Process a method prior to rendering'''
    
    #Change functions with string return type to have a result property
    if the_method.return_type != None and the_method.return_type.name.lower() in ['string', 'triangle', 'linesarray']:
        result_param = SGParameter('result')
        
        for param in the_method.params:
            if 'result' == param.name:
                logger.error('PARSER RUN: Error adding result parameter to %s', the_method.name)
                assert False
        param_list = list(the_method.params)
        param_list.append(result_param)
        the_method.params = tuple(param_list)
        
        result_param.maps_result = True
        result_param.modifier = 'out'
        result_param.data_type = the_method.return_type
        
        the_method.return_type = None
        the_method.was_function = True

def post_parse_process(the_file):
    '''Create temporary variables for out/var string parameters, and string return types'''
    logger.info('Post parsing library')
    
    the_file.members[0].visit_methods(method_process_visitor, None)

def run_for_all_units(file_visitor):
    parser = SGPasParser()
    
    lib_file = find_or_add_file('SGSDK','SGSDK_Lib','./sgsdk.pas')
    
    files = [
            lib_file,
            find_or_add_file('SGSDK_Audio', 'Audio', '../../CoreSDK/src/SGSDK_Audio.pas'),
            find_or_add_file('SGSDK_Core', 'Core', '../../CoreSDK/src/SGSDK_Core.pas'),
            find_or_add_file('SGSDK_Shapes', 'Shapes', '../../CoreSDK/src/SGSDK_Shapes.pas')
        ]
    
    for a_file in files[1:]:
        parser.parse(a_file)
    
    find_or_add_class('lib').check_methods()
    
    for key,each_file in all_files().items():
        if each_file != lib_file: lib_file.uses.append(each_file)
    
    lib_file.members.append(find_or_add_class('lib'))
    
    post_parse_process(lib_file)
    
    logger.info('Processing files')
    for each_file in files:
        logger.debug('Visiting file %s', each_file.name)
        each_file.visit(file_visitor, None)
    

def main():
    pass


if __name__ == '__main__':
    main()

