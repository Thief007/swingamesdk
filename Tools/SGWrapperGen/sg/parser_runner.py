#!/usr/bin/env python
# encoding: utf-8
"""
parser_runner.py

Created by Andrew Cain on 2009-06-02.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import logging
import sys

from sg_cache import find_or_add_file, find_or_add_class, find_or_add_type, all_files, logger
from sg_pas_parser import SGPasParser
from sg_parameter import SGParameter

def _add_parameter(the_method, param):
    param_list = list(the_method.params)
    param_list.append(param)
    the_method.params = tuple(param_list)
    

def method_process_visitor(the_method, other):
    '''Process a method prior to rendering'''
    
    #Change functions with string or array return type to have a result property
    if the_method.return_type != None and (the_method.return_type.wraps_array or the_method.return_type.name.lower() in ['string']):
        #print 'PARS RUN PARAMS', [p.name for p in the_method.params]
        result_param = SGParameter('result')
        
        for param in the_method.params:
            if 'result' == param.name:
                logger.error('PARSER RUN: Error adding result parameter to %s', the_method.name)
                assert False
        _add_parameter(the_method, result_param)
        
        result_param.maps_result = True
        result_param.modifier = None #they are pointers... 'var'
        
        result_param.data_type = the_method.return_type
        
        the_method.return_type = None
        the_method.was_function = True
        #print 'PARS RUN PARAMS', [p.name for p in the_method.params]
        
    #Replace any variable length arrays
    orig_params = the_method.params
    for param in orig_params:
        #does it wrap a variable length array
        if param.data_type.array_wrapper:
            the_method.has_length_params = True
            logger.debug('PARSER RUN: Altering variable length array of %s\'s parameter %s', the_method.name, param.name)
            old_type = param.data_type
            param.data_type = param.data_type #.fields[0].data_type
            len_param = SGParameter('%s_len' % param.name)
            len_param.is_length_param = True
            len_param.data_type = find_or_add_type('LongInt')
            len_param.modifier = param.modifier if param.modifier is ['out', 'var'] else None
            _add_parameter(the_method, len_param)

def post_parse_process(the_file):
    '''Create temporary variables for out/var string parameters, and string return types'''
    logger.info('Post parsing library')
    
    the_file.members[0].visit_methods(method_process_visitor, None)

def run_for_all_units(file_visitor):
    parser = SGPasParser()
    
    lib_file = find_or_add_file('SGSDK','SGSDK','./sgsdk.pas')
    
    files = [
            lib_file,
            find_or_add_file('sgAudio', 'Audio', '../../CoreSDK/src/sgAudio.pas'),
            find_or_add_file('sgCore', 'Core', '../../CoreSDK/src/sgCore.pas'),
            find_or_add_file('sgCamera', 'Camera', '../../CoreSDK/src/sgCamera.pas'),
            find_or_add_file('sgText', 'Text', '../../CoreSDK/src/sgText.pas'),
            find_or_add_file('sgShapes', 'Shapes', '../../CoreSDK/src/sgShapes.pas'),
            find_or_add_file('sgInput', 'Input', '../../CoreSDK/src/sgInput.pas'),
            find_or_add_file('sgGraphics', 'Graphics', '../../CoreSDK/src/sgGraphics.pas'),
            find_or_add_file('sgPhysics', 'Physics', '../../CoreSDK/src/sgPhysics.pas'),
            find_or_add_file('sgTileMap', 'TileMap', '../../CoreSDK/src/sgTileMap.pas'),
            find_or_add_file('sgTypes', 'Types', '../../CoreSDK/src/sgTypes.pas'),
            find_or_add_file('sgMath', 'Math', '../../CoreSDK/src/sgMath.pas')
            
        ]
    
    for a_file in files[1:]:
        parser.parse(a_file)
    
    find_or_add_class('lib').check_methods()
    
    for key,each_file in all_files().items():
        if each_file != lib_file: 
            lib_file.uses.append(each_file)
    
    lib_file.members.append(find_or_add_class('lib'))
    
    post_parse_process(lib_file)
    
    logger.info('Processing files')
    for each_file in files:
        logger.debug('Visiting file %s', each_file.name)
        each_file.visit(file_visitor, None)
    

def show_file(the_file, other):
    print 'Done', the_file.name

def main():
    logging.basicConfig(level=logging.DEBUG,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    run_for_all_units(show_file)


if __name__ == '__main__':
    main()

