#!/usr/bin/env python
# encoding: utf-8
"""
parser_runner.py

Created by Andrew Cain on 2009-06-02.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

from sgcache import find_or_add_file, find_or_add_class, find_or_add_class, all_files, logger
from SGPasParser import SGPasParser

def run_for_all_units(file_visitor):
    parser = SGPasParser()
    
    lib_file = find_or_add_file('SGSDK','SGSDK_Lib','./sgsdk.pas')
    
    files = [
            lib_file,
            find_or_add_file('SGSDK_Audio', 'Audio', '../../CoreSDK/src/SGSDK_Audio.pas'),
            find_or_add_file('SGSDK_Core', 'Core', '../../CoreSDK/src/SGSDK_Core.pas')
        ]
    
    for a_file in files[1:]:
        parser.parse(a_file)
    
    find_or_add_class('lib').check_methods()
    
    for key,each_file in all_files().items():
        if each_file != lib_file: lib_file.uses.append(each_file)
    
    lib_file.members.append(find_or_add_class('lib'))
    
    for each_file in files:
        logger.debug('Visiting file %s', each_file.name)
        each_file.visit(file_visitor, None)
    

def main():
    pass


if __name__ == '__main__':
    main()

