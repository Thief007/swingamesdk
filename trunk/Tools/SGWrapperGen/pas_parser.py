#!/usr/bin/env python
# encoding: utf-8
'''Reads in a pascal source code file and outputs an abstract
syntax tree, with attached meta tags.

Meta tag syntax:
> /// @tag [parameters] 

parameters= parameter[, parameters]
parameter= <value>|([parameters])
pas_parser.py

Created by Andrew Cain on 2009-05-25.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
'''
    
import logging
import sys
from SGPasTokeniser import SGPasTokeniser
from SGPasParser import SGPasParser
from SGParameter import SGParameter
from sgcache import all_classes, find_or_add_file, find_or_add_class
from sgcodemodule import SGCodeModule
from SGMethod import SGMethod
from sgfile import SGFile

DEBUG = False

def property_visitor(element, last, other):
    print '\tpublic %s%s %s'%(
        'static ' if element.is_static else '',
        element.data_type,
        element.name
        )
    print '\t{'
    print '\t',
    method_visitor(element.getter, other)
    print '\t',
    method_visitor(element.setter, other)
    print '\t}'
    if last: print

def field_visitor(element, last, other):
    print '\tprivate %s %s;'%(
        element.data_type,
        element.name
        )
    if last: print

def param_visitor(element, last, other):
    print '%s%s %s%s'%(
        '' if element.modifier == None else element.modifier + ' ',
        element.data_type,
        element.name, 
        ',' if not last else ''), 
    
def arg_visitor(arg, last, other):
    print '%s%s'%(
        arg.name if isinstance(arg, SGParameter) else arg,
        ',' if not last else ''), 
    

def method_visitor(element, other):
    print '\t<@%s> %s%s%s %s(' % (
        hex(id(element)),
        'static ' if element.is_static else '',
        'extern ' if element.is_external else '',
        element.return_type if element.return_type != None else 'void' , 
        element.uname),
    
    element.visit_params(param_visitor, other)
    
    print ')' 
    print '\t\t-> in_file\t', element.in_file
    print '\t\t-> calls',
    
    if element.method_called() == None:
        print '???'
        return
    
    print '\t<@%s> %s%s%s %s(' % (
        hex(id(element.method_called())),
        'static ' if element.method_called().is_static else '',
        'extern ' if element.method_called().is_external else '',
        element.method_called().return_type if element.method_called().return_type != None else 'void' , 
        element.method_called().name),
    
    element.method_called().visit_params(param_visitor, other)
    
    print ') from ', element.method_called().in_file
    
    print '\t\t-> args\t\t', 
    element.visit_args(arg_visitor, other)
    print '\n'

def visitor(element):
    if element.is_static:
        print 'static',
    print 'class %s' % element.name
    print '{'
    element.visit_fields(field_visitor, None)
    element.visit_properties(property_visitor, None)
    element.visit_methods(method_visitor, None)
    print '}'

def main():
    logging.basicConfig(level=logging.DEBUG,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    
    # tokeniser = SGPasTokeniser()
    # tokeniser.tokenise('../../CoreSDK/src/SGSDK_Audio.pas')
    # tok = tokeniser.next_token()
    # while tok[1] != 'implementation':
    #     print tok
    #     if tok[0] == 'attribute':
    #         tok = tokeniser.next_token()
    #         print tok
    #         if tok[1] == 'param':
    #             tok = tokeniser.next_token()
    #             print tok
    #             print tokeniser.read_to_eol()
    #         elif tok[1] == 'note':
    #             print tokeniser.read_to_eol()
    #     tok = tokeniser.next_token()
    # 
    parser = SGPasParser()
    
    files = [
            find_or_add_file('SGSDK_Audio', 'Audio', '../../CoreSDK/src/SGSDK_Audio.pas'),
            find_or_add_file('SGSDK_Core', 'Core', '../../CoreSDK/src/SGSDK_Core.pas')
        ]
    
    for a_file in files:
        parser.parse(a_file)
    
    find_or_add_class('lib').check_methods()
    
    for key,each_class in all_classes().items():
        each_class.visit(visitor)

if __name__ == '__main__':
    main()
