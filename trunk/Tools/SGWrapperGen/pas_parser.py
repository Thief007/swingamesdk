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
from sgcache import all_classes
from SGClass import SGClass
from SGMethod import SGMethod

DEBUG = False

def property_visitor(element, last):
    print '\tpublic %s%s %s'%(
        'static ' if element.is_static else '',
        element.data_type,
        element.name
        )
    print '\t{'
    print '\t',
    method_visitor(element.getter)
    print '\t',
    method_visitor(element.setter)
    print '\t}'
    if last: print

def field_visitor(element, last):
    print '\tprivate %s %s;'%(
        element.data_type,
        element.name
        )
    if last: print

def param_visitor(element, last):
    print '%s%s %s%s'%(
        '' if element.modifier == None else element.modifier + ' ',
        element.data_type,
        element.name, 
        ',' if not last else ''), 

def method_visitor(element):
    print '\tpublic %s%s %s(' % (
        'static ' if element.is_static else '',
        element.return_type if element.return_type != None else 'void' , 
        element.name),
    
    element.visit_params(param_visitor)
    
    print ')'

def visitor(element):
    if element.is_static:
        print 'static',
    print 'class %s' % element.name
    print '{'
    element.visit_fields(field_visitor)
    element.visit_properties(property_visitor)
    element.visit_methods(method_visitor)
    print '}'

def main():
    logging.basicConfig(level=logging.INFO,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    
    tokeniser = SGPasTokeniser()
    tokeniser.tokenise('../../CoreSDK/src/SGSDK_Audio.pas')
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
    parser.parse('../../CoreSDK/src/SGSDK_Audio.pas')
    
    for key,each_class in all_classes().items():
        each_class.visit(visitor)

if __name__ == '__main__':
    main()
