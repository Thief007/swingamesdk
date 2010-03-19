#!/usr/bin/env python
# encoding: utf-8
"""
check_pas_parameters.py

Created by Andrew Cain on 2010-03-19.
Copyright (c) 2010 Swinburne University of Technology. All rights reserved.
"""

import logging
import sys

from sg import parser_runner
from sg.sg_cache import logger, find_or_add_file
from sg.sg_type import SGType
from sg.sg_parameter import SGParameter
from sg.file_writer import FileWriter


_out_path="../../Templates/Documentation"

def param_visitor(param, last, other):
    '''Check that all types in the types list are passed by reference...'''
    
    global _param_txt, _param_types
    
    if len(_param_txt) > 0:
        comma = ', '
    else:
        comma = ''
    
    if param.modifier != None:
        _param_txt += "%s<span style=\"color: #000000; font-weight: bold;\">%s</span> %s" % (comma, param.modifier, param.name)
    else:
        _param_txt += "%s%s" % (comma, param.name)
    
    _param_types += "<tr><td>%s</td><td>%s</td></tr>" % (param.name, param.data_type.name)

def method_visitor(method, other):
    global _param_txt, _param_types, file_writer
    
    _param_txt = ""
    _param_types = "<table><tr><th>Parameter Name</th><th>Data Type</th></tr>"
    
    method.visit_params(param_visitor, other)
    
    _param_types += "</table>"
    
    if method.return_type != None:
        file_writer.writeln('<pre>%s(%s) : %s</pre>' % (method.name, _param_txt, method.return_type))
    else:
        file_writer.writeln('<pre>%s(%s)</pre>' % (method.name, _param_txt))
    
    file_writer.writeln(method.doc)
    
    if len(method.params) > 0:
        file_writer.writeln(_param_types)

def visitor(the_file, other):
    if the_file.name in ['SGSDK', 'Types']: return
    
    global file_writer
    
    file_writer = FileWriter('%s/%s.html'% (_out_path, the_file.name))
    
    
    
    print '---------- %s ----------' % (the_file.name)
    
    for element in the_file.members:
        element.visit_methods(method_visitor, None)


def main():
    logging.basicConfig(level=logging.WARNING,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    parser_runner.run_for_all_units(visitor)

if __name__ == '__main__':
    main()
