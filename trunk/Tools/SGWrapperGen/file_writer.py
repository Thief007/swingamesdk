#!/usr/bin/env python
# encoding: utf-8
"""
file_writer.py

Created by Andrew Cain on 2009-06-02.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import sys
import os

class FileWriter(object):
    def __init__(self, filename):
        self._indent = 0
        self.out_file = open(filename, 'w')
    
    def indent(self):
        self._indent += 1
    
    def outdent(self):
        self._indent -= 1
    
    def write(self,data):
        self.out_file.write('%s%s' % ('  ' * self._indent, data)),
    
    def writeln(self,data=''):
        self.out_file.write('%s%s' % ('  ' * self._indent, data))
    
    def close(self):
        self.out_file.close()
    

def main():
    pass


if __name__ == '__main__':
    main()

