#!/usr/bin/env python
# encoding: utf-8
"""
file_writer.py

Created by Andrew Cain on 2009-06-02.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import sys
import os
import wrapper_helper

class FileWriter(object):
    def __init__(self, filename):
        self._indent = 0
        self._at_newline=True
        self.out_file = open(filename, 'w')
    
    def indent(self, count = 1):
        self._indent += count
    
    def outdent(self, count = 1):
        self._indent -= count
    
    def _write(self,data):
        if wrapper_helper.hasError(): return
        
        if self._at_newline:
            self.out_file.write('%s%s' % ('  ' * self._indent, data)),
        else:
            self.out_file.write('%s' % data),
        
        self._at_newline = data[-1] == "\n"
    
    def write(self,data):
        if wrapper_helper.hasError(): return
        
        lines = data.splitlines(True)
        for line in lines:
            self._write(line)
    
    def writeln(self,data=''):
        if wrapper_helper.hasError(): return
        
        lines = data.splitlines(True)
        for line in lines:
            self._write(line)
        self._at_newline = True
        self.out_file.write('\n')
    
    def close(self):
        self.out_file.close()
    

def main():
    pass


if __name__ == '__main__':
    main()

