#!/usr/bin/env python
# encoding: utf-8
"""
h_file_wrapper.py

The h_file_wrapper is used to wrap access to a c preprocessed header file.

Created by Andrew Cain on 2011-11-29.
Copyright (c) 2011 Swinburne University of Technology. All rights reserved.
"""

import sys
import os
import re
import subprocess

class HFileWrapper():
    
    def __init__(self):
        self.filename = ""
        self.line_no = 0
        self.in_filename = "unknown"
        self.c_lines = []
        self.skip_standard_libs = True
        self.unk_enum_count = 0
    
    def current_line(self):
        return self.c_lines[self.line_no]
    
    def _skip_standard_library(self):
        """
        Detect preprocessor includes of standard libraries and skip them...
        """
        current_file = self.in_filename.strip()
        cl = self.current_line()
        while len(cl.strip()) == 0 or cl[0] != '#' or cl.split(' ')[2].strip() != current_file:
            self.line_no += 1
            cl = self.current_line()

    def _test_and_skip_c_files(self):
        """
        Check for any c code, and skip (only want headers...)
        """
        current_file = self.in_filename.strip()
        # print current_file[-2] == "c", current_file, current_file[-2]
        return current_file[-2] == "c"
    
    def _test_and_process_line_info(self):
        '''
        Process all lines that start with a #... format is:
        
        # lineno filename flags
        
        Flags are 
            1 = start of a new file
            2 = returning from a file
            3 = system header
            4 = extern c header
        '''
        line = self.current_line()
        if line[0] == "#": 
            parts = line.split(' ')
            
            if self.skip_standard_libs and '3' in parts:
                print ' * Skipping ', parts[2]
                self._skip_standard_library()
                # print ' *** -----'
                return self._test_and_process_line_info()
            
            self.in_filename = parts[2]
            # print ' * In file ', self.in_filename
            
            return True
        return False
    
    def _test_and_skip_inline(self):
        '''
        Checks for inline functions. These written to a separate output file, and skipped from the standard input.
        '''
        if "__inline" in self.current_line():
            # Read to the end of the function...
            open_brace = 0;
            close_brace = 0;
            
            while open_brace == 0 or open_brace != close_brace:
                cl = self.current_line()
                # print cl
                self.inline_file.write(cl)
                open_brace += cl.count('{')
                close_brace += cl.count('}')
                self.line_no += 1
            
            return True
        return False;
        
    def _skip_empty_lines(self):
        '''
        Skips empty lines, lines with meta information ( # ... ), and standard libraries (based on skip_standard_libs)
        '''
        while not self.end_of_input() and (
                len(self.current_line().strip()) == 0 or 
                self._test_and_process_line_info() or 
                self._test_and_skip_inline() or
                self._test_and_skip_c_files() ):
            self.line_no += 1
    
    def process_file(self, filename, outfile, inlinefile, hidden_types = []):
        """
        
        """
        self.filename = filename
        self.line_no = 0
        self.unk_enum_count = 0
        f = open(filename)
        self.c_lines = f.readlines()
        f.close()
        self._skip_empty_lines()
        
        self.out_file = open(outfile, 'w')
        self.inline_file = open(inlinefile, 'w')
        
        while not self.end_of_input():
            self.out_file.write(self.read_line())
            
        self.close()
        
        subprocess.call("h2pas -e -d -pr -p %s" % outfile, shell=True)
        
        pp_name = outfile.replace(".h", ".pp")
        
        
        
    
    def _strip_extern_attributes(self, line):
        return line.replace('__attribute__ ((visibility("default"))) ', '')

    def _add_enum_names(self, line):
        if re.search('(^|\s)enum\s', line) and 'typedef' not in line:
            self.unk_enum_count += 1
            return line.replace('enum', 'enum unk_%d' % self.unk_enum_count);
        return line
    
    def _fix_char_quote(self, line):
        return line.replace("'\\''", '39')
        
    '''
    h2pas messes up typedef struct sname name; maps it to sname = name...
    but should map it to name = sname... so switch them in the file :) HACK
    '''
    def _fix_typedef_struct(self, line):
        m = re.search('typedef\s+struct\s+([^{}* ]*?)\s+([^{}* ]*?);', line.strip())
        if m and m.group(1) != m.group(2):
            print ' * Switching ', m.group(1), m.group(2)
            return 'typedef struct %s %s;' % (m.group(2), m.group(1))
        
        return line
        
    def _update_line(self, line):
        fns = [ lambda line: self._strip_extern_attributes(line), 
                lambda line: self._add_enum_names(line),
                lambda line: self._fix_char_quote(line),
                lambda line: self._fix_typedef_struct(line) ]
        
        for fn in fns:
            line = fn(line)
        
        return line
        
    def read_line(self):
        result = self.current_line()
        self.line_no += 1
        self._skip_empty_lines()
        
        return self._update_line(result)
    
    def end_of_input(self):
        return self.line_no >= len(self.c_lines)
    
    def close(self):
        self.out_file.close()
        self.inline_file.close()
        
def main():
    fproc = HFileWrapper();
    fproc.process_file("../test/allsdl.c", "../test/sdl.h", "../test/sdl_inline.c")

if __name__ == '__main__':
    main()

    # need to add...
    #     
    # Type
    #     size_t = cuint32;
    #     wchar_t = WideChar;
    #     int8_t = cint8;
    #     uint8_t = cuint8;
    #     int16_t = cint16;
    #     uint16_t = cuint16;
    #     int32_t = cint32;
    #     uint32_t = cuint32;
    #     int64_t = cint64;
    #     uint64_t = cuint64;
    #     Ppcchar = ^pchar;

        
        