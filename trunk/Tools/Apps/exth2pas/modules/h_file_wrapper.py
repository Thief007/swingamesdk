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

class HFileWrapper(object):
    
    def __init__(self, path):
        self.path = path    # The path to the headers
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
            
    def _export_defines_from(self, filename):
        '''
        The preprocessor will have stripped the #defines...
        This code attempts to add these back in...
        '''
        fullname = self.path + filename.replace('"', '')
        if os.path.exists(fullname):
            # print "Opening ", fullname
            hfile = open(fullname)
            lines = hfile.readlines()
            hfile.close()
            
            for line in lines:
                if re.search("#define", line):
                    self.define_file.write(line)
                    # print line

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
            
            if '1' in parts[1]:
                # scan file for #defines and reimport them
                self._export_defines_from(parts[2])
            
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
    
    def process_file(self, filename, outfile, inlinefile, definefile, hidden_types = [], defines = ""):
        """
        
        """
        self.filename = filename
        self.line_no = 0
        self.unk_enum_count = 0
        f = open(filename)
        self.c_lines = f.readlines()
        f.close()
        
        self.out_file = open(outfile, 'w')
        self.inline_file = open(inlinefile, 'w')
        self.define_file = open(definefile, 'w')
        
        self._skip_empty_lines()
        
        print defines
        self.out_file.write(defines)
        
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
        self.define_file.close()
        
def main():
    fproc = HFileWrapper('/Users/acain/Downloads/SDL-1.3.0-6050/include/');
    
    defines = """
        #define SDL_INIT_TIMER          0x00000001
        #define SDL_INIT_AUDIO          0x00000010
        #define SDL_INIT_VIDEO          0x00000020
        #define SDL_INIT_JOYSTICK       0x00000200
        #define SDL_INIT_HAPTIC         0x00001000
        #define SDL_INIT_NOPARACHUTE    0x00100000      /**< Don't catch fatal signals */
        #define SDL_INIT_EVERYTHING     0x0000FFFF
        #define SDL_BUTTON(X)		(1 << ((X)-1))
        #define SDL_BUTTON_LEFT		1
        #define SDL_BUTTON_MIDDLE	2
        #define SDL_BUTTON_RIGHT	3
        #define SDL_BUTTON_X1		4
        #define SDL_BUTTON_X2		5
        #define SDL_BUTTON_LMASK	SDL_BUTTON(SDL_BUTTON_LEFT)
        #define SDL_BUTTON_MMASK	SDL_BUTTON(SDL_BUTTON_MIDDLE)
        #define SDL_BUTTON_RMASK	SDL_BUTTON(SDL_BUTTON_RIGHT)
        #define SDL_BUTTON_X1MASK	SDL_BUTTON(SDL_BUTTON_X1)
        #define SDL_BUTTON_X2MASK	SDL_BUTTON(SDL_BUTTON_X2)
        #define SDL_HAT_CENTERED	0x00
        #define SDL_HAT_UP		0x01
        #define SDL_HAT_RIGHT		0x02
        #define SDL_HAT_DOWN		0x04
        #define SDL_HAT_LEFT		0x08
        #define SDL_HAT_RIGHTUP		(SDL_HAT_RIGHT|SDL_HAT_UP)
        #define SDL_HAT_RIGHTDOWN	(SDL_HAT_RIGHT|SDL_HAT_DOWN)
        #define SDL_HAT_LEFTUP		(SDL_HAT_LEFT|SDL_HAT_UP)
        #define SDL_HAT_LEFTDOWN	(SDL_HAT_LEFT|SDL_HAT_DOWN)
        #define SDL_SWSURFACE       0x00000000  /**< Note Not used */
        #define SDL_SRCALPHA        0x00010000
        #define SDL_SRCCOLORKEY     0x00020000
        #define SDL_ANYFORMAT       0x00100000
        #define SDL_HWPALETTE       0x00200000
        #define SDL_DOUBLEBUF       0x00400000
        #define SDL_FULLSCREEN      0x00800000
        #define SDL_RESIZABLE       0x01000000
        #define SDL_NOFRAME         0x02000000
        #define SDL_OPENGL          0x04000000
        #define SDL_HWSURFACE       0x08000001  /**< Note Not used */
        #define SDL_ASYNCBLIT       0x08000000  /**< Note Not used */
        #define SDL_RLEACCELOK      0x08000000  /**< Note Not used */
        #define SDL_HWACCEL         0x08000000  /**< Note Not used */
        #define SDL_APPMOUSEFOCUS	0x01
        #define SDL_APPINPUTFOCUS	0x02
        #define SDL_APPACTIVE		0x04
        #define SDL_LOGPAL 0x01
        #define SDL_PHYSPAL 0x02
        #define SDL_ACTIVE_EVENT	SDL_EVENT_COMPAT1
        #define SDL_VIDEORESIZE	SDL_EVENT_COMPAT2
        #define SDL_VIDEOEXPOSE	SDL_EVENT_COMPAT3
        #define SDL_BUTTON_WHEELUP	4
        #define SDL_BUTTON_WHEELDOWN	5
        #define SDL_DEFAULT_REPEAT_DELAY	500
        #define SDL_DEFAULT_REPEAT_INTERVAL	30
        #define SDL_YV12_OVERLAY  0x32315659    /**< Planar mode: Y + V + U  (3 planes) */
        #define SDL_IYUV_OVERLAY  0x56555949    /**< Planar mode: Y + U + V  (3 planes) */
        #define SDL_YUY2_OVERLAY  0x32595559    /**< Packed mode: Y0+U0+Y1+V0 (1 plane) */
        #define SDL_UYVY_OVERLAY  0x59565955    /**< Packed mode: U0+Y0+V0+Y1 (1 plane) */
        #define SDL_YVYU_OVERLAY  0x55595659    /**< Packed mode: Y0+V0+Y1+U0 (1 plane) */
    """
    
    fproc.process_file("../test/allsdl.c", "../test/sdl.h", "../test/sdl_inline.c", "../test/sdl_define.c", defines=defines)

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

        
        