#!/usr/bin/env python
# encoding: utf-8
"""
pas_post_processor.py

Created by Andrew Cain on 2011-12-01.
Copyright (c) 2011 Swinburne University of Technology. All rights reserved.
"""

import sys
import os
import re

class PasPostProcessor:
    
    def _current_line(self):
        return self.pas_lines[self.line_no - 1]
    
    def _in_comment(self):
        return self._current_line().find("(*") == 0 or self.brace_count > 0
    
    def _advance_to_nextline(self):
        """Advance to the next line of code in the file... only basic checking for comments and non-empty lines"""
        self.line_no += 1
        
        while len(self._current_line().strip()) == 0 or self._in_comment():
            self.line_no += 1
            if self._current_line().find("{") == 0: 
                self.brace_count += 1
            if self._current_line().find("}") == 0: 
                self.brace_count -= 1
                self.line_no += 1
        return self._current_line()
    
    def _process_unit_header(self):
        """Read the start of the unit, searching for first type, function, or const"""
        line = self._current_line()
        
        assert line.find("unit") == 0
        
        self._write_output("// \n")
        self._write_output("// Exported by exth2pas\n")
        self._write_output("// \n")
        self._write_output("{$mode objfpc}\n")
        self._write_output("{$packrecords C}\n")
        self._write_output("\n")
        self._write_output(line)
        
        line = self._advance_to_nextline()
        assert line.find("interface") == 0
        self._write_output(line)
        
    
    def _type_name_on_line(self, line):
        result = re.match("\s*([A-Za-z_][A-Za-z0-9_]*)\s*=", line)
        # print result
        return None if result is None else result.group(1)
    
    def _is_current_line_enum(self, name):
        line = self._current_line()
        if re.search(name + "\s*=\s*\(", line): #look for standard enum
            return True
        elif re.search(name + " =  Longint;$", line): #look for const mapping
            if re.search("unk_[0-9]+", name): #unnamed enum
                return False
            #possible enum... search forward...            
            line_no = self.line_no - 1  #-1 as self.line_no is 1 based... this is current line
            line_no += 1                # check the next line
            if re.search("Const", self.pas_lines[line_no]) is None:
                return False            # Not followed by Const
            line_no += 1
            while re.search("^\s*[A-Za-z_][A-Za-z0-9_]*\s*=\s*.+;$", self.pas_lines[line_no]):
                line_no += 1
            return re.search("^;$", self.pas_lines[line_no]) is not None #search for erroneous ; at end of enums mapped to conts :)
        else:
            return False
    
    def _read_enum_type(self, name):
        """The current line is the start of an enum... read to its end"""
        result = list()
        line = self._current_line()
        
        if re.search(name + " =  Longint;$", line): #need to map const list to enum
            result.append("    %s = (\n" % name)     #add start of enum...
            line = self._advance_to_nextline()
            assert re.search("Const", line)
            line = self._advance_to_nextline()
            while re.search("^\s*[A-Za-z_][A-Za-z0-9_]*\s*=\s*.+;$", line):
                result.append(line.replace("=", ":=").replace(";", ","))
                line = self._advance_to_nextline()
            #remove last ,
            result[-1] = result[-1].rstrip(",\n") + "\n"
            
            assert re.search("^;$", line)
            result.append("    );\n")     #add end of enum
        else:
            while not re.search("\);", line):
                result.append(line)
                line = self._advance_to_nextline()
            
            result.append(line)
        
        # print result
        return result
    
    def _read_record_type(self):
        """The current line is the start of a record... read to its end"""
        result = list()
        nest = 0
        
        line = self._current_line()
        while nest > 0 or not re.search("end;", line):
            #print re.search("end;", line)
            result.append(line)
            line = self._advance_to_nextline()
            if re.search("\s(record|case)\s", line):
                nest += 1
            elif re.search("\send(\s|;$)", line):
                nest -= 1
            #print nest, line
        
        result.append(line)
        
#         print result  
        return result
    
    def _is_undefined_structure(self, lst):
        return len(lst) == 3 and re.search("\{undefined structure\}", lst[1]) is not None
    
    def _is_ignored(self, name):
        for fn in self.ignore_types:
            if fn(name): 
                return True
        return False
    
    def _remove_struct(self, type_name, found_types):
        
        for (k1, lines1) in found_types.items():
            if k1 == type_name: continue    #skip self
            
            for (idx, line) in enumerate(lines1):   # loop through lines switching ^type_name with Ptype_name
                if re.search("(=|:)\s*%s" % type_name, line):
                    if re.search("=\s*%s\s*" % type_name, line):    #alias of type
                        alias_name = re.match("^\s*([A-Za-z_][A-Za-z0-9_]*)\s*=\s*%s\s*;" % type_name, line).group(1)
                        assert alias_name
                        print "   - Removing alias", alias_name
                        found_types[alias_name] = list()
                        self._remove_struct(alias_name, found_types)
                    else: print "   - Error... access to undefined type ", k
                elif re.search("P%s\s*=\s*\^%s\s*;" % (type_name,type_name), line):
                    print "   - Updating P%s to Pointer" % type_name
                    newline = line.replace("^%s" % type_name, "Pointer")
                    lines1[idx] = newline
                elif re.search("(=|:)\s*\^%s\s*;" % type_name, line):
                    print "   - Switching ^%s to P%s" % (type_name, type_name)
                    newline = line.replace("^%s" % type_name, "P%s" % type_name)
                    lines1[idx] = newline
        
    
    def _remove_undefined_structures(self, found_types):
        '''
        Forward declared types that are only accessed by pointers should be removed
        so they cannot be accidentally used...
        '''
        for (k, lines) in found_types.items():
            if len(lines) == 3 and re.search("{undefined structure}", lines[1]):
                print " * Removing undefined type %s" % k
                found_types[k] = list()     #remove its output :)
                
                self._remove_struct(k, found_types)
                
    
    def _process_consts(self):
        start_line = self.line_no
        
        in_const_block = False
        self._write_output("const\n")
        
        line = self._current_line()
        sline = line.strip()
        
        while self.line_no < len(self.pas_lines):
            # print self._current_line().strip(), in_const_block
            if re.search("^implementation$", sline):
                break   #end type search at implementation
            elif re.search("^const$", sline, flags=re.IGNORECASE):   #found the start of a type block
                #step back a line and see if this is an enum...
                self.line_no -= 1
                name = self._type_name_on_line(self._current_line())
                if name and self._is_current_line_enum(name):
                    self.line_no += 2   #skip current line
                    in_const_block = False
                else:
                    self.line_no += 1
                    #print "** True at ", self._current_line().strip(), line.strip()
                    in_const_block = True
            elif re.search("^((function)|(procedure)|(type))", sline, flags=re.IGNORECASE): #at end of type block
                in_const_block = False
            elif in_const_block: #in type block, not at the end...
                self._write_output("  " + line)
            
            #print line, in_const_block
            
            line = self._advance_to_nextline()
            sline = line.strip()
        
        # return to the starting line
        self.line_no = start_line
        
    
    def _process_types(self, cust_types, type_procs):
        '''
        The types exported from h2pas require some editing...
        
        * enums are exported as const, and need to be detected and returned to enum types
          - Note: h2pas has an error exporting enums -> enums with truncating long lines it merges
          - It also has an error exporting as const that we are exploiting. The list of consts ends with an erroneous ;
        * pointer types are added to the top, but their components are not in the same type block
        
        This code then builds a list of the type code, and exports it as a single type block.
        This checks for duplicates, as well as adding the custom types to allow for customisation
        '''
        # remember where we were in the file
        start_line = self.line_no
        
        #build a list of the types...
        found_types = dict()
        keys = list()
        
        for tpl in cust_types:
            keys.append(tpl[0])
            # found_types[tpl[0]] = tpl[1]
        
        # print keys
        
        #scan looking for new types...
        in_type_block = False
        line = self._current_line()
        sline = line.strip()
        while self.line_no < len(self.pas_lines):
            if re.search("^implementation$", sline):
                break   #end type search at implementation
            elif re.search("^type$", sline, flags=re.IGNORECASE):   #found the start of a type block
                in_type_block = True
            elif re.search("^((function)|(procedure)|(const))", sline, flags=re.IGNORECASE): #at end of type block
                in_type_block = False
            elif in_type_block: #in type block, not at the end...
                name = self._type_name_on_line(line)
                # print name
                if self._is_current_line_enum(name):
                    to_add = self._read_enum_type(name)
                elif re.search("record", sline):
                    to_add = self._read_record_type()
                else:
                    to_add = list()
                    to_add.append(line)
                    
                if not name.lower() in map(str.lower, keys):
                    keys.append(name)
                    found_types[name] = to_add
                elif not name in found_types:
                    print " - error type duplicate on case mismatch or custom type " + name
                else:
                    if self._is_undefined_structure(found_types[name]):
                        #if it was previously an alias, make it this new version
                        keys.remove(name)
                        keys.append(name) # move to end
                        found_types[name] = to_add
                        print " - replacing undefined structure... "
                    else:
                        print " - duplicate type - ", name
                        
            line = self._advance_to_nextline()
            sline = line.strip()
        
        # return to the starting line
        self.line_no = start_line
        
        self._remove_undefined_structures(found_types)
        
        #write the types to the output file.
        self._write_output("type\n")
        for tpl in cust_types:
            self._write_output("  " + tpl[1])
            keys.remove(tpl[0])
        for key in keys: # keep the order the same as in the original file
            #print "****%s****" % key
            if self._is_ignored(key):
                continue    #skip ignored types
            for line in found_types[key]:
                for fn in type_procs:
                    line = fn(line)
                
                self._write_output("  " + line)
                #print line
            self._write_output("\n")
    
    def _process_functions(self, fn_proc):
        # remember where we were in the file
        start_line = self.line_no
        
        # read each line...
        #scan looking for new types...
        line = self._current_line()
        sline = line.strip()
        while self.line_no < len(self.pas_lines):
            if re.search("^implementation$", sline):
                break   #end type search at implementation
            elif re.search("^((function)|(procedure))", sline, flags=re.IGNORECASE): #at end of type block
                while True:
                    while re.search("\^[A-Za-z_][A-Za-z0-9_]*\s*[;)]", line):
                        type_name = re.search("\^([A-Za-z_][A-Za-z0-9_]*)\s*[;)]", line).group(1)
                        line = line.replace("^%s" % type_name, "P%s" % type_name)
                    
                    for fn in fn_proc:
                        line = fn(line)
                    self._write_output(line)
                    
                    line = self._advance_to_nextline()
                    sline = line.strip()
                    
                    #test if not ended by external...
                    if re.search("^(function|procedure|const|type|implementation)", sline, flags=re.IGNORECASE):
                        self.line_no -= 1 # back up one line...
                        break #end while True
                
            # Now advance to the new line
            line = self._advance_to_nextline()
            sline = line.strip()
        
        # return to the starting line
        self.line_no = start_line
    
    def _process_implementation(self):
        line = self._current_line()
        while self.line_no < len(self.pas_lines):
            if re.search("^implementation$", line):
                break   #end type search at implementation
            line = self._advance_to_nextline()
        
        while self.line_no < len(self.pas_lines):
            self._write_output(line)
            line = self._advance_to_nextline()
    
    def _write_output(self, data):
        self.out_file.write(data)
        # print data
    
    def process_file(self, pas_file_name, out_file, cust_types = [], type_procs = [], ignore_types = [], fn_proc=[]):
        """Post process the supplied pas file."""
        
        # Read the lines from the file
        f = open(pas_file_name)
        self.pas_lines = f.readlines()
        f.close()
        
        self.ignore_types = ignore_types
        
        self.brace_count = 0
        self.line_no = 0
        self._advance_to_nextline()
        
        #override the file
        self.out_file = open(out_file, "w")
        
        #process the file...
        self._process_unit_header()
        self._process_types(cust_types, type_procs)
        self._process_consts()
        self._process_functions(fn_proc)
        self._process_implementation()
        
        self._write_output("implementation\n")
        self._write_output("end.\n")
        
    

def main():
    cust_types = [
        ("size_t",              "    size_t = PtrUInt;\n"),
        ("int64_t",             "    int64_t = Int64;\n"),
        ("Uint64_t",            "    Uint64_t = QWord;\n"),
        ("Uint64",              "    Uint64 = QWord;\n"),   
        ("int32_t",             "    int32_t = Longint;\n"),
        ("Uint32_t",            "    Uint32_t = Longword;\n"),
        ("Uint32",              "    Uint32 = Longword;\n"),
        ("int16_t",             "    int16_t = SmallInt;\n"),
        ("Uint16_t",            "    Uint16_t = Word;\n"),
        ("Uint16",              "    Uint16 = Word;\n"),
        ("int8_t",              "    int8_t = ShortInt;\n"),
        ("uint8_t",             "    uint8_t = Byte;\n"),
        ("Uint8",               "    Uint8 = Byte;\n"),
        ("PUint8",             "     PUint8 = ^Uint8;\n"),
        ("PPUint8",             "    PPUint8 = ^PUint8;\n"),
        ("wchar_t",             "    wchar_t = WideChar;\n"),
        ("PSDL_Finger",         "    PSDL_Finger = ^SDL_Finger;\n"),
        ("PPSDL_Finger",        "    PPSDL_Finger = ^PSDL_Finger;\n"),
        ("PSDL_SysWMmsg",       "    PSDL_SysWMmsg = Pointer;\n"),
        ("PSDL_VideoInfo",       "    PSDL_VideoInfo = ^SDL_VideoInfo;\n"),
        ("PPSDL_Rect",       "    PPSDL_Rect = ^PSDL_Rect;\n"),
        # ("PSDL_assert_data",    "    PSDL_assert_data = ^SDL_assert_data;\n"),
        # ("PSDL_PixelFormat",    "    PSDL_PixelFormat = ^SDL_PixelFormat;\n"),
    ]
    ignore_types = [
        lambda name: re.search("_dummy_|^unk_", name) is not None,
    ]
    proc = [
        lambda line: line.replace("mod : Uint16;", "kmod : Uint16;"),
        lambda line: line.replace("^^SDL_Finger", "PPSDL_Finger"),
        lambda line: re.sub(": \^(private_yuvhwdata|private_yuvhwfuncs|SDL_BlitMap|SDL_SysWMmsg|SDL_Cursor|SDL_Window|_SDL_iconv_t)", ": Pointer", line), # replace with pointers...
        lambda line: re.sub("= \^(_SDL_iconv_t|SDL_Cursor|SDL_Window)", "= Pointer", line), # replace with pointers...
        lambda line: line.replace(": ^^", ": PP"),
        lambda line: line.replace(": ^", ": P"),
        # These types are also... enum values... argh.. change the enum names
        lambda line: line.replace("SDL_WINDOWEVENT :=", "SDL_WINDOW_EVENT :="),
        lambda line: line.replace("SDL_USEREVENT :=", "SDL_USER_EVENT :="),
        lambda line: line.replace("SDL_SYSWMEVENT :=", "SDL_SYSWM_EVENT :="),
        lambda line: line.replace("SDL_QUIT :=", "SDL_QUIT_EVENT :="),
        # This is a type and a function... rename the type
        lambda line: line.replace("SDL_threadID =", "SDL_Thread_ID ="),
    ]
    
    fnproc = [
        lambda line: line.replace(":SDL_threadID", ":SDL_Thread_ID"),   # Convert SDL_ThreadID parameters
        lambda line: line.replace("file:Pchar", "fname:Pchar"),         # Convert file parameter names
        lambda line: line if "va_list" not in line else re.sub("; [A-Za-z_][A-Za-z0-9_]*:va_list", "", line).replace("external;", "varargs; external;"),         # ?? va_list 
    ]
    
    fproc = PasPostProcessor();
    fproc.process_file("../test/sdl.pp1", "../test/sdl.pas", cust_types, proc, ignore_types, fnproc)

if __name__ == '__main__':
    main()
        
        
