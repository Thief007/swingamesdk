# #!/usr/bin/env python
# # encoding: utf-8
# """
# create_c_lib.py
# 
# Created by Andrew Cain on 2009-06-02.
# Copyright (c) 2009 Swinburne. All rights reserved.
# """
# 
import logging
import sys

from lang_c import create_c_code_for_file, write_c_code_files
from sg import parser_runner

def main():
    logging.basicConfig(level=logging.WARNING,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    
    parser_runner.parse_all_units()
    parser_runner.visit_all_units(create_c_code_for_file)
    parser_runner.visit_all_units(write_c_code_files)

if __name__ == '__main__':
    main()

