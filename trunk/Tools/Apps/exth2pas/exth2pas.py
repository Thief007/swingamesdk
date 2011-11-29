#!/usr/bin/env python
# encoding: utf-8
"""
exth2pas.py

This script strips preprocessed C header files, so that they can be passed through h2pas to create pascal interfaces.

Steps include:
1: 
2: 

Created by Andrew Cain on 2011-11-29.
Copyright (c) 2011. All rights reserved.
"""

import logging
import sys

from lang_pas import create_pas_code_for_file, write_pas_code_files
from lang_c import create_c_code_for_file
from sg import parser_runner

def main():
    logging.basicConfig(level=logging.WARNING,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    
    


if __name__ == '__main__':
    main()
