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

DEBUG = False

types = {
  'function': None,
  'procedure': None,
  'comment line': None,
  'attribute': None,
  'none': None
}

class SGToken():
    def __init__(self):
        self._id = 'none'

def read_token(pas_lines):
    pass

def main():
    pass


if __name__ == '__main__':
    main()

