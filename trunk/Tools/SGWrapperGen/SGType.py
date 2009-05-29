#!/usr/bin/env python
# encoding: utf-8
"""
SGType.py

Created by Andrew Cain on 2009-05-29.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import logging

from SGMetaDataContainer import SGMetaDataContainer

_loaded_types = {}
logger = logging.getLogger("SGWrapperGen")

class SGType(SGMetaDataContainer):
    """Represents a data type."""
    
    def __init__(self, name):
        """Initialise the type, setting its name"""
        SGMetaDataContainer.__init__(self, ['fields','class','uname'])
        self.name = name
        self.set_tag('fields', [])
    
    def __str__(self):
        '''String rep'''
        return self.name
    
    fields = property(lambda self: self['fields'].other, None, None, "The fields for the type.")

def find_or_add_type(name):
    global _loaded_types
    global logger
    
    if name in _loaded_types:
        return _loaded_types[name]
    else:
        logger.info('Created Type %s', name)
        result = SGType(name)
        _loaded_types[name] = result
        return result
    

def main():
  pass

if __name__ == '__main__':
  main()

