#!/usr/bin/env python
# encoding: utf-8
"""
SGType.py

Created by Andrew Cain on 2009-05-29.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import logging

from SGMetaDataContainer import SGMetaDataContainer
from sgcache import logger

class SGType(SGMetaDataContainer):
    """Represents a data type."""
    
    def __init__(self, name):
        """Initialise the type, setting its name"""
        SGMetaDataContainer.__init__(self, ['fields',
            'class','uname', 'dimensions','nested_type', 'related_type',
            'pointer_wrapper', 'data_wrapper', 'values', 'is_pointer',
            'struct'])
        self.name = name
        self.set_tag('fields', [])
        self.dimensions = None
        self.related_type = None
        self.pointer_wrapper = False
        self.data_wrapper = False
        self.values = None
        self.is_pointer = False
        self.is_class = False
        self.is_struct = False
    
    def __str__(self):
        '''String rep'''
        return self.name
    
    def __repr__(self):
        return self.name
    
    def clone(self, other):
        '''Clones all but name'''
        self.set_tag('fields',other.fields)
        self.dimensions = other.dimensions
        self.is_pointer = other.is_pointer
        self.related_type = other
    
    fields = property(lambda self: self['fields'].other, None, None, "The fields for the type.")
    
    dimensions = property(lambda self: self['dimensions'].other, 
        lambda self, value: self.set_tag('dimensions', value), 
        None, 'The dimensions of this if it is an array')
    
    is_pointer = property(lambda self: self['is_pointer'].other, 
        lambda self, value: self.set_tag('is_pointer', value), 
        None, 'The type is a pointer to its related type')
    
    is_class = property(lambda self: self['class'].other, 
        lambda self, value: self.set_tag('class', value), 
        None, 'The type is class')
    
    is_struct = property(lambda self: self['struct'].other, 
        lambda self, value: self.set_tag('struct', value), 
        None, 'The type is struct')
    
    nested_type = property(lambda self: self['nested_type'].other, 
        lambda self, value: self.set_tag('nested_type', value), 
        None, 'The other type contained within this type eg. array of ...')
    
    related_type = property(lambda self: self['related_type'].other, 
        lambda self, value: self.set_tag('related_type', value), 
        None, 'The other type this one relates to')
    
    pointer_wrapper = property(lambda self: self['pointer_wrapper'].other, 
        lambda self, value: self.set_tag('pointer_wrapper', value), 
        None, 'This type wraps a pointer')
    
    data_wrapper = property(lambda self: self['data_wrapper'].other, 
        lambda self, value: self.set_tag('data_wrapper', value), 
        None, 'This type wraps a pointer')
    
    values = property(lambda self: self['values'].other, 
        lambda self, value: self.set_tag('values', value), 
        None, 'The values for a enumeration')
    
    is_array = property(lambda self: self.dimensions != None, None, None, 'Indicates that this type is an array')
    is_enum = property(lambda self: self.values != None, None, None, 'Indicates that this type is an enumeration')

def main():
  pass

if __name__ == '__main__':
  main()

