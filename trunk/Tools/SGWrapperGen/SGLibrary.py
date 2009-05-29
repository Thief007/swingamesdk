#!/usr/bin/env python
# encoding: utf-8
"""
SGLibrary.py

Created by Andrew Cain on 2009-05-22.
Copyright (c) 2009 Swinburne. All rights reserved.
"""

from SGMethod import SGMethod
from SGMetaDataContainer import SGMetaDataContainer

class SGLibrary(SGMetaDataContainer):
    """Represents the SwinGame SDK library."""
    
    def __init__(self):
        """Initialise the library"""
        SGMetaDataContainer.__init__(self, [])
        self.methods = {}
    
    def add_method(self, member):
        """Add a method to the library"""
        if isinstance(member, SGMethod):
            self.methods[member.name] = member
        else:
            raise Exception, "Unknown member type"
    

import nose
from nose.tools import raises 

def test_library_creation():
    """test basic library creation"""
    my_library = SGLibrary()
    
    assert len(my_library.methods) == 0

def test_add_method():
    """test adding a method to a library"""
    my_library = SGLibrary()
    my_method = SGMethod('test')
    
    my_library.add_method(my_method)
    
    print my_library.methods
    
    assert len(my_library.methods) == 1
    assert my_method == my_library.methods["test"]

@raises(Exception)
def test_add_unkown_member():
    """test adding some unknown member type to the library, expects to fail"""
    my_library = SGLibrary("Hello")
    my_library.add_method("Hello")
        
if __name__ == '__main__':
    nose.run()
