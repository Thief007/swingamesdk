#!/usr/bin/env python
# encoding: utf-8
"""
SGParameter.py

Created by Andrew Cain on 2009-05-21.
Copyright (c) 2009 Swinburne. All rights reserved.
"""

from SGMetaDataContainer import SGMetaDataContainer

class SGParameter(SGMetaDataContainer):
    """Represents a parameter to a method"""
    
    def __init__(self, name):
        """initialise the parameter with a name, sets type to None"""
        SGMetaDataContainer.__init__(self)
        self.name = name
        self.set_type(None)
    
    def set_as_output(self):
        """marks this as an output parameter"""
        self.set_tag('output')
    
    def is_output(self):
        """returns True if the parameter is an output parameter"""
        #print self.tags.keys()
        return 'output' in self.tags.keys()
    
    def set_as_array(self):
        """marks this as an array parameter"""
        self.set_tag('array')
    
    def is_array(self):
        """returns True if the parameter is an array parameter"""
        #print self.tags.keys()
        return 'array' in self.tags.keys()
    
    def set_type(self, param_type):
        """sets the type of the parameter"""
        self.set_tag('type', [param_type])
    
    def type(self):
        """returns the type set for this parameter"""
        return self.tags['type'].other[0]
    

#
# Test methods
#

def test_parameter_creation():
    """tests basic creation of a parameter"""
    my_param = SGParameter("test")
    
    assert my_param.name == "test"
    assert len(my_param.tags) == 1 #type
    assert my_param.type() == None

def test_output_param():
    """tests the creation of an output parameter"""
    my_param = SGParameter("Test")
    assert False == my_param.is_output()
    
    my_param.set_as_output()
    assert my_param.is_output()

def test_array_param():
    """tests the creation of an array parameter"""
    my_param = SGParameter("Test")
    assert False == my_param.is_array()
    
    my_param.set_as_array()
    assert my_param.is_array()

def test_type_param():
    """tests the setting of a parameters type"""
    my_param = SGParameter("Test")
    assert None == my_param.type()
    
    my_param.set_type("SoundEffect")
    assert "SoundEffect" == my_param.type()

if __name__ == '__main__':
    import nose
    nose.run()