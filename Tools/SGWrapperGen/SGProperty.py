#!/usr/bin/env python
# encoding: utf-8
"""
SGProperty.py

Created by Andrew Cain on 2009-05-20.
Copyright (c) 2009 Swinburne. All rights reserved.
"""

import SGMetaDataContainer

class SGProperty(SGMetaDataContainer.SGMetaDataContainer):
    """Represents a property, defining a getter and setter method"""
    
    def __init__(self, name):
        """
        initialises the property setting its name. 
        
        getter and setter are set to none
        """
        
        SGMetaDataContainer.SGMetaDataContainer.__init__(self)
        
        self.name = name
        self.getter = None
        self.setter = None
    
    def set_getter(self, method):
        """sets the getter method of the property"""
        self.getter = method
    
    def set_setter(self, method):
        """sets the setter method of the property"""
        self.setter = method
    

from SGMethod import SGMethod

def test_property_creation():
    """tests the creation of a basic property"""
    my_property = SGProperty("name")
    
    assert my_property.name == "name"
    assert my_property.getter == None
    assert my_property.setter == None

def test_setting_setter():
    """tests the adding of a setter"""
    my_property = SGProperty("name")
    my_setter = SGMethod("setName")
    
    my_property.set_setter(my_setter)
    
    assert my_property.name == "name"
    assert my_property.getter == None
    assert my_property.setter == my_setter

def test_setting_getter():
    """tests the adding of a getter"""
    my_property = SGProperty("name")
    my_getter = SGMethod("name")
    
    my_property.set_getter(my_getter)
    
    assert my_property.name == "name"
    assert my_property.getter == my_getter
    assert my_property.setter == None

if __name__ == '__main__':
    import nose
    nose.run()
