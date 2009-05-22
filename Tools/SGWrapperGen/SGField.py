#!/usr/bin/env python
# encoding: utf-8
"""
SGField.py

Created by Andrew Cain on 2009-05-20.
Copyright (c) 2009 Swinburne. All rights reserved.
"""

import SGMetaDataContainer

class SGField(SGMetaDataContainer.SGMetaDataContainer):
    """Represents a Field, defining a getter and setter method"""
    
    def __init__(self, name):
        """initialises the Field setting its name."""
        
        SGMetaDataContainer.SGMetaDataContainer.__init__(self)
        
        self.name = name
    
    def set_data_type(self, type_name):
        """Sets the data type of the field."""
        self.set_tag('type', [type_name])
    
    def data_type(self):
        """Gets the SGField's data type"""
        return self.tags['type'].other[0]
    

from SGMethod import SGMethod

def test_field_creation():
    """tests the creation of a basic field"""
    my_field = SGField("name")
    
    assert my_field.name == "name"

def test_field_with_type():
    """tests creating a field with a type"""
    my_field = SGField("name")
    
    my_field.set_data_type("String")
    
    assert my_field.name == "name"
    assert my_field.data_type() == "String"

if __name__ == '__main__':
    import nose
    nose.run()
