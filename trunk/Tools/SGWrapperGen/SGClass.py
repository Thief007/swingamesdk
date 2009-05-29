#!/usr/bin/env python
# encoding: utf-8
"""
SGClass.py

Created by Andrew Cain on 2009-05-20.
Copyright (c) 2009 Swinburne. All rights reserved.
"""

import logging

from SGMethod import SGMethod
from SGProperty import SGProperty
from SGField import SGField
from SGMetaDataContainer import SGMetaDataContainer

logger = logging.getLogger("SGWrapperGen")

class SGClass(SGMetaDataContainer):
    """Represents a class in the comment meta language."""
    
    def __init__(self, name):
        """Initialise the class, setting its name"""
        SGMetaDataContainer.__init__(self, ['static','struct','class'])
        self.name = name
        self.methods = dict()
        self.properties = dict()
        self.fields = dict()
    
    def add_member(self, member):
        """Add a member (method, property) to the class"""
        global logger
        
        if isinstance(member, SGMethod):
            self.methods[member.name] = member
        elif isinstance(member, SGProperty):
            self.properties[member.name] = member
        elif isinstance(member, SGField):
            logger.info('Adding field (%s) %s.%s', member.data_type, self.name, member.name)
            self.fields[member.name] = member
        else:
            raise Exception, "Unknown member type"
    
    def set_as_static(self):
        """sets the class as static, deny object creation"""
        self.set_tag('static', True)
    
    def is_static(self):
        """returns true if this should be a static class"""
        return 'static' in self.tags
    
    def set_as_struct(self):
        """sets the class as a struct"""
        self.set_tag('struct')
    
    def is_struct(self):
        """returns true if this should be a struct"""
        return 'struct' in self.tags
    
    def setup_from(self, the_type):
        global logger
        
        fields = the_type.fields
        
        for field in fields:
            self.add_member(field)
    
    def __str__(self):
        '''returns a string representation of the class'''
        name = 'struct ' + self.name if self.is_struct() else 'class ' + self.name
        name = 'static ' + name if self.is_static() else name
        
        
        return '%s\n%s' % (self.doc, name)
    

import nose
from nose.tools import raises 

def test_class_creation():
    """test basic class creation"""
    my_class = SGClass("Hello")
    
    assert my_class.name == "Hello"
    assert len(my_class.methods) == 0

def test_add_method():
    """test adding a method to a class"""
    my_class = SGClass("Hello")
    my_method = SGMethod("test")
    
    my_class.add_member(my_method)
    
    assert len(my_class.methods) == 1

def test_add_property():
    """test adding a property to the class"""
    my_class = SGClass("Hello")
    my_property = SGProperty("test")
    
    my_class.add_member(my_property)
    
    assert len(my_class.properties) == 1

def test_add_field():
    """test adding a field to the class"""
    my_class = SGClass("Hello")
    my_field = SGField("test")
    
    my_class.add_member(my_field)
    
    assert len(my_class.fields) == 1

@raises(Exception)
def test_add_unkown_member():
    """test adding some unknown member type to the class, expects to fail"""
    my_class = SGClass("Hello")
    my_class.add_member("Hello")

def test_static_class():
    """tests the creation of a static class"""
    my_class = SGClass("Hello")
    assert False == my_class.is_static()
    
    my_class.set_as_static()
    assert my_class.is_static()

def test_struct():
    """tests the creation of a struct"""
    my_class = SGClass("Hello")
    assert False == my_class.is_struct()
    
    my_class.set_as_struct()
    assert my_class.is_struct()

# def test_has_pointer():
#     """tests the creation of a class used to wrap a pointer"""
#     my_class = SGClass("SoundEffect")
#     assert False == my_class.has_pointer()
#     
#     my_class.set_has_pointer()
#     assert my_class.has_pointer()
# 
if __name__ == '__main__':
    nose.run()
    