#!/usr/bin/env python
# encoding: utf-8
"""
sgcodemodule.py

Created by Andrew Cain on 2009-05-20.
Copyright (c) 2009 Swinburne. All rights reserved.
"""

import logging

from SGMethod import SGMethod
from SGProperty import SGProperty
from SGField import SGField
from SGMetaDataContainer import SGMetaDataContainer

logger = logging.getLogger("SGWrapperGen")

class SGCodeModule(SGMetaDataContainer):
    """Represents a class in the comment meta language."""
    
    def __init__(self, name):
        """Initialise the class, setting its name"""
        super(SGCodeModule,self).__init__(['static','module_kind','data_wrapper','pointer_wrapper'])
        self.name = name
        self.methods = dict()
        self.properties = dict()
        self.fields = dict()
        self.is_static = False
        self.module_kind = 'unknown'
    
    def add_member(self, member):
        """Add a member (method, property) to the class"""
        if isinstance(member, SGMethod):
            logger.info('Adding method %s.%s(%s)', self.name, member.name, member.param_string())
            if self.is_static: member.is_static = True
            self.methods[member.signature] = member
        elif isinstance(member, SGProperty):
            logger.info('Adding property %s.%s', self.name, member.name)
            self.properties[member.name] = member
        elif isinstance(member, SGField):
            logger.info('Adding field %s.%s', self.name, member.name)
            self.fields[member.name] = member
        else:
            raise Exception, "Unknown member type"
    
    def del_member(self, member):
        if isinstance(member, SGMethod):
            del(self.methods[member.name])
        else:
            raise Exception, "Unknown member type"
    
    def set_tag(self, title, other = None):
        if title == 'class':
            self.module_kind = 'class'
        elif title == 'module':
            self.module_kind = 'module'
        else:
            super(SGCodeModule,self).set_tag(title, other)
    
    is_static = property(lambda self: self['static'].other, 
        lambda self,value: self.set_tag('static', value), 
        None, 'Is the class static (no instance members).')
    
    is_pointer_wrapper = property(lambda self: self['pointer_wrapper'].other, 
        lambda self,value: self.set_tag('pointer_wrapper', value), 
        None, 'Does the class wrap a pointer in SwinGame')
    
    is_data_wrapper = property(lambda self: self['data_wrapper'].other, 
        lambda self,value: self.set_tag('data_wrapper', value), 
        None, 'Does the class wrap data from SwinGame')
    
    module_kind = property(lambda self: self['module_kind'].other, 
        lambda self,value: self.set_tag('module_kind', value), 
        None, 'The kind of code module this represents (class,module,library).')
    
    is_class = property(lambda self: self.module_kind == 'class', 
        None, None, 'Is the module a class?')
    
    is_library = property(lambda self: self.module_kind == 'library', 
        None, None, 'Is the module a library?')
    
    is_module = property(lambda self: self.module_kind == 'module', 
        None, None, 'Is the module a module?')
    
    def set_as_struct(self):
        """sets the class as a struct"""
        self.set_tag('struct')
    
    def is_struct(self):
        """returns true if this should be a struct"""
        return 'struct' in self.tags
    
    def setup_from(self, the_type):
        fields = the_type.fields
        for field in fields:
            self.add_member(field)
        if the_type['class']:
            self.module_kind = 'class'
    
    def __str__(self):
        '''returns a string representation of the class'''
        name = 'struct ' + self.name if self.is_struct() else 'class ' + self.name
        name = 'static ' + name if self.is_static else name
        
        #return '%s\n%s' % (self.doc, name)
        return name
    
    def visit_methods(self, visitor, other):
        logger.debug('visiting method of %s' % self.name)
        for key, method in self.methods.items():
            logger.debug('visiting method %s' % method.uname)
            visitor(method, other)
    
    def visit_fields(self, visitor, other):
        for key, field in self.fields.items():
            visitor(field, key == self.fields.keys()[-1], other)
    
    def visit_properties(self, visitor, other):
        for key, prop in self.properties.items():
            visitor(prop, key == self.properties.keys()[-1], other)

import nose
from nose.tools import raises 

def test_class_creation():
    """test basic class creation"""
    my_class = SGCodeModule("Hello")
    
    assert my_class.name == "Hello"
    assert len(my_class.methods) == 0

def test_add_method():
    """test adding a method to a class"""
    my_class = SGCodeModule("Hello")
    my_method = SGMethod("test")
    
    my_class.add_member(my_method)
    
    assert len(my_class.methods) == 1

def test_add_property():
    """test adding a property to the class"""
    my_class = SGCodeModule("Hello")
    my_property = SGProperty("test")
    
    my_class.add_member(my_property)
    
    assert len(my_class.properties) == 1

def test_add_field():
    """test adding a field to the class"""
    my_class = SGCodeModule("Hello")
    my_field = SGField("test")
    
    my_class.add_member(my_field)
    
    assert len(my_class.fields) == 1

@raises(Exception)
def test_add_unkown_member():
    """test adding some unknown member type to the class, expects to fail"""
    my_class = SGCodeModule("Hello")
    my_class.add_member("Hello")

def test_static_class():
    """tests the creation of a static class"""
    my_class = SGCodeModule("Hello")
    assert False == my_class.is_static
    
    my_class.is_static = True
    assert my_class.is_static

def test_struct():
    """tests the creation of a struct"""
    my_class = SGCodeModule("Hello")
    assert False == my_class.is_struct()
    
    my_class.set_as_struct()
    assert my_class.is_struct()

# def test_has_pointer():
#     """tests the creation of a class used to wrap a pointer"""
#     my_class = SGCodeModule("SoundEffect")
#     assert False == my_class.has_pointer()
#     
#     my_class.set_has_pointer()
#     assert my_class.has_pointer()
# 
if __name__ == '__main__':
    nose.run()
    