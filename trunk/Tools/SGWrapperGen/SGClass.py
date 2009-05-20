#!/usr/bin/env python
# encoding: utf-8
"""
SGClass.py

Created by Andrew Cain on 2009-05-20.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import sys
import os
from SGMethod import SGMethod
from SGProperties import SGProperties
from SGMetaDataContainer import SGMetaDataContainer

class SGClass(SGMetaDataContainer):
    """Represents a class in the comment meta language."""

    def __init__(self, name):
        self.name = name
        self.methods = dict()
        self.properties = dict()

    def addMember(self, member):
        if isinstance(member, SGMethod):
            self.methods[member.name] = method
        elif isinstance(member, SGProperty)
            self.properties[member.name] = member
        else:
            raise Exception, "Unknown member type"

import nose
from nose.tools import * 

def testClassCreation():
    myClass = SGClass("Hello")
    
    assert myClass.name == "Hello"
    assert len(myClass.methods) == 0
    
def testAddMethod():
    myClass = SGClass("Hello")
    myMethod = SGMethod("test")
    
    myClass.addMember(myMethod)
    
    assert len(myClass.methods) == 1
    
def testAddMethod():
    myClass = SGClass("Hello")
    myProperty = SGProperty("test")

    myClass.addMember(myProperty)

    assert len(myClass.properties) == 1

@raises(Exception)
def testAddUnkownMember():
    myClass = SGClass("Hello")
    myClass.addMember("Hello")
        
if __name__ == '__main__':
    import nose
    nose.run()