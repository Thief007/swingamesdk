#!/usr/bin/env python
# encoding: utf-8
"""
SGParameter.py

Created by Andrew Cain on 2009-05-21.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import sys
import os

from SGMetaDataContainer import SGMetaDataContainer

class SGParameter(SGMetaDataContainer):
    def __init__(self, name):
        SGMetaDataContainer.__init__(self)
        self.name = name
        self.setType(None)
    
    def setAsOutput():
        addTag('output')
    
    def isOutput(self):
        #print self.tags.keys()
        return 'output' in self.tags.keys()
    
    def setAsArray():
        addTag('array')
    
    def isArray(self):
        #print self.tags.keys()
        return 'array' in self.tags.keys()
    
    def setType(self, type):
        self.addTag('type', [type])
    
    def type(self):
        return self.tags['type'].other[0]
    


#
# Test methods
#

def testMethodCreation():
    myMethod = SGMethod("Test")
    
    assert myMethod.name == "Test"
    assert len(myMethod.params) == 0
    assert len(myMethod.tags) == 1 #return type
    assert myMethod.returnType() == None

def testStaticMethods():
    myMethod = SGMethod("Test")
    assert False == myMethod.isStatic()
    
    myMethod.addTag("static")
    assert myMethod.isStatic()

def testReturnTypes():
    myMethod = SGMethod("Test")
    
    myMethod.setReturnType("SoundEffect")
    assert myMethod.returnType() == "SoundEffect"

if __name__ == '__main__':
    import nose
    nose.run()