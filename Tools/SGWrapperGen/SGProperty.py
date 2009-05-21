#!/usr/bin/env python
# encoding: utf-8
"""
SGProperty.py

Created by Andrew Cain on 2009-05-20.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import sys
import os

import SGMetaDataContainer

class SGProperty(SGMetaDataContainer.SGMetaDataContainer):
    def __init__(self, name):
        self.name = name

    def setGetter(method):
        self.getter = method
        
    def setSetter(method):
        self.setter = method

from SGMethod import SGMethod

def testPropertyCreation():
    myProperty = SGProperty("name");
    
    assert myProperty.name == "name"
    
if __name__ == '__main__':
    import nose
    nose.run()

