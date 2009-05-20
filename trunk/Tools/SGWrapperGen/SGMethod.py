#!/usr/bin/env python
# encoding: utf-8
"""
SGMethod.py

Created by Andrew Cain on 2009-05-20.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import sys
import os

from SGMetaDataContainer import SGMetaDataContainer

class SGMethod(SGMetaDataContainer):
    def __init__(self, name):
        self.name = name
        self.params = []
        
        pass


def testMethodCreation():
    myMethod = SGMethod("Test")
    
    assert myMethod.name == "Test"
    assert len(myMethod.params) == 0


if __name__ == '__main__':
    import nose
    nose.run()