#!/usr/bin/env python
# encoding: utf-8
"""
SGMetaDataContainer.py

Created by Andrew Cain on 2009-05-20.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import sys
import os


class SGMetaDataContainer:
    def __init__(self):
        self.tags = {}

    def addDocumentation(self, doc):
        self.doc = doc
        
    def addTag(self, tag):
        self.tags[tag.title] = tag


from SGTag import SGTag

def testMetaDataCreation():
    cont = SGMetaDataContainer()
    
    assert len(cont.tags) == 0

def testMetaDataCreation():
    cont = SGMetaDataContainer()
    tag = SGTag("tag")
    
    cont.addTag(tag)
    
    assert len(cont.tags) == 1
    assert cont.tags["tag"] == tag

if __name__ == '__main__':
    import nose
    nose.run()
