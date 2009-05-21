#!/usr/bin/env python
# encoding: utf-8
"""
SGMetaDataContainer.py

Created by Andrew Cain on 2009-05-20.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import sys
import os

class SGMetaDataContainer(object):
    def __init__(self):
        self.tags = {}
    
    def addDocumentation(self, doc):
        self.doc = doc
    
    def addTag(self, tag, other = []):
        if isinstance(tag, SGTag):
            print "Adding tag ", tag.title, " for ", tag
            self.tags[tag.title] = tag
        else:
            self.addTag(SGTag(tag, other))
    


from nose.tools import *
from SGTag import SGTag

def testMetaDataCreation():
    cont = SGMetaDataContainer()
    
    assert len(cont.tags) == 0


def testMetaDataCreationWithTag():
    cont = SGMetaDataContainer()
    tag = SGTag("tag")
    
    cont.addTag(tag)
    
    assert len(cont.tags) == 1
    assert cont.tags["tag"] == tag
    assert "tag" in cont.tags


def testAddTagData():
    cont = SGMetaDataContainer()
    
    cont.addTag(10)
    assert 10 in cont.tags
    
    cont.addTag("test")
    assert "test" in cont.tags
    
    cont.addTag("blah", ["blah"])
    assert "blah" == cont.tags["blah"].other[0]


if __name__ == '__main__':
    import nose
    nose.run()
    