#!/usr/bin/env python
# encoding: utf-8
"""
SGTag.py

Created by Andrew Cain on 2009-05-20.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import sys
import os


class SGTag:
    def __init__(self, title, other=[]):
        self.title = title
        self.other = other



def testTagCreation():
    myTag = SGTag("tag", ["Hello", "World"]);
    
    assert myTag.title == "tag"
    assert myTag.other[0] == "Hello"
    assert myTag.other[1] == "World"
    
def testDefaultTagCreation():
    myTag = SGTag("tag");

    assert myTag.title == "tag"
    assert len(myTag.other) == 0

if __name__ == '__main__':
    import nose
    nose.run()

