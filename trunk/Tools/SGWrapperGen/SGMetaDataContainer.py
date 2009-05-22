#!/usr/bin/env python
# encoding: utf-8
"""
SGMetaDataContainer.py

Created by Andrew Cain on 2009-05-20.
Copyright (c) 2009 Swinburne. All rights reserved.
"""

class SGMetaDataContainer(object):
    """A container for meta data tag information"""
    
    def __init__(self):
        """initialise the container setting up the tags dictionary"""
        self.tags = {}
        self.doc = ""
    
    def add_doc(self, doc):
        """adds documentation to the meta data container"""
        self.doc = doc
    
    def set_tag(self, tag, other = None):
        """sets the tag for the meta data container with optional other data.
           
           parameters:
           tag = name or SGTag object, 
           other = list of data (default None)"""
        if isinstance(tag, SGTag):
            print "Adding tag ", tag.title, " for ", tag
            self.tags[tag.title] = tag
        else:
            self.set_tag(SGTag(tag, other))
    

from SGTag import SGTag

def test_meta_data_creation():
    """tests the basic construction of the container"""
    cont = SGMetaDataContainer()
    
    assert len(cont.tags) == 0


def test_md_creation_with_tag():
    """tests the setting of a tag"""
    cont = SGMetaDataContainer()
    tag = SGTag("tag")
    
    cont.set_tag(tag)
    
    assert len(cont.tags) == 1
    assert cont.tags["tag"] == tag
    assert "tag" in cont.tags


def test_set_tag_data():
    """tests the creation of tags using passed in data"""
    cont = SGMetaDataContainer()
    
    cont.set_tag(10)
    assert 10 in cont.tags
    
    cont.set_tag("test")
    assert "test" in cont.tags
    
    cont.set_tag("blah", ["blah"])
    assert "blah" == cont.tags["blah"].other[0]


if __name__ == '__main__':
    import nose
    nose.run()
    