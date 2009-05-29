#!/usr/bin/env python
# encoding: utf-8
"""
SGMetaDataContainer.py

Created by Andrew Cain on 2009-05-20.
Copyright (c) 2009 Swinburne. All rights reserved.
"""

import logging

logger = logging.getLogger("SGWrapperGen")

class SGMetaDataContainer(object):
    """A container for meta data tag information"""
    
    def __init__(self, known_tags = []):
        """initialise the container setting up the tags dictionary"""
        self._known_tags = known_tags
        
        for known in ['note','name','version']: self._known_tags.append(known)
        
        self.tags = {}
        self.doc = ""
        self.notes = []
    
    def add_doc(self, doc):
        """adds documentation to the meta data container"""
        self.doc = doc
    
    def add_note(self, note):
        '''Adds a note to the node'''
        self.notes.append(note)
    
    def set_tag(self, tag, other = None):
        '''sets the tag for the meta data container with optional other data.
           
           parameters:
           tag = name or SGTag object, 
           other = list of data (default None)
        '''
        global logger
        
        if isinstance(tag, SGTag):
            self._check_known_tags(tag)
            logger.debug("Adding %s", str(tag))
            self.tags[tag.title] = tag
        else:
            self.set_tag(SGTag(tag, other))
    
    def _check_known_tags(self, tag):
        if not tag.title in self._known_tags:
            global logger
            logger.error('Unknown tag @%s added for %s', tag.title, str(self.__class__))
    
    def keys(self):
        return self.tags.keys()
    
    def __getitem__(self, key):
        return self.tags[key]
    
    def __setitem__(self, key, value):
        self.tags[key] = value
    
    name = property(lambda self: self['name'].other, lambda self,name: self.set_tag('name', name), None, "The name of the element.")
    version = property(lambda self: self['version'].other, lambda self,version: self.set_tag('version', version), None, "The version of the element.")

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
    