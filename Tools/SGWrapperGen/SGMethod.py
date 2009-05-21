#!/usr/bin/env python
# encoding: utf-8
"""
SGMethod.py

Created by Andrew Cain on 2009-05-20.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

from SGMetaDataContainer import SGMetaDataContainer
from SGParameter import SGParameter

class SGMethod(SGMetaDataContainer):
    """A SGMethod represents a function or a procedure in SwinGame."""
    def __init__(self, name):
        """Initialise the SGMethod, setting its name."""
        SGMetaDataContainer.__init__(self)
        self.name = name
        self.params = []
        self.param_cache = {}
        self.set_return_type(None)
    
    def set_as_static(self):
        """Set this as a static method"""
        self.addTag('static')
    
    def is_static(self):
        """Checks if this is a static method, returns True if static."""
        #print self.tags.keys()
        return 'static' in self.tags.keys()
    
    def set_return_type(self, type_name):
        """Sets the return type to type."""
        self.addTag('returns', [type_name])
    
    def return_type(self):
        """Gets the SGMethod's return type"""
        return self.tags['returns'].other[0]
    
    def cache_parameter(self, param):
        """
        Adds a parameter to a temporary cache during loading of the Method.
        The cache does not need to be loaded in the methods parameter order.

        Parameters are then loaded using createParameter
        """
        
        self.param_cache[param.name] = param
    
    def create_parameter(self, name):
        """creates a new parameter with the indicated name, or fetches it from 
        the cache. The new parameter is returned, and added to the params 
        list."""
        if name in self.param_cache:
            result = self.param_cache[name]
            del self.param_cache[name]
        else:
            result = SGParameter(name)
        
        self.params.append(result)
        return result
    

#
# Test methods
#

def test_method_creation():
    """test the creation of a basic SGMethod"""
    my_method = SGMethod("Test")
    
    assert my_method.name == "Test"
    assert len(my_method.params) == 0
    assert len(my_method.tags) == 1 #return type
    assert my_method.return_type() == None

def test_static_methods():
    """test the creation of a static method"""
    my_method = SGMethod("Test")
    assert False == my_method.is_static()
    
    my_method.set_as_static()
    assert my_method.is_static()

def test_return_types():
    """test the return type value"""
    my_method = SGMethod("Test")
    
    my_method.set_return_type("SoundEffect")
    assert my_method.return_type() == "SoundEffect"

def test_parameter_cache():
    """test the parmeter cache process"""
    my_method = SGMethod("Test")
    prm = SGParameter("p1")
    
    my_method.cache_parameter(prm)
    
    assert my_method.param_cache["p1"] == prm
    
    prm0 = my_method.create_parameter("p0")
    prm1 = my_method.create_parameter("p1")
    prm2 = my_method.create_parameter("p2")
    
    assert prm == prm1
    print prm1
    assert my_method.params[1] == prm
    assert my_method.params[0] == prm0
    assert my_method.params[2] == prm2
    
    assert len(my_method.param_cache) == 0

if __name__ == '__main__':
    import nose
    nose.run()