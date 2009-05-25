#!/usr/bin/env python
# encoding: utf-8
"""
SGMethod.py

Created by Andrew Cain on 2009-05-20.
Copyright (c) 2009 Swinburne. All rights reserved.
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
        self.set_uname(name)
    
    def set_uname(self, uname):
        """sets the unique name for the method, defaults to name"""
        self.set_tag('uname', [uname])
    
    def set_as_static(self):
        """Set this as a static method"""
        self.set_tag('static')
    
    def is_static(self):
        """Checks if this is a static method, returns True if static."""
        #print self.tags.keys()
        return 'static' in self.tags.keys()
    
    def set_as_operator(self, operator):
        """Set this as a method representing an operator overload"""
        self.set_tag('operator', [operator])
    
    def is_operator(self):
        """Checks if this is an operator overload."""
        return 'operator' in self.tags.keys()
    
    def set_as_constructor(self):
        """Set this as a method representing a constructor"""
        self.set_tag('constructor')
    
    def is_constructor(self):
        """Checks if this is a constructor."""
        return 'constructor' in self.tags.keys()
    
    def operator(self):
        """returns the operator this method overloads"""
        return self.tags['operator'].other[0]
    
    def set_return_type(self, type_name):
        """Sets the return type to type."""
        self.set_tag('returns', [type_name])
    
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
    
    def has_parameter(self, name):
        """check if the method has a parameter with the matching name"""
        for par in self.params:
            if par.name == name:
                return True
        return False
    
    def calls(self, method, args=None):
        """indicate which method this method calls, and args if any"""
        self.set_tag('calls', [method, args])
    
    def check_call_validity(self):
        """Validate that the call is correctly configured, raises exception on error"""
        method = self.tags['calls'].other[0]
        args = self.tags['calls'].other[1]
        called_params = method.params
        
        if args == None:
            args = self.params
            self.set_tag('calls', [method, args])
        else:
            #check that the args match params
            for arg in args:
                if isinstance(arg, SGParameter) and not self.has_parameter(arg.name):
                    raise Exception("Cannot match parameter " + arg.name + 
                        " in call to " + str(method) + " from " + str(self))
        
        if len(args) != len(called_params):
            raise Exception("Error in call wrapper," + 
                " insufficient arguments for call to " + 
                str(method) + 
                " from " + str(self))
    
    def method_called(self):
        """returns the method that this method needs to call"""
        return self.tags['calls'].other[0]
    
    def args_for_method_call(self):
        """returns the argument call list for the called method"""
        args = self.tags['calls'].other[1]

        if args == None:
            self.set_tag('calls', [self.tags['calls'].other[0], self.params])
            args = self.tags['calls'].other[1]
        
        return args
    
    def __str__(self):
        if self.return_type() != None:
            result = self.return_type() + ' '
        else:
            result = 'void '
        
        result += self.name + '('
        
        if self.params:
            result += ', '.join([n.name for n in self.params])
            #result += reduce(lambda f, s: f + ', ' + s, [n.name for n in self.params])
        
        result += ')'
        
        return result
    

#
# Test methods
#

from nose.tools import raises

def test_method_creation():
    """test the creation of a basic SGMethod"""
    my_method = SGMethod("Test")
    
    assert my_method.name == "Test"
    assert len(my_method.params) == 0
    assert len(my_method.tags) == 2 #return type + uname
    assert my_method.return_type() == None

def test_static_methods():
    """test the creation of a static method"""
    my_method = SGMethod("Test")
    assert False == my_method.is_static()
    
    my_method.set_as_static()
    assert my_method.is_static()

def test_operator_methods():
    """test the creation of a operator overload method"""
    my_method = SGMethod("Multiply")
    assert False == my_method.is_operator()
    
    my_method.set_as_operator('*')
    assert my_method.is_operator()
    assert '*' == my_method.operator()

def test_constructor_methods():
    """test the creation of a constructor"""
    my_method = SGMethod("init")
    assert False == my_method.is_constructor()
    
    my_method.set_as_constructor()
    assert my_method.is_constructor()

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

def test_basic_method_call_wrapper():
    """test the creation of a simple method wrapper"""
    my_method = SGMethod("test")
    other_method = SGMethod("other")
    
    my_method.calls(other_method)
    my_method.check_call_validity();
    
    assert other_method == my_method.method_called()
    assert len(my_method.args_for_method_call()) == 0

def test_wrapper_with_params():
    """test the creation of a method wrapper with one parameter in call"""
    my_method = SGMethod("test")
    par = my_method.create_parameter("par1")
    other_method = SGMethod("other")
    par1 = other_method.create_parameter("par1")
    
    my_method.calls(other_method)
    my_method.check_call_validity();
    
    assert other_method == my_method.method_called()
    assert len(my_method.args_for_method_call()) == 1
    assert par == my_method.args_for_method_call()[0]

def test_wrapper_with_args():
    """test the creation of a method wrapper with non-default args"""
    my_method = SGMethod("test")
    other_method = SGMethod("other")
    par1 = other_method.create_parameter("par1")
    
    my_method.calls(other_method, ['"test"'])
    my_method.check_call_validity();
    
    assert other_method == my_method.method_called()
    assert len(my_method.args_for_method_call()) == 1
    assert par1 != my_method.args_for_method_call()[0]
    assert '"test"' == my_method.args_for_method_call()[0]
    

@raises(Exception)
def test_wrapper_with_missing_args():
    """test the creation of a method wrapper with non-default args"""
    my_method = SGMethod("test")
    other_method = SGMethod("other")
    par1 = other_method.create_parameter("par1")
    par2 = other_method.create_parameter("par2")
    
    assert 2 == len(other_method.params)
    
    my_method.calls(other_method, ['"test"'])
    my_method.check_call_validity();

@raises(Exception)
def test_wrapper_with_wrong_args():
    """test creation of a method wrapper with args with no matching params"""
    my_method = SGMethod("test")
    par = my_method.create_parameter("p1")
    
    other_method = SGMethod("other")
    par1 = other_method.create_parameter("par1")
    
    my_method.calls(other_method, [par1])
    my_method.check_call_validity();

@raises(Exception)
def test_wrapper_missing_default_args():
    """test the creation of a call with insufficient params to match args"""
    my_method = SGMethod("test")
    other_method = SGMethod("other")
    par1 = other_method.create_parameter("par1")
    
    assert 2 == len(other_method.params)
    
    my_method.calls(other_method)
    my_method.check_call_validity();

if __name__ == '__main__':
    import nose
    nose.run()