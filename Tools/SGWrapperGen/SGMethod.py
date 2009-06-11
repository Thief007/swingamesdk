#!/usr/bin/env python
# encoding: utf-8
"""
SGMethod.py

Created by Andrew Cain on 2009-05-20.
Copyright (c) 2009 Swinburne. All rights reserved.
"""

import logging
import sys

from SGMetaDataContainer import SGMetaDataContainer
from SGParameter import SGParameter
from SGProperty import SGProperty
from sgcache import find_or_add_class, logger

class SGMethod(SGMetaDataContainer):
    """A SGMethod represents a function or a procedure in SwinGame."""
    def __init__(self, name):
        """Initialise the SGMethod, setting its name."""
        SGMetaDataContainer.__init__(self, ['uname','static','operator',
            'is_constructor','return_type','other_class','dispose',
            'method','overload','returns','is_setter','is_getter','is_external', 
            'called_by_lib', 'my_class', 'class_method','in_property', 'called_by',
            'method_called', 'args'])
        self.name = name
        self.uname = name
        self.params = list()
        #self.param_cache = {}
        self.return_type = None
        self.operator = None
        self.in_class = None
        self.is_static = False
        self.is_constructor = False
        self.is_external = False
        self.is_getter = False
        self.is_setter = False
        self.in_property = None
        self.called_by_lib = False
        self.method_called = None
        self.args = None
        self.class_method = None
        self.other_class = None
        self.called_by = list()
        
        self.local_vars = list()
        self.was_function = False
        self.has_length_params = False
    
    def to_keyed_dict(self, param_visitor, type_visitor = None, arg_visitor = None):
        '''Returns a dictionary containing the details of this function/procedure
        
        The param_visitor is called to convert each parameter to a string. 
        param_visitor(the_param, last)
        '''
        result = {}
        result['name'] = self.name
        result['uname'] = self.uname
        result['return_type'] = self.return_type if type_visitor == None else type_visitor(self.return_type) 
        result['params'] = self.param_string(param_visitor)
        result['calls.file.pascal_name'] = self.method_called.in_class.in_file.pascal_name
        result['calls.file.name'] = self.method_called.in_class.in_file.name
        result['calls.file.filename'] = self.method_called.in_class.in_file.filename
        result['calls.class'] = self.method_called.in_class.name
        result['calls.name'] = self.method_called.name
        result['calls.args'] = self.args_string_for_called_method(arg_visitor)
        
        return result

    name = property(lambda self: self['name'].other, 
        lambda self,name: self.set_tag('name', name), 
        None, 'The name of the method.')
    
    uname = property(lambda self: self['uname'].other, 
        lambda self,uname: self.set_tag('uname', uname), 
        None, 'The uname of the method.')
    
    in_property = property(lambda self: self['in_property'].other, 
        lambda self,value: self.set_tag('in_property', value), 
        None, 'The property the method is in (or None).')
    
    is_setter = property(lambda self: self['is_setter'].other, 
        lambda self,value: self.set_tag('is_setter', value), 
        None, 'The method is a setter.')
    
    is_getter = property(lambda self: self['is_getter'].other, 
        lambda self,value: self.set_tag('is_getter', value), 
        None, 'The method is a getter.')
    
    is_static = property(lambda self: self['static'].other, 
        lambda self,value: self.set_tag('static', value), 
        None, 'Is the method associated with a class.')
    
    called_by_lib = property(lambda self: self['called_by_lib'].other, 
        lambda self,value: self.set_tag('called_by_lib', value), 
        None, 'Is the method associated with a class.')
    
    called_by = property(lambda self: self['called_by'].other, 
        lambda self,value: self.set_tag('called_by', value), 
        None, 'The methods that call this method.')
    
    operator = property(lambda self: self['operator'].other, 
        lambda self,value: self.set_tag('operator', value), 
        None, 'Is the method an operator overload?')
    
    is_constructor = property(lambda self: self['is_constructor'].other, 
        lambda self,value: self.set_tag('is_constructor', value), 
        None, 'Is the method a constructor?')
    
    is_external = property(lambda self: self['is_external'].other, 
        lambda self,value: self.set_tag('is_external', value), 
        None, 'Is the method from the library?')
    
    in_class = property(lambda self: self['my_class'].other, 
        lambda self,value: self.set_tag('my_class', value), 
        None, 'the class containing the method')
    
    other_class = property(lambda self: self['other_class'].other, 
        lambda self,value: self.set_tag('other_class', value), 
        None, 'the class of this methods clone')
    
    return_type = property(lambda self: self['return_type'].other, 
        lambda self,return_type: self.set_tag('return_type', return_type), 
        None, "The return type of the method.")
    
    class_method = property(lambda self: self['class_method'].other, 
        lambda self,return_type: self.set_tag('class_method', return_type), 
        None, "The class method that duplicates this methods behaviour.")
    
    is_function = property (lambda self: self.return_type != None,
        None, None, 'The method is a function'
        )
    
    args = property(lambda self: self['args'].other, 
        lambda self,value: self.set_tag('args', value), 
        None, "The arguments passed to the method called by this method.")
    
    method_called = property(lambda self: self['method_called'].other, 
        lambda self,value: self.set_tag('method_called', value), 
        None, "The method called by this method.")
    
    @property
    def signature(self):
        return (self.name, tuple([(p.modifier + ' ' if p.modifier != None else '') + str(p.data_type) for p in self.params]))
    
    # def cache_parameter(self, param):
    #     """
    #     Adds a parameter to a temporary cache during loading of the Method.
    #     
    #     The cache does not need to be loaded in the methods parameter order.
    #     Parameters are then loaded using createParameter
    #     """
    #     
    #     self.param_cache[param.name] = param
    
    def create_parameter(self, name):
        """creates a new parameter with the indicated name, or fetches it from 
        the cache. The new parameter is returned, and added to the params 
        list."""
        result = SGParameter(name)
        self.params.append(result)
        return result
    
    def has_parameter(self, name):
        """check if the method has a parameter with the matching name"""
        for par in self.params:
            if par.name == name:
                return True
        return False
    
    def set_tag(self, title, other = None):
        if title == "params":
            #process parameter comments
            for param_details in other:
                param_name = param_details[0]
                param_doc = param_details[1]
                
                done = False
                for param in self.params:
                    if param.name == param_name:
                        param.add_doc(param_doc)
                        done = True
                        break
                if not done:
                    logger.error('Method    : Unable to find parameter %s for %s', param_name, self.uname)
                    assert False
        elif title == "class":
            #the class indicates that the @method is for this other class...
            from sgcodemodule import SGCodeModule
            from SGLibrary import SGLibrary
            if other == None: 
                super(SGMethod,self).set_tag(title, None)
                return
            elif isinstance(other, SGCodeModule):
                other_class = other
            elif isinstance(other, SGLibrary):
                other_class = other
            else:
                other_class = find_or_add_class(other)
            
            super(SGMethod,self).set_tag('other_class', other_class)
        elif title == 'getter' or title == 'setter':
            # 1: mark as getter/setter
            super(SGMethod,self).set_tag('is_' + title, True) 
            # 2: set property name
            self.in_property = other
            # 3: mark for later processing
            class_method = SGMethod(other + ' ' + title)
            super(SGMethod,self).set_tag('class_method', class_method)
        elif title == 'constructor':
            const = SGMethod(self.other_class.name)
            const.is_constructor = True
            super(SGMethod,self).set_tag('class_method', const)
        else:
            super(SGMethod,self).set_tag(title, other)
    
    def clone_to(self, other):
        #dont copy uname...
        other.in_file = self.in_file
        other.return_type = self.return_type
        other.file_line_details = self.file_line_details
        if self.is_static or other.is_constructor:
            other.params = self.params
            other.is_static = self.is_static
        else:
            other.params = self.params[1:]
        
        if self.other_class != None:
            other.in_class = self.other_class
        else:
            other.in_class = self.in_class
    
    def calls(self, method, args=None):
        """indicate which method this method calls, and args if any"""
        if self.method_called != None:
            logger.error('Model Error: Changing method called by %s', self.name)
            assert False
        #self.set_tag('calls', (method, args))
        self.method_called = method
        self.args = args
    
    def _process_args(self):
        '''
        Convert args to parameters, fields, and literals
        '''
        new_args = list()
        args = self.args
        if args == None:
            self.args = self.params
            return
        for argv in args:
            if argv[0] in ['number', 'string']:
                new_args.append(argv[1])
            elif argv[0] in ['id']:
                param = self.get_parameter(argv[1])
                if param != None:
                    new_args.append(param)
                else:
                    field = self.in_class.get_field(argv[1])
                    if field != None:
                        new_args.append(field)
                    else:
                        logger.error('Method    : Error cannot find %s in method %s', argv[1], self.uname)
                        assert False
        #self.tags['calls'].other[1] = new_args
        self.args = new_args
    
    def setup_lib_method(self, lib_method):
        '''
        Setup the library method
        
        Set:
            return type
            parameters
            calls
        '''
        self._process_args()
        args = self.args #lib is called method
        
        if lib_method.file_line_details == None:
            lib_method.file_line_details = []
        lib_method.file_line_details.append(self.file_line_details)
        
        if self.called_by_lib: #Set up the call from the library to this method
            logger.debug('Method    : Setting %s in library to call %s', lib_method, self)
            lib_method.return_type = self.return_type #set return type
            lib_method.calls(self, self.params) #..calls this method at other end
            lib_method.params = self.params #add parameters
    
    def create_and_add_property(self, class_method):
        '''
        The added class method is actually a property, so
        create and add a property or update the existing property.
        '''
        property_name = self.in_property
        #get or create the property
        if property_name in self.other_class.properties:
            prop = self.other_class.properties[property_name]
        else:
            prop = SGProperty(property_name)
            prop.in_class = self.other_class;
            prop.is_static = self.is_static;
            prop.in_file = self.in_file
            #add property to class
            self.other_class.add_member(prop)
        
        #setup name of method and its position in the property
        if self.is_getter:
            class_method.name = 'get' + property_name
            prop.getter = class_method
        elif self.is_setter:
            class_method.name = 'set' + property_name
            prop.setter = class_method
        else:
            assert(False, '{Property is not a getter or a setter}')
        #change uname as well...
        class_method.uname = class_method.name
    
    def setup_class_method(self, class_method):
        '''
        Setup the class's method.
        
        The class method is a clone of the current method, in 
        a class wrapper. This comes from @method or @overload
        in the pascal source.
        
        Steps:
            1: clone self to class_method
            2: add it to its class or property
            3: alter args (add pointer field access)
        '''
        self.clone_to(class_method) #copy self into other
        
        #if the class method is actually a property...
        if self.is_getter or self.is_setter:
            self.create_and_add_property(class_method)
        else:
            class_method.in_class.add_member(class_method) #add to its class
        
        #static methods and constructors directly call the method
        #  instance methods change the first argument for the pointer field
        if class_method.is_static or class_method.is_constructor:
            #use self's args - i.e. it calls the method in the same way
            args = self.args
        else:  #other is an instance (with ptr)
            args = ['pointer'] #add extra first argument
            #add in self's arguments (-1st which is pointer)
            if self.args != None:
                args.extend(self.args[1:])
        
        #set class method to call the same method this does
        class_method.calls(self.method_called, args)
        
        logger.debug('Method    : Setting up call: %s calls %s with args %s', class_method,
            class_method.method_called, class_method.args)
        
        class_method._create_other_params_from_args()
    
    def get_parameter(self, name):
        for par in self.params:
            if par.name == name:
                return par
        return None
    
    def complete_method_processing(self):
        '''
        This is called on methods that are read by the parser from the 
        Pascal file.
        
        Set up the call from the library to this method if marked.
        Check the call's validity
        
        Steps:
            1: Get other methods related to this one
            2: Set parameters on library method (if called)
        '''
        logger.info(' Method    : Completing processing of %s', self)
        
        self.params = tuple(self.params)
        
        #Get other methods
        lib_method = self.method_called
        class_method = self.class_method
        
        #check rules
        if lib_method == None:
            logger.error('Method    : Found method %s without lib', self)
            assert False
        
        logger.info(' Method    : %s calls %s', self.name, lib_method.name)
        
        lib_method.called_by.append(self)
        
        #set up library method
        self.setup_lib_method(lib_method)
        
        #set up class method
        if class_method != None:
            logger.info(' Method    : %s is also %s', self.name, class_method)
            self.setup_class_method(class_method)
            lib_method.called_by.append(class_method) #library is also called by class
        
        # Cant check here... need to wait until all are read
        # self.check_call_validity()
        # class_method.check_call_validity()
    
    def _create_other_params_from_args(self):
        '''
        Ensure that the arguments in the call match the available parameters,
        if no arguments are provided copy across read in parameters
        '''
        method_called = self.method_called #self.tags['calls'].other[0] #get called method
        args = self.args #self.tags['calls'].other[1] #the arguments self passes to the method
        
        logger.debug('Method    : Checking arguments used by %s calling %s (%s)', self, method_called, args)
        
        if args == None:
            args = self.params
            # if len(args) != len(method_called.params):
            #     print args[0], method_called.params
            #     logger.error('Error in %s calling %s', self.uname, method_called.uname)
            #     assert False
            logger.debug('Method    : Altering call to supply arguments that map to parameters: %s', self)
            self.args = args
        else:
            #check that the args match params
            assert(len(args) == len(method_called.params), 'Error in %s calling %s', self.uname, method_called.uname)
            for arg in args:
                if isinstance(arg, SGParameter) and not self.has_parameter(arg.name):
                    logger.error("Cannot match parameter %s in call to %s from %s", str(arg), str(method_called), self)
                    assert False
    
    def check_arguments(self):
        '''
        Ensure that the arguments in the call match the available parameters,
        if no arguments are provided copy across read in parameters
        '''
        if self.method_called == None:
            logger.error('Method    : Method %s does not call anything. Check attributes. %s', self.uname, self.file_line_details)
            assert False
        
        method_called = self.method_called #get called method
        args = self.args #the arguments self passes to the method
        
        logger.debug('Method    : Checking arguments used by %s calling %s (%s)', self, method_called, args)
        
        if len(args) != len(method_called.params):
            logger.error('Method    : Error in %s calling %s', self.uname, method_called.uname)
            assert False
        
        for arg in args:
            if isinstance(arg, SGParameter):
                if not self.has_parameter(arg.name):
                    logger.error("Cannot match parameter %s in call to %s from %s", str(arg), str(method_called), self)
                    assert False
    
    def args_string_for_called_method(self, arg_visitor = None):
        args = self.args
        #print self.method_called
        params = self.method_called.params
        
        arg_list = [ a.arg_name() if isinstance(a, SGParameter) else a for a in args ]
        if arg_visitor != None:
            return ','.join([ arg_visitor(a, params[i]) for i,a in enumerate(arg_list) ])
        else:
            return ','.join(arg_list)
    
    def param_string(self, param_visitor = None):
        if self.params: 
            if param_visitor == None:
                return ', '.join([str(n) for n in self.params])
            else:
                return ''.join([param_visitor(param, param == self.params[-1]) for param in self.params])
                #i.e. map(lambda param: param_visitor(param, param == self.params[-1]), self.params)
        else: return ''
    
    def __str__(self):
        if self.return_type != None:
            result = str(self.return_type.name) + ' '
        else:
            result = 'void '
        
        if self.in_class != None:
            result += self.in_class.name + '.'
        result += self.uname + '('
        result += self.param_string()
        result += ')'
        
        return result
    
    def visit_params(self, visitor, other):
        for param in self.params:
            visitor(param, param == self.params[-1], other)
    
    def visit_args(self, visitor, other):
        args = self.args
        for arg in args:
            visitor(arg, arg == args[-1], other)
    

#
# Test methods
#

from nose.tools import raises

def test_method_creation():
    """test the creation of a basic SGMethod"""
    my_method = SGMethod("Test")
    
    assert my_method.name == "Test"
    assert len(my_method.params) == 0
    assert my_method.return_type == None

def test_static_methods():
    """test the creation of a static method"""
    my_method = SGMethod("Test")
    assert False == my_method.is_static
    
    my_method.is_static = True
    assert my_method.is_static

def test_constructor_methods():
    """test the creation of a constructor"""
    my_method = SGMethod("init")
    assert False == my_method.is_constructor
    
    my_method.is_constructor = True
    assert my_method.is_constructor

def test_return_types():
    """test the return type value"""
    my_method = SGMethod("Test")
    
    my_method.return_type = "SoundEffect"
    assert my_method.return_type == "SoundEffect"

def test_basic_method_call_wrapper():
    """test the creation of a simple method wrapper"""
    my_method = SGMethod("test")
    other_method = SGMethod("other")
    
    my_method.calls(other_method)
    my_method.check_call_validity();
    
    assert other_method == my_method.method_called
    assert len(my_method.args) == 0

def test_wrapper_with_params():
    """test the creation of a method wrapper with one parameter in call"""
    my_method = SGMethod("test")
    par = my_method.create_parameter("par1")
    other_method = SGMethod("other")
    par1 = other_method.create_parameter("par1")
    
    my_method.calls(other_method)
    my_method.check_call_validity();
    
    assert other_method == my_method.method_called
    assert len(my_method.args) == 1
    assert par == my_method.args[0]

def test_wrapper_with_args():
    """test the creation of a method wrapper with non-default args"""
    my_method = SGMethod("test")
    other_method = SGMethod("other")
    par1 = other_method.create_parameter("par1")
    
    my_method.calls(other_method, ['"test"'])
    my_method.check_call_validity();
    
    assert other_method == my_method.method_called
    assert len(my_method.args) == 1
    assert par1 != my_method.args[0]
    assert '"test"' == my_method.args[0]
    

# @raises(Exception)
# def test_wrapper_with_missing_args():
#     """test the creation of a method wrapper with non-default args"""
#     my_method = SGMethod("test")
#     other_method = SGMethod("other")
#     par1 = other_method.create_parameter("par1")
#     par2 = other_method.create_parameter("par2")
#     
#     assert 2 == len(other_method.params)
#     
#     my_method.calls(other_method, ['"test"'])
#     my_method.check_call_validity();
# 
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