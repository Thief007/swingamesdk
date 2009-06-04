#!/usr/bin/env python
# encoding: utf-8
"""
SGPasParser.py

Created by Andrew Cain on 2009-05-26.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import logging
import sys

from SGPasTokeniser import SGPasTokeniser
from sgcodemodule import SGCodeModule
from SGType import SGType
from SGField import SGField
from SGMethod import SGMethod
from sgcache import find_or_add_class, find_or_add_type, logger, find_or_add_file, all_types

class SGPasParser():
    def __init__(self):
        self._tokeniser = SGPasTokeniser()
        self._processors = {
            'attribute': self.process_attribute,
            'comment': self.process_comment,
            'meta comment': self.process_meta_comment
        }
        self._file_processors = {
            'unit': self.process_unit
        }
        self._attribute_processors = {
            'param': self.process_id_and_comment_attribute,
            'class': self.process_id_attribute,
            'module': self.process_id_attribute,
            'static': self.process_true_attribute,
            'struct': self.process_true_attribute,
            'lib':self.process_lib_attribute,
            #'has_pointer': self.process_true_attribute,
            'note': self.process_note_attribute,
            'uname': self.process_id_attribute,
            'field': self.process_field_attribute,
            'constructor': self.process_true_attribute,
            'dispose': self.process_true_attribute,
            'method': self.process_method_attribute,
            'overload': self.process_overload_attribute,
            'version': self.process_number_attribute,
            'setter': self.process_id_attribute,
            'getter': self.process_id_attribute,
            'returns': self.process_comment_attribute,
            'swingame': self.process_swingame_attribute,
            'pointer_wrapper': self.process_true_attribute,
            'data_wrapper': self.process_true_attribute
        }
        self._block_header_processors = {
            'type': self.process_block_types,
            'procedure': self.process_method_decl,
            'function': self.process_method_decl
        }
        self._lookahead_toks = []
        self._meta_comments = []
        self._attributes = {}
        self._ordered_attributes = []
        self._current_file = None
        # self._classes = {}
        
    
    def parse(self, a_file):
        #clear all existing data
        self._lookahead_toks = []
        self._meta_comments = []
        self._attributes = {}
        self._ordered_attributes = []
        self._current_file = a_file
        self._tokeniser.tokenise(a_file.filename)
        
        #read the meta comments before the node
        self.process_meta_comments()
        #read the token after meta-comments
        tok = self._next_token()
        
        if tok[0] == 'id' and tok[1] in ['unit','library']:
            self._file_processors[tok[1]](tok)
        else:
            logger.error('Parse Error %s: found %s expected unit or library', 
                self._tokeniser.line_details(), tok[1])
            assert(False)
    
    def _apply_attributes_to(self, model_element):
        '''
        Processing of the model element is complete, now apply
        all of the attributes we read in the order they were read
        '''
        #apply all attributes
        for attr_name, attr in self._ordered_attributes:
            model_element.set_tag(attr_name, attr)
        
        #add all meta comments to the model_element
        map(lambda f: model_element.add_doc(f), self._meta_comments)
        
        #clear all 
        self._attributes = {}
        self._ordered_attributes = []
        self._meta_comments = []
    
    def _create_model_element(self, kind):
        global logger
        
        name = self._get_attribute('name')
        if kind == SGCodeModule:
            result = find_or_add_class(name)
            if not result in self._current_file.members:
                self._current_file.members.append(result)
        elif kind == SGType:
            assert False
            #result = find_or_add_type(name)
        else:
            result = kind(name)
            
        result.in_file = self._current_file
        logger.debug('Creating model element: %s with kind:%s', name, kind)
        return result
    
    def _next_token(self):
        current_token = None
        while current_token == None or current_token[0] == 'comment':
            if len(self._lookahead_toks) > 0:
                current_token = self._lookahead_toks[0]
                self._lookahead_toks = self._lookahead_toks[1:]
            else:
                current_token = self._tokeniser.next_token()
            if current_token[0] == 'comment':
                logger.debug('Skipping comment: %s', current_token[1])
        return current_token
    
    def _lookahead(self,count=1):
        while len(self._lookahead_toks) < count:
            current_token = self._tokeniser.next_token()
            while current_token[0] == 'comment':
                current_token = self._tokeniser.next_token()
            self._lookahead_toks.append(current_token)
        return self._lookahead_toks
    
    def _match_lookahead(self, token_kind, token_value = None, consume = False):
        token = self._lookahead(1)[0]
        result = token[0] == token_kind and (token_value == None or token_value == token[1].lower())
        if consume and result:
            self._match_token(token_kind, token_value)
        return result
    
    def _match_token(self, token_kind, token_value = None):
        global logger
        
        tok = self._next_token()
        
        if tok[0] != token_kind and (token_value != None or token_value != tok[1]):
            logger.error('Parse Error %s: found a %s (%s) expected %s (%s)', 
                self._tokeniser.line_details(), 
                tok[0], tok[1], token_kind, token_value)
            assert(False)
            
        logger.debug('Matched token %s (%s)', tok[0], tok[1])
        return tok
    
    def _match_one_token(self, token_kind_lst):
        global logger
        
        matched = False
        tok = self._next_token()
        
        for token_kind,token_value in token_kind_lst:
            if tok[0] == token_kind and (token_value == None or token_value == tok[1]):
                matched = True
                logger.debug('Matched %s with %s', tok[0], tok[1])
                break
            
        if not matched:
            logger.error('Parser Error %s: unexpected %s(%s) expected %s', 
                self._tokeniser.line_details(), 
                tok[0], tok[1], 
                map(lambda n: '%s(%s)' % (n[0],n[1]),token_kind_lst))
            assert(False)
        return tok
    
    def process_meta_comments(self):
        logger.debug('starting to process meta comments: clearing old comments and attributes')
        #self._meta_comments = []
        #self._attributes = {}
        tok = self._lookahead(1)[0] #_next_token()
        
        attrs_started = False
        while tok[0] in ['meta comment','attribute','comment']:
            if tok[0] == 'attribute': 
                attrs_started = True
            if attrs_started and tok[0] == 'meta comment':
                tok = self._next_token() #actually read token
                if len(tok[1]) > 0:
                    logger.error('Parser Error %s: Found additional meta comment after start of attributes', 
                        self._tokeniser.line_details())
                    assert(False)
            else:
                tok = self._next_token() #actually read token
                self._processors[tok[0]](tok)
            tok = self._lookahead(1)[0]
    
    def process_unit(self, token):
        tok = self._match_token('id')
        unit_name = tok[1]
        name = self._get_attribute('module', unit_name)
        self._add_attribute('name',name)
        
        logger.info('-' * 70)
        logger.info('Processing unit: %s', unit_name)
        logger.info('-' * 70)
        
        #self._check_attributes(['class','static'],'unit ' + unit_name)
        self._check_meta_comments(1, 'unit ' + unit_name + '\'s declaration')
                
        #Create the unit
        unit = self._create_model_element(SGCodeModule)
        unit.module_kind = 'module'
        unit.is_static = True
        logger.info('Creating class %s', unit.name)
        
        #logger.info('Creating class: %s', name)
        #unit = SGCodeModule(name)
        
        # if name in self._classes:
        #     logger.error('Parser error: found a second declaration of %s from unit %s', name, tok[1])
        #     sys.exit(-1)
        # 
        # self._classes[tok[1]] = unit
        self._apply_attributes_to(unit)
        
        #unit name;
        tok = self._match_token('symbol', ';')
        
        #interface - ignore comments
        #self._skip_comments('unit ' + unit_name + '\'s interface')
        self._match_token('id', 'interface')
        
        #check 'uses'
        tok = self._lookahead()[0]
        if tok[0] == 'id' and tok[1] == 'uses':
            tok = self._next_token()
            self.process_uses_clause(tok)
            logger.info('-' * 70)
        
        
        while True:
            logger.debug('At start of loop to process elements in unit')
            #read comments if they follow this position, otherwise leave 
            #  attributes as they are
            if self._match_lookahead('meta comment'): self.process_meta_comments()
            
            #Process the body of the unit's interface
            tok = self._lookahead()[0] #check the next token
            
            if tok[0] == 'id' and tok[1] == 'implementation':
                logger.info('Found end of unit\'s interface')
                logger.info('-' * 70)
                break
            
            #interface contains types, functions, procedures, var, const
            tok = self._match_one_token([['id','type'],
                ['id','function'],
                ['id','procedure'],
                ['id','const'],
                ['id','var']])
            self._block_header_processors[tok[1]](unit, tok);
        
        logger.debug('Finished processing unit. Resulting class is:\n%s', str(unit))
    
    def process_comment(self, token):
        global logger
        logger.debug('Processing comment: %s', token[1])
        pass
    
    def process_attribute(self, token):
        logger.debug('Processing attribute: %s', token[1])
        self._attribute_processors[token[1]](token)
        pass
    
    def _append_attribute(self, attr, val):
        global logger
        logger.debug('Appending attribute %s with value %s to %s',attr,val, attr + 's')
        if (attr + 's') in self._attributes.keys():
            self._attributes[attr + 's'].append(val)
        else:
            self._attributes[attr + 's'] = [val]
            self._ordered_attributes.append([attr + 's', [val]]) #will this work?
    
    def _add_attribute(self, attr, val):
        global logger
        logger.debug('Adding attribute %s with value %s',attr,val)
        self._attributes[attr] = val
        self._ordered_attributes.append([attr, val])
    
    def _get_attribute(self, attr, default=None):
        if attr in self._attributes:
            return self._attributes[attr]
        return default
    
    def _check_attributes(self, attrs, desc):
        global logger
        for key in self._attributes.keys():
            if not key in attrs:
                logger.warning('Parser Warning: found unknown attribute %s while processing %s at %s',key,desc, self._tokeniser.line_details())
    
    def _check_meta_comments(self, count, desc):
        global logger
        if len(self._meta_comments) != count:
            logger.error('Parser Error %s: expected %d but foung %d meta comments for %s', 
                self._tokeniser.line_details(), count, len(self._meta_comments), desc)
            sys.exit(-1)
    
    def _check_non_commented(self, desc):
        self._check_meta_comments(0, desc)
        self._check_attributes([], desc)
    
    def process_id_and_comment_attribute(self, token):
        '''read the id and attach comments etc as attributes'''
        param_tok = self._match_token('id')
        doc_tok = self._tokeniser.read_to_end_of_comment()
        self._append_attribute(token[1], [param_tok[1], doc_tok])
    
    def process_comment_attribute(self, token):
        '''read the id and attach comments etc as attributes'''
        doc_tok = self._tokeniser.read_to_end_of_comment()
        self._add_attribute(token[1], doc_tok)

    def _skip_comments(self, desc):
        self.process_meta_comments()
        self._check_non_commented(desc)
    
    def process_uses_clause(self, token):
        '''Read the list of units referred to be the uses clause'''
        self._check_non_commented('uses clause')
        
        while True:
            tok = self._match_token('id')
            self._current_file.uses.append(find_or_add_file(tok[1]))
            logger.debug('Found using unit %s', tok[1])
            
            #found a token/unit
            next_tok = self._match_token('symbol')
            if next_tok[1] == ';': 
                break; #found end
            elif next_tok[1] != ',':
                logger.error('Parser Error %s: expected , or ; but found %s at %s', 
                    self._tokeniser.line_details(), next_tok[1])
                sys.exit(-1)
    
    def process_id_id_attribute(self, token):
        '''Process an attribute that is followed by a two identifiers'''
        tok1 = self._match_token('id') #load id of thing...
        tok2 = self._match_token('id') #load id of thing...
        self._add_attribute(token[1], [tok1[1], tok2[1]])
    
    def process_id_attribute(self, token):
        '''Process an attribute that is followed by a single identifier'''
        tok = self._match_token('id') #load id of thing...
        self._add_attribute(token[1], tok[1])
    
    def process_field_attribute(self, token):
        '''
        Have encountered @field - create a field for the generated class to use
        '''
        tok = self._match_token('id') #field name
        field = SGField(tok[1])
        
        #todo: proper type matching...
        tok = self._match_token('symbol', ':')
        #tok = self._match_token('id')
        #the_type = find_or_add_type(tok[1]) #todo: should be matching type for this...
        the_type = self._read_type_usage()
        
        field.data_type = the_type
        self._append_attribute('field', field)
    
    def process_true_attribute(self, token):
        self._add_attribute(token[1], True)
    
    def process_swingame_attribute(self, token):
        tok = self._match_token('number')
        lib = find_or_add_class('lib')
        lib.version = tok[1]
    
    def process_method_attribute(self, token):
        name_tok = self._match_token('id')
        
        method = SGMethod(name_tok[1])
        self._add_attribute('class_method', method)
    
    def process_overload_attribute(self, token):
        name_tok = self._match_token('id')
        uname_tok = self._match_token('id')
        
        method = SGMethod(name_tok[1])
        method.uname = uname_tok[1]
        self._add_attribute('class_method', method)
    
    def process_lib_attribute(self, token):
        '''
        process a lib attribute = what to call in the library
        
        Steps: 
        1: find method (and possibly args)
        2: find method in library (all names are unique here)
        3: read in args
        4: add attribute 'calls' to indicate the processed method calls the
           method from the library
        '''
        #find method
        name_tok = self._match_token('id')
        uname = name_tok[1]
        
        #search in library
        the_lib = find_or_add_class('lib')
        method = the_lib.find_method(uname)
        if method == None: 
            method = SGMethod(uname)
            method.in_class = the_lib
            method.in_class.add_member(method)
        
        if self._match_lookahead('symbol', '(', True):
            #the lib attr has arguments
            # open_tok = self._match_token('symbol', '(')
            args = []
            while True:
                #argument is a number, or an identifier
                if self._match_lookahead('number') or self._match_lookahead('string'):
                    value = self._next_token()
                    args.append(value[1])
                elif self._match_lookahead('id'): 
                    arg = self._next_token()
                    args.append(method.create_parameter(arg[1]))
                else:
                    assert(False, 'Error in arguments') 
                
                #if not comma following - end args
                if not self._match_lookahead('symbol', ','): break;
                comma = self._next_token() #read comma
                
            close_tok = self._match_token('symbol', ')')
        else: args = None
        
        self._add_attribute('calls', [method, args])
    
    def process_note_attribute(self, token):
        self._add_attribute('note', self._tokeniser.read_to_end_of_comment())
    
    def process_value_attribute(self, token):
        self._add_attribute(token[0], token[1])
    
    def process_number_attribute(self, token):
        num_tok = self._match_token('number')
        self._add_attribute(token[1], num_tok[1])
    
    def process_meta_comment(self, token):
        global logger
        logger.debug('Processing meta comment: %s', token[1])
        self._meta_comments.append(token[1])
    
    def process_block_types(self, block, token):
        '''Reads the types within a type declaration'''
        logger.info('Processing types')
        logger.info('-' * 70)
        
        #following type... in pascal
        while True:
            self.process_meta_comments()
            tok1, tok2 = self._lookahead(2)
            #looking for... type_name = 
            if tok2[0] != 'symbol' or tok2[1] != '=' or tok1[0] != 'id':
                logger.debug('At end of block types')
                break
            
            type_name = self._match_token('id')[1] #read the types name
            self._match_token('symbol','=') #read the =
            
            the_type = find_or_add_type(type_name)
            
            self._parse_type_declaration(the_type)
            
            if 'class' in the_type.keys(): 
                self.add_class(the_type)
    
    def add_class(self, the_type):
        logger.info('-' * 60)
        logger.info('Adding class %s', the_type.name)
        new_class = find_or_add_class(the_type.name)
        if not new_class in self._current_file.members:
            self._current_file.members.append(new_class)
        new_class.setup_from(the_type)
        # 
        # self._classes[the_type.name] = new_class
    
    def _read_variable_list(self):
        '''
        reads in a list of variables. i.e. 
        name, name2: type
        This expects at least one variable
        '''
        names = []
        
        #read name, name... :
        while True:
            name = self._match_token('id')[1]
            names.append(name)
            if self._match_lookahead('symbol', ':', True): break
            self._match_token('symbol', ',') #comma separated
        
        #read ...: the_type
        the_type = self._read_type_usage()
        #expand out so each is is its own
        return [(name, the_type) for name in names]
    
    def _parse_type_declaration(self, the_type):
        '''
        Parse a type from the next token, add details to the_type.
        this is called for each type declaration... type_name = BLAH
        Need to process BLAH and add details to the_type
        '''
        #check what kind of type it is...
        if self._match_lookahead('symbol', '(', True):
            values = []
            while not self._match_lookahead('symbol',')'):
                values.append(self._match_token('id')[1])
                self._match_lookahead('symbol', ',', True) #consume commas
            self._match_token('symbol',')') #read close bracket
            the_type.values = tuple(values)
        elif self._match_lookahead('id', 'record', True):
            #record field: Type end;
            while not self._match_lookahead('id', 'end', True):
                #read field
                variables = self._read_variable_list()
                self._match_token('symbol', ';')
                for name, data_type in variables:
                    field = SGField(name)
                    field.data_type = data_type
                    the_type.fields.append(field)
        elif self._match_lookahead('id', 'array'):
            the_type.clone(self._read_type_usage())
        elif self._match_lookahead('id'):
            other_id = self._match_token('id')[1]
            other_type = find_or_add_type(other_id)
            the_type.related_type = other_type
        else:
            tok = self._next_token()
            logger.error('Parser Error %s: unknown type %s(%s)', 
                self._tokeniser.line_details(), tok[0], tok[1])
                
        if the_type.related_type != None:
            logger.info('Setup type %s = %s', the_type, the_type.related_type)
        else:
            logger.info('Setup type %s = %s', the_type, the_type.values)
        self._apply_attributes_to(the_type)
        self._match_token('symbol', ';')
    
    def _read_type_usage(self):
        '''Read a type identifier, or array of type, etc. and return a SGType'''
        #todo: finish this!!!
        
        if self._match_lookahead('id', 'array', True):
            #found an array
            dimensions = []
            # array [0..n,0..m] of
            if self._match_lookahead('symbol', '[', True):
                while True:
                    low_idx = self._match_token('number')[1]
                    self._match_token('symbol', '.')
                    self._match_token('symbol', '.')
                    high_idx = self._match_token('number')[1]
                    dimensions.append((low_idx,high_idx))
                    
                    if self._match_lookahead('symbol', ']', True):
                        break
                    self._match_token('symbol', ',')
            # of type...
            self._match_token('id', 'of')
            nested_type = self._read_type_usage() #recursive call
            
            name = nested_type.name + '[]' * len(dimensions)
            was_new = not name in all_types()
            
            result = find_or_add_type(name)
            if was_new:
                result.dimensions = dimensions
                result.nested_type = nested_type
            return result
        else:
            id_tok = self._match_token('id')
            return find_or_add_type(id_tok[1])
    
    def process_method_decl(self, block, token):
        '''process a method read from a unit.
        
        block is the block that contains the method, token is the first token.
        At the end of this the method has been read in and added to block.
        '''
        name_tok = self._match_token('id') #name of function/procedure
        open_tok = self._match_token('symbol', '(')
        
        #add class!
        self._add_attribute('name', name_tok[1]) #name comes from function/procedure
        method = self._create_model_element(SGMethod)
        #print '** %s -> %s' % (method.name, method.in_file.name)
        method.called_by_lib = True
        method.in_class = block
        
        #look for parameters
        while True:
            param_tok, other_tok = self._lookahead(2)
            if param_tok[0] == 'id':
                if other_tok[0] == 'id': #first one is a modifier
                    modifier = self._next_token()[1]
                else:
                    modifier = None
                param_toks = [self._match_token('id')]
                
                while self._match_lookahead('symbol', ','):
                    #there is a list of parameters
                    self._next_token() #consume ,
                    param_toks.append(self._match_token('id'))
                
                colon_tok = self._match_token('symbol', ':')
                the_type = self._read_type_usage()
                
                for param_tok in param_toks:
                    param = method.create_parameter(param_tok[1])
                    param.data_type = the_type
                    param.modifier = modifier
                    logger.debug('Adding parameter %s (%s) to %s', param.name, param.data_type, method.name)
                
                if self._match_lookahead('symbol', ';'):
                    self._next_token()
                else: break
            else: break
        
        close_tok = self._match_token('symbol', ')')
        
        if token[1] == 'function': #return return details
            colon_tok = self._match_token('symbol', ':')
            the_type = self._read_type_usage()
            method.return_type = the_type
            logger.debug('Set return type of method %s to %s', method.name, method.return_type)
        
        end_tok = self._match_token('symbol', ';')
        
        #check overload ;
        if self._match_lookahead('id', 'overload'):
            self._next_token();
            self._match_token('symbol', ';')
            self._add_attribute('overload', True)
        
        #logger.info('Adding method %s.%s(%s)', block.name, method.name, method.param_string())
        self._apply_attributes_to(method)
        method.complete_method_processing()
        
        block.add_member(method)
    
if __name__ == '__main__':
    import nose
    nose.run()

