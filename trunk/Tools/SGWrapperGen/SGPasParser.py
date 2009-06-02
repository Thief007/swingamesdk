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
from sgcache import find_or_add_class, find_or_add_type, logger, find_or_add_file
from SGType import SGType

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
            'overload': self.process_id_id_attribute,
            'version': self.process_number_attribute,
            'setter': self.process_id_attribute,
            'getter': self.process_id_attribute,
            'returns': self.process_comment_attribute
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
        self._current_file = a_file
        self._tokeniser.tokenise(a_file.filename)
        
        #read the meta comments before the node
        self.process_meta_comments()
        #read the token after meta-comments
        tok = self._next_token()
        
        if tok[0] == 'id' and tok[1] in ['unit','library']:
            self._file_processors[tok[1]](tok)
        else:
            logger.error('Parse error: found %s expected unit or library', tok[1])
            sys.exit(-1)
    
    def _apply_attributes_to(self, model_element):
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
            result = find_or_add_type(name)
        else:
            result = kind(name)
            
        result.in_file = self._current_file
        logger.debug('Creating model element: %s with kind:%s', name, kind)
        #self._apply_attributes_to(result)
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
    
    def _match_lookahead(self, token_kind, token_value = None):
        token = self._lookahead(1)[0]
        result = token[0] == token_kind and (token_value == None or token_value == token[1])
        return result
    
    def _match_token(self, token_kind, token_value = None):
        global logger
        
        tok = self._next_token()
        
        if tok[0] != token_kind and (token_value != None or token_value != tok[1]):
            logger.error('Parse error: found a %s (%s) expected %s (%s)', tok[0], tok[1], token_kind, token_value)
            sys.exit(-1)
            
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
            logger.error('Parser error: unexpected %s(%s) expected %s', tok[0], tok[1], map(lambda n: '%s(%s)' % (n[0],n[1]),token_kind_lst))
            sys.exit(-1)
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
                    logger.error('Parser Error: Found additional meta comment after start of attributes at %s', self._tokeniser.line_details())
                    sys.exit(-1)
            else:
                tok = self._next_token() #actually read token
                self._processors[tok[0]](tok)
            tok = self._lookahead(1)[0]
    
    def process_unit(self, token):
        tok = self._match_token('id')
        unit_name = tok[1]
        name = self._get_attribute('class', unit_name)
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
        tok = self._match_token('token', ';')
        
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
            logger.error('Parser Error: expected %d but foung %d meta comments for %s at %s', count, len(self._meta_comments), desc, self._tokeniser.line_details())
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
            next_tok = self._match_token('token')
            if next_tok[1] == ';': 
                break; #found end
            elif next_tok[1] != ',':
                logger.error('Parser Error: expected , or ; but found %s at %s', next_tok[1], self._tokeniser.line_details())
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
        global logger
        tok = self._match_token('id') #field name
        field = SGField(tok[1])
        
        #todo: proper type matching...
        tok = self._match_token('token', ':')
        tok = self._match_token('id')
        the_type = find_or_add_type(tok[1]) #todo: should be matching type for this...
        
        field.data_type = the_type
        self._append_attribute('field', field)
    
    def process_parameter_list_attribute(self, token): #deprecated
        '''Process an attribute that is followed  by an id and a type'''
        #id [, ids] : type
        tok = self._match_token('id')
        param_name = tok[1]
        #todo: proper type matching...
        tok = self._match_token('token', ':')
        tok = self._match_token('id')
        type_name = tok[1] #todo: should be matching type for this...
        
        params = [[param_name, type_name]]
        
        self._add_attribute('params', params)
    
    def process_true_attribute(self, token):
        self._add_attribute(token[1], True)
    
    def process_number_attribute(self, token):
        tok = self._match_token('number')
        self._add_attribute(token[1], tok[1])
    
    def process_method_attribute(self, token):
        name_tok = self._match_token('id')
        
        method = SGMethod(name_tok[1])
        self._add_attribute('clone', method)
    
    def process_lib_attribute(self, token):
        '''process a method attribute'''
        name_tok = self._match_token('id')
        
        method = SGMethod(name_tok[1])
        method.in_class = find_or_add_class('lib')
        method.in_class.add_member(method)
        
        next = self._lookahead(1)[0]
        if next[0] == 'token' and next[1] == '(':
            #the lib attr has arguments
            open_tok = self._match_token('token', '(')
            args = []
            while True:
                if self._match_lookahead('number') or self._match_lookahead('string'):
                    #attr = self._next_token()
                    value = self._next_token()
                    args.append(value[1])
                elif not self._match_lookahead('id'): 
                    break
                else:
                    arg = self._next_token()
                    args.append(method.create_parameter(arg[1]))
                
                if not self._match_lookahead('token', ','): break;
                comma = self._next_token()
                
            close_tok = self._match_token('token', ')')
        else: args = None
        
        self._add_attribute('calls', [method, args])
    
    def process_note_attribute(self, token):
        self._add_attribute('note', self._tokeniser.read_to_end_of_comment())
    
    def process_value_attribute(self, token):
        self._add_attribute(token[0], token[1])
    
    def process_meta_comment(self, token):
        global logger
        logger.debug('Processing meta comment: %s', token[1])
        self._meta_comments.append(token[1])
    
    def process_block_types(self, block, token):
        '''Reads the types within a type declaration'''
        logger.info('Processing types')
        logger.info('-' * 70)
        
        while True:
            self.process_meta_comments()
            tok1, tok2 = self._lookahead(2)
            if tok2[0] != 'token' or tok2[1] != '=' or tok1[0] != 'id':
                logger.debug('At end of block types')
                break
            
            tok = self._match_token('id')
            type_name = tok[1]
            
            tok = self._match_token('token','=')
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
    
    def _parse_type_declaration(self, the_type):
        '''Parse a type from the next token, add details to the_type.'''
        tok = self._lookahead(1)[0]
        
        if tok[0] == 'token' and tok[1] == '(':
            print 'enum'
            sys.exit(-2)
        elif tok[0] == 'id':
            if tok[1] == 'record':
                print 'record'
                sys.exit(-2)
            elif tok[1] == 'array':
                print 'array'
                sys.exit(-2)
            else:
                self._parse_type_simple(the_type);
                
        else:
            logger.error('Parser Error, unknown type %s(%s)', tok[0], tok[1])
    
    def _parse_type_simple(self, the_type):
        tok = self._match_token('id')
        type_name = tok[1]
        
        self._match_token('token', ';')
        logger.debug('Simple type using %s', tok[1])
        
        self._apply_attributes_to(the_type)
        
        return find_or_add_type(tok[1])
    
    def _read_type_usage(self):
        '''Read a type identifier, or array of type, etc. and return a SGType'''
        #todo: finish this!!!
        
        id_tok = self._match_token('id')
        return find_or_add_type(id_tok[1])
    
    def process_method_decl(self, block, token):
        '''block is the block that contains the method, token is the first token'''
        name_tok = self._match_token('id') #name of function/procedure
        open_tok = self._match_token('token', '(')
        
        #add class!
        self._add_attribute('name', name_tok[1]) #name comes from function/procedure
        method = self._create_model_element(SGMethod)
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
                param_tok = self._next_token()
                colon_tok = self._match_token('token', ':')
                the_type = self._read_type_usage()
                param = method.create_parameter(param_tok[1])
                param.data_type = the_type
                param.modifier = modifier
                logger.debug('Adding parameter %s (%s) to %s', param.name, param.data_type, method.name)
                if self._match_lookahead('token', ';'):
                    self._next_token()
                else: break
            else: break
        
        close_tok = self._match_token('token', ')')
        
        if token[1] == 'function': #return return details
            colon_tok = self._match_token('token', ':')
            the_type = self._read_type_usage()
            method.return_type = the_type
            logger.debug('Set return type of method %s to %s', method.name, method.return_type)
        
        end_tok = self._match_token('token', ';')
        
        #check overload ;
        if self._match_lookahead('id', 'overload'):
            self._next_token();
            self._match_token('token', ';')
            self._add_attribute('overload', True)
        
        #logger.info('Adding method %s.%s(%s)', block.name, method.name, method.param_string())
        self._apply_attributes_to(method)
        method.complete_method_processing()
        
        block.add_member(method)
    
if __name__ == '__main__':
    import nose
    nose.run()

