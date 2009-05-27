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
from SGClass import SGClass

logger = logging.getLogger("SGWrapperGen")

class SGPasParser():
    def __init__(self):
        self._tokeniser = SGPasTokeniser()
        self._processors = {
            'unit': self.process_unit,
            'attribute': self.process_attribute,
            'comment': self.process_comment,
            'meta comment': self.process_meta_comment
        }
        self._attribute_processors = {
            'param': self.process_param_attribute,
            'class': self.process_class_attribute,
            'static': self.process_static_attribute
        }
        self._lookahead_toks = []
        self._meta_comments = []
        self._attributes = {}
        self._classes = {}
    
    def parse(self, filename):
        global logger
        self._tokeniser.tokenise(filename)
        
        #read the meta comments before the node
        self.process_meta_comments()
        #read the token after meta-comments
        tok = self._next_token()
        
        if tok[0] == 'id' and tok[1] in ['unit','library']:
            self._processors[tok[1]](tok)
        else:
            logger.error('Parse error: found %s expected unit or library', tok[1])
            sys.exit(-1)
    
    def _next_token(self):
        if len(self._lookahead_toks) > 0:
            current_token = self._lookahead_toks[0]
            self._lookahead_toks = self._lookahead_toks[1:]
        else:
            current_token = self._tokeniser.next_token()
        return current_token
    
    def _lookahead(self,count=1):
        while len(self._lookahead_toks) < count:
            self._lookahead_toks.append(self._tokeniser.next_token())
        return self._lookahead_toks
    
    def _match_token(self, token_kind, token_value = None):
        global logger
        tok = self._next_token()
        
        if not(tok[0] == token_kind or (token_value == None or token_value == tok[1])):
            logger.error('Parse error: found a %s (%s) expected a %s', tok[0], tok[1], token_kind)
            sys.exit(-1)
            
        logger.info('Matched token %s (%s)', tok[0], tok[1])
        return tok
    
    def process_meta_comments(self):
        self._meta_comments = []
        self._attributes = {}
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
        global logger
        tok = self._match_token('id')
        unit_name = tok[1]
        logger.info('Processing unit: %s', unit_name)
        
        self._check_attributes(['class','static'],'unit ' + unit_name)
        self._check_meta_comments(1, 'unit ' + unit_name + '\'s declaration')
                
        #Create the unit
        name = self._get_attribute('class', unit_name)
        logger.info('Creating class: %s', name)
        unit = SGClass(name)
        
        if name in self._classes:
            logger.error('Parser error: found a second declaration of %s from unit %s', name, tok[1])
            sys.exit(-1)
        
        self._classes[tok[1]] = unit
        
        if self._get_attribute('static', False):
            logger.info('Making class %s static', name)
            unit.set_as_static()
        
        #unit name;
        tok = self._match_token('token', ';')
        
        #interface - ignore comments
        self._skip_comments('unit ' + unit_name + '\'s interface')
        self._match_token('id', 'interface')
        
        #check 'uses'
        tok = self._lookahead()[0]
        if tok[0] == 'id' and tok[1] == 'uses':
            tok = self._next_token()
            self.process_uses_clause(tok)
        
        while True:
            #Process the body of the unit's interface
            self.process_meta_comments() #read next elements meta comments... if any
            tok = self._lookahead()[0] #check the next token
            break
        
        logger.info('Finished processing unit. Resulting class is:\n%s', str(unit))
        pass
    
    def process_comment(self, token):
        global logger
        logger.info('Processing comment: %s', token[1])
        pass
    
    def process_attribute(self, token):
        global logger
        logger.info('Processing attribute: %s', token[1])
        self._attribute_processors[token[1]](token)
        pass
    
    def _add_attribute(self, attr, val):
        global logger
        logger.info('Adding attribute %s with value %s',attr,val)
        self._attributes[attr] = val
    
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
    
    def process_param_attribute(self, token):
        pass
    
    def _skip_comments(self, desc):
        self.process_meta_comments()
        self._check_non_commented(desc)
    
    def process_uses_clause(self, token):
        '''Read the list of units referred to be the uses clause'''
        self._check_non_commented('uses clause')
        
        #unit_list = unit_name[, unit_list]
        while True:
            self._skip_comments('unit in uses clause')
            tok = self._next_token()
            if tok[0] == 'id':
                logger.info('Found using unit %s', tok[1])
                self._skip_comments('unit in uses clause')
                
                #found a token/unit
                next_tok = self._next_token()
                if next_tok[0] == 'token':
                    if next_tok[1] == ';': 
                        break; #found end
                    elif next_tok[1] != ',':
                        logger.error('Parser Error: expected , or ; but found %s at %s', next_tok[1], self._tokeniser.line_details())
                        sys.exit(-1)
                else:
                    logger.error('Parser Error: expected , or ; but found %s at %s', next_tok[1], self._tokeniser.line_details())
                    sys.exit(-1)
            else:
                logger.error('Parser Error: expected unit name but found %s at %s', tok[1], self._tokeniser.line_details())
                sys.exit(-1)
    
    def process_class_attribute(self, token):
        tok = self._match_token('id') #load id of class
        self._add_attribute('class', tok[1])
    
    def process_static_attribute(self, token):
        self._add_attribute('static', True)
    
    def process_meta_comment(self, token):
        global logger
        logger.info('Processing meta comment: %s', token[1])
        self._meta_comments.append(token[1])
        pass

if __name__ == '__main__':
    import nose
    nose.run()

