#!/usr/bin/env python
# encoding: utf-8
"""
SGPasParser.py

Created by Andrew Cain on 2009-05-26.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

from SGPasTokeniser import SGPasTokeniser


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
            'class': self.process_class_attribute
        }
        self._current_token = None
    
    def parse(self, filename):
        self._tokeniser.tokenise(filename)
        
        #read the meta comments before the node
        self.process_meta_comments()
        #read the token after meta-comments
        tok = self._current_token
        
        if tok[0] == 'id' and tok[1] in ['unit','library']:
            self._processors[tok[1]](tok)
        else:
            print 'Parse error: found', tok[1], 'expected unit or library'
    
    def _match_token(self, token_kind):
        tok = self._tokeniser.next_token()
        
        if tok[0] != token_kind:
            print 'Error: found a ', tok[0], '(', tok[1] ,')' ,'expected a', token_kind
            die()
            
        self._current_token = tok
        return tok
    
    def process_meta_comments(self):
        tok = self._tokeniser.next_token()
        while tok[0] in ['meta comment','attribute','comment']:
            self._processors[tok[0]](tok)
            tok = self._tokeniser.next_token()
        self._current_token = tok
    
    def process_unit(self, token):
        tok = self._match_token('id')
        print 'Processing unit: ', tok[1]
        pass
    
    def process_comment(self, token):
        print 'Processing comment:', token[1]
        pass
    
    def process_attribute(self, token):
        print 'Processing attribute:', token[1]
        self._attribute_processors[token[1]](token)
        pass
    
    def process_param_attribute(self, token):
        pass
    
    def process_class_attribute(self, token):
        tok = self._match_token('id') #load id of class
        print 'found class', tok[1]
        
        pass
    
    def process_meta_comment(self, token):
        print 'Processing meta comment:', token[1]
        pass

if __name__ == '__main__':
    import nose
    nose.run()

