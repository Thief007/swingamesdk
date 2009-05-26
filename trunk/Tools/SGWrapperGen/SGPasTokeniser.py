#!/usr/bin/env python
# encoding: utf-8
"""
Tokeniser.py

Created by Andrew Cain on 2009-05-26.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

COMMMENT = 200

class SGPasTokeniser():
    
    def __init__(self, filename):
        '''Initialises the tokeniser with the details from
        the passed in filename.
        '''
        
        if isinstance(filename, list):
            self.pas_lines = filename
        else:
            f = open(filename)
            self.pas_lines = f.readlines()
            f.close()
        
        self._char_no = -1
        self._line_no = 0 #starts at first line
        self._token_val = 'none'
        pass
        
    
    def _next_char(self):
        '''Returns the next character from the input file'''
        self._char_no += 1
        #print self._char_no, ' ', self._line_no
        if len(self.pas_lines[self._line_no]) <= self._char_no:
            return '\n'
        return self.pas_lines[self._line_no][self._char_no]
    
    def _peek(self, chars):
        if len(self.pas_lines[self._line_no]) <= self._char_no + 1:
            result = '\n'
        else:
            result = self.pas_lines[self._line_no][self._char_no + 1:self._char_no + chars + 1]
        #print 'peeking at', result
        return result
    
    def _read_matching(self, start, match_fn):
        result = start
        cha = self._next_char();
        #print 'matching', cha
        while match_fn(cha):
            result += cha
            cha = self._next_char();
            #print 'matching', cha
        self._char_no -= 1 #step back
        return result
        
    def _advance_line(self):
        self._line_no += 1
        self._char_no = -1
    
    def _read_until(self, start, end_fn):
        result = start
        cha = self._next_char();
        result += cha
        #print 'until', result
        while not end_fn(result):
            if cha == '\n':
                self._advance_line()
            cha = self._next_char();
            result += cha
            #print 'until', result
        #self._char_no -= 1 #step back
        return result
    
    def _match_and_read(self, cha):
        '''
        if cha matches the following character, then read that character
        and return True, otherwise return False
        '''
        if self._peek(1) == cha:
            self._next_char()
            return True
        else:
            return False
    
    def read_to_eol(self):
        result = self.pas_lines[self._line_no][self._char_no + 1:-1]
        self._advance_line()
        return result.strip()
    
    def next_token(self):
        '''Returns the current token'''
        
        while (True):
            t = self._next_char();
            
            if (t == ' ' or t == '\t'): #ignore white space
                pass
            elif (t == '\n'): #advance to next line
                self._advance_line()
            elif (t in '1234567890'): #is digit
                return ['number', self._read_matching(t, lambda cha: cha in '1234567890.')]
            elif (t == '/'): #start of comment
                temp = self._read_matching(t, lambda cha: cha in '/@')
                temp = temp.strip() #remove surrounding white space
                
                if temp == '//':
                    comment = self.read_to_eol()
                    return ['comment', comment]
                elif temp == '///':
                    comment = self.read_to_eol()
                    return ['meta comment', comment]
                elif temp == '///@':
                    return ['attribute', None]
                else:
                    return ['error', temp]
            elif t.isalpha():
                name = self._read_matching(t, lambda cha: cha.isalnum() or cha in '_')
                return ['id', name]
            elif t in '(),:;{':
                if t == '(' and self._peek(1) == '*':
                    comment = self._read_until('', lambda temp: temp[-2:] == '*)')
                    return ['comment', comment[:-2]]
                elif t == '{':
                    comment = self._read_until('', lambda temp: temp[-1:] == '}')
                    return ['comment', comment[:-1]]
                return ['token', t]
            elif t == '\'':
                string = self._read_until('', lambda temp: (temp[-1:] == '\'') and (not self._match_and_read('\'')))
                return ['string', string[:-1]]
            else:
                return ['error', t]
    

def test_basic():
    lines = ['// Hello World\n', '/// Special Comment\n', '///@test(attr)\n', 
        '12345 123.45\n', '{ test multi line 1234\n', 'comments} end\n', 
        '///@another(attr,attr2) \'a\'\'end\'']
    tokeniser = SGPasTokeniser(lines)
    
    token = tokeniser.next_token()
    print token
    assert token[0] == 'comment'
    
    token = tokeniser.next_token()
    print token
    assert token[0] == 'meta comment'
    
    token = tokeniser.next_token()
    print token
    assert token[0] == 'attribute'
    
    token = tokeniser.next_token()
    print token
    assert token[0] == 'id'
    assert token[1] == 'test'
    
    token = tokeniser.next_token()
    print token
    assert token[0] == 'token'
    assert token[1] == '('
    
    token = tokeniser.next_token()
    print token
    assert token[0] == 'id'
    assert token[1] == 'attr'
    
    token = tokeniser.next_token()
    print token
    assert token[0] == 'token'
    assert token[1] == ')'
    
    token = tokeniser.next_token()
    print token
    assert token[0] == 'number'
    assert token[1] == '12345'
    
    token = tokeniser.next_token()
    print token
    assert token[0] == 'number'
    assert token[1] == '123.45'
    
    token = tokeniser.next_token()
    print token
    assert token[0] == 'comment'
    
    token = tokeniser.next_token()
    print token
    assert token[0] == 'id'
    assert token[1] == 'end'
    
    token = tokeniser.next_token()
    print token
    assert token[0] == 'attribute'
    
    token = tokeniser.next_token()
    print token
    assert token[0] == 'id'
    assert token[1] == 'another'
    
    token = tokeniser.next_token()
    print token
    assert token[0] == 'token'
    assert token[1] == '('
    
    token = tokeniser.next_token()
    print token
    assert token[0] == 'id'
    assert token[1] == 'attr'
    
    token = tokeniser.next_token()
    print token
    assert token[0] == 'token'
    assert token[1] == ','
    
    token = tokeniser.next_token()
    print token
    assert token[0] == 'id'
    assert token[1] == 'attr2'
    
    token = tokeniser.next_token()
    print token
    assert token[0] == 'token'
    assert token[1] == ')'
    
    token = tokeniser.next_token()
    print token
    assert token[0] == 'string'
    assert token[1] == 'a\'end'

if __name__ == '__main__':
    import nose
    nose.run()

