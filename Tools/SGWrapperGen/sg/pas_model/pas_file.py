import logging

from pas_token_stream import *
from pas_program import *
from pas_token_kind import *
from pas_token import *

from Program import *   # this should be replaced

class PascalFile(object):
    """
    Describes a pascal source file.
    The PascalFile can be parsed.
    """
    def __init__(self):
        self._filename = '';
        self._contains_kind = None
        self._contents = None       # PascalProgram or PascalLibrary

    # __str__ overrides the Object's __str__ method and changes what happens
    # when the PascalFile object is converted to a string
    def __str__ (self):
        return self._filename + ' is a ' + self._contains_kind

    def parse(self, filename, tokens):
        """
        parse initiates the parsing of the file described by this PascalFile
        It populates itself with details of the program and parses the contents.
        """
        self._filename = filename
        
        if tokens.match_lookahead(TokenKind.Identifier, 'program'):
            self._contains_kind = 'program';
            self._contents = PascalProgram()
        # unit?
        else:
            #logger
            assert False
        
        self._contents.parse(tokens)
        
        #check end of tokens...
            

    