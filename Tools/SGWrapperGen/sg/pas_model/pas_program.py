import logging

from pas_token import *
from pas_block import *
from Program import *

class PascalProgram(object):
    """The model object to represent a pascal program:
    
    Syntax for a program is:
        program = "program", identifier, ";", [uses clause], block, "." ;
        block = "begin", { statement }+(";"), "end" ;
    """
    
    def __init__(self):
        self._name = None
        self._block = None
    
    def parse(self, tokens):
        """
        Parses the entire pascal program
        """
        tokens.match_token(TokenKind.Identifier, 'program');
        self._name = tokens.match_token(TokenKind.Identifier)._value;
        tokens.match_token(TokenKind.Symbol, ';')
        
        # Read block
        self._block = PascalBlock(None)
        self._block.parse(tokens)
      

    