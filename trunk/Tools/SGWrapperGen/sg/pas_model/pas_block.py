import logging

from pas_token_kind import *
from pas_token import *
from pas_token_stream import *

from pas_var_declaration import *
from pas_compound_statement import *
from Program import *   # this should be replaced
from pas_parsable import Parsable

class PascalBlock(object, Parsable):
    """
    The PascalBlock object stores the entire pascal block
    """

    #declaration part
        #constants
        #type
        #var
        #procedure/function
    #statement part

    
    def __init__(self, parent):
        self._parent = parent                 #parent block

        # list with block contents (in order
        self._contents = list()

    def parse(self, tokens):
        # while the next token is not begin (start of compound statement), scan the block for declarations
        while (True):
            # variable declaration part
            if (tokens.match_lookahead(TokenKind.Identifier, 'var')):
                # read variable declaration part
                # and append to current active variables
                # local variables can overwrite global variables 
                current_part = PascalVarDeclaration()
            elif tokens.match_lookahead(TokenKind.Identifier, 'begin'):
                break
            else:
                print 'Unknown block token...', tokens.next_token()
                assert False
            current_part.parse(tokens)
            self._contents.append(current_part)

        # at this point we must be at a begin
        self._compound_statement = PascalCompoundStatement()
        self._compound_statement.parse(tokens)

    def get_variable(name):
        result = None
        if (name in self._variables):
            result = self._variables[name]
        elif(name in self._parent._variables):
            result = self._parent._variables[name]
        else:
            logger.Warning("Unable to locate variable in scope: " + name)


