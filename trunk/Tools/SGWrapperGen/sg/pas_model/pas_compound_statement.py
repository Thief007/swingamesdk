import logging

from pas_token_kind import *
from pas_token import *
from pas_token_stream import *
from Program import *

from pas_var_declaration import *

class PascalCompoundStatement(object):
    """
    The PascalCompoundStatement object parses a compound statement
    """

    def __init__(self):
        self._statements = []

    def parse(self, tokens):
        logger.debug("Parsing compound statement")
        tokens.match_token(TokenKind.Identifier, 'begin')
        while (True):
            if tokens.match_lookahead(TokenKind.Identifier):
                break
            elif tokens.match_lookahead(TokenKind.Identifier, 'end'):
                break




