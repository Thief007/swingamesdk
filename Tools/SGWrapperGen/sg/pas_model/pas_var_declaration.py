import logging
from pas_token_kind import *
from pas_token import *
from pas_token_stream import *
from pas_var import *
from Program import *   # this should be replaced
from pas_parsable import Parsable
from pas_parser_utils import *

class PascalVarDeclaration(object, Parsable):
    """
    The PascalVarDeclaration describes a variable declaration in Pascal
    It stores the identifier-type pairs in the declaration
    """

    # var
    # 'identifier' : 'type' ;

    
    def __init__(self):
        # vars stores the variables declared in this declaration in a dictionary
        # name of the variable is the key, type is the value
        self._vars = dict()
        self._contents = list()

    def part_kind(self):
        return 'variable declaration'

    # TODO enable parsing of arrays
    def parse(self, tokens):
        tokens.match_token(TokenKind.Identifier, 'var')

        reservedWords = [(TokenKind.Identifier, 'begin'), (TokenKind.Identifier, 'const'), (TokenKind.Identifier, 'type'), (TokenKind.Identifier, 'var'), (TokenKind.Identifier, 'procedure'), (TokenKind.Identifier, 'function')]
        logging.debug("Parsing variable declaration")
        while True:
            # identifier list, ':', type, ';'
            idList = parse_identifier_list(tokens)
            tokens.match_token(TokenKind.Symbol, ':')
            typeName = tokens.match_token(TokenKind.Identifier)._value
            tokens.match_token(TokenKind.Symbol, ';')

            for varName in idList:
                if not varName in self._vars:
                    self._vars[varName] = PascalVariable(varName, typeName)  # create and assign the PascalVariable
                    self._contents.append(self._vars[varName])
                    logging.debug("Parsed variable : " + varName + " : " + typeName)
                else:
                    logging.error("Duplicate variable identifier found: " + str(current_token))
                    assert(False)

            if tokens.match_one_lookahead(reservedWords):
                logging.debug("Finished parsing variable declaration")
                return self._vars
                
