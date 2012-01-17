from pas_token_kind import TokenKind
from pas_parser_utils import logger, parse_variable_declaration


class PascalVarDeclaration(object):
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

    @property
    def kind(self):
        return 'variable declaration'

    @property
    def variables(self):
        return self._vars

    def parse(self, tokens):
        tokens.match_token(TokenKind.Identifier, 'var')
        self._vars = parse_variable_declaration(tokens)
        self._contents = self._vars
                
