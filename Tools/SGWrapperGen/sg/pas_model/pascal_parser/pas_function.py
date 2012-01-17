from pas_token_kind import TokenKind
from pas_parser_utils import logger, parse_statement, parse_variable_declaration
from pas_var import PascalVariable

class PascalFunction(object):
    """
    PascalFunction is a container for a procedure or function in Pascal
    """

    #declaration part
        #constants
        #type
        #var
        #procedure/function
    #statement part

    
    def __init__(self, parent):
        from pas_block import PascalBlock
        self._parent = parent                 #parent block
        self._result = None    # PascalVariable
        self._parameters = dict()
        self._block = PascalBlock(parent)
        self._name = ''
        self._return_type = None    #PascalType
        self._isFunction = False

    @property
    def name(self):
        return self._name

    @property
    def parameters(self):
        return self._parameters

    @property
    def return_type(self):
        return self._return_type

    @property
    def isFunction(self):
        return self._isFunction

    @property
    def parent(self):
        return self._parent

    @property
    def block(self):
        return self._block

    @property
    def result(self):
        return self._result

    @property
    def kind(self):
        return 'function'

    def parse(self, tokens):
        from pas_type_cache import find_or_add_type
        self._result = None
        self._return_type = None

        if (tokens.match_lookahead(TokenKind.Identifier, 'function')):
            tokens.match_token(TokenKind.Identifier, 'function')
            self._isFunction = True
        else:
            tokens.match_token(TokenKind.Identifier, 'procedure')

        self._name = tokens.match_token(TokenKind.Identifier).value
        tokens.match_token(TokenKind.Symbol, '(')
        if tokens.match_lookahead(TokenKind.Identifier):
            self._block._variables = parse_variable_declaration(tokens)
            self._parameters.update(self._block._variables)
        tokens.match_token(TokenKind.Symbol, ')')
        if (self._isFunction):
            # read return type
            tokens.match_token(TokenKind.Symbol, ':')
            typeName = tokens.match_token(TokenKind.Identifier).value
            type = find_or_add_type(typeName)
            self._return_type = type
            self._block._variables['result'] = PascalVariable('result', type)
            self._result = self._block.get_variable('result')

        tokens.match_token(TokenKind.Symbol, ';')
        self._block.parse(tokens)
        

