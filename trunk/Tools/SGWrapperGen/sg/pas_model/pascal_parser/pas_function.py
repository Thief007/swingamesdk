from pascal_parser.tokeniser.pas_token_kind import TokenKind
from pascal_parser.pas_parser_utils import logger, parse_statement
from pas_var import PascalVariable

class PascalFunction(object):
    """
    PascalFunction is a container for a procedure or function in Pascal
    """
   
    def __init__(self, parent):
        """
        parent is the current block the function is 
        being initialised from... current block is the function's parent
        """
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

    def add_parameter(self, variable):
        self._parameters[variable.name] = variable
        self._block._variables[variable.name] = variable

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
        from types.pas_type_cache import find_or_add_type
        from pas_param_declaration import PascalParameterDeclaration
        self._result = None
        self._return_type = None

        # function / procedure
        if (tokens.match_lookahead(TokenKind.Identifier, 'function')):
            tokens.match_token(TokenKind.Identifier, 'function')
            self._isFunction = True
        else:
            tokens.match_token(TokenKind.Identifier, 'procedure')

        # name
        self._name = tokens.match_token(TokenKind.Identifier).value
        
        # ( parameter declaration )
        
        declaration = PascalParameterDeclaration()
        declaration.parse(tokens, self)
        if tokens.match_lookahead(TokenKind.Identifier):
            self._parameters = PascalVarDeclaration()
            self._parameters.parse(tokens, self)
            self._block._variables.update(self._parameters.variables)

        if (self._isFunction):
            # read return type
            # : type
            tokens.match_token(TokenKind.Symbol, ':')
            typeName = tokens.match_token(TokenKind.Identifier).value
            type = find_or_add_type(typeName)
            self._return_type = type
            self._block._variables['result'] = PascalVariable('result', type)
            self._result = self._block.get_variable('result')

        # ;
        tokens.match_token(TokenKind.Symbol, ';')
        self._block.parse(tokens)
        

