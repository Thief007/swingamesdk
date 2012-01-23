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
        from pas_param_declaration import PascalParameterDeclaration

        self._parent = parent                 #parent block
        self._result = None    # PascalVariable
        self._parameters = PascalParameterDeclaration()
        self._block = PascalBlock(parent)
        self._name = ''
        self._return_type = None    #PascalType
        self._isFunction = False
        self._code = dict()
        self._comments = list()

    @property
    def code(self):
        return self._code
    @property
    def name(self):
        return self._name

    @property
    def parameters(self):
        return self._parameters

    def add_parameter(self, variable):
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

        self._result = None
        self._return_type = None

        self._comments = tokens.get_comments()

        # function / procedure
        if (tokens.match_lookahead(TokenKind.Identifier, 'function')):
            tokens.match_token(TokenKind.Identifier, 'function')
            self._isFunction = True
        else:
            tokens.match_token(TokenKind.Identifier, 'procedure')

        # name
        self._name = tokens.match_token(TokenKind.Identifier).value
        
        # ( parameter declaration )
        
        self._parameters.parse(tokens, self)

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
        
    def to_code(self):
        '''
        This method creates the code to declare all it's variables
        for each of the modules
        '''
        import converter_helper

        my_data = dict()

        my_data['pas_lib_identifier'] = self._name 
        if (self._isFunction):
            my_data['pas_lib_type'] = self._return_type.name
        my_data['c_lib_identifier'] = converter_helper.lower_name(self._name)

        self._parameters.to_code()
        self._block.to_code()

        for (name, module) in converter_helper.converters.items():
            # types need to be evaluated in the loop because they are module specific
            comments = ''
            for current_line in self._comments:
                comments += module.comment_template % { "comment" : current_line}
            my_data[name + '_comments'] = comments
            my_data[name + '_parameters'] = self._parameters.code[name]
            my_data[name + '_block'] = self._block.code[name]
            if (self._isFunction):
                my_data[name + '_type'] = converter_helper.convert_type(module._type_switcher, self._return_type)
                self._code[name] = module.function_declaration_template % my_data
            else:
                self._code[name] = module.procedure_declaration_template % my_data