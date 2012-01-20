from tokeniser.pas_token_kind import TokenKind
from pas_parser_utils import logger

class PascalFunctionCall(object):
    """
    The function call class represents a pascal function call
        - Will stop reading a function call if an end brace ')' is encountered
    """

    def __init__(self, block, inExpr):
        self._identifier = None         # identifier of the function
        self._arguments = None       # list of expressions
        self._block = block
        self._code = dict()
        self._inExpr = inExpr

    @property
    def code(self):
        return self._code

    def parse(self, tokens):
        """
        parses a function call

        inExpr = False by default
        if inExpr is True a semi-colon is expected at the end of the function call
        """
        from pas_arguments import PascalArguments

        logger.debug('Processing function call %s', self._identifier)
        self._identifier = tokens.match_token(TokenKind.Identifier).value

        self._arguments = PascalArguments(self._block)
        self._arguments.parse(tokens)

        if (not self._inExpr) :
            tokens.match_lookahead(TokenKind.Symbol, ';', consume=True)

        logger.debug('Ended function call %s', self._identifier)

    @property
    def kind(self):
        return 'function_call'

    @property
    def identifier(self):
        return self._identifier

    def to_code(self, indentation = 0):
        '''
        This method creates the code to declare all it's variables
        for each of the modules
        '''
        import converter_helper

        # Need to convert arguments
        # -> parse arguments into an arguments object?
        # not always comma seperated (obj-c)

        my_data = dict()

        my_data['pas_identifier'] = self._identifier 
        my_data['c_lib_identifier'] = converter_helper.lower_name(self._identifier)
        
        self._arguments.to_code()


        for (name, module) in converter_helper.converters.items():
            my_data[name + '_args'] = self._arguments.code[name]
            self._code[name] = ((indentation * '    ') + module.function_call_template % my_data) + (';' if not self._inExpr else '')