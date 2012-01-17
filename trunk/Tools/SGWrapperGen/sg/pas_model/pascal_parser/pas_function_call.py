from pas_token_kind import TokenKind
from pas_parser_utils import logger

class PascalFunctionCall(object):
    """
    The function call class represents a pascal function call
        - Will stop reading a function call if an end brace ')' is encountered
    """

    def __init__(self, block):
        self._identifier = None         # identifier of the function
        self._parameters = list()       # list of expressions
        self._block = block

    def parse(self, tokens, inExpr = False):
        from pas_expression import PascalExpression
        logger.debug('Processing function call %s', self._identifier)
        self._identifier = tokens.match_token(TokenKind.Identifier).value
        tokens.match_token(TokenKind.Symbol, '(')
        while True:
            if tokens.match_lookahead(TokenKind.Symbol, ')', consume=True):
                break
            newExpression = PascalExpression(self._block)
            newExpression.parse(tokens)
            self._parameters.append(newExpression)
        
        if (not inExpr) :
            tokens.match_lookahead(TokenKind.Symbol, ';', consume=True)

        logger.debug('Ended function call %s', self._identifier)

    def __str__(self):
        result += self._identifier
        for item in self._parameters:
            result += str(item)
        return result

    @property
    def kind(self):
        return 'function_call'

    @property
    def identifier(self):
        return self._identifier

    @property
    def parameters(self):
        return self._parameters