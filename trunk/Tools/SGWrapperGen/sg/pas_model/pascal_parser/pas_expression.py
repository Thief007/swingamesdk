from pas_token_kind import TokenKind
from pas_parser_utils import logger
from pas_function_call import PascalFunctionCall

from types.pas_number import PascalNumber
from types.pas_string import PascalString
from types.pas_operator import PascalOperator
from types.pas_string import PascalString

class PascalExpression(object):
    """
    The Expression statement stores all the information required to evaluate an expression
    """

    # stores operators in one queue, and operands in another
    
    def __init__(self, owner_block):
        self._contents = list()
        self._block = owner_block

    @property
    def kind(self):
        return 'expression'

    def parse(self, tokens, innerExpr = False):
        """
        this method parses the expression
        """
        logger.debug('Parsing %s expression' % ('inner' if innerExpr else ''))
        newContent = None
        while (True):
            if tokens.match_lookahead(TokenKind.Symbol, ';') or tokens.match_lookahead(TokenKind.Symbol, ',') or tokens.match_lookahead(TokenKind.Identifier, 'then') or tokens.match_lookahead(TokenKind.Identifier, 'do'):
                if innerExpr:
                    logger.error('Inner expression ended with ; or , : ', tokens.next_token().line_details)
                    assert false
                tokens.next_token() # consume the delimiter
                logger.debug('Expression ended')
                break
            elif tokens.match_lookahead(TokenKind.Symbol, ')'):
                if innerExpr:
                    tokens.next_token() # consume the delimiter
                    logger.debug('Inner expression ended')
                break

            elif tokens.match_lookahead(TokenKind.Number):
                # read number
                newContent = PascalNumber(tokens.next_token().value)
            elif tokens.match_lookahead(TokenKind.String):
                newContent = PascalString(tokens.next_token().value)
            elif tokens.match_lookahead(TokenKind.Identifier):
                # either a variable or a function call - which?
                if tokens.lookahead(2)[1].value == '(':
                    #function call
                    funcCall = PascalFunctionCall(self._block)
                    funcCall.parse(tokens, inExpr=True)
                    newContent = funcCall
                else:
                    varName = tokens.match_token(TokenKind.Identifier).value
                    newContent = self._block.get_variable(varName)
            elif tokens.match_lookahead(TokenKind.Operator):
                newContent = PascalOperator(tokens.match_token(TokenKind.Operator).value)

            elif tokens.match_lookahead(TokenKind.Symbol, '('):
                tokens.next_token() # consume the delimiter
                newContent = PascalExpression(self._block)
                newContent.parse(tokens, innerExpr = True)
            else:
                logger.error('Unknown expression token... %s' % str(tokens.next_token().value))
                assert False

            self._contents.append(newContent)

    @property
    def contents(self):
        return self._contents

    def __str__(self):
        result = None
        for item in self._contents:
            result += str(item)
        return result