from pas_token_kind import TokenKind
from pas_parser_utils import logger
from pas_function_call import PascalFunctionCall

class PascalExpression(object):
    """
    The Expression statement stores all the information required to evaluate an expression
    """

    # stores operators in one queue, and operands in another
    
    def __init__(self):
        self._contents = list()

    # todo: add parsing of variables (identifiers) and function calls (identifier followed by an open brace)
    def parse(self, tokens, innerExpr = False):
        """
        this method parses the expression
        """
        logger.debug('Starting to parse %s expression' % ('INNER' if innerExpr else ''))
        while (True):
            if tokens.match_lookahead(TokenKind.Number):
                # read number
                self._contents.append(tokens.next_token().value)
            elif tokens.match_lookahead(TokenKind.Identifier):
                # either a variable or a function call - which?
                if tokens.lookahead(2)[1].value == '(':
                    #function call
                    funcCall = PascalFunctionCall()
                    print 'here'
                    funcCall.parse(tokens)
                    self._contents.append(funcCall)
                else:
                    #variable
                    self._contents.append(tokens.match_token(TokenKind.Identifier))
            elif tokens.match_lookahead(TokenKind.Operator):
                self._contents.append(tokens.next_token().value)
            elif tokens.match_lookahead(TokenKind.Symbol, '('):
                tokens.next_token() # consume the delimiter
                parenExpr = PascalExpression()
                parenExpr.parse(tokens, innerExpr = True)
                self._contents.append(parenExpr)
            elif tokens.match_lookahead(TokenKind.Symbol, ';') or tokens.match_lookahead(TokenKind.Symbol, ','):
                if innerExpr:
                    print 'Inner expression ended with ; or ,', tokens.next_token().line_details
                    assert false
                tokens.next_token() # consume the delimiter
                break
            elif tokens.match_lookahead(TokenKind.Symbol, ')'):
                if innerExpr:
                    tokens.next_token() # consume the delimiter
                break
            else:
                logger.error('Unknown expression token... %s' % str(tokens.next_token().value))
                assert False

    @property
    def contents(self):
        return self._contents

    def __str__(self):
        result = None
        for item in self._contents:
            result += str(item)
        return result