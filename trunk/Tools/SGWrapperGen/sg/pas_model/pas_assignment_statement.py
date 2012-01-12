from pas_token_kind import TokenKind
from pas_expression import PascalExpression
class AssignmentStatement(object):
    """
    The assignment statement stores the information about an assignment statement
    """

    def __init__(self):
        self._operand = None
        self._operator = None       # += *= /= -= := token
        self._expression = None

    def parse(self, tokens):
        self._operand = tokens.match_token(TokenKind.Identifier).value
        self._operator = tokens.match_token(TokenKind.Operator).value ################
        self._expression = PascalExpression()
        self._expression.parse(tokens)

    @property
    def kind(self):
        return 'assignment'

    @property
    def operand(self):
        return self._operand

    @property
    def operator(self):
        return self._operator

    @property
    def expression(self):
        return self._expression