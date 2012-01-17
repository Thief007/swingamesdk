from pas_token_kind import TokenKind
from pas_expression import PascalExpression
from types.pas_operator import PascalOperator
class AssignmentStatement(object):
    """
    The assignment statement stores the information about an assignment statement
    """

    def __init__(self, block):
        self._operand = None
        self._operator = None       # += *= /= -= := token
        self._expression = None
        self._block = block

    def parse(self, tokens):
        varName = tokens.match_token(TokenKind.Identifier).value
        operatorValue = tokens.match_token(TokenKind.Operator).value

        self._operand = self._block.get_variable(varName)
        self._operator = PascalOperator(operatorValue)

        self._expression = PascalExpression(self._block)
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

    @property
    def block(self):
        return self._block