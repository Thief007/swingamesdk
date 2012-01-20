from tokeniser.pas_token_kind import TokenKind
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
        self._code = dict()

    @property
    def code(self):
        return self._code

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

    def to_code(self, indentation = 0):
        '''
        This method creates the code to declare all it's variables
        for each of the modules
        '''
        import converter_helper
        my_data = dict()

        my_data['operand'] = self.operand.name
        my_data['operand_lower'] = converter_helper.lower_name(self.operand.name)

        self._operator.to_code()
        self._expression.to_code()

        for (name, module) in converter_helper.converters.items():
            # operator / expression
            my_data[name + '_expression'] = self._expression.code[name]
            my_data[name + '_operator'] = self._operator.code[name]
            self._code[name] =  (indentation * '    ') + module.assignment_template % my_data