from pascal_parser.pas_token_kind import TokenKind
from pas_parser_utils import logger
class PascalVariableReference(object):
    """
    Describes a reference to a variable in Pascal
    """
    
    def __init__(self, block, record = None):
        from pas_parser import logger
        self._variable = ''
        self._block = block
        self._next = None   # this links to the next reference (after a symbol)
        self._symbols = list()  # list of expressions or symbols ('^'...)
        self._code = dict()     
        self._record = record

    def parse(self, tokens):
        from pascal_parser.pas_expression import PascalExpression
        name = tokens.match_token(TokenKind.Identifier).value
        if not (self._record is None):
            if self._record.has_field(name):
                self._variable = self._record.get_field(name)
            else:
                logger.error("Record %s does not contain field %s" % (self._record.name, name))
                assert False
        else:
            self._variable = self._block.resolve_variable(name)
        if self._variable is None:
            logger.error("Unable to resolve variable: %s", varName)
            assert False
        # enumeration value -> cannot be dereferenced
        if self._variable.kind is 'enumeration value':
            self._symbol = None
            self._next = None
        else:
            # variable ref is an array dereference...
            if tokens.match_lookahead(TokenKind.Symbol, '[', consume=True):
                array_expr = PascalExpression(self._block)
                array_expr.parse(tokens)
                self._symbols.append(array_expr)

            # pointer dereference...
            if tokens.match_lookahead(TokenKind.Symbol, '^'):
                self._symbol = tokens.match_token(TokenKind.Symbol, '^').value
                self._symbols.append('^')

            # record dereference...
            if tokens.match_lookahead(TokenKind.Symbol, '.'):
                self._symbols = tokens.match_token(TokenKind.Symbol, '.').value
                self._next = PascalVariableReference(self._block, self._variable.type)
                self._next.parse(tokens)

    @property
    def code(self):
        return self._code

    @property
    def name(self):
        return self._variable.name

    @property
    def type(self):
        return self._variable._type

    @property
    def kind(self):
        return 'variable'

    @property
    def is_record(self):
        return (self.type.kind == 'record')

    def to_code(self):
        '''
            Creates a _code entry for each of the converter modules

        '''
        import converter_helper
        from pascal_parser.pas_expression import PascalExpression
        my_data = dict()
        my_data['c_lib_identifier'] = converter_helper.lower_name(self._variable.name)
        my_data['pas_lib_identifier'] = self._variable.name
        for (name, module) in converter_helper.converters.items():
            my_data['var_reference'] = my_data[name + '_identifier']
            for symbol in self._symbols:
                # pointer
                if symbol == '^':
                    my_data['var_reference'] = module.var_pointer_dereference_template % my_data
                elif symbol == '.':
                    self._next.to_code()
                    my_data['field'] = self._next.code[name + '_reference']
                    my_data['var_reference'] = module.var_record_dereference_template % my_data
                elif isinstance(symbol, PascalExpression):
                    symbol.to_code()
                    my_data['expression'] = symbol.code[name]
                    my_data['var_reference'] = module.var_array_dereference_template % my_data
                else:
                    logger.error("Unknown symbol type when converting... %s", symbol)
                    assert False
            self._code[name + '_reference'] = my_data['var_reference']