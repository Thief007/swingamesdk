from pascal_parser.tokeniser.pas_token_kind import TokenKind

class PascalRecordField(object):
    """
    Describes a record field in pascal
    """
    
    def __init__(self):
        self._name = ''
        self._type = None
        self._code = dict()

    @property
    def code(self):
        return self._code
        
    @property
    def kind(self):
        return 'record field'

    @property
    def name(self):
        return self._name

    def parse(self, tokens):
        from pascal_parser.pas_parser_utils import parse_type
        self._name = tokens.match_token(TokenKind.Identifier).value
        tokens.match_token(TokenKind.Symbol, ':')
        self._type = parse_type(tokens)
        tokens.match_token(TokenKind.Symbol, ';')


    def to_code(self):
        import converter_helper

        my_data = dict()

        my_data['pas_lib_identifier'] = self._name 
        my_data['c_lib_identifier'] = converter_helper.lower_name(self._name)

        for (name, module) in converter_helper.converters.items():
            my_data[name + '_type'] = converter_helper.convert_type(module._type_switcher, self._type, None)
            self._code[name] = module.record_field_template % my_data