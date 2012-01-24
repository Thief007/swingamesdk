from pascal_parser.tokeniser.pas_token_kind import TokenKind
class PascalRecord(object):
    """
    Describes a record in pascal
    """
    
    def __init__(self, name):
        self._name = name
        self._fields = list()   # list of tuples
        self._code = dict()

    @property
    def code(self):
        return self._code
        
    @property
    def kind(self):
        return 'record'

    @property
    def name(self):
        return self._name

    def parse(self, tokens):
        from pascal_parser.types.pas_record_field import PascalRecordField
        if (tokens.match_lookahead(TokenKind.Identifier, 'packed', consume=True) and tokens.match_lookahead(TokenKind.Identifier, 'record', consume=True)) or tokens.match_lookahead(TokenKind.Identifier, 'record', consume=True):                #packed record field: Type end;
            while not tokens.match_lookahead(TokenKind.Identifier, 'end', consume=True):
                field = PascalRecordField()
                field.parse(tokens)
                self._fields.append(field)
            tokens.match_token(TokenKind.Symbol, ';')

    def to_code(self):
        import converter_helper

        my_data = dict()

        my_data['pas_lib_identifier'] = self._name 
        my_data['c_lib_identifier'] = converter_helper.lower_name(self._name)

        for field in self._fields:
            field.to_code()

        for (name, module) in converter_helper.converters.items():
            my_data["fields"] = ""
            for field in self._fields:
                my_data["fields"] += field.code[name]
            self._code[name] = module.record_template % my_data