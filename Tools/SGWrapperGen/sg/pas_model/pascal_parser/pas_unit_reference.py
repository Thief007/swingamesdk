from tokeniser.pas_token_kind import TokenKind

class PascalUnitReference(object):
    """
    The model object to represent a reference to a unit:
    """
    
    def __init__(self):
        self._name = None
        self._points_to = None  # PascalUnit
        self._code = dict()

    @property
    def code(self):
        return self._code

    @property
    def name(self):
        return self._name

    @property
    def points_to(self):
        return self._points_to
    
    def parse(self, tokens):
        """
        Parses the entire pascal program
        expects: 'program name;' at the start
        """
        self._name = tokens.match_token(TokenKind.Identifier).value;

    def to_code(self):
        import converter_helper
        my_data = dict()
        my_data['pas_lib_identifier'] = self._name 
        my_data['c_lib_identifier'] = converter_helper.lower_name(self._name)

        for (name, module) in converter_helper.converters.items():
            self._code[name] = module.unit_reference_template % my_data