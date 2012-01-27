from pascal_parser.tokeniser.pas_token_kind import TokenKind
class PascalType(object):
    """
    Describes a general type... Integer, String etc...
    That does not need to record any additional information 
    """
    
    def __init__(self):
        self._name = ''
        self._type = ''
        
    @property
    def name(self):
        return self._name

    @property
    def kind(self):
        return self._type

    def parse(self, tokens):
        self._name = tokens.match_token(TokenKind.Identifier).value
        self._type = self._name
