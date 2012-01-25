from pascal_parser.tokeniser.pas_token_kind import TokenKind
class PascalType(object):
    """
    Describes a general type... Integer, String etc...
    That does not need to record any additional information 
    """
    
    def __init__(self):
        self._name = ''
        
    @property
    def name(self):
        return self._name

    def parse(self, tokens):
        self._name = tokens.match_token(TokenKind.Identifier).value
