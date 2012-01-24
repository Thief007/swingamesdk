from pascal_parser.tokeniser.pas_token_kind import TokenKind
class PascalPointer(object):
    """
    Describes a pointer
    """
    
    def __init__(self):
        self._points_to = None      # PascalType  
        self._name = ''
        
    @property
    def name(self):
        return self._name

    @property
    def points_to(self):
        return self._points_to

    def parse(self, tokens):
        from pascal_parser.pas_parser_utils import parse_type
        tokens.match_token(TokenKind.Symbol, '^')
        self._points_to = parse_type(tokens)
        self._name = '^' + self._points_to.name