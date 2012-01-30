from pascal_parser.tokeniser.pas_token_kind import TokenKind
class PascalPointer(object):
    """
    Describes a pointer
    """
    
    def __init__(self):
        self._points_to = None      # PascalType  
        self._name = ''
        self._function_pointer = False
        
    @property
    def name(self):
        return self._name

    @property
    def points_to(self):
        return self._points_to

    def parse(self, tokens):
        from pascal_parser.pas_parser_utils import parse_type
        from pascal_parser.pas_function import PascalFunction
        if tokens.match_lookahead(TokenKind.Symbol, '^', consume=True):
            self._points_to = parse_type(tokens)
            self._name = '^' + self._points_to.name
        elif tokens.match_lookahead(TokenKind.Identifier, 'procedure') or tokens.match_lookahead(TokenKind.Identifier, 'function'):
            self._points_to = PascalFunction(None, tokens, func_pointer = True)
            self._points_to.parse(tokens, is_forward=True)
        else:
            logger.error("Invalid pointer declaration: ", tokens.next_token())
            assert False