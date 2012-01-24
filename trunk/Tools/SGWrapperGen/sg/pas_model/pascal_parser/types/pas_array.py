from pascal_parser.tokeniser.pas_token_kind import TokenKind
class PascalArray(object):
    """
    Describes an array
    """
    
    def __init__(self):
        self._dimensions = list()   # list of tuple (low_idx, high_idx)
        self._nested_type = None    #PascalType...
        self._name = ''
        
    @property
    def name(self):
        return self._name
              
    @property
    def kind(self):
        return 'array'

    @property
    def type(self):
        return self._type

    def parse(self, tokens):
        from pascal_parser.pas_parser_utils import parse_type
        from pas_type_cache import *

        tokens.match_token(TokenKind.Identifier, 'array')
        # array [0..n,0..m] of
        if tokens.match_lookahead(TokenKind.Symbol, '[', consume=True):
            while True:
                low_idx = tokens.match_token(TokenKind.Number).value
                tokens.match_token(TokenKind.Symbol, '.')
                tokens.match_token(TokenKind.Symbol, '.')
                high_idx = tokens.match_token(TokenKind.Number).value
                self._dimensions.append((low_idx,high_idx))
                    
                if tokens.match_lookahead(TokenKind.Symbol, ']', consume=True):
                    break
                tokens.match_token(TokenKind.Symbol, ',')
        else:
            self._dimensions.append((0,'n - 1'))
            
        # of type...
        tokens.match_token(TokenKind.Identifier, 'of')
        self._nested_type = parse_type(tokens) #recursive call
            
        self._name = self._nested_type.name + ''.join(['[%s..%s]' % (low_idx, high_idx) for low_idx, high_idx in self._dimensions])
        
