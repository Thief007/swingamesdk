class PascalType(object):
    """
    Describes a type
    """
    
    def __init__(self, name):
        self._name = name
        self._points_to = None      # PascalType
        self._is_pointer = False     
        
    def __str__(self):
        return self._name

    @property
    def name(self):
        return self._name

    @staticmethod
    def create_type(tokens):
        from pascal_parser.tokeniser.pas_token_kind import TokenKind
        from pas_type_cache import all_types, find_or_add_type
        '''Read a type identifier, or array of type'''

        if tokens.match_lookahead(TokenKind.Identifier, 'array', consume=True):
            #found an array
            dimensions = []
            # array [0..n,0..m] of
            if tokens.match_lookahead(TokenKind.Symbol, '[', consume=True):
                while True:
                    low_idx = tokens.match_token(TokenKind.Number).value
                    tokens.match_token(TokenKind.Symbol, '.')
                    tokens.match_token(TokenKind.Symbol, '.')
                    high_idx = tokens.match_token(TokenKind.Number).value
                    dimensions.append((low_idx,high_idx))
                    
                    if tokens.match_lookahead(TokenKind.Symbol, ']', consume=True):
                        break
                    tokens.match_token(TokenKind.Symbol, ',')
            else:
                dimensions.append((0,'n - 1'))
            
            # of type...
            tokens.match_token(TokenKind.Identifier, 'of')
            nested_type = PascalType.create_type(tokens) #recursive call
            
            name = nested_type.name + ''.join(['[%s..%s]' % (low_idx, high_idx) for low_idx, high_idx in dimensions])
            was_new = not name in all_types()
            
            result = find_or_add_type(name)
            if was_new:
                result.dimensions = dimensions
                result.nested_type = nested_type
            return result
        elif tokens.match_lookahead(TokenKind.Symbol, '^', True):
            # Creates a pointer type
            other_id = tokens.match_token(TokenKind.Identifier).value
            other_type = find_or_add_type(other_id)
            
            # type name is ^variable_name
            name = '^' + other_type.name
            
            was_new = not name in all_types()
            result = find_or_add_type(name)
            if was_new:
                result.is_pointer = True
                result.points_to = other_type
            return result
        else:
            identifier = tokens.match_token(TokenKind.Identifier).value
            return find_or_add_type(identifier)
