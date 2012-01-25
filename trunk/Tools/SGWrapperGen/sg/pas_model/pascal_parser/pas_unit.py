from tokeniser.pas_token_kind import TokenKind
from pas_block import PascalBlock

class PascalUnit(object):
    """
    The model object to represent a pascal unit:
    """
    
    def __init__(self):
        self._name = None
        self._variables = dict()
        self._functions = dict()
        self._types = dict()
        self._code = dict()

    @property
    def code(self):
        return self._code

    @property
    def name(self):
        return self._name

    @property
    def block(self):
        return self._block
    
    def parse(self, tokens):
        """
        Parses the entire pascal program
        expects: 'program name;' at the start
        """
        tokens.match_token(TokenKind.Identifier, 'unit');
        self._name = tokens.match_token(TokenKind.Identifier)._value;
        tokens.match_token(TokenKind.Symbol, ';')
        
        tokens.match_token(TokenKind.Identifier, 'interface')
        # read interface header
        while (True):
            # variable declaration part
            if (tokens.match_lookahead(TokenKind.Identifier, 'var')):
                # read variable declaration part
                # and append to current active variables
                # local variables can overwrite global variables 
                current_part = PascalVarDeclaration()
                current_part.parse(tokens)
                self._variables.update(current_part.variables)
            elif (tokens.match_lookahead(TokenKind.Identifier, 'procedure') or tokens.match_lookahead(TokenKind.Identifier, 'function')):
                current_part = PascalFunction(self)
                current_part.parse(tokens)
                self._functions[current_part.name] = current_part
            elif (tokens.match_lookahead(TokenKind.Identifier, 'type')):
                current_part = PascalTypeDeclaration(self)
                current_part.parse(tokens)
                self._types.update(_type_declarations.types)
            elif tokens.match_lookahead(TokenKind.Identifier, 'begin'):
                break
            else:
                logger.error('Unknown block token...' + str(tokens.next_token()))
                assert False
            self._contents.append(current_part)

    def to_code(self):
        import converter_helper

        self._block.to_code()

        for (name, module) in converter_helper.converters.items():
            my_data = dict()
            my_data[name + '_name'] = self._name
            my_data[name + '_block'] = self._block.code[name]
            self._code[name] = module.program_template % my_data
      

    