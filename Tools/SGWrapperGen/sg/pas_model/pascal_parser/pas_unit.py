from tokeniser.pas_token_kind import TokenKind
from pascal_parser.pas_parser_utils import _parse_compound_statement
from pas_function import PascalFunction

class PascalUnit(object):
    """
    The model object to represent a pascal unit:
    """
    
    def __init__(self, file):
        self._name = None
        self._file = file

        self._interface = list()
        self._implementation = list()

        self._variables = dict()
        self._functions = dict()
        self._types = dict()
        self._contents = list()
        self._code = dict()

    @property
    def code(self):
        return self._code

    @property
    def is_parsed(self):
        return self._parsed

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
        # read unit header
        tokens.match_token(TokenKind.Identifier, 'unit');
        self._name = tokens.match_token(TokenKind.Identifier)._value;
        tokens.match_token(TokenKind.Symbol, ';')

        # read interface
        tokens.match_token(TokenKind.Identifier, 'interface')

        # if there is a uses clause - read it
        if (tokens.match_lookahead(TokenKind.Identifier, 'uses')):
            uses_clause = PascalUsesClause()
            self._interface.append(uses_clause)

        function_forward_declarations = list()

        # read declarations
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
                current_part = PascalFunction(self, isForward=True)
                current_part.parse(tokens)
                function_forward_declarations.append(current_part.method_fingerprint)
            elif (tokens.match_lookahead(TokenKind.Identifier, 'type')):
                current_part = PascalTypeDeclaration(self)
                current_part.parse(tokens)
                self._types.update(_type_declarations.types)
            elif tokens.match_lookahead(TokenKind.Identifier, 'implementation'):
                break
            else:
                logger.error('Unknown unit token...' + str(tokens.next_token()))
                assert False
            self._interface.append(current_part)

        init_present = False
        tokens.match_token(TokenKind.Identifier, 'implementation')
        # read implementation
        while (True):
            if (tokens.match_lookahead(TokenKind.Identifier, 'procedure') or tokens.match_lookahead(TokenKind.Identifier, 'function')):
                current_part = PascalFunction(self)
                current_part.parse(tokens)
                if (current_part.method_fingerprint in function_forward_declarations):
                    function_forward_declarations.remove(current_part.method_fingerprint)
                self._functions[current_part.name] = current_part
            elif tokens.match_lookahead(TokenKind.Identifier, 'end'):
                break
            elif tokens.match_lookahead(TokenKind.Identifier, 'begin') or tokens.match_lookahead(TokenKind.Identifier, 'initialization'):
                init_present = True
            else:
                logger.error('Unknown unit token...' + str(tokens.next_token()))
                assert False
            self._implementation.append(current_part)

        # read initialization?
        if (init_present):
            tokens.match_token(TokenKind.Identifier)
            _parse_compound_statement(tokens, None)
            if tokens.match_lookahead(TokenKind.Identifier, 'finalization'):
                _parse_compound_statement(tokens, None)
        tokens.match_token(TokenKind.Identifier, 'end')
            
      

    