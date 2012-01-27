from tokeniser.pas_token_kind import TokenKind
from pascal_parser.pas_parser_utils import _parse_compound_statement, logger
from pas_function import PascalFunction
from pas_var_declaration import PascalVarDeclaration
from pascal_parser.pas_type_declaration import PascalTypeDeclaration

class PascalUnit(object):
    """
    The model object to represent a pascal unit:
    """
    
    def __init__(self, file):
        self._name = None
        self._file = file
        self._uses_clause = None
        self._interface = list()
        self._implementation = list()
        self._function_forward_declarations = list()

        self._variables = dict()
        self._functions = dict()
        self._types = dict()
        self._contents = list()
        self._code = dict()

    @property
    def code(self):
        return self._code

    @property
    def variables(self):
        return self._variables

    @property
    def functions(self):
        return self._functions

    @property
    def is_parsed(self):
        return self._parsed

    @property
    def name(self):
        return self._name
    
    def resolve_function_call(self, function):
        # only check the functions I have declared
        # in Pascal you cannot chain imports
        for declared_function in self._functions:
            if (function.name == declared_function):
                return declared_function
        # check forward declarations -> recursive calls shouldn't get flagged as unresolvable
        for forward_declaration in self._function_forward_declarations:
            if (function.name == forward_declaration):
                return function
        # check the units I have included...
        if self._uses_clause != None:
            for unit in self._uses_clause._units:
                # check all the functions that are declared in the unit
                for (key, declared_function) in unit.points_to.contents.functions.items():
                    if (function.name == declared_function.name):
                        return declared_function
        return None

    def get_variable(self, name):
        result = None
        if (name in self._variables):
            result = self._variables[name]
                # check the units I have included...
        if not (self._uses_clause is None):
            for unit in self._uses_clause._units:
                # check all the functions that are declared in the unit
                for (key, global_variable) in unit.points_to.contents.variables.items():
                    if (name == global_variable.name):
                        return global_variable
        return result

    def parse(self, tokens):
        """
        Parses the entire pascal unit
        expects: 'unit name;' at the start
        """
        # read unit header
        tokens.match_token(TokenKind.Identifier, 'unit');
        self._name = tokens.match_token(TokenKind.Identifier)._value;
        tokens.match_token(TokenKind.Symbol, ';')

        # read interface
        tokens.match_token(TokenKind.Identifier, 'interface')

        # if there is a uses clause - read it
        if (tokens.match_lookahead(TokenKind.Identifier, 'uses')):
            self._uses_clause = PascalUsesClause()
            self._interface.append(self._uses_clause)

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
                current_part = PascalFunction(self, tokens)
                self._function_forward_declarations.append(current_part)
                current_part.parse(tokens, is_forward = True)
            elif (tokens.match_lookahead(TokenKind.Identifier, 'type')):
                current_part = PascalTypeDeclaration(self)
                current_part.parse(tokens) 
                self._types.update(current_part.types)
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
                current_part = PascalFunction(self, tokens)
                for func in self._function_forward_declarations:
                    if (current_part.name == func.name):
                        current_part = func
                        self._function_forward_declarations.remove(func)
                        self._functions[func.name] = func
                        func.parse(tokens)
                self._functions[current_part.name] = current_part
            elif tokens.match_lookahead(TokenKind.Identifier, 'end'):
                break
            elif tokens.match_lookahead(TokenKind.Identifier, 'begin') or tokens.match_lookahead(TokenKind.Identifier, 'initialization'):
                init_present = True
            else:
                logger.error('Unknown unit token...' + str(tokens.next_token()))
                assert False
            self._implementation.append(current_part)
        if (len(self._function_forward_declarations)) > 0:
            logger.error("Unable to resolve forward function declarations: ", self._function_forward_declarations)
            assert False

        # read initialization?
        if (init_present):
            tokens.match_token(TokenKind.Identifier)
            _parse_compound_statement(tokens, None)
            if tokens.match_lookahead(TokenKind.Identifier, 'finalization'):
                _parse_compound_statement(tokens, None)
        tokens.match_token(TokenKind.Identifier, 'end')
            
      

    