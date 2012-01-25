from pascal_parser.tokeniser.pas_token_kind import TokenKind

from pascal_parser.pas_parser_utils import logger, parse_statement

from pas_var_declaration import PascalVarDeclaration 
from pas_function import PascalFunction
from pas_type_declaration import PascalTypeDeclaration

class PascalBlock(object):
    """
    The PascalBlock object stores the entire pascal block
    """

    #declaration part
        #constants
        #type
        #var
        #procedure/function
    #statement part

    
    def __init__(self, parent):
        self._parent = parent                 #parent block
        self._compound_statement = None
        # list with block contents (in order)
        self._contents = list()
        self._variables = dict()
        self._functions = dict()
        self._types = dict()
        self._code = dict()     

    @property
    def code(self):
        return self._code

    @property
    def parent(self):
        return self._parent

    @property
    def compound_statement(self):
        return self._compound_statement

    @property
    def contents(self):
        return self._contents

    @property
    def variables(self):
        return self._variables

    def parse(self, tokens):
        # while the next token is not begin (start of compound statement), scan the block for declarations
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
                self._types.update(current_part.types)
            elif tokens.match_lookahead(TokenKind.Identifier, 'begin'):
                break
            else:
                logger.error('Unknown block token...' + str(tokens.next_token()))
                assert False
            self._contents.append(current_part)
        # at this point we must be at a begin

        self._compound_statement = parse_statement(tokens, self)

    def get_variable(self, name):
        result = None
        if (name in self._variables):
            result = self._variables[name]
        elif(self._parent != None and self._parent.get_variable(name) != None):
            result = self._parent.get_variable(name)
        else:
            logger.error("Unable to locate variable in scope: " + name)
            assert False
        return result

    def to_code(self, exportStatements=True):
        '''
        This method creates the code to declare all it's variables
        for each of the modules
        '''
        import converter_helper
        
        for part in self._contents:
            part.to_code()

        if (self._compound_statement != None):
            self._compound_statement.to_code()

        for (name, module) in converter_helper.converters.items():
            part_code = ""
            lang_data = dict()
            for part in self._contents:
                part_code += part.code[name]
            lang_data['declarations'] = part_code;

            if (self._compound_statement is None):
                lang_data['statement'] = ''
            else:
                lang_data['statement'] = self._compound_statement.code[name]

            self._code[name] = module.block_template % lang_data

