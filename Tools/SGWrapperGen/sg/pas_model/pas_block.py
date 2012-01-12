from pas_token_kind import TokenKind

from pas_parser_utils import logger, parse_statement

from pas_var_declaration import PascalVarDeclaration 

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
        # list with block contents (in order
        self._contents = list()
        self._variables = dict()

    @property
    def parent(self):
        return self._parent

    @property
    def compound_statement(self):
        return self._compound_statement

    @property
    def contents(self):
        return self._contents

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
        elif(self._parent != None and name in self._parent._variables):
            result = self._parent._variables[name]
        else:
            logger.error("Unable to locate variable in scope: " + name)
            assert False
        return result
