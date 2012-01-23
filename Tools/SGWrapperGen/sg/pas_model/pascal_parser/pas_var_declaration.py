from pascal_parser.tokeniser.pas_token_kind import TokenKind
from pascal_parser.pas_parser_utils import logger


class PascalVarDeclaration(object):
    """
    The PascalVarDeclaration describes a variable declaration in Pascal
    It stores the identifier-type pairs in the declaration
    """

    # var
    # 'identifier' : 'type' ;

    
    def __init__(self):
        # vars stores the variables declared in this declaration in a dictionary
        # name of the variable is the key, type is the value
        self._vars = dict()
        self._code = dict()

    @property
    def code(self):
        return self._code

    @property
    def kind(self):
        return 'variable_declaration'

    @property
    def variables(self):
        return self._vars

    def parse(self, tokens):
        from types.pas_type_cache import find_or_add_type
        from types.pas_type import *
        from pas_parser_utils import _parse_identifier_list, reservedWords
        from pas_var import PascalVariable

        paramDeclaration = False
        logger.debug("Parsing variable declaration")
        
        tokens.match_token(TokenKind.Identifier, 'var')

        variables = dict()
        while True:
            modifier = None
            # (modifier) identifier list, ':', type, ';'

            idList = _parse_identifier_list(tokens)
            tokens.match_token(TokenKind.Symbol, ':')
            type = PascalType.create_type(tokens)

            if (not tokens.match_lookahead(TokenKind.Symbol, ')')):
                tokens.match_token(TokenKind.Symbol, ';')

            for varName in idList:
                if not varName in self._vars:
                    self._vars[varName] = PascalVariable(varName, type, modifier)  # create and assign the PascalVariable
                    logger.debug("Parsed variable : " + varName + " : " + type.name)
                else:
                    logger.error("Duplicate variable identifier found: " + str(tokens.next_token()))
                    assert(False)

            if tokens.match_one_lookahead(reservedWords) or tokens.match_lookahead(TokenKind.Symbol, ')'):
                logger.debug("Finished parsing variable declaration")
                break

    def to_code(self):
        '''
        This method creates the code to declare all it's variables
        for each of the modules
        '''
        import converter_helper

        for (key, variable) in self._vars.items():
            variable.to_code()

        for (name, module) in converter_helper.converters.items():
            variables = ""
            for (key, variable) in self._vars.items():
                variables += variable.code[name]
            self._code[name] = module.variable_decl_template % { "variables": variables }

