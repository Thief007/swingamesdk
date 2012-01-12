from pas_token_kind import TokenKind
from pas_var import PascalVariable
from pas_parser_utils import logger, parse_identifier_list, reservedWords

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
        self._contents = list()

    def part_kind(self):
        return 'variable declaration'

    @property
    def variables(self):
        return self._vars

    # TODO enable parsing of arrays
    def parse(self, tokens):
        tokens.match_token(TokenKind.Identifier, 'var')

        logger.debug("Parsing variable declaration")
        while True:
            # identifier list, ':', type, ';'
            idList = parse_identifier_list(tokens)
            tokens.match_token(TokenKind.Symbol, ':')
            typeName = tokens.match_token(TokenKind.Identifier)._value
            tokens.match_token(TokenKind.Symbol, ';')

            for varName in idList:
                if not varName in self._vars:
                    self._vars[varName] = PascalVariable(varName, typeName)  # create and assign the PascalVariable
                    self._contents.append(self._vars[varName])
                    logger.debug("Parsed variable : " + varName + " : " + typeName)
                else:
                    logger.error("Duplicate variable identifier found: " + str(current_token))
                    assert(False)

            if tokens.match_one_lookahead(reservedWords):
                logger.debug("Finished parsing variable declaration")
                return self._vars
                
