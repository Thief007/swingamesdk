from pascal_parser.tokeniser.pas_token_kind import TokenKind
from pascal_parser.pas_parser_utils import logger


class PascalParameterDeclaration(object):
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

    @property
    def kind(self):
        return 'parameter declaration'

    @property
    def variables(self):
        return self._vars

    def parse(self, tokens, method):
        """Read in the parameters declared in a function, procedure or operator"""
        from types.pas_type_cache import find_or_add_type
        from pas_parser_utils import _parse_identifier_list, reservedWords
        from pas_var import PascalVariable
        from types.pas_type import PascalType

        tokens.match_token(TokenKind.Symbol, '(')
        #look for parameters
        while not tokens.match_lookahead(TokenKind.Symbol, ')'): #consume ) at end
            param_tok, other_tok = tokens.lookahead(2)
            modifier = None
            #Look for modifier
            if param_tok.kind == TokenKind.Identifier:
                # First value is modifier
                if other_tok.kind == TokenKind.Identifier: 
                    modifier = tokens.match_token(TokenKind.Identifier).value
                # No modifier found
                else:
                    modifier = None

            # get parameter names
            parameters = [tokens.match_token(TokenKind.Identifier).value]
            while tokens.match_lookahead(TokenKind.Symbol, ',', True):
                #there is a list of parameters
                parameters.append(tokens.match_token(TokenKind.Identifier).value)
            
            # colon seperates identifiers and type
            tokens.match_token(TokenKind.Symbol, ':')
            the_type = PascalType.create_type(tokens)   # reads the type and returns PascalType
            
            for parameter_name in parameters:
                toAdd = PascalVariable(parameter_name, the_type, modifier)
                method.add_parameter(toAdd)
                logger.debug('Parser    : Adding parameter %s (%s) to %s', parameter_name, the_type, method.name)
            
            if tokens.match_lookahead(TokenKind.Symbol, ';', consume=True): break
        
        tokens.match_token(TokenKind.Symbol, ')')
                
