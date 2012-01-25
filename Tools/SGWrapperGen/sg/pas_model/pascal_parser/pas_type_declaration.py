from pascal_parser.tokeniser.pas_token_kind import TokenKind
from pascal_parser.pas_parser_utils import logger, reservedWords, token_has_values
from pascal_parser.types.pas_type_cache import *
from pascal_parser.types.pas_enum import PascalEnum
from pascal_parser.types.pas_pointer import PascalPointer
from pascal_parser.types.pas_record import PascalRecord


class PascalTypeDeclaration(object):
    """
    PascalFunction is a container for a procedure or function in Pascal
    """
   
    def __init__(self, parent):
        """
        parent is the current block the type declaration is 
        being initialised from
        """

        #parent block
        self._parent = parent       
        self._name = ''
        # stores the type name (key) and type declaration
        self._type_declarations = dict()       
        self._code = dict()
        self._comments = list()

    @property
    def code(self):
        return self._code
    @property
    def name(self):
        return self._name

    @property
    def types(self):
        return self._type_declarations

    @property
    def kind(self):
        return 'type declaration'

    def parse(self, tokens):
        from pas_parser_utils import parse_type
        self._comments = tokens.get_comments()
        tokens.match_token(TokenKind.Identifier, 'type')

        while not token_has_values(tokens.lookahead()[0], reservedWords):
            type_name = tokens.match_token(TokenKind.Identifier).value
            new_type = None
            if tokens.match_lookahead(TokenKind.Operator, '=', consume=True):
                # enumeration...
                if tokens.match_lookahead(TokenKind.Symbol, '('):
                    new_type = PascalEnum(type_name)
                    new_type.parse(tokens)
                #record...
                elif (tokens.match_lookahead(TokenKind.Identifier, 'packed') and tokens.match_lookahead(TokenKind.Identifier, 'record')) or tokens.match_lookahead(TokenKind.Identifier, 'record'):
                    new_type = PascalRecord(type_name)
                    new_type.parse(tokens)
                # other type...
                elif (tokens.match_lookahead(TokenKind.Identifier)):
                    type_temp = parse_type(tokens)
                else:
                    logger.error("Type Declaration:     Unknown type in type declaration", tokens.next_token())
                    assert False
            # parse new type and add it to the type declaration
            self._type_declarations[type_name] = new_type
        
    def to_code(self):
        '''

        '''
        import converter_helper

        for key, type in self._type_declarations.items():
            type.to_code()

        my_data = dict()
        # seperators need to be set for every new package
        #   these could go in the __init__ for each library?
        for (key, type) in self._type_declarations.items():
            type.to_code()

        for (name, module) in converter_helper.converters.items():
            declaration = ""
            index = 0
            for (key, type) in self._type_declarations.items():
                declaration += type.code[name]
            self._code[name] =  module.type_declaration_template % {"declaration" : declaration}