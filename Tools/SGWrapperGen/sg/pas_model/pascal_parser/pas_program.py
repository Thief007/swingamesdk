from tokeniser.pas_token_kind import TokenKind
from pas_block import PascalBlock

class PascalProgram(object):
    """The model object to represent a pascal program:
    
    Syntax for a program is:
        program = "program", identifier, ";", [uses clause], block, "." ;
        block = "begin", { statement }+(";"), "end" ;
    """
    
    def __init__(self):
        self._name = None
        self._block = None  # program block
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
        tokens.match_token(TokenKind.Identifier, 'program');
        self._name = tokens.match_token(TokenKind.Identifier)._value;
        tokens.match_token(TokenKind.Symbol, ';')
        
        # Read block
        self._block = PascalBlock(None)
        self._block.parse(tokens)

    def to_code(self):
        import converter_helper

        self._block.to_code()

        for (name, module) in converter_helper.converters.items():
            my_data = dict()
            my_data[name + '_name'] = self._name
            my_data[name + '_block'] = self._block.code[name]
            self._code[name] = module.program_template % my_data
      

    