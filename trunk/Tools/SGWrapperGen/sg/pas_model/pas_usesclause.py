
# from sg_pas_tokeniser import SGPasTokeniser
from pas_token_kind import TokenKind

class PascalUsesClause(object):
    '''
    Syntax:
        uses clause = "uses", { unit identifier }+(","), ";" ;
	'''
    def __init__(self):
        pass
    
    def parse(self, tokens):
        """Parse the clause from the tokeniser"""
        tokens.match_token(TokenKind.Identifier, 'uses', True)