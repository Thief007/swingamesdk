
# from sg_pas_tokeniser import SGPasTokeniser

class PascalUsesClause(object):
    """
    
    Syntax:
        uses clause = "uses", { unit identifier }+(","), ";" ;
    """
    def __init__(self, arg):
        super(PascalUsesClause, self).__init__()
        self.arg = arg
    
    def parse(self, tokens):
        """Parse the clause from the tokeniser"""
        
        tokens.match_token('id', 'uses', True)
        
    