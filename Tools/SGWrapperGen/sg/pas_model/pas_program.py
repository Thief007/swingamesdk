
class PascalProgram(object):
    """The model object to represent a pascal program:
    
    Syntax for a program is:
        program = "program", identifier, ";", [uses clause], block, "." ;
        block = "begin", { statement }+(";"), "end" ;
    """
    
    def __init__(self):
        super(PascalProgram, self).__init__()
        self._name = None
    
    def parse(self, tokens):
        
        tokens.match_token('id', 'program');
        self._name = tokens.match_token('id')[1];
        tokens.match_token('symbol', ';')
        
        
        
if __name__ == '__main__':
    stream = SGTokenStream('../../../../CoreSDK/Test/ParserTest.pas')
    
    
    
    while True:
        print stream.next_token()

    