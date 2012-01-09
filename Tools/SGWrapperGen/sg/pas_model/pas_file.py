from sg_token_stream import SGTokenStream
from pas_program import PascalProgram

class PascalFile(object):
    
    def __init__(self):
        self._filename = '';
        self._contains_kind = None
        self._contents = None
    
    def parse(self, filename, tokens):
        self._filename = filename
        
        if tokens.match_lookahead('id', 'program'):
            self._contains_kind = 'program';
            self._contents = PascalProgram()
        # unit?
        else:
            #logger
            assert false
        
        self._contents.parse(tokens)
        
        #check end of tokens...
            

if __name__ == '__main__':
    stream = SGTokenStream('../../../../CoreSDK/Test/ParserTest.pas')
    
    afile = PascalFile()
    afile.parse('ParserTest.pas', stream)
    
    print afile