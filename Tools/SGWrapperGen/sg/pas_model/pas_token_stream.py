
from sg_pas_tokeniser import SGPasTokeniser

class SGTokenStream(object):
    """A stream of tokens the parsing code can move through"""
    
    def __init__(self, filename):
        super(SGTokenStream, self).__init__()
        
        # Create the tokeniser
        self._tokeniser = SGPasTokeniser()
        self._tokeniser.tokenise(filename)
        self._current_file = filename
        
        # Keep track of the lookahead tokens
        self._lookahead_toks = []
    
    def next_token(self):
        current_token = None
        while current_token == None or current_token[0] == 'comment':
            if len(self._lookahead_toks) > 0:
                current_token = self._lookahead_toks[0]
                self._lookahead_toks = self._lookahead_toks[1:]
            else:
                current_token = self._tokeniser.next_token()
            # if current_token[0] == 'comment':
            #     logger.debug('Parser    : Skipping comment: %s', current_token[1])
        return current_token
    
    def lookahead(self,count=1):
        # logger.debug('Parser    : Looking ahead %d', count)
        while len(self._lookahead_toks) < count:
            current_token = self._tokeniser.next_token()
            while current_token[0] == 'comment':
                current_token = self._tokeniser.next_token()
            self._lookahead_toks.append(current_token)
        return self._lookahead_toks
    
    def match_lookahead(self, token_kind, token_value = None, consume = False):
        # logger.debug('Parser    : Looking to find %s (%s)%s', token_kind, 
        #     token_value if token_value != None else 'any',
        #     ' will consume' if consume else '')
        token = self.lookahead(1)[0]
        result = token[0] == token_kind and (token_value == None or token_value == token[1].lower())
        if consume and result:
            self._match_token(token_kind, token_value)
        return result

    def match_token(self, token_kind, token_value = None):
        tok = self.next_token()
        
        if tok[0] != token_kind or (token_value != None and token_value != tok[1].lower()):
            # logger.error('Parse Error %s: found a %s (%s) expected %s (%s)', 
            #     self._tokeniser.line_details(), 
            #     tok[0], tok[1], token_kind, token_value)
            assert False
            
        # logger.debug('Parser    : Matched token %s (%s)', tok[0], tok[1])
        return tok
    
    def match_one_token(self, token_kind_lst):
        matched = False
        tok = self.next_token()
        
        for token_kind,token_value in token_kind_lst:
            if tok[0] == token_kind and (token_value == None or token_value == tok[1]):
                matched = True
                logger.debug('Parser    : Matched %s with %s', tok[0], tok[1])
                break

        if not matched:
            logger.error('Parser Error %s: unexpected %s(%s) expected %s', 
                self._tokeniser.line_details(), 
                tok[0], tok[1], 
                map(lambda n: '%s(%s)' % (n[0],n[1]),token_kind_lst))
            assert False
        return tok

if __name__ == '__main__':
    stream = SGTokenStream('../../../../CoreSDK/Test/ParserTest.pas')
    
    while True:
        print stream.next_token()
        