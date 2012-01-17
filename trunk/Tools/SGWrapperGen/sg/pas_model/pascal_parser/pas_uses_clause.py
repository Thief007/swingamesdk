from pas_token_kind import TokenKind
from pas_parser_utils import logger

class PascalUsesClause(object):
    """
    The PascalCompoundStatement object parses a compound statement
    """

    def __init__(self):
        self._units = list()

    @property
    def units(self):
        return self._units

    @property
    def kind(self):
        return 'uses clause'

    def parse(self, tokens):
        logger.debug("Parsing uses clause")
        tokens.match_token(TokenKind.Identifier, 'uses')
        while (True):
            if (tokens.match_lookahead(TokenKind.Symbol, ';')):
                tokens.match_token(TokenKind.Symbol, ';')
                break
            elif (tokens.match_lookahead(TokenKind.Symbol, ',')):
                tokens.match_token(TokenKind.Symbol, ',')
            elif (tokens.match_lookahead(TokenKind.Identifier)):
                self._units.append(tokens.match_token(TokenKind.Identifier).value)
        logger.debug("Finished parsing uses clause")


                




