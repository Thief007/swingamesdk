from tokeniser.pas_token_kind import TokenKind
from pas_parser_utils import logger
from pas_expression import PascalExpression

class PascalRepeatStatement(object):
    """
    The PascalCompoundStatement object parses a compound statement
    """

    def __init__(self, block):
        self._expression = None
        self._block = block
        self._statements = list()

    @property
    def expression(self):
        return self._expression

    @property
    def statements(self):
        return self._statements

    @property
    def kind(self):
        return 'repeat statement'

    def parse(self, tokens):
        from pas_parser_utils import parse_statement
        logger.debug("Parsing repeat statement")
        tokens.match_token(TokenKind.Identifier, 'repeat')
        while not tokens.match_lookahead(TokenKind.Identifier, 'until'):
            self._statements.append(parse_statement(tokens, self._block))
        tokens.match_token(TokenKind.Identifier, 'until')
        self._expression = PascalExpression(self._block)
        self._expression.parse(tokens)
        # expression will consume the 'then' delimiter
        logger.debug("Finished parsing repeat statement")


                




