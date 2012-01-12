from pas_token_kind import TokenKind
from pas_parser_utils import logger
from pas_expression import PascalExpression

class PascalIfStatement(object):
    """
    The PascalCompoundStatement object parses a compound statement
    """

    def __init__(self, block):
        self._expression = None
        self._compound_statement = None
        self._block = block

    @property
    def expressions(self):
        return self._expressions

    @property
    def kind(self):
        return 'if statement'

    def parse(self, tokens):
        from pas_parser_utils import parse_statement
        logger.debug("Parsing if statement")
        tokens.match_token(TokenKind.Identifier, 'if')
        self._expression = PascalExpression(self._block)
        self._expression.parse(tokens)
        self._compound_statement = parse_statement(tokens, self._block)
        # expression will consume the 'then' delimiter
        logger.debug("Finished parsing if statement")


                




