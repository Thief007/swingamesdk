from pas_token_kind import TokenKind
from pas_parser_utils import logger, token_has_values, parse_statement

from pas_assignment_statement import AssignmentStatement

class PascalCompoundStatement(object):
    """
    The PascalCompoundStatement object parses a compound statement
    """

    def __init__(self, block):
        self._statements = []
        self._block = block

    @property
    def statements(self):
        return self._statements

    @property
    def block(self):
        return self._block

    def parse(self, tokens):
        logger.debug("Parsing compound statement")
        tokens.match_token(TokenKind.Identifier, 'begin')
        while (True):
            # compound statement currently consumes the end keyword, but not the symbol ';' or '.'
            if tokens.match_lookahead(TokenKind.Identifier, 'end'):
                tokens.next_token()             # consume end token
                break
            elif tokens.match_lookahead(TokenKind.Symbol, ';') or tokens.match_lookahead(TokenKind.Symbol, '.'):
                # do not consume -> '.' is needed to tell the first block that it is at the end of the program
                break   
            else:
                self._statements.append(parse_statement(tokens, self._block))

        logger.debug("Finished parsing compound statement")




                




