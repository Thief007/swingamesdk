from pas_token_kind import TokenKind
from pas_parser_utils import logger, token_has_values, assignmentOperators

from pas_assignment_statement import AssignmentStatement

class PascalCompoundStatement(object):
    """
    The PascalCompoundStatement object parses a compound statement
    """

    def __init__(self):
        self._statements = []

    @property
    def statements(self):
        return self._statements

    def parse(self, tokens):
        # variable = 5;
        # myFunc(variable);
        # while (
        # if x == 5 then
        # if (x == 5) then
        logger.debug("Parsing compound statement")
        tokens.match_token(TokenKind.Identifier, 'begin')
        while (True):
            print tokens.lookahead(1)[0].value
            if tokens.match_lookahead(TokenKind.Identifier, 'end'):
                break
            # identifier expected first
            elif tokens.match_lookahead(TokenKind.Identifier):
                # { variable } ( '+=', '-=', '/=', '*=' ) { variable }
                if token_has_values(tokens.lookahead(2)[1], assignmentOperators):
                    # do read assignment statement
                    # return statement to list of statements
                    logger.debug("Found assignment statement")
                    newStatement = AssignmentStatement()
                    newStatement.parse(tokens)
                    self._statements.append(newStatement)
                    logger.debug("Finished parsing assignment statement")
            else:
                logger.error('Unknown statement token...' + str(tokens.next_token()))
                assert False

            #newStatement.parse()
            #_statements.append(newstatement)


                




