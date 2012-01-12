from pas_token_kind import TokenKind

import logging

reservedWords = [   (TokenKind.Identifier, 'begin'),      (TokenKind.Identifier, 'const'), 
                    (TokenKind.Identifier, 'type'),       (TokenKind.Identifier, 'var'), 
                    (TokenKind.Identifier, 'procedure'),  (TokenKind.Identifier, 'function'),
                    (TokenKind.Identifier, 'end')]

assignmentOperators = [(TokenKind.Operator, '+='),  (TokenKind.Operator, '-='), 
                        (TokenKind.Operator, '/='),  (TokenKind.Operator, '*='), 
                        (TokenKind.Operator, ':=')]

operators   =   [   (TokenKind.Operator, '+'), (TokenKind.Operator, '-'), (TokenKind.Operator, '/'), (TokenKind.Operator, '*'),
                    (TokenKind.Operator, '<'), (TokenKind.Operator, '>'), (TokenKind.Operator, '<>'), (TokenKind.Operator, '=')]   


# Logger object is used to log events to console
logger = logging.getLogger("sgLogger")

def parse_identifier_list(tokens):
    """
    This method parses a Pascal identifier list, it returns a list of
    variable identifiers
    """
    # { identifier }+(',')
    names = list()
    while True:
        nameTok = tokens.match_token(TokenKind.Identifier)
        names.append(nameTok._value)
        if not tokens.match_lookahead(TokenKind.Symbol, ',', consume=True):
            return names

def token_has_values(token, list_values):
    """
    This method searches through all the values in a list and returns true
    if the token has a value equivalent to an element in the list.
    """
    for value in list_values:
        if value[1] == token.value:
            return True
    return False

def parse_statement(tokens, block):
    """
    parses a statement and returns the class describing the statement
    can parse compound statements.
    """
    result = None
    if tokens.match_lookahead(TokenKind.Identifier, 'begin'):
        result = _parse_compound_statement(tokens, block)
    elif tokens.match_lookahead(TokenKind.Identifier, 'if'):
          result = _parse_if_statement(tokens, block)
    # identifier expected first
    elif tokens.match_lookahead(TokenKind.Identifier):
        # { variable } ( '+=', '-=', '/=', '*=' ) { variable }
        if token_has_values(tokens.lookahead(2)[1], assignmentOperators):
            result = _parse_assignment_statement(tokens, block)
    return result

def _parse_compound_statement(tokens, block):
    from pas_compound_statement import PascalCompoundStatement 

    result = PascalCompoundStatement(block)
    result.parse(tokens)
    return result

def _parse_if_statement(tokens, block):
    from pas_if_statement import PascalIfStatement 

    result = PascalIfStatement(block)
    result.parse(tokens)
    return result

def _parse_assignment_statement(tokens, block):
    from pas_assignment_statement import AssignmentStatement

    result = AssignmentStatement(block)
    result.parse(tokens)
    return result

    