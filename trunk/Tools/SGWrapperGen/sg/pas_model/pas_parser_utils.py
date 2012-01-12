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