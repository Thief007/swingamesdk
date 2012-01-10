from pas_token_kind import *

class Token(object):
    """
    The token stores a kind-value pair that describes a word in Pascal
    It also stores details about the word such as line position.
    """

    def __init__(self, kind, value, lineNumber, columnNumber):
        self._kind = kind
        self._value = value
        self._lineNumber = lineNumber
        self._lineColumn = columnNumber

    def __str__ (self):
        return self._kind + ' : "' + self._value + '" at ' + str(self._lineNumber) + ',' + str(self._lineColumn)





