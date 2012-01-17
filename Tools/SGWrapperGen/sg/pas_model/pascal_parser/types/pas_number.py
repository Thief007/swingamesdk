class PascalNumber(object):
    """
    Describes a number used in expressions
    """
    
    def __init__(self, value):
        self._value = value
                
    @property
    def kind(self):
        return 'number'

    @property
    def value(self):
        return self._value

    def __str__(self):
        return str(value)
