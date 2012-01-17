class PascalString(object):
    """
    Describes a string
    """
    
    def __init__(self, value):
        self._value = value
                
    @property
    def kind(self):
        return 'string'

    @property
    def value(self):
        return self._value

    def __str__(self):
        return str(value)