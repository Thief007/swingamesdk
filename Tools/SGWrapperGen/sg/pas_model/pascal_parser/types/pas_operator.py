class PascalOperator(object):
    """
    Describes a operator used in expressions
    """
    
    def __init__(self, value):
        self._value = value
                
    @property
    def kind(self):
        return 'operator'

    @property
    def value(self):
        return self._value
