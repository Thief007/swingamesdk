class PascalArray(object):
    """
    Describes an array
    """
    
    def __init__(self, low_bound, high_bound, type):
        self._low_bound = low_bound
        self._high_bound = high_bound
        self._type = type
        self._contents = list()
                
    @property
    def kind(self):
        return 'array'

    @property
    def type(self):
        return self._type

    @property
    def value(self):
        return self._contents

    def __str__(self):
        return str(self._contents)