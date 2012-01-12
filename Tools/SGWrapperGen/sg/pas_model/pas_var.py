class PascalVariable(object):
    """
    Describes a single variable in pascal
        Contains a name and type
    """
    
    def __init__(self, name, type):
        self._type = type
        self._name = name
                
    @property
    def type(self):
        return self._type

    @property
    def name(self):
        return self._name

    @property
    def kind(self):
        return 'variable'
