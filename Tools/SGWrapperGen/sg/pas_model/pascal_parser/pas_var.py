class PascalVariable(object):
    """
    Describes a single variable in pascal
        Contains a name and type
    """
    
    def __init__(self, name, type, modifier = None):
        self._type = type
        self._name = name
        self._modifier = modifier
                
    @property  
    def modifier(self):
        return self._modifier

    @property
    def type(self):
        return self._type

    @property
    def name(self):
        return self._name

    @property
    def kind(self):
        return 'variable'
