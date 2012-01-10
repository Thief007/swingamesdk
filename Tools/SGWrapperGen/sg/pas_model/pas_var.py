import logging
from pas_token_kind import *
from pas_token import *
from pas_token_stream import *
from Program import *   # this should be replaced

class PascalVariable(object):
    """
    Describes a single variable in pascal
        Contains a name and type
    """
    
    def __init__(self, name, type):
        self._type = type
        self._name = name
                
