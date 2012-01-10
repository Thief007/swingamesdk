import logging
from pas_token_kind import *
from pas_token import *
from pas_token_stream import *
from Program import *   # this should be replaced
from abc import ABCMeta, abstractmethod

class Parsable:
    """
    Abstract class that describes an object the can be parsed
        eg. a variable declaration can be parsed, and so inherits from
        Parsable
    """
    def parse(self, tokens):
        raise NotImplementedError( "Should have implemented this" )

                
