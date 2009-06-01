
import logging

_classes = {}
_loaded_types = {}

logger = logging.getLogger("SGWrapperGen")

def all_classes():
    return _classes

def find_or_add_class(name):
    '''finds or creates and adds a class with the indicated name'''
    if name == None: return None
    from SGClass import SGClass
    if name in _classes.keys():
        return _classes[name]
    else:
        logger.info('Created class %s', name)
        result = SGClass(name)
        _classes[name] = result
        return result

def find_or_add_type(name):
    if name == None: return None
    from SGType import SGType
    
    if name in _loaded_types:
        return _loaded_types[name]
    else:
        logger.info('Created Type %s', name)
        result = SGType(name)
        _loaded_types[name] = result
        return result



