from pascal_parser.pas_parser_utils import logger 

_loaded_types = dict()

def all_types():
    return _loaded_types

def get_type(name):
    '''
    gets a type from the type cache and returns it
    if no type is found then it returns None
    '''
    if name is None:
        logger.error("Type Cache:       name was None")
        assert False

    if name in _loaded_types:
        return _loaded_types[name]
    else:
        return None

def add_type(to_add):
    '''
    adds a new type to the type cache.
    will fail if the type already exists
    '''
    if get_type(to_add.name) is None:
        _loaded_types[to_add.name] = to_add
    else:
        logger.error("Type Cache:       Type already exists in cache (%s)", to_add.name)
        assert False