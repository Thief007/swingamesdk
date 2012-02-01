from pascal_parser.pas_parser_utils import logger

_loaded_units = dict()
_files = dict()

def files():
    return _files

def get_file_named(name):
    '''
    '''
    if name != None:
        return _files[name]
    else:
        return None

def add_file(file):
    '''
    '''
    if file != None:
        _files[file.name] = file
        if file.contains_kind == 'unit':
            _loaded_units[file.name] = file
            logger.info("File Cache:        Added file: %s", file.name)
    else:
        logger.error("File Cache:       Unable to add None unit")
        assert False

def get_unit_named(name):
    '''
    gets a unit of a specific name from the unit cache
    '''
    if (name != None) and (name in _loaded_units):
        return _loaded_units[name]
    else:
        return None