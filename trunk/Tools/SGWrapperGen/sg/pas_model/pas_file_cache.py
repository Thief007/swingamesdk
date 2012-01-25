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
    else:
        logger.error("File Cache:       Unable to add None unit")
        assert False

def get_unit_named(name):
    '''
    gets a unit of a specific name from the unit cache
    '''
    if name != None:
        return _loaded_units[name]
    else:
        return None

def add_unit(unit):
    '''
    adds a unit to the unit cache
    '''
    if unit != None:
        _loaded_units[unit.name] = unit
    else:
        logger.error("File Cache:       Unable to add None unit")
        assert False