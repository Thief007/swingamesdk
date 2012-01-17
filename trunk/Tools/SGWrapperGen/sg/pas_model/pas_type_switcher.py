from pascal_parser.pas_parser_utils import logger

def convert_type(the_dict, the_type, modifier = None, dict_name = '_type_switcher'):
    '''
    switch types for the SwinGame library.
    
    Params:
     - the_dict:    The dictionary with the type changes with modifier keys
     - the_type:    The type being checked
     - modifier:    The modifier for the type
     - dict_name:   The name of the dictionary for error reporting
    '''
    if (the_type is None):
        return 'void'

    key = the_type.name.lower() if the_type != None else None
    
    if modifier == 'result': modifier = 'return'
    
    if key not in the_dict[modifier]:
        logger.error('HELPER    : Error changing model type %s - %s', modifier, the_type)
        logger.error('          : Add \'%s[%s]\': \'%s\': \'????\',', dict_name, modifier, the_type.name.lower())
                
        the_dict[modifier][key] = "UNKNOWN"
        
    return the_dict[modifier][key]

def convert_operator(the_dict, the_operator, dict_name = '_operator_conversion_table'):
    '''
    converts operators from Pascal to other language operators
    Does not fail if the operator wasn't found, the returned value
    will be the same as the current operator
    '''
    key = the_operator.value.lower() if the_operator != None else None
    
    if key not in the_dict:
        logger.debug('          : Adding \'%s[%s]\'????\',', dict_name, the_operator.value.lower())
        the_dict[key] = the_operator.value
        
    return the_dict[key]