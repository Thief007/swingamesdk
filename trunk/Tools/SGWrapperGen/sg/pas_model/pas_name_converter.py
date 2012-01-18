def camel_case_name(string):
    """
    Converts a pascal case name into a camel case name
    eg. ConvertCase -> convertCase
    """
    return string.lower()[0] + string[1:]

def upper_name(string):
    """
    ConvertCase -> CONVERTCASE
    """
    if string == None: return None
    result = ''
    last_was_us = False
    last_was_lc = False
    for i,c in enumerate(string):
        if c.islower():
            result += c.upper()
            last_was_us = False
            last_was_lc = True
        elif c in ['_','-']:
            last_was_us = True
            result += '_'
        elif c.isupper():
            if (not last_was_us) and last_was_lc and (i > 0):
                result += '_'
            result += c
            last_was_us = False
            last_was_lc = False
        else:
            result += c
            last_was_us = False
    return result

def lower_name(string):
    """
    Converts pascal case into c-style casing
    ConvertCase -> convert_case
    """
    if string == None: return None
    string = string.replace('2D', '2d')
    result = ''
    last_was_us = False
    last_was_lc = False
    for i,c in enumerate(string):
        if c.islower():
            result += c
            last_was_us = False
            last_was_lc = True
        elif c in ['_','-']:
            last_was_us = True
            result += '_'
        elif c.isupper():
            if (not last_was_us) and last_was_lc and (i > 0):
                result += '_'
            result += c.lower()
            last_was_us = False
            last_was_lc = False
        else:
            result += c
            last_was_us = False
    return result