from types.pas_type_cache import find_or_add_type
from pas_var import PascalVariable

_global_variables = {
    'ColorGreen'    : PascalVariable('ColorGreen', find_or_add_type('color')),
    'ColorBlue'     : PascalVariable('ColorBlue', find_or_add_type('color')),
    'ColorWhite'    : PascalVariable('ColorWhite', find_or_add_type('color')),
    'ColorRed'      : PascalVariable('ColorRed', find_or_add_type('color')),
    'AlignCenter'   : PascalVariable('AlignCenter', find_or_add_type('enumeration')),  # the type doesn't matter because it is never declared in the parsed code
    'vk_LEFT'       : PascalVariable('vk_LEFT', find_or_add_type('enumeration')),  # the type doesn't matter because it is never declared in the parsed code
    'vk_RIGHT'      : PascalVariable('vk_RIGHT', find_or_add_type('enumeration')),  # the type doesn't matter because it is never declared in the parsed code
    'vk_UP'         : PascalVariable('vk_UP', find_or_add_type('enumeration')),  # the type doesn't matter because it is never declared in the parsed code
    'vk_DOWN'       : PascalVariable('vk_DOWN', find_or_add_type('enumeration'))  # the type doesn't matter because it is never declared in the parsed code

    }