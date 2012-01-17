from pas_type_cache import find_or_add_type
from pas_var import PascalVariable

_global_variables = {
    'ColorGreen' : PascalVariable('ColorGreen', find_or_add_type('Color')),
    'ColorWhite' : PascalVariable('ColorWhite', find_or_add_type('Color')),
    'ColorRed' : PascalVariable('ColorRed', find_or_add_type('Color')),
    'AlignCenter' : PascalVariable('AlignCenter', find_or_add_type('FontAlignment'))
    }