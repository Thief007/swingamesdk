#!/usr/bin/env python
# encoding: utf-8
"""
__init__.py

Created by Andrew Cain on 2011-01-04.
Copyright (c) 2009 Swinburne. All rights reserved.
"""

import sys
import os

_val_switcher = {
    'True': 'true',
    'False': 'false'
}

_data_switcher = {
    #Pascal type: what values of this type switch to %s = data value
    'Boolean': '%s != 0'
}

_type_switcher = {
    None :      dict(),
    'const':    dict(),
    'const-cpp':    dict(),
    'var':      dict(),
    'out':      dict(),
    'return':   dict(),
}

_adapter_type_switcher = {
    None:       dict(),
    'const':    dict(),
    'const-cpp':    dict(),
    'var':      dict(),
    'out':      dict(),
    'result':   dict(),
    'return':   dict(),
}

_type_dicts = {
    '_type_switcher': _type_switcher, 
    '_adapter_type_switcher': _adapter_type_switcher,
    }

#
# The _type_dictionary_creation_data contains the data used to build
# the dictionaries for creating the type conversion details.
# 
# This cant use standard template string (%s) as the string values
# must contain the %s for the dictionary. So the string must have
# the #2# replaced by the 2nd element of the tupple eg.
# 
# The data in the identifiers list of tupples is merged into the
# dictionaries indicated by the remaining keys. The #1# values are
# taken from the tupples within the identifiers list. This allows
# a simple replication of the types across a range of type changing
# dictionaries in a consistent manner
#
_type_dictionary_creation_data = [
    # basic types
    {
        'identifiers': [
            ('boolean',     'bool'),
            ('byte',        'unsigned char'),
            ('longword',    'uint32_t'),
            ('single',      'float'),
            ('longint',     'int32_t'),
            ('uint16',      'unsigned short int'),
            ('color',       'color'),
        ],
        '_type_switcher': {
            None:       '#2# ',
            # 'const':    '',
            'var':      '#2# *',
            'out':      '#2# *',
            'return':   '#2#',
        },
        '_adapter_type_switcher': {
            None:       '#2# ',
            # 'const':    '',
            'var':      '#2# *',
            'out':      '#2# *',
            #'result':   '#2# *',
            'return':   '#2#',
        }
    },
    # resource types 
    {
        'identifiers': [
            ('soundeffect',     'sound_effect'),
            ('music',           'music'),
            ('timer',           'timer'),
            ('bitmap',          'bitmap'),
            ('character',       'character'),
            ('sprite',          'sprite'),
            ('font',            'font'),
            ('map',             'map'),
            ('animationscript', 'animation_script'),
            ('animation',       'animation'),
            ('shapeprototype',  'shape_prototype'),
            ('shape',           'shape'),
        ],
        '_type_switcher': {
            None:       '#2# ',
            'var':      '#2# *',
            'return':   '#2#',
        },
        '_adapter_type_switcher': {
            None:       '#2#_data *',
            'var':      '#2# *',
            'return':   '#2#',
        }
    },
    # void type
    {
        'identifiers': [
            (None,     'void'),
        ],
        '_type_switcher': {
            'return':   '#2#',
        },
        '_adapter_type_switcher': {
            'return':   '#2#',
        }
    },
    # pointer types
    {
        'identifiers': [
            ('pointer',         'void *'),
            ('freenotifier',    'free_notifier '),
            ('animationframe',  'animation_frame'),
        ],
        '_type_switcher': {
            None:       '#2# ',
            'var':      '#2# *',
            'out':      '#2# ',
            'return':   '#2#',
        },
        '_adapter_type_switcher': {
            None:       'void *',
            'return':   '#2#',
        }
    },
    # string types
    {
        'identifiers': [
            ('string',          'char *'),
        ],
        '_type_switcher': {
            None:       'const #2#',
            'var':      '#2#',
            'out':      '#2#',
            'return':   '#2#',
        },
        '_adapter_type_switcher': {
            None:       'const #2#',
            'return':   '#2#',
        }
    },
    # array types
    {
        'identifiers': [
            ('triangle',        'triangle '),
            ('linesarray',     'lines_array '),
            ('stringarray',     'string_array '),
            ('point2darray',    'point2d *'),
            ('longintarray',    'int32_t *'),
            ('trianglearray',   'triangle_array '),
            ('bitmaparray',     'bitmap *'),
            ('singlearray',     'float *'),
            ('pointer[0..n - 1]',   'void *'),
        ],
        '_type_switcher': {
            None:       '#2#',
            'const':    'const #2#',
            'const-cpp':    'const #2#',
            'var':      '#2#',
            'return':   '#2#',
        },
        '_adapter_type_switcher': {
            None:       '#2#',
            'return':   '#2#',
        }
    },
    # 2d array types
    {
        'identifiers': [
            ('matrix2d',        'matrix2d '),
        ],
        '_type_switcher': {
            None:       '#2#',
            'const':    '#2#',
            'const-cpp':    '#2#',
            'var':      '#2#',
            'return':   '#2#',
        },
        '_adapter_type_switcher': {
            None:       '#2#',
            'return':   '#2#',
        }
    },
    # struct types
    {
        'identifiers': [
            ('directionangles',     'direction_angles'),
            ('vector',              'vector'),
            ('point2d',             'point2d'),
            ('linesegment',         'line_segment'),
            ('rectangle',           'rectangle'),
            ('circle',              'circle'),
            ('bitmapcell',          'bitmap_cell'),
        ],
        '_type_switcher': {
            None:       '#2# ', #used for _byval methods
            'const':    'const #2# *', #const parameters in Pascal are passed by reference
            'const-cpp':    'const #2# &', 
            'var':      '#2# *',
            'out':      '#2# *',
            'return':   '#2#',
        },
        '_adapter_type_switcher': {
            None:       '#2# ',
            'return':   '#2#',
        }
    },
    # enum types
    {
        'identifiers': [
            ('keycode',             'key_code'),
            ('resourcekind',        'resource_kind'),
            ('spritekind',          'sprite_kind'),
            ('spriteendingaction',  'sprite_ending_action'),
            ('fontalignment',       'font_alignment'),
            ('fontstyle',           'font_style'),
            ('mousebutton',         'mouse_button'),
            ('collisionside',       'collision_side'),
            ('collisiontestkind',   'collision_test_kind'),
            ('shapekind',           'shape_kind'),
        ],
        '_type_switcher': {
            None:       '#2# ',
            'var':      '#2# *',
            'return':   '#2#',
        },
        '_adapter_type_switcher': {
            None:       '#2# ',
            'return':   '#2#',
        }
    },
    # types for mapping structs and typedefs to Pascal
    {
        'identifiers': [
            ('longint[0..n - 1]',                       'int32_t *'),
            ('longint[0..n - 1][0..n - 1][0..n - 1]',   'int32_t *'),
            ('string[0..n - 1]',                        'char **'),
            ('single[0..n - 1]',                        'float *%s'),
            ('point2d[0..n - 1]',                       'point2d *'),
            ('animationframe[0..n - 1]',                'animation_frame *'),
            ('boolean[0..n - 1][0..n - 1]',             'bool *'),
            ('boolean[0..n - 1]',                       'bool *'),
            ('rectangle[0..n - 1]',                     'rectangle *'),
            ('shape[0..n - 1]',                         'shape *'),
            ('bitmap[0..n - 1]',                        'bitmap *'),
            ('directionangles[0..n - 1]',               'direction_angles *'),
            ('dirstatedata[0..n - 1][0..n - 1]',        'dir_state_data *'),
            ('linesegment[0..n - 1]',                   'line_segment *'),
            ('triangle[0..n - 1]',                      'triangle *'),
            
            ('point2d[0..2]',                           'point2d %s[3]'),
            ('single[0..2][0..2]',                      'float %s[3][3]'),
            
            ('pttf_font',                               'void *'),
            ('pmix_chunk',                              'void *'),
            ('pmix_music',                              'void *'),
            ('psdl_surface',                            'void *'),
            ('shapedrawingfn',                          'void *'),
            
            ('namedindexcollection',                    'named_index_collection ',),
            # ('animationframe',                          'animation_frame '),
        ],
        '_type_switcher': {},
        '_adapter_type_switcher': {
            None:       '#2#',
        }
    }
]

# table stores the operators that are different in Pacal and C
# if no match is found in the table then the operator is unchanged
_operator_conversion_table = {
    ':=': '=', 
    '=' : '==',
    '<>' : '!=',
    'and' : '&&',
    'or' : '||'
    }                     



#------------------

def _add_to_dict(into_dict, details_dict, ident_tupple):
    #find all the part
    for key,val in details_dict.iteritems():
        to_ins = val
        for idx,part in enumerate(ident_tupple):
            to_ins = to_ins.replace('#%d#' % (idx + 1), ident_tupple[idx] if not ident_tupple[idx] is None else '')
        # print 'inserting -> ', key, ':', to_ins
        
        if  ident_tupple[0] in into_dict[key]:
            print 'ERROR: Adding into type dictionary : ', into_dict[key][ident_tupple[0]]
            print 'key: ', key, 'type', ident_tupple[0]
            assert False
        
        into_dict[key][ident_tupple[0]] = to_ins

def build_type_dictionary(type_dictionary_creation_data, dicts):
    """Builds the conversion dictionary."""
    my_keys = dicts.keys()
    
    for type_mapping in type_dictionary_creation_data:
        #print type_mapping
        # Process each type in this type mapping
        for identifier_tupple in type_mapping['identifiers']:
            for a_key in my_keys:
                _add_to_dict(dicts[a_key], type_mapping[a_key], identifier_tupple)

#------------------

def main():
    build_type_dictionary(_type_dictionary_creation_data, _type_dicts)   


main()