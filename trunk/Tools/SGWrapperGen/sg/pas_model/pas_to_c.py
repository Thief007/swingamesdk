
def convert_pas_to_c(pas_file):
    if pas_file._contains_kind == 'program':
        convert_to_c_program(pas_file._contents)


def convert_to_c_program(program):
    print '#include "SwinGame.h'
    
    for part in program._block._contents:
        if part.part_kind() == 'variable declaration':
            print_variable_decl(part)
        else:
            print part.part_kind()
            assert false

def print_variable_decl(variable_decl):
    for var in variable_decl._contents:
        print var._type, var._name, ';'
    