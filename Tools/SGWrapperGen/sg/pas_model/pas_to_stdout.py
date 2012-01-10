
def print_pas_file(file):
    print file
    if file._contains_kind == 'program':
        print_program(file._contents)


def print_program(program):
    print 'program ', program._name, ';'
    
    for part in program._block._contents:
        if part.part_kind() == 'variable declaration':
            print_variable_decl(part)
        else:
            print part.part_kind()
            assert false

def print_variable_decl(variable_decl):
    print 'var'

    for var in variable_decl._contents:
        print var._name, ':', var._type, ';'
    