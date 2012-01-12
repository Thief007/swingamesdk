from pas_parser_utils import logger

def convert_pas_to_c(pas_file):
    if pas_file._contains_kind == 'program':
        convert_to_c_program(pas_file._contents)


def convert_to_c_program(program):
    print '#include "SwinGame.h'
    
    for part in program._block._contents:
        if part.part_kind() == 'variable declaration':
            print_c_variable_decl(part)
        else:
            print part.part_kind()
            assert false
    print_c_compound_statement(program.block.compound_statement)

def print_c_variable_decl(variable_decl):
    for var in variable_decl._contents:
        print var._type, var._name, ';'
    
def print_c_compound_statement(compound_statement):
    print "{"
    for statement in compound_statement.statements:
        if statement.kind == 'assignment':
            print_c_assignment(statement)
        else:
            logger.error("Error printing compound statement")
            assert(False)
    print "}"
        
def print_c_assignment(statement):
     print statement.operand + " " + statement.operator + " " + print_c_expression(statement.expression)

def print_c_expression(expression):
    """
    The print_expression function returns an expression in a string format
    """
    return str(expression.contents)