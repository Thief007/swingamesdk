from pas_parser_utils import logger
def print_pas_file(file):
    print file
    if file._contains_kind == 'program':
        print_program(file._contents)


def print_program(program):
    print 'program ', program.name, ';'
    
    for part in program.block.contents:
        if part.part_kind() == 'variable declaration':
            print_variable_decl(part)
        else:
            print part.part_kind()
            assert false
    print_compound_statement(program.block.compound_statement)

def print_variable_decl(variable_decl):
    print 'var'

    for var in variable_decl._contents:
        print var.name, ':', var.type, ';'
    
def print_compound_statement(compound_statement):
    print "begin"
    for statement in compound_statement.statements:
        if statement.kind == 'assignment':
            print_assignment(statement)
        else:
            logger.error("Error printing compound statement")
            assert(False)
    print "end"
        
def print_assignment(statement):
     print statement.operand + " " + statement.operator + " " + print_expression(statement.expression)

def print_expression(expression):
    """
    The print_expression function returns an expression in a string format
    """
    return str(expression.contents)