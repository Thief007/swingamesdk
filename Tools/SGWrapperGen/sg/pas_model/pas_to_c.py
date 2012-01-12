from pas_parser_utils import logger

def convert_pas_to_c(pas_file):
    if pas_file._contains_kind == 'program':
        convert_to_c_program(pas_file._contents)


def convert_to_c_program(program):
   
    for part in program.block.contents:
        if part.part_kind() == 'variable declaration':
            _print_c_variable_decl(part)
        else:
            print part.part_kind()
            assert false
    _print_c_compound_statement(program.block.compound_statement)

def _print_c_variable_decl(variable_decl):
    for var in variable_decl._contents:
        print var.type, var.name, ';'
    
def _print_c_compound_statement(compound_statement):
    if compound_statement.block == None:
        print 'int main()'

    print "{"
    for statement in compound_statement.statements:
        if statement.kind == 'assignment':
            _print_c_assignment(statement)
        elif statement.kind == 'if statement':
            _print_c_if_statement(statement)
        else:
            logger.error("Error printing compound statement")
            assert(False)
    print "}"
        
def _print_c_assignment(statement):
     print statement.operand.name + " " + statement.operator.value + " " + _c_expression_to_str(statement.expression)

def _c_expression_to_str(expression):
    """
    The print_expression function returns an expression in a string format
    """
    result = ''
    for item in expression.contents:
        if item.kind == 'expression':
            result += '(' + _c_expression_to_str(item) + ')'
        elif item.kind == 'function_call':
            result += function_call_to_str(item)
        elif item.kind == 'variable':
            result += item.name
        else:
            result += item.value
    return result

def _c_function_call_to_str(function_call):
    """
    returns a string that describes a function call in Pascal
    """
    count = len(function_call.parameters)
    result = function_call.identifier + '('
    for parameter in function_call.parameters:
        result += expression_to_str(parameter)
        count -= 1
        if (count >= 1 ):
            result += ','
    result += ')'
    return result

def _print_c_if_statement(if_statement):
    """
    prints an if statement to the screen
    """
    if_expression = _c_expression_to_str(if_statement._expression)
    print 'if (' + if_expression + ')'
    _print_c_compound_statement(if_statement._compound_statement)
 

