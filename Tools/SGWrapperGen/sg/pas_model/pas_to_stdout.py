from pascal_parser.pas_parser_utils import logger

def print_pas_file(file):
    print file
    if file._contains_kind == 'program':
        _print_program(file._contents)


def _print_program(program):
    print 'program ', program.name, ';'
    _print_block(program.block)

def _print_block(block):
    for part in block.contents:
        if part.kind == 'variable_declaration':
            _print_variable_decl(part)
        elif part.kind == 'function':
            _print_function(part)
        elif part.kind == 'uses_clause':
            print 'uses...'
        else:
            print part.kind
            assert False
    _print_compound_statement(block.compound_statement)


def  _print_function(part):
    print ('procedure ' if part.isFunction else 'function ') + part.name + '( '
    _print_variables(part.parameters)
    print ')'

def _print_variable_decl(variable_decl):
    print 'var'
    _print_variables(variable_decl.variables)

def _print_variables(variables):
    for identifier in variables:
        print variables[identifier].name, ':', variables[identifier].type.name, ';'

def _print_statement(statement):
    if statement.kind == 'compound statement':
        _print_compound_statement(statement)
    elif statement.kind == 'if statement':
        _print_if_statement(statement)
    elif statement.kind == 'while statement':
        _print_while_statement(statement)
    elif statement.kind == 'assignment':
        _print_assignment_statement(statement)
    elif statement.kind == 'function_call':
        _print_procedure_call_statement(statement)
    else:
        logger.error('Error printing statement: ' + statement.kind)
        assert False
    
def _print_compound_statement(compound_statement):
    print "begin"
    for statement in compound_statement.statements:
        _print_statement(statement)
    print 'end'
        
def _print_assignment_statement(statement):
     print statement.operand.name + " " + statement.operator.value + " " + _expression_to_str(statement.expression) + ";"

def _print_if_statement(if_statement):
    """
    prints an if statement to the screen
    """
    if_expression = _expression_to_str(if_statement.expression)
    print 'if ' + if_expression + ' then'
    _print_statement(if_statement.statement)

def _print_while_statement(while_statement):
    expression = _expression_to_str(while_statement.expression)
    print 'while ' + expression + ' do'
    _print_statement(while_statement.statement)

def _expression_to_str(expression):
    """
    The print_expression function returns an expression in a string format
    """
    result = ''
    for item in expression.contents:
        if item.kind == 'expression':
            result += '(' + _expression_to_str(item) + ')'
        elif item.kind == 'function_call':
            result += _function_call_to_str(item)
        elif item.kind == 'variable':
            result += item.name
        elif item.kind == 'string':
            result += "'" + item.value + "'"
        else:
            result += item.value
    return result

def _print_procedure_call_statement(statement):
    print _function_call_to_str(statement)

def _function_call_to_str(function_call):
    """
    returns a string that describes a function call in Pascal
    """
    count = len(function_call.parameters)
    result = function_call.identifier + '('
    for parameter in function_call.parameters:
        result += _expression_to_str(parameter)
        count -= 1
        if (count >= 1 ):
            result += ','
    result += ');'
    return result


