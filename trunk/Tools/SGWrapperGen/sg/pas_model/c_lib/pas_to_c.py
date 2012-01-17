from pascal_parser.pas_parser_utils import logger
from pas_type_switcher import convert_type, convert_operator
from converted_file import ConvertedFile
import c_lib

# list.extend is used when a function returns a list
# list.append is used when a function returns a string

def convert_pas_to_c(file):
    """
    returns a list of strings in the c program
    """
    c_program = list()
    print file
    if file._contains_kind == 'program':
        c_program.append('#include <stdint.h>')
        c_program.append('#include <stdio.h>')
        c_program.extend(_convert_program(file.contents))
    return ConvertedFile(file._filename, c_program)

def _convert_program(program):  
    program_contents = list()
    program_contents.extend(_convert_block(program.block))    
    return program_contents

def _convert_block(block):
    contents = list()  
    for part in block.contents:
        if part.kind == 'variable declaration':
            contents.extend(_convert_variable_list(part.variables))
        elif part.kind == 'function':
            contents.extend(_convert_function(part))
        elif part.kind == 'uses clause':
            pass
        else:
            logger.error(" Unidentified program part : %s", part.kind)
            assert False

    # C doesn't have a begin..end. part in the program -> this is replaced by a main() function
    if (block.parent != None):
        contents.extend(_convert_compound_statement(block.compound_statement))
    return contents

def _convert_variable_list(variables, areParams=False):
    result = list()
    count = len(variables)
    currentVariable = ''
   
    for identifier in variables:
        currentVariable += convert_type(c_lib._type_switcher, variables[identifier].type) + identifier 
        if (count > 1 and areParams):
            currentVariable += ','
        else:
            if (not areParams):
                currentVariable += ';'
            result.append(currentVariable)
            currentVariable = ''
        count -= 1
    return result

def _convert_function(function):
    result = list()
    return_type = convert_type(c_lib._type_switcher, function.return_type)
    parameter_declaration = ''
    parameters = _convert_variable_list(function.parameters, areParams=True)
    count = len(function.parameters)
    parameter_declaration += '('
    for parameter in parameters:
        parameter_declaration += parameter
        count -= 1
        if (count >= 1 ):
            parameter_declaration += ','
    parameter_declaration += ')'
    result.append(return_type + ' ' + function.name + parameter_declaration)
    result.extend(_convert_block(function.block))
    return result

def _convert_statement(statement):
    result = list()
    if statement.kind == 'compound statement':
        result.extend(_convert_compound_statement(statement))
    elif statement.kind == 'if statement':
        result.extend(_convert_if_statement(statement))
    elif statement.kind == 'while statement':
        result.extend(_convert_while_statement(statement))
    elif statement.kind == 'assignment':
        result.append(_convert_assignment_statement(statement))
    elif statement.kind == 'function_call':
        result.append(_convert_procedure_call_statement(statement))
    else:
        logger.error('Unidentified statement: ' + statement.kind)
        assert False
    return result
    
def _convert_compound_statement(compound_statement):
    result = list()
    result.append("{")
    for statement in compound_statement.statements:
        result.extend(_convert_statement(statement))
    result.append('}')
    return result
        
def _convert_assignment_statement(statement):
     return statement.operand.name + " " + convert_operator(c_lib._operator_conversion_table, statement.operator) + " " + _expression_to_str(statement.expression) + ';'

def _convert_if_statement(if_statement):
    """
    prints an if statement to the screen
    """
    result = list()
    if_expression = _expression_to_str(if_statement.expression)
    result.append( 'if (' + if_expression + ' )')
    result.extend(_convert_statement(if_statement.statement))
    return result

def _convert_while_statement(while_statement):
    result = list()
    expression = _expression_to_str(while_statement.expression)
    result.append('while (' + expression + ' )')
    result.extend(_convert_statement(while_statement.statement))
    return result

def _expression_to_str(expression):
    """
    The print_expression function returns an expression in a string format
    """
    result = ''
    for item in expression.contents:
        if item.kind == 'expression':
            result += '(' + _expression_to_str(item) + ')'
        elif item.kind == 'function_call':
            result += _function_call_to_str(item, inExpr=True)
        elif item.kind == 'variable':
            result += item.name
        elif item.kind == 'string':
            result += '"' + item.value + '"'
        elif item.kind == 'operator':
            result += ' ' + convert_operator(c_lib._operator_conversion_table, item) + ' '
        else:
            result += item.value
    return result

def _convert_procedure_call_statement(statement):
    return _function_call_to_str(statement)

def _function_call_to_str(function_call, inExpr = False):
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
    result += ')'

    if (not inExpr):
        result += ';'

    return result


