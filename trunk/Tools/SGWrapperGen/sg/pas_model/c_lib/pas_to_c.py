from pascal_parser.pas_parser_utils import logger
from pas_type_switcher import convert_type, convert_operator
from converted_file import ConvertedFile
from pas_name_converter import lower_name, upper_name
import string
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
        c_program.append('#include "swingame.h"')
        c_program.extend(_convert_program(file.contents))
    return ConvertedFile(file._filename, c_program)

def _convert_program(program):  
    program_contents = list()
    program_contents.extend(_convert_block(program.block))    
    return program_contents

def _convert_block(block, isFunction=False):
    contents = list()  

    for part in block.contents:
        if part.kind == 'variable_declaration':
            contents.extend(_convert_variable_list(part.variables))
        elif part.kind == 'function':
            contents.extend(_convert_function(part))
        elif part.kind == 'uses_clause':
            pass
        else:
            logger.error(" Unidentified program part : %s", part.kind)
            assert False

    # C doesn't have a begin..end. part in the program -> this is replaced by a main() function
    if (block.parent != None):
        contents.extend(_convert_compound_statement(block.compound_statement, isFunction))

    return contents

def _convert_variable_list(variables, areParams=False):
    result = list()
    count = len(variables)
    currentVariable = ''
    for identifier in variables:
        c_type = convert_type(c_lib._type_switcher, variables[identifier].type, variables[identifier].modifier)
        var_declaration = c_lib.get_template('variable_declaration.c')
        var_declaration = var_declaration % { "type": c_type, "uname_lower" : lower_name(identifier) }
        result.append(var_declaration)
    return result

def _convert_function(function):
    result = list()

    if (lower_name(function.name) == 'main'): 
        return_type = 'int'
    else:
        return_type = convert_type(c_lib._type_switcher, function.return_type)

    parameter_declaration = ''
    parameters = _convert_variable_list(function.parameters, areParams=True)
    count = len(function.parameters)
    parameter_declaration += '('
    for parameter in parameters:
        parameter_declaration += parameter
        count -= 1
        if (count > 1 ):
            parameter_declaration += ','

    parameter_declaration += ')'
    result.append(return_type + ' ' + lower_name(function.name) + parameter_declaration)

    result.append('{')
    if (lower_name(function.name) == 'main'):
        result.append('load_default_colors();')
    if (function.isFunction) :
        result.append(convert_type(c_lib._type_switcher, function.return_type) + 'result;')
    result.extend(_convert_block(function.block, function.isFunction))  # return result will be added if isFunction
    result.append('}')
    return result

def _convert_statement(statement):
    result = list()
    if statement.kind == 'compound statement':
        result.extend(_convert_compound_statement(statement))
    elif statement.kind == 'if statement':
        result.extend(_convert_if_statement(statement))
    elif statement.kind == 'while statement':
        result.extend(_convert_while_statement(statement))
    elif statement.kind == 'repeat statement':
        result.extend(_convert_repeat_statement(statement))

    elif statement.kind == 'assignment':
        result.append(_convert_assignment_statement(statement))
    elif statement.kind == 'function_call':
        result.append(_convert_procedure_call_statement(statement))

    else:
        logger.error('Unidentified statement: ' + statement.kind)
        assert False
    return result
    
def _convert_compound_statement(compound_statement, inFunction=False):
    result = list()
    for statement in compound_statement.statements:
        result.extend(_convert_statement(statement))
    if (inFunction):
        result.append('return result;')
    return result
        
def _convert_repeat_statement(repeat_statement):
    from pascal_parser.types.pas_operator import PascalOperator

    result = list()
    # prepend C not operator
    repeat_statement.expression.contents.reverse()
    repeat_statement.expression.contents.append(PascalOperator('!'))
    repeat_statement.expression.contents.reverse()

    result.append('do')
    result.append('{')
    for statement in repeat_statement.statements:
        result.extend(_convert_statement(statement))
    result.append('} while (' + _expression_to_str(repeat_statement.expression) + ');')
    return result


def _convert_assignment_statement(statement):
     return lower_name(statement.operand.name) + " " + convert_operator(c_lib._operator_conversion_table, statement.operator) + " " + _expression_to_str(statement.expression) + ';'

def _convert_if_statement(if_statement):
    """
    prints an if statement to the screen
    """
    result = list()
    if_expression = _expression_to_str(if_statement.expression)
    result.append( 'if (' + if_expression + ' )')
    result.append('{')
    result.extend(_convert_statement(if_statement.statement))
    result.append('}')
    return result

def _convert_while_statement(while_statement):
    result = list()
    expression = _expression_to_str(while_statement.expression)
    result.append('while (' + expression + ' )')
    result.append('{')
    result.extend(_convert_statement(while_statement.statement))
    result.append('}')
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
            result += _variable_to_str(item)
        elif item.kind == 'string':
            result += '"' + item.value + '"'
        elif item.kind == 'operator':
            result += ' ' + convert_operator(c_lib._operator_conversion_table, item) + ' '
        else:
            result += item.value
    return result

def _convert_procedure_call_statement(statement):
    return _function_call_to_str(statement)

def _variable_to_str(var):
    from pascal_parser.types.pas_type_cache import find_or_add_type
    if var.type is find_or_add_type('enumeration'):     # if the type is an enumeration type...
        return upper_name(var.name)
    elif var.type is find_or_add_type('color'):
        return var.name # assuming input is PascalCase
    else:
        return lower_name(var.name)

def _function_call_to_str(function_call, inExpr = False):
    """
    returns a string that describes a function call in C
    """
    count = len(function_call.parameters)
    result = lower_name(function_call.identifier) + '('
    for parameter in function_call.parameters:
        result += _expression_to_str(parameter)
        count -= 1
        if (count >= 1 ):
            result += ','
    result += ')'

    if (not inExpr):
        result += ';'

    return result


