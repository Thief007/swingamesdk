from converter_helper import load_templates, get_template


_val_switcher = {

}

_data_switcher = {

}

_type_switcher = {

}

_adapter_type_switcher = {

}

_operator_conversion_table = {

    }     

_type_dicts = {
    '_type_switcher': _type_switcher, 
    '_adapter_type_switcher': _adapter_type_switcher,
    }


extension = '.pas'

load_templates("pas_lib/", ".pas")
# templates must be added to this list otherwise they will be unavailable
variable_decl_template          = get_template("variable_declaration.pas")
variable_template               = get_template("variable.pas")
expression_template             = get_template("expression.pas")
string_template                 = get_template("string.pas")
function_call_template          = get_template("function_call.pas")
assignment_template             = get_template("assignment_statement.pas")
argument_template               = get_template("arguments.pas")
identifier_template             = get_template("identifier.pas")
compound_statement_template     = get_template("compound_statement.pas")
while_statement_template        = get_template("while.pas")
parameter_template              = get_template("parameters.pas")
if_statement_template           = get_template("if.pas")
block_template                  = get_template("block.pas")
function_declaration_template   = get_template("function_declaration.pas")
procedure_declaration_template  = get_template("procedure_declaration.pas")
program_template                = get_template("program.pas")
inner_expression_template       = get_template("inner_expression.pas")
comment_template                = get_template("comment.pas")
type_declaration_template       = get_template("type_declaration.pas")
record_template                 = get_template("record.pas")
record_field_template           = get_template("record_field.pas")
enum_value_template             = get_template("enum_values.pas")
enum_template                   = get_template("enum.pas")
unit_reference_template         = get_template("unit_reference.pas")
uses_clause_template            = get_template("uses_clause.pas")