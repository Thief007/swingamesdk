from sg_parameter import SGParameter
from sg.sg_cache import logger, find_or_add_type


def add_local_var_for_param(to_method, for_parameter):
    '''
    Add a local variable to_method for_parameter. This is used for array
    processing in cases where the array data need to be marshalled between
    the program and the DLL and back.
    '''
    
    if not for_parameter.maps_result:
        local_var = SGParameter(for_parameter.name + '_temp')
    else: 
        local_var = SGParameter(for_parameter.name)
    
    local_var.data_type = for_parameter.data_type
    local_var.modifier = for_parameter.modifier
    local_var.maps_result = for_parameter.maps_result
    
    if for_parameter.data_type.is_struct and for_parameter.data_type.wraps_array:
        local_var.has_field = True
        local_var.field_name = for_parameter.data_type.fields[0].name
    
    to_method.local_vars.append(local_var)
    for_parameter.maps_to_temp = True
    return local_var


def add_local_var_for_result(to_method):
    result_param = to_method.method_called.params[-1]
    if not result_param.maps_result: #in case of returning var length array
        result_param = to_method.method_called.params[-2]
    
    assert result_param.maps_result
    
    #to_method.local_vars.append(result_param)
    var = add_local_var_for_param(to_method, result_param)
    to_method.args.append(var)

def add_length_params(to_method, len_str):
    for param in to_method.method_called.params:
        if param.is_length_param:
            #possibly need an extra local for this... if out
            if param.length_of.maps_result:
                # need to indicate the size of the returned array...
                if to_method.fixed_result_size > 0:
                    to_method.args.append(str(to_method.fixed_result_size))
                else:
                    to_method.args.append(len_str % param.length_of.name)
            elif param.modifier == 'out':
                var_name = param.length_of.local_var_name() + '_length'
                
                local_var = SGParameter(var_name)
                local_var.data_type = find_or_add_type('LongInt')
                local_var.modifier = param.modifier
                
                to_method.local_vars.append(local_var)
                to_method.args.append(var_name)
            elif not param.data_type.is_struct:
                to_method.args.append(len_str % param.length_of.name)
            else:
                to_method.args.append(len_str % param.length_of.local_var_name())

