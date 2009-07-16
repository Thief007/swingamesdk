from sg_parameter import SGParameter
from sg.sg_cache import logger, find_or_add_type


def std_type_visitor(the_dict, the_type, modifier = None):
    '''
    switch types for the SwinGame library.
    
    Params:
     - the_dict:    The dictionary with the type changes with modifier keys
     - the_type:    The type being checked
     - modifier:    The modifier for the type
    '''
    key = the_type.name.lower() if the_type != None else None
    
    if modifier == 'result': modifier = 'return'
    
    if key not in the_dict[modifier]:
        logger.error('WRAPPER   : Error changing model type %s - %s', modifier, the_type)
        assert False
    return the_dict[modifier][key]



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
        local_var.is_returned = True
    
    local_var.data_type = for_parameter.data_type
    local_var.modifier = for_parameter.modifier
    local_var.maps_result = for_parameter.maps_result
    local_var.local_for = for_parameter
    local_var.pass_through = for_parameter.pass_through
    
    if for_parameter.data_type.is_struct and for_parameter.data_type.wraps_array:
        local_var.has_field = True
        local_var.field_name = for_parameter.data_type.fields[0].name
    
    to_method.local_vars.append(local_var)
    for_parameter.maps_to_temp = True
    return local_var


def add_local_var_for_result(to_method):
    '''
    Result parameters require two local variables. One that is
    the same type as the called value, and one that is the smae
    as the language type.
    '''
    
    if to_method.method_called.was_function:
        result_param = to_method.method_called.params[-1]
        if not result_param.maps_result: #in case of returning var length array
            result_param = to_method.method_called.params[-2]
    else:
        #skip void functions...
        if to_method.return_type is None: return
        
        result_param = SGParameter('result')
        result_param.maps_result = True
        result_param.pass_through = True
        result_param.modifier = 'result'        
        result_param.data_type = to_method.return_type
    
    assert result_param.maps_result
    
    #Add the local variable ...
    var = add_local_var_for_param(to_method, result_param)
    
    if to_method.method_called.was_function:
        to_method.args.append(var) # and pass it as an additional argument

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
                local_var.local_for = param
                local_var.data_type = find_or_add_type('LongInt')
                local_var.modifier = param.modifier
                
                to_method.local_vars.append(local_var)
                to_method.args.append(var_name)
            elif not param.data_type.is_struct:
                to_method.args.append(len_str % param.length_of.name)
            else:
                to_method.args.append(len_str % param.length_of.local_var_name())

def add_local_var_processing(the_method, details, local_variable_switcher):
    if len(the_method.local_vars) > 0:
        temp = ''
        temp_process_params = details['pre_call']
        temp_process_result = details['post_call']
        
        #process all local variables
        for local_var in the_method.local_vars:
            #Setup dictionary:
            #  - %(var)s = local variable name
            #  - %(param)s = parameter name
            #  - %(size)s = size expression (literal or call)
            
            lower_type = local_var.data_type.name.lower()
            
            var_details = dict()
            var_details['var'] = local_var.name
            var_details['param'] = local_var.local_for.name if local_var.local_for != None else None
            var_details['modifier'] = 'const ' if local_var.modifier == 'const' else ''
            
            if local_var.pass_through:
                #just passes out return value:
                #TODO: fix these...
                temp += '%(return_type)s result;\n    ' % details
                temp_process_result = temp_process_result + '\n    return %s;' % local_var.name;
                continue
            elif lower_type == 'string':
                if local_var.modifier in ['var', 'const', None]:
                    var_details['size'] = local_variable_switcher['length-of'][lower_type] % var_details
                else: #out or return
                    var_details['size'] = '2048'
            elif local_var.maps_result and isinstance(local_var, SGParameter):
                #Size for fixed_result_size is already known
                if the_method.fixed_result_size > 0:
                    var_details['size'] = the_method.fixed_result_size
                elif the_method.length_call != None:
                    #TODO: change call creator to excluse return keyword
                    var_details['size'] = details['length_call'].replace('return ', '')
                else:
                    # print 'No size for...', local_var
                    var_details['size'] = ''
            elif local_var.modifier in ['var', 'const', None] and local_var.data_type.array_wrapper:
                #Read the length of an array from its length... - variable length only
                if lower_type not in local_variable_switcher['length-of']:
                    logger.error('Missing "length-of" for type %s in method %s - for %s', lower_type, the_method.name, var_details['param'])
                    assert false
                var_details['size'] = local_variable_switcher['length-of'][lower_type] % var_details
            else:
                # print 'No size for...', local_var
                var_details['size'] = ''
            
            #Add code to declare local variable
            temp += local_variable_switcher['declare'][lower_type] % var_details
            
            #Add code to initialise local variable
            if local_var.modifier in ['var', 'const', None]:
                temp_process_params += local_variable_switcher['initialise-param'][lower_type] % var_details
            # else:
            #     print 'No pre-processing for ', local_var
            
            #Add code to process out results
            if local_var.modifier in ['out', 'var']:
                temp_process_result = local_variable_switcher['process-out-param'][lower_type] % var_details + temp_process_result
            elif local_var.maps_result:
                temp_process_result = local_variable_switcher['process-result'][lower_type] % var_details + temp_process_result
            
        # if the_method.has_out_params and not the_method.was_function:
        #     #need to store result... add a new local variable...
        #     # var_details = dict()
        #     # var_details['var'] = 'temp_result'
        #     # var_details['param'] = local_var.local_for.name if local_var.local_for != None else None
        #     # var_details['modifier'] = 'const ' if local_var.modifier == 'const' else ''
        #     # 
        #     # temp += local_variable_switcher['declare'][lower_type] % var_details
        #     print 'here', the_method.name
            
        details['vars'] = temp
        details['post_call'] = temp_process_result
        details['pre_call'] = temp_process_params
        
    else:
        details['vars'] = ''
