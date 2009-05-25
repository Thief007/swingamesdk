'''Read a .pas file, extract comments, names, parameters and types suitable
for conversion to C# or Python writers. 

Looks for function/procedure blocks and any a preceding {...} comment block 
(which is used for documentation and parameters detail assistance).

There must be no lines between the preceding comment block and the code block.

The module should be imported and used by writers (pas_py_writer.py) and the
method ReadPasFile(filename) called, which will return a list of dictionaries,
one for each function or procedure that has a "cdecl; export;" at the end of 
its signature.

See ReadPasFile() for details of the format of data it will return.

If this module is run directly as main, it will load either the default 
sgsdk.pas file or a filename provided via sys.argv, and run in debug mode with 
lots of details printed about the processing.

'''
DEBUG = False


def _extract_blocks(pas_lines):
    '''Return a list extracted code block details from the provided
    lines (a list of strings). 
    
    Each block is a dictionary with the following "raw" details stored under
    each key name (preceded with an '_' to denote their raw quality). 
    
     - '_doc': [], # entire documentation comment (list of str)
     - '_sig': str, # entire signature as a single line (str)
     - '_code': [], # entire code block for the method

    '''
    in_method = False
    in_sig = False
    in_comment = False
    current_sig = ''
    block = None
    result = [] # list of signature blocks...
    # consider each line of the file
    for i, line in enumerate(pas_lines):
        # leave this loop if we're up to the exports section
        if line.find('exports') == 0:  
            break
        line = line.strip() # remove whitespace
        
        # basic comment block detection
        if line.find('{') >= 0: 
            in_comment = True
        if line.find('}') >= 0: 
            in_comment = False
        
        # start of a method?
        if not in_comment:
            # We are looking for "function" or "procedure"
            if line.find('function') == 0 or line.find('procedure') == 0:
                in_method = in_sig = True
                block = {}
                block['_start_at'] = i
                # go back and find any preceding comment block
                if pas_lines[i-1].find('}') >= 0:
                    start_line = i-1
                    while  pas_lines[start_line].find('{') < 0:
                        start_line -= 1
                    block['_doc'] = pas_lines[start_line:i]
                # there is no block comment
                else: 
                    block['_doc'] = None
            # end of a signature?
            if in_method:
                if in_sig:
                    current_sig += line
                    if line.find(')') >= 0:
                        #if line.find('cdecl; export;') >= 0:
                        block['_sig'] = current_sig
                        in_sig = False
                        current_sig = ''
                        block['_code'] = [] #list
                        stack = 0
                else:
                    block['_code'].append(line)
                    
                    if line.find('begin') == 0:
                        stack += 1
                    elif line.find('end;') == 0:
                        stack -= 1
                        if stack == 0 and len(block['_code']) > 0:
                            in_method = False
                            block['_stop_at'] = i
                            result.append(block)
                            
    # debug details to show?
    if DEBUG:
        for block in result:
            print block['_sig']
    return result

def _process_signature(sig, pas_types):
    '''Break up a signature string into the relevant pieces we want to know 
    about. Returns a dictionary with the extracted details.
    
    'name': str, # the name of the method 
    'type': str, # either 'function' or 'procedure'
    'params': [], # list of tuples for each parameter (name, type, modifier)
    'return': str, # the Type of the return value (if applicable)
     
    '''
    sig = sig.replace('  ',' ') # consistent spaces
    sig = sig.replace('; cdecl; export;','') #  remove unwanted bits
    #Extract method parameters
    #function FnName(n1: T1; [out] n2, n3: T2): T3; cdecl; export;
    pos0 = sig.find(' ') 
    pos1 = sig.find('(')  
    pos2 = sig.find(')')
    name = sig[pos0:pos1].strip()
    param_str = sig[pos1+1:pos2].strip()
    retn = 'None'
    sig_type = 'procedure' #default
    # get the return type for a function
    if sig.find('function') == 0:
        pos3 = sig.find(':', pos2)
        retn = sig[pos3+1:].strip()
        sig_type = 'function'
        pas_types.add(retn)
    # break up the arguments into types
    params = param_str.split(';')
    param_bits = []
    for param in params:
        param = param.strip()
        if param:
            pnames, ptype = param.split(':')
            modifier = None
            # check for "out" or "var" modifiers
            if param.find('out ') == 0: 
                modifier = 'out'
            elif param.find('var ') == 0: 
                modifier = 'var'
            if modifier:
                pnames = pnames.replace(modifier+' ', '') 
            # expand parameter name: types where compounded with ',' 
            for pname in pnames.split(','):
                ptype = ptype.strip()
                param_bits.append((pname.strip(), ptype, modifier))
                pas_types.add(ptype)
        
    if DEBUG: 
        print '-'*80
        print sig
        print 'type:\t', sig_type
        print 'name:\t', name
        print 'param str:\t', param_str
        print 'param bits:\t', param_bits
        print 'return:\t', retn
        
        
    # update the block with the signature details
    return {'name':name, 'type':sig_type, 'params':param_bits, 'return':retn}  
        
        
def _create_doc_dict(block):
    '''Create a default comment block stub (list of strings) based on the 
    extracted signature details, for documentation. 
    
    Returns a dict with keys for 
    'abtract': '', #@abstract 
    'params': [], # list of params as (name, type, modifier) #@param 
    'return': '', # type, if applicable, @return
    'since': 'VERSION', #@since
    'group': 'GROUP', #@group
    '''
    result = {}
    # senseless defaults - only make sense if updated by developer later 
    result['abstract'] = 'DESCRIPTION'
    result['since'] = 'VERSION'
    result['group'] = 'GROUP'
    # return details?
    if block['type'] == 'function':
        result['return'] = block['return']
    
    # If there are parameters, allocated each to a list of details 
    # Also treat modifiers for @array and @out to change the JavaDoc format
    result['params'] = []
    for param in block['params']:
        _name, _type, _mod = param
        # Set the modifier value if the param name is "result"
        if _name == 'result': 
            _mod = 'out'
        # Handle modifiers eg. @param @array bitmaps description...
        if _mod: 
            result['params'].append(['@'+_mod, _name])
        else:
            result['params'].append([_name])
    return result


def _doc_to_pas_str(doc):
    '''Converts the provided comment dict of details into a Pascal/JavaDoc 
    styled comment string (list of str) that can be saved to a file.
    { abstract/description ...
    
      @param ...
      @return ...
      @since 
      @group }
    '''
    result = []
    result.append('{ '+doc['abstract'])
    result.append('')
    for param in doc['params']:
        result.append('  @param '+' '.join(param))
    if 'return' in doc:
        result.append('  @return '+doc['return'])
    result.append('  @since '+doc['since'])
    result.append('  @group '+doc['group']+'  }') # end
    # create a single string with newlines between each provided item.        
    return result
                        
   
def _extract_doc_details(pas_str):
    '''Extract from a pas_str (list of strings) the standard doc details,
    and return both a doc dict of keys/values, but also the specific 
    parameters and their modifiers (which is useful for export writers). 
    '''
    result = { 'abstract': '', 'since': '', 'group': '' } # empty defaults
    
    # trim off the abstract lines until a blank line is found, or an @tag
    abstract = []
    idx = 0
    line = pas_str[idx].replace('{','').strip()
    while not (line.find('@') >= 0 or line == ''):
        abstract.append(line)
        idx += 1
        line = pas_str[idx]
    if abstract:
        result['abstract'] = ' '.join(abstract)        

    # look for param, return, since and group
    last = ''
    for line in pas_str[idx:]:
        line = line.replace('}','').strip()
        # @param @mod name description ...
        if line.find('@param ') > -1:
            last = '@param'
            # make sure there is somewhere to keep the parameters we find
            if '@param' not in result: 
                result['params'] = []
            # break into whitespace separated bits and check for @mod
            bits = line.split()
            if bits[1].find('@') == 0:
                #@mod name desc

                result['params'].append([bits[1][1:], bits[2], ' '.join(bits[2:])]) 
#                if bits[1] != '@out':
#                    print bits[1], bits
#                    print result
#
#                    die
            else:
                # name desc
                result['params'].append([None, bits[1], ' '.join(bits[2:])]) 
        # @return
        elif line.find('@return') >= 0: 
            result['return'] = line[8:]
            last = 'return' 
        # @since 
        elif line.find('@since') >= 0: 
            result['since'] = line[7:]
            last = 'since' 
        # @group 
        elif line.find('@group') >= 0: 
            result['group'] = line[7:]
            last = 'group'
        # additional lines to the last line?
        else:
            if line == '': 
                last = ''
            else:
                if last == 'param':
                    # last param, extend the description string
                    result[last][-1][-1] += ' ' + line 
                else:
                    print line
                    result[last] += ' ' + line   
    # done

    return result
            
def _test_docs(current_docs, auto_docs):
    '''Compare the current comments (if they were available) with the comment
    stub based on the signature alone.
    '''
    issues = []
    # check parameters (name, type, mod) and return details
    if auto_docs['params'] or current_docs['params']:
        if len(auto_docs['param']) != len(current_docs['params']):
            issues.append('param length mismatch')
        # check each param and its details
#        for param in comments
# TODO:
                        
def _process_block(block, pas_types):
    '''Extract details from the block signature, docs (comments) and code. 
    Modifies the block (dict) in-place with new extracted details.
    
    If doc/comments do not yet exists, creates a stub template for them to be 
    completed later. If doc comments do exists, but don't match the signature, 
    raises the issue. 
    
    Also performs some simple checks such as exception name
    strings matching method name.
    
    '''
    # start by processing the sig line
    sig = block['_sig']
    details = _process_signature(sig, pas_types)
    block['name'] = details['name']
    block['type'] = details['type']
    block['params'] = details['params']
    block['return'] = details['return']
    
    # Generate a comment block stub of 
    doc = _create_doc_dict(block)
    
    # If there is a comment block, extract from it the parameter and make 
    # sure they match the sig details for parameters and return values
    if block['_doc'] is not None:
        current_doc = _extract_doc_details(block['_doc'])
        #_test_comments(current_comment, comment)
        # apply comment extract param modifiers to the param details
        if 'params' in current_doc:
            old_params = block['params']
            for param in current_doc['params']:
                print param
                np_mod, np_name, np_doc = param
                if np_name == 'bitmaps':
                    die
                if np_mod is not None:
                    for i, (p_name, p_type, p_mod) in enumerate(old_params):
                        if p_name == np_name:
                            block['params'][i] = (p_name, p_type, np_mod)
                            break
#                    print block['params']
#                if param[0] == '@array':
#                    print param
#                    die
    else:
        # generate a new comment stub to be filled in later...
        block['newdoc'] = _doc_to_pas_str(doc)
        current_doc = None
        
    #
    if DEBUG:
        print 'auto doc:', doc
        print 'user doc:', current_doc
    

def ReadPasFile(filename):
    '''Processes the specified file and extracts details for each exported
    method. Returns a list of dictionaries (one each for block of code) as well
    as a set of observed data types which may be used by other modules to 
    assist in processing (converted) the extracted details into other export 
    formats.
    
    '''
    f = open(filename)
    pas_lines = f.readlines()
    f.close()
    
    # Each block of code (function/procedure) is stored as a list
    # Each block is a dict with
    # [Raw Details]
    # - '_doc': [], # entire documentation comment (list of str)
    # - '_sig': str, # entire signature as a single line (str)
    # - '_code': [], # entire code block for the method
    # [Extracted Details]
    # - 'name': str,
    # - 'type': ('function','procedure'), 
    # - 'desc': str, # description (only) lines, extracted from the doc
    # - 'param': [], # list of (name,type,extra) details for each param
    # - 'ver': str # version when introduced, extracted from doc
    
    # process file into manageable block chunks
    blocks = _extract_blocks(pas_lines)
    if DEBUG: print 'Total number of blocks: %d' % len(blocks)

    # isolate the exported functions from internal ones
    blocks = [ b for b in blocks if b['_sig'].find('cdecl; export;') > 0 ]
    if DEBUG: print 'Number of export blocks: %d' % len(blocks)
    
    # process each block, extract signature and comment details, do tests
    # store the types we discover (alert later if we don't know what to do about them)
    pas_types = set() #set of unique types
    for block in blocks:
        _process_block(block, pas_types)
    
    # DEBUG dump of details? 
    if DEBUG:
        print '-'*80
        print 'Done' 
    # return the list of signature strings
    return blocks, pas_types
           
           
def main(argv):
    # set the global debug flag so we get lots of pretty details...
    global DEBUG
    DEBUG = True
    
    # default file?
    filename = 'sgsdk.pas' if len(argv) != 2 else argv[1]
    
    # read the pas file.
    sigs, sg_types = ReadPasFile(filename)

#===============================================================================

if __name__ == '__main__':
    import sys
    main(sys.argv) 