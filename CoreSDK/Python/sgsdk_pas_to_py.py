# convert the swingame sgsdk.pas dll library file to a nice set of python + ctypes friendly functions
# input = sgsdk.pas
# output = sgsdk.py
# Clinton Woodward, Feb 27 2008

# See __main__ section at the end of this file
from sgsdk_types import sgsdk_types

def ReadAndStripMethodsFromPasFile(filename):
    # open .pas file
    sg_pas = open(filename)

    #------------------------------------------------------------------------------
    # strip function/procedure header lines
    # create list of single-line signature for processing
    #------------------------------------------------------------------------------
    current_line = ''
    signatures = []
    method_count = 0
    in_method = False
    in_comment = False
    for line in sg_pas:
        line = line.strip() # remove whitespace
        # basic comment block detection
        if line.find('{') >= 0: in_comment = True
        if line.find('}') >= 0: in_comment = False
        # start of a method?
        if not in_comment:
            if line.find('function') == 0 or line.find('procedure') == 0:
                method_count += 1
                in_method = True
            # end of a method?
            if in_method:
                current_line += line
                if line.find(')') >= 0:
                    if line.find('cdecl; export;') >= 0:
                        signatures.append(current_line)
                    current_line = ''
                    in_method = False
    sg_pas.close()


    # # -- DEBUG DUMP --
    # print 'len',len(signatures) 
    # ends = len([ _ for _ in signatures if _.find(')') >= 0])
    # print 'ends', ends 
    # print 'starts', method_count

    return signatures


#------------------------------------------------------------------------------
# Process each signature to identify parameter names and types
# Use this to create objects with argument names and constraints later
#  - eg. function DistancePointToLine(x, y: Single; line: LineSegment): Single
#    args = x, y: Single; line: LineSegment
#    retn = Single
#  - in python with ctypes
#    DistancePointToLine = sgsdk.DistancePointToLine # function reference
#    DistancePointToLine.argtypes = [('x',c_float),
#                                    ('y',c_float),
#                                    ('line',LineSegment)]# argument types
#    DistancePointToLine.restype = c_float # return type single
#  - Note that LineSegment will also need to be an existing definition
#------------------------------------------------------------------------------

def ExtractTuplesAndTypesFromSignatures(signatures):
    sig_tuples = [] # tuples with the lot
    sig_names = [] # just the names
    # store the types we discover (alert later if we don't know what to do about them)
    sg_types = set() #set of unique types
                
    for line in signatures:
        line = line.replace('  ',' ') # consistent spaces
        line = line.replace('; cdecl; export;','') #  remove unwanted bits
        # method arguments
        pos0 = line.find(' ')
        pos1 = line.find('(')
        pos2 = line.find(')')
        name = line[pos0:pos1].strip()
        args = line[pos1+1:pos2].strip()
        retn = 'None'
        type = 'procedure' #default
        # get the return type for a function
        if line.find('function') == 0:
            pos3 = line.find(':',pos2)
            retn = line[pos3+1:].strip()
            type = 'function'
        sg_types.add(retn)
        # break up the arguments into types
        params = args.split(';')
        arg_bits = []
        for param in params:
            if param:
                varnames, vartype = param.split(':')
                vartype = vartype.strip()
                for varname in varnames.split(','):
                    arg_bits.append((varname.strip(),vartype))
                    sg_types.add(vartype)
        # # print method details (mainly for debug)
        # print '\t', name
        # print '\t', args
        # print '\t', arg_bits
        # print '\t', retn
        sig_names.append(name)
        sig_tuples.append({'name':name,
                           'type':type,
                           'args':arg_bits,
                           'retn':retn})
    return sig_tuples, sg_types 



def WriteSigNamesToFile(filename,sig_tuples):
    # extract just a list of names
    sig_names = [ sig['name'] for sig in sig_tuples ]
    sig_names.sort()
    f = open(filename,'w')
    f.write('# Detected Functions/Procedures Names\n')
    i = 0
    for name in names:
        f.write('%4d %s\n' % (i, name))
        i += 1
    f.close()

def WriteSigTypesToFile(filename,sig_type_set):
    f = open(filename, 'w')
    f.write('# Argument/Retun types found \n')
    for type in sg_types:
        f.write('%s\n' % type)   
    f.close()
# convert header lines to python equivalents

def args_to_str(args):
    str = []
    for arg in args:
        name,type = arg
        #str.append("('%s', %s)" % (name,type)) 
        str.append("%s" % type) 
    return ', '.join(str)

def CreateAndSaveSignatures(filename,sig_tuples):
    # assumes that the module global dictionary of sgsdk_types is ready!
    # sgsdk_types = {'sg_type': py_type,...
    for sig in sig_tuples:
        sig['retn'] = sgsdk_types[sig['retn']]
        new_args = []
        for arg in sig['args']:
            new_args.append((arg[0],sgsdk_types[arg[1]]))
        sig['args'] = new_args
    
#    DistancePointToLine = sgsdk.DistancePointToLine # function reference
#    DistancePointToLine.argtypes = [('x',c_float),
#                                    ('y',c_float),
#                                    ('line',LineSegment)]# argument types
#    DistancePointToLine.restype = c_float # return type single   
#    DistancePointToLine.errcheck = err_check

    f = open(filename,'w')
    header = '''
import sys
from ctypes import * 
from sgsdk_types import *

if sys.platform == "win32":
    sgsdk = CDLL("sgsdk\SGSDK.dll")   #to check
elif sys.platform == "darwin":
    sgsdk = CDLL("sgsdk/libSGSDK.dylib")
else: #assume linux
    sgsdk = CDLL("sgsdk/libSGSDK.so")   #to check

#
# This file has been generated by sgsdk_pas_to_py.py. Do not edit by hand! :)
#
class SGSDKException(Exception):
    pass

def _sg_errcheck(result, func, args):
    # ask the SGSDK if there was an error, raise exception if so...
    if ExceptionOccured() == sgTrue.value:
        msg = GetExceptionMessage()
        raise SGSDKException(msg)
    return result
        
def _decorate_function(func, name):
    # decorate with _sg_errcheck if needed for debug 
    if options['debug']:
        if name not in ('ExceptionOccured','GetExceptionMessage'):
            func.errcheck = _sg_errcheck

    
'''
    f.write(header)
    names = []
    for sig in sig_tuples:
        name, args, retn = sig['name'], sig['args'], sig['retn']
        f.write('%s = sgsdk.%s\n' % (name, name))
        f.write('%s.argtypes = [%s]\n' % (name, args_to_str(args)))
        f.write('%s.restype = %s\n' % (name, retn))
        f.write('_decorate_function(%s, "%s")\n' % (name, name))
        f.write('\n')
        names.append(name)
    # write the __all__ list
    f.write(list_to_80char_lines(names, '__all__ = '))
    f.close()
   

def list_to_80char_lines(list, start):
    ''' util function for pretty formatting '''
    list.sort()
    inset = ' '*(len(start) + 1) # plus '['
    current_line = start + '['     
    lines = []
    for item in list:
        item = "'" + item + "'"
        if len(current_line) + 3 + len(item) > 80: # inc. ", ..., " space
            lines.append(current_line)
            current_line = inset + item # new line
        else:
            if current_line != (start+'['):
                current_line += ", "+item # add to current line
            else:
                current_line += item
    lines.append(current_line)
    return ',\n'.join(lines)+']'
    
    
#------------------------------------------------------------------------------

if __name__ == '__main__':
    pas_filename = 'sgsdk.pas'
    py_filename = 'sgsdk.py'
    # get signatures
    signatures = ReadAndStripMethodsFromPasFile(pas_filename)
    # extract signatures as [(name,type,(argtypes),retype)]
    # keep a set of the types encountered
    sig_tuples, sig_types = ExtractTuplesAndTypesFromSignatures(signatures)
#    # save just the names?
#    WriteSigNamesToFile('sgsdk_names.txt',sig_tuples)
#    # save just the types?
#    WriteSigTypesToFile('sgsdk_types.txt',sig_types)
    
    # Create and save c_type function signatures
    CreateAndSaveSignatures(py_filename,sig_tuples)