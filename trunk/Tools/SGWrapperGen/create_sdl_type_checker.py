import logging
import sys
import re


#Get the Size of the type
#Get the offset of fields from a record

doesNotNeedLastEnd = False
inCaseStatement = False
pascalProcedures = []

# ----- Enum Code -----    
def _is_current_line_enum(line):
    if re.search("^[^:]*\s*=\s*\([^(]", line): #look for standard enum
        return True
    return False

def enumEnded(line):
    if re.search("\s*\);\s*", line): #look for standard enum
        return True
    return False

def OutputCheckPascalEnum(enum):  
    #
    # From C:
    # Passes in ptr to [size,val0,val1,..., valn]
    #
    fn_prototype = """
   procedure PascalCheckEnum_%(type_name)s(ptr: IntPtr); cdecl; export;
   var
     _%(type_name)s : %(type_name)s;
     cptr: IntPtr;
   begin 
     cptr := ptr;
     if cptr^ <> sizeof(%(type_name)s) then WriteLn('Different size for %(type_name)s!');
     cptr += 1;
     
     for _%(type_name)s := Low(%(type_name)s) to High(%(type_name)s) do
     begin
        try
            if cptr^ <> LongInt(_%(type_name)s) then
                WriteLn('Values differ in %(type_name)s for ', _%(type_name)s, ' C Value: ', cptr^, ' Pas Value:', LongInt(_%(type_name)s));
            cptr += 1;
        except
        end;
     end;
   end;
    """
    global pascalProcredures
    pascalProcedures.append('   PascalCheckEnum_' + enum["type_name"] + ',')
    print fn_prototype % enum


def getPascalEnum(subEnumList):  
    fn_prototype = """
   procedure outputEnum_%(type_name)s();
   var
     _%(type_name)s : %(type_name)s;
   begin 
     WriteLn('Base Enum: %(type_name)s Size: ', sizeof(%(type_name)s));
     for _%(type_name)s := Low(%(type_name)s) to High(%(type_name)s) do
     begin
        try
            WriteLn(_%(type_name)s, ' Value: ', LongInt(_%(type_name)s));
        except
        end;
     end;
   end;
    """
    global pascalProcredures
    pascalProcedures.append('   outputEnum_' + subEnumList['type_name'] + '();')
    print fn_prototype % subEnumList
    
def getAllPascalEnum(enumList):
    for i in range(0, len(enumList)):     
        # getPascalEnum(enumList[i])
        OutputCheckPascalEnum(enumList[i])

# ----- Records and Fields Code -----

def _is_current_line_record(line):
    if re.search("\s(record)\s", line): #look for standard enum
        return True
    return False
    
def recordEnded(line):
    if re.search("\s*\end;\s*", line): #look for standard enum
        return True
    return False

def getInnerPackedRecord(line, fieldList, typeList, innerPackedRecordCount, innerPackedRecord):  
    global inCaseStatement
    tmpPrefix = ''
    for i in range(0, len(innerPackedRecord)):
        tmpPrefix += innerPackedRecord[i] + '.'
    if not inCaseStatement and innerPackedRecordCount is 0:
        newPackedRecord = _type_innerRecord_on_line(line)
        fieldList.append(tmpPrefix + newPackedRecord)
        typeList.append('==Packed Record==')
        innerPackedRecord.append(newPackedRecord)
    else:
        result = re.split("\(", line)
        newPackedRecord = _type_innerRecord_on_line(result[1])
        fieldList.append(tmpPrefix + newPackedRecord)
        typeList.append('==Packed Record==')
        innerPackedRecord.append(newPackedRecord)
    innerPackedRecordCount = innerPackedRecordCount + 1
    return innerPackedRecordCount

def solveRecordCaseParse(line, fieldList, typeList, innerPackedRecord):
    global inCaseStatement
    result = line.replace('\(', '')
    result = result.replace('\)', '')
    result = result.replace(';', '')
    result = result.replace(' ', '')
    result = re.split(':', result)
    identifier = re.split("\(", result[1])[1]
    typ = re.split("\)", result[2])[0]
    
    tmpPrefix = ''
    for i in range(0, len(innerPackedRecord)):
        tmpPrefix += innerPackedRecord[i] + '.'
    
    fieldList.append(tmpPrefix + identifier)
    typeList.append(typ)

def getRecordData(line, fieldList, typeList, innerPackedRecordCount, innerPackedRecord):
    global inCaseStatement
    
    if (('case' in line) and ('of' in line)):
        inCaseStatement = True
        return innerPackedRecordCount
    elif 'end;' in line:
        innerPackedRecordCount = innerPackedRecordCount - 1
        del innerPackedRecord[innerPackedRecordCount + 1]
        return innerPackedRecordCount
    if (('packed ' in line) and ('record' in line)):
        return getInnerPackedRecord(line, fieldList, typeList, innerPackedRecordCount, innerPackedRecord)
    elif 'end );' in line:
        innerPackedRecordCount = innerPackedRecordCount - 1
        del innerPackedRecord[innerPackedRecordCount + 1]
        return innerPackedRecordCount
    elif inCaseStatement and innerPackedRecordCount is 0:
        solveRecordCaseParse(line, fieldList, typeList, innerPackedRecord)
        return innerPackedRecordCount

    if not ':' in line:
        return innerPackedRecordCount
        
    result = re.split(":", line)
    result[0] = result[0].replace(' ', '')
    result[1] = result[1].replace(' ', '')
    
    if 'function' in result[1] or 'procedure' in result[1]:
        result[1] = result[len(result) - 1]
    result[1] = 'Pointer' #re.split(";", result[1])[0]    
    
    tmpPrefix = ''
    for i in range(0, len(innerPackedRecord)):
        tmpPrefix += innerPackedRecord[i] + '.'
        
    fieldList.append(tmpPrefix + result[0])
    typeList.append(result[1])
    #print fieldList[len(fieldList) - 1] + ' : ' + typeList[len(typeList) - 1]
    return innerPackedRecordCount
    

def createPascalSizeFn(fieldList, typeList):
    fn_prototype = """
    function PascalGetSizeOf%(type_name)s() : Cardinal;
    var
        _%(type_name)s: %(type_name)s;
    begin
        result := Cardinal(sizeof(_%(type_name)s));
    end;
    """
    print fn_prototype % { "type_name": fieldList[0] }

def getPascalRecord(fieldList, typeList):
    global pascalProcredures
    pascalProcedures.append('   PasOutputRecord_' + fieldList[0] + '();')
    print 'procedure PasOutputRecord_' + fieldList[0] + '();' 
    print 'var'
    print '  ' + '_' + fieldList[0] + ' : ' + fieldList[0] + ';'
    print '  offset: Cardinal = 0;' 
    print 'begin' 
    print '  WriteLn(\'--Current Record: ' + fieldList[0] + '\');'
    print '  WriteLn(\'Size: \' +  IntToStr(sizeof(' + fieldList[0] + ')) + \' Address: \' + HexStr(@' + '_' + fieldList[0] + '));'
    for i in range(1, len(fieldList)):
        print '  offset := Cardinal(@_' + fieldList[i] + ') - Cardinal(@_' + fieldList[0] + ');'
        if '==Packed Record==' in typeList[i]:
            print '  WriteLn(\' ----Inner Record: ' + fieldList[i] + '  Size: \' +  IntToStr(sizeof(' + fieldList[i] + ')) + \' Address: \' + HexStr(@' + '_' + fieldList[i] + "), ' Offset: ', offset" + ');'
        else:
            print '  WriteLn(\'' + fieldList[i] + ' Size: \' +  IntToStr(sizeof(' + typeList[i] + ')) + \' Address: \' + HexStr(@' + '_' + fieldList[i] + "), ' Offset: ', offset" + ');'
    print 'end;'
    print '\n'    
 
def getCRecord(fieldList, typeList):
    print 'void outputRecord_' + fieldList[0] + '();' 
    print '{'
    print '  ' + fieldList[0] + ' ' + '_' + fieldList[0] + ';'
    print '  printf(\'--Current Record: ' + fieldList[0] + '\');'
    print '  printf(\'Size: %d | Address: %p \', sizeof(' + fieldList[0]  + '), &_' + fieldList[0] + ');'
    for i in range(1, len(fieldList)):
        if '==Packed Record==' in typeList[i]:
            print '  printf(\' ----Inner Record: ' + fieldList[i] + '\' Size: %d | Address: %p \', sizeof(' + fieldList[i]  + '), &_' + fieldList[i] + ');'
        else:
            print '  printf(\'' + fieldList[i] + ' Size: %d | Address: %p \', sizeof(' + typeList[i]  + '), &_' + fieldList[i] + ');'
    print '}'
    print '\n'
         
def getAllRecord(fieldList, typeList, typ):
    if '-p' in typ:
        for i in range(0, len(fieldList)):     
            getPascalRecord(fieldList[i], typeList[i])
    elif '-c' in typ:
        print 'Outputting C Methods'
        for i in range(0, len(fieldList)):     
            getCRecord(fieldList[i], typeList[i])

# ----- Types Code -----
def getPascalType(types):
    global pascalProcredures
    pascalProcedures.append('   outputTypeSize();')
    print 'procedure outputTypeSize();'
    print 'begin'
    for i in range(0, len(types) - 1):
        print '  WriteLn(\'' + types[i] + ' Size :  \' + IntToStr(sizeof(' + types[i] + ')));'
    print 'end;'
    
def getCType(types):
    print 'void outputTypeSize()'
    print '{'
    for i in range(0, len(types) - 1):
        print '  print(\"' + types[i] + ' Size :  \" + sizeof(' + types[i] + '));'
    print '};'
    
# ----- Misc -----

def _type_name_on_line(line):
    result = re.match("\s*([A-Za-z_][A-Za-z0-9_]*)\s*=", line)
    return None if result is None else result.group(1)

def _type_innerRecord_on_line(line):
    result = re.match("\s*([A-Za-z_][A-Za-z0-9_]*)\s*:", line)
    return None if result is None else result.group(1)

def createPascalProgram(types, subEnumList, fieldList, typeList):
    # print 'program SDLTypeChecker;'
    # print 'uses SDL, SysUtils;'
    # print '\n'
    
    print 'library PascalSDLTypeChecker;'
    print 'uses SDL, SysUtils;'
    print 'type IntPtr = ^LongInt;'

  #  getPascalType(types)
    getAllPascalEnum(subEnumList)
  #  getAllRecord(fieldList, typeList, '-p')

    print 'exports'
    for i in range(0, len(pascalProcedures)):
        print '  ' + pascalProcedures[i]

    print 'end.'

   
def main():
    #Types
    types = []
    
    #Enum
    isEnum = 0
    currentEnumName = ''
    enumList = []
    enumValues = []
    
    #Records
    global inCaseStatement
    isRecord = 0
    recordFields = []
    recordTypes = []
    fieldCollection = []
    typeCollection = []
    innerPackedRecord = []
    innerPackedRecordCount = 0
   # inCaseStatement = False
    
    f = open('../../CoreSDK/libsrc/sdl.pas')
    
    while 1:
        line = f.readline()
        if not line: break
        if not isEnum and _is_current_line_enum(line):
            isEnum = 1
            currentEnumName = _type_name_on_line(line)
        elif isEnum and enumEnded(line):
            enumList.append({"type_name": currentEnumName, "values": enumValues})
            enumValues = []
            isEnum   = 0
        elif isEnum:
            result = re.match("\s*([A-Za-z_][A-Za-z0-9_]*)", line)
            if result:
                # print line, result.group(1)
                enumValues.append(result)
        elif _is_current_line_record(line) and not isRecord:
            packedRecord = _type_name_on_line(line)
            recordFields.append(packedRecord)
            recordTypes.append('==Packed Record==')
            innerPackedRecord.append(packedRecord)
            isRecord = 1    
        elif isRecord and recordEnded(line) and not inCaseStatement:
            isRecord = 0
            fieldCollection.append(recordFields)
            typeCollection.append(recordTypes)
            recordFields = []
            recordTypes = []
            innerPackedRecord = []
            inCaseStatement = False
            innerPackedRecordCount = 0
        elif isRecord:
            innerPackedRecordCount = getRecordData(line, recordFields, recordTypes, innerPackedRecordCount, innerPackedRecord)
        
        if (innerPackedRecordCount < 0):
            isRecord = 0
            fieldCollection.append(recordFields)
            typeCollection.append(recordTypes)
            recordFields = []
            recordTypes = []
            innerPackedRecord = []
            inCaseStatement = False
            innerPackedRecordCount = 0
            
      #  elif isEnum:
      #      getSubEnum(line, enumList)
        line = _type_name_on_line(line)
        if line is not None:
            #print line
            types.append(line)
     
    createPascalProgram(types, enumList, fieldCollection, typeCollection)
    #getAllRecord(fieldCollection, typeCollection, '-c') 
    #getAllEnum(enumList)     
         
    #getPascalType(types)
    #getCType(types)


if __name__ == '__main__':
    main()


