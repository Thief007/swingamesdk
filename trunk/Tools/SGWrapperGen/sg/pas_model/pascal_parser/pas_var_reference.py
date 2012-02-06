class PascalVariableReference(object):
    """
    Describes a reference to a variable in Pascal
    """
    
    def __init__(self, name, block, field = ''):
        from pas_parser import logger

        self._name = name
        self._var = block.resolve_variable(name)
        self._is_record = False
        if not (self._var.kind == 'enumeration value'):
            self._type = self._var.type
            if self._var is None:
                logger.error("Unable to resolve variable: %s", varName)
                assert False
            self._is_record = True if self._type.kind is 'record' else False
            self._field_reference = None
            if self._is_record and field != '':
                if self._type.has_field(field):
                    self._field_reference = field
                else:
                    logger.error("Variable %s does not contain field %s" % (self._name, field))
                    assert False
        """
        Stores the line of code for each of the language converters
            eg. code["c_lib"] returns the code to declare a variable in c
        """
        self._code = dict()     

    @property
    def code(self):
        return self._code

    @property
    def type(self):
        return self._type

    @property
    def name(self):
        return self._name

    @property
    def kind(self):
        return 'variable'

    @property
    def is_record(self):
        return (type.kind == 'record')

    def to_code(self):
        '''
            Creates a _code entry for each of the converter modules

        '''
        import converter_helper
        my_data = dict()

        if self._is_record and self._field_reference != None:
            self._code['pas_lib_reference'] = "%s.%s" % ( self._name, self._field_reference )
            self._code['c_lib_reference'] = "%s.%s" % ( converter_helper.lower_name(self._name), converter_helper.lower_name(self._field_reference ))
        else:
            self._code['pas_lib_reference'] = "%s" % ( self._name )
            self._code['c_lib_reference'] = "%s" % ( converter_helper.lower_name(self._name))