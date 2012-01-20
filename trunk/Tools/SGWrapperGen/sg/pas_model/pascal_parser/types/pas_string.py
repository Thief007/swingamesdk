class PascalString(object):
    """
    Describes a string
    """
    
    def __init__(self, value):
        self._value = value
                
    @property
    def kind(self):
        return 'string'

    @property
    def value(self):
        return self._value

    def __str__(self):
        return str(value)

    def to_code(self, indentation=0):
        import converter_helper
        for (name, module) in converter_helper.converters.items():
            variables = ""
            self._code[name + '_string'] = (identation * '    ') +  (module.string_template % self._value)
