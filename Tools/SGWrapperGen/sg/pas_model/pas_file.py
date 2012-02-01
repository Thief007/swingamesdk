from pascal_parser.tokeniser.pas_token_stream import SGTokenStream
from pascal_parser.tokeniser.pas_token_kind import TokenKind
from pascal_parser.pas_parser_utils import logger
from pascal_parser.pas_program import PascalProgram
from pascal_parser.pas_unit import PascalUnit
class PascalFile(object):
    """
    Describes a pascal source file.
    The PascalFile can be parsed.

    Can be specified to be virtual, a virtual file refers to a file that does not exist
    therefore no TokenStream is created for it, and so it cannot be parsed.
    The virtual file is used to create system libraries that contain the base types.
    """
    def __init__(self, path=None, is_virtual=False):
        import os
        from pascal_parser.tokeniser.pas_meta_comment import PascalMetaComment
        self._filename = ''
        self._name = ''
        self._contains_kind = None
        self._contents = None       # PascalProgram or PascalLibrary
        self._code = dict()
        self._is_parsed = False
        self._meta_comment = None
        self._is_virtual = is_virtual
        self._meta_comment = None
        self._tokens = None
        self._stream = None
        if not self._is_virtual:
            self._stream = SGTokenStream(path)
            self._meta_comment = PascalMetaComment(self._stream)
            self._meta_comment.process_meta_comments()
            self._filename = os.path.basename(path)
            # program?
            if self._stream.match_lookahead(TokenKind.Identifier, 'program'):
                self._contains_kind = 'program';
                self._contents = PascalProgram(self)
            # unit?
            elif self._stream.match_lookahead(TokenKind.Identifier, 'unit'):
                self._contains_kind = 'unit';
                self._contents = PascalUnit(self)   
            else:
                #logger
                logger.error("Error in program syntax: %s in %s", self._stream.next_token(), self._filename)
                assert False

            self._name = self._stream.lookahead(2)[1].value     # file's 'name' is after the kind of file is stated
        else:
            self._is_parsed = True

    @staticmethod
    def create_unit_from(name, variable_names = [()], type_names = [], function_names = []):
        result = PascalFile(None, is_virtual=True)
        result._name = name
        result._contents = PascalUnit.create_from(result, variable_names, type_names, function_names)
        result._contains_kind = 'unit'
        return result

    @property
    def name(self):
        return  self._name

    @property
    def is_parsed(self):
        return self._is_parsed

    @property
    def filename(self):
        return self._filename

    @property
    def code(self):
        return self._code

    @property
    def contents(self):
        return self._contents

    @property
    def contains_kind(self):
        return self._contains_kind

    @property
    def filename(self):
        return self._filename

    def resolve_unit_reference(self, reference):
        from pas_file_cache import get_unit_named
        logger.info("File:     Resolving Unit reference: %s", reference.name)
        unit = get_unit_named(reference.name)
        if (not unit.is_parsed):
            unit.parse()
        return unit

    def resolve_variable(self, var_name):
        from pas_file_cache import get_unit_named
        logger.info("File:     Resolving Variable: %s", var_name)
        for unit_reference in self._contents.uses:
            unit = unit_reference.points_to.contents
            for unit_name, unit_var in unit.variables.items():
                if unit_name.lower() == var_name.lower():
                    return unit_var
        # check the System unit...
        system = get_unit_named('System')
        if (system != None):
            for name, unit_var in system.contents.variables.items():
                if name.lower() == var_name.lower():
                    return unit_var
        logger.error("File:     Unable to resolve variable: %s", var_name)
        assert False

    def resolve_function_call(self, function):
        """
        Assumes the file has checked itself for the function before calling this
        """
        from pas_file_cache import get_unit_named
        logger.info("File:     Resolving Function call: %s", function.name)
        for unit_reference in self._contents.uses:
            unit = unit_reference.points_to.contents
            for unit_function in unit.functions:
                if unit_function.name.lower() == function.name.lower():
                    return unit_function
            for unit_declared_function in unit.function_declarations:
                if unit_declared_function.name.lower() == function.name.lower():
                    return unit_declared_function 
        # check the System unit...
        system = get_unit_named('System')
        if (system != None):
            for unit_function in system.contents.functions:
                if unit_function.name.lower() == function.name.lower():
                    return unit_function
        logger.error("File:     Unable to resolve function: %s", function.name)
        assert False

    def resolve_type(self, type):
        from pas_file_cache import get_unit_named
        logger.info("File:     Resolving Type: %s", type)
        for unit_reference in self._contents.uses:
            if unit_reference.points_to != None:
                unit = unit_reference.points_to.contents
                for unit_name, unit_type in unit.types.items():
                    if unit_name.lower() == type.lower():
                        return unit_type
        # check the System unit...
        system = get_unit_named('System')
        if (system != None):
            for name, unit_type in system.contents.types.items():
                if name.lower() == type.lower():
                    return unit_type
        logger.error("File:     Unable to resolve type: %s", type)
        assert False

    def parse(self):
        """
        parse initiates the parsing of the file described by this PascalFile
        It populates itself with details of the program and parses the contents.
        """     
        self._contents.parse(self._stream)
        self._is_parsed = True

    def to_code(self):
        import converter_helper
        self._contents.to_code()

        for (name, module) in converter_helper.converters.items():
            self._code[name] = self._contents.code[name]
            

    