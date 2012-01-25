from pascal_parser.tokeniser.pas_token_stream import SGTokenStream
from pascal_parser.tokeniser.pas_token_kind import TokenKind
from pascal_parser.pas_program import PascalProgram
from pascal_parser.pas_unit import PascalUnit
class PascalFile(object):
    """
    Describes a pascal source file.
    The PascalFile can be parsed.
    """
    def __init__(self, path):
        import os
        self._filename = os.path.basename(path)
        self._name = ''
        self._contains_kind = None
        self._contents = None       # PascalProgram or PascalLibrary
        self._code = dict()
        self._stream = SGTokenStream(path)
        self._is_parsed = False

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
            assert False

        self._name = self._stream.lookahead(2)[1].value     # file's 'name' is after the kind of file is stated

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
        unit = get_unit_named(reference.name)
        if unit is None:
            return None
        else:
            if (not unit.is_parsed):
                unit.parse()
            return unit

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
            

    