class ConvertedFile(object):
    """Stores a file that is ready to be converted"""
    def __init__(self, name, lines):
        self._name = name
        self._lines = lines

    @property
    def filename(self):
        return self._name

    @property
    def lines(self):
        return self._lines
