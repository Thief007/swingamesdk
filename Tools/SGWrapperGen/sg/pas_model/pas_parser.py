import logging
import sys
import os
import glob

from pas_token_stream import *
from pas_file import *
from pas_to_c import *
from pas_to_stdout import *

# Logger object is used to log events to console
logger = logging.getLogger("sgLogger")
_files = []

def create_pas_file(name, path):
    stream = SGTokenStream(path)
    result = PascalFile()
    result.parse(name, stream)
    return result

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG,
                    format='%(asctime)s - %(levelname)s - %(message)s',
                    stream=sys.stdout)
    path = 'test\Pascal'
    for file in glob.glob(os.path.join(path, '*.pas')):
        _files.append(create_pas_file(file, file))

    for file in _files:
        print '---------- Pascal ----------'
        print_pas_file(file)
        print '----------    C   ----------'
        convert_pas_to_c(file)

    