import logging
import sys

from pas_token_stream import *
from pas_file import *

# Logger object is used to log events to console
logger = logging.getLogger("sgLogger")

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG,
                    format='%(asctime)s - %(levelname)s - %(message)s',
                    stream=sys.stdout)
    stream = SGTokenStream('ParserTest.pas')
    
    afile = PascalFile()
    afile.parse('ParserTest.pas', stream)
    
    print afile