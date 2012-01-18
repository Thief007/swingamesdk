import logging
import sys
import os
import glob

from pascal_parser.pas_file import PascalFile
from pascal_parser.pas_parser_utils import logger
from c_lib.pas_to_c import convert_pas_to_c

from pas_to_stdout import print_pas_file

_files = []

def change_file_extension(fileName, new_extension):
    '''
    returns a string with a new file type
    eg. change_file_extension (PascalTest.pas, '.c')
    will return PascalTest.c
    '''
    base = fileName.split('.')[0]
    return (base + new_extension)

def write_file(file_data, path):
    newPath = change_file_extension(path + file_data.filename, '.c')
    file = open(newPath, "w")
    tabs = 0
    for line in file_data.lines:
        if ('}' in line):
            tabs -= 1

        string_to_write = ''
        for count in range(tabs):
            string_to_write += '\t'
        string_to_write += line + '\n'
        file.write(string_to_write)

        if ('{' in line):
            tabs += 1


    file.close()

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG,
                    format='%(asctime)s - %(levelname)s - %(message)s',
                    stream=sys.stdout)
    path = 'test\Pascal'
    for file in glob.glob(os.path.join(path, '*.pas')):
        _files.append(PascalFile.create_pas_file(os.path.basename(file), file))

    for file in _files:
        print '---------- Pascal ----------'
        print_pas_file(file)
        print '----------    C   ----------'
        c_file = convert_pas_to_c(file)
        for line in c_file.lines:
            print line
        write_file(c_file, 'test\\C\\')

    