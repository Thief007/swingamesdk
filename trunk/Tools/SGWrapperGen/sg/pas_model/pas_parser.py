import logging
import sys
import os
import glob

from pas_file import PascalFile
from pascal_parser.pas_parser_utils import logger
from pas_converter import run_convert

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
    tabs = 0
    for (name, module) in converter_helper.converters.items():
        newPath = 'test/' + name + '/'

        if not os.path.exists(newPath):
            os.makedirs(newPath)

        file = open(change_file_extension(newPath +  file_data.filename, module.extension), "w")
        for line in file_data.code[name].split('\n'):
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
    import c_lib
    import pas_lib
    import converter_helper
    
    converter_helper.converters["c_lib"] = c_lib
    converter_helper.converters["pas_lib"] = pas_lib


    logging.basicConfig(level=logging.DEBUG,
                    format='%(asctime)s - %(levelname)s - %(message)s',
                    stream=sys.stdout)
    path = 'test\Pascal'
    for file in glob.glob(os.path.join(path, '*.pas')):
        _files.append(PascalFile.create_pas_file(os.path.basename(file), file))

    for file in _files:
        #print '---------- Pascal ----------'
        #print_pas_file(file)
        print '----------    C   ----------'
        c_file = run_convert(file)
        write_file(c_file, 'test\\C\\')
        #for line in c_file.lines:
        #    print line
        #write_file(c_file, 'test\\C\\')

    