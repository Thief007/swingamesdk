import logging
import sys
import os
import glob

from pas_file import PascalFile
from pascal_parser.pas_parser_utils import logger
from pas_converter import run_convert
from pas_file_cache import add_file, get_file_named, files

def change_file_extension(fileName, new_extension):
    '''
    returns a string with a new file type
    eg. change_file_extension (PascalTest.pas, '.c')
    will return PascalTest.c
    '''
    base = fileName.split('.')[0]
    return (base + new_extension)

def write_file(file_data):
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

    logging.basicConfig(level=logging.INFO,
                    format='%(asctime)s - %(levelname)s - %(message)s',
                    stream=sys.stdout)
    path = 'test\Pascal'

    add_file(PascalFile.create_unit_from('System', [('myVar', 'LongInt')], ['LongInt', 'Byte', 'String', 'Single', 'Pointer', 'LongWord', 'Boolean'], ['myFunc']))

    print '----------   Adding Files  ----------'
    for file in glob.glob(os.path.join(path, '*.pas')):
        add_file(PascalFile(file))

    print '----------     Parsing    ----------'
    for (name, file) in files().items():
        if not file.is_parsed:
            file.parse()

    print '----------    Converting   ----------'
    for (name, file) in files().items():
        run_convert(file)

    print '----------     Writing    ----------'
    for (name, file) in files().items():
        if file.contains_kind == 'program':
            write_file(file)


    