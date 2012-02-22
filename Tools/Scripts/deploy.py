#!/usr/bin/python

#
# Requires ssh module: easy_install paramiko
#

import os
import glob
import platform
import ssh
import getpass

from swin_template_utils import *


local_how_to_folder = dist_folder + 'HowTo/'
server = "ictnetcontrolsvm2.ict.swin.edu.au"

server_base_path        = '/home/shared/g_sgdocs/www/htdocs/'
server_downloads_path   = server_base_path + 'images/downloads/'
server_install_path     = server_downloads_path + 'SwinGame%s/' % sg_version
server_api_path         = server_base_path + 'apidocs/'
server_howto_path       = server_base_path + 'howto/'


_languages = [
    {
        'language' : "Pascal",
        'target' : "FPC",
    },
    {
        'language' : "C",
        'target' : "gcc",
    },
    {
        'language' : "C",
        'target' : "gpp",
    },
        ]

"""
Stores the files to transfer to the server in a list of tuples.
    (local_source, server_destination)
"""
_files_to_transfer = list() 



def get_local_template_directory():
    return "../../Templates"

# need to change this shit later
def get_server_source_directory():
    return "source/"
# also change this...
def get_server_template_directory():
    return "templates/"
#also this...
def get_server_source_bundles_directory():
    return "source/bundles/"

def gather_how_to_source():
    """
    Finds all the how_to source files and appends
    them to the list of files to transfer.
    """
    for lang in _languages:
        # Source_Code/lang_name
        lang_source = local_how_to_folder + "/Source_Code/" + lang['language']
        how_to_files = glob.glob(os.path.join(lang_source, "HowTo*"))
        for how_to in how_to_files:
            server_path = server_howto_path + os.path.basename(how_to)
            _files_to_transfer.append( (how_to, server_path) )
            
def gather_how_to_projects():
    """
    Finds all the how to projects and appends them to the list
    of files to transfer
    """
    
    for lang in _languages:
        lang_source = local_how_to_folder + lang['language'] + '/Archive/'
        server_path = server_downloads_path + lang['language'] + '/'
        # zip archive?
        # add zipped archive to list?

def gather_templates():
    """
    Finds all the templates, zips them, and appends them
    to the list of files to transfer
    """
    
    temp_files = deploy_list()
    
    for temp in temp_files:
        server_path = server_install_path + os.path.basename(temp)
        _files_to_transfer.append( (temp, server_path) )

def deploy_to_server():
    """
    Copies the files from the local machine to the server.
    Files transfered from _files_to_transfer.
    _files_to_transfer - (source_file, server_destination_path)
        TODO: Implement...
    """
    usr = raw_input('Enter username: ')
    pwd = getpass.getpass('Enter server password: ')
    s = ssh.Connection(server, username=usr, password=pwd)
    
    s.execute('if [ ! -d "%s" ]; then mkdir -p "%s"; fi' % ( server_install_path, server_install_path ) )
    
    for (from_file, to_file) in _files_to_transfer:
        output_line('Copying %s' % os.path.basename(from_file))
        s.put(from_file, to_file)
        
    
    s.close()

def main():
    output_header(['Copying SwinGame Files to Server'])
    
    gather_how_to_source()
    gather_templates()
    gather_how_to_projects()
    deploy_to_server()

main()