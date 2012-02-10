import os
import glob
import platform
import ssh

_languages = [
	{
		language : "Pascal",
		target : "FPC",
	},
	{
		language : "C",
		target : "gcc",
	},
	{
		language : "C",
		target : "gpp",
	},
		]

"""
Stores the files to transfer to the server in a list of tuples.
	(local_source, server_destination)
"""
_files_to_transfer = [()]

def get_local_dist_directory():
	return "../../Dist"

def get_local_how_to_directory():
	return get_dist_directory() + "/HowTo"

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
		lang_source = get_how_to_directory() + "/Source_Code/" + lang['language']
		how_to_files = glob.glob(os.path.join(lang_source, "HowTo*"))
        for how_to in how_to_files:
        	server_path = get_server_source_directory() + lang['language'] + os.path.basename(how_to)
        	_files_to_transfer.append(how_to, server_path)
        	
def gather_how_to_projects():
	"""
	Finds all the how to projects and appends them to the list
	of files to transfer
	"""
	# zip each set of how tos?
	# unzip on server? -> probably easier to keep the files all together that way
	for lang in _languages:
		lang_source = get_how_to_directory() + lang['language'] + '/Archive'
		server_path = get_server_source_bundles_directory() + lang['language']
		# zip archive?
		# add zipped archive to list?

def gather_templates():
	"""
	Finds all the templates, zips them, and appends them
	to the list of files to transfer
	"""
	pass

def deploy_to_server():
	"""
	Copies the files from the local machine to the server.
	Files transfered from _files_to_transfer.
	_files_to_transfer - (source_file, server_destination_path)
		TODO: Implement...
	"""
	raise Exception('Not Implemented...')
	# server = ssh.Connection(host = 'example.com', username = 'warrior', password = 'lennalenna')
	# server.put('/home/warrior/hello.txt', '/home/zombie/textfiles/report.txt')
	# server.execute('ls -l')
	# server.close()

def main():
	gather_how_to_source()
	gather_templates()
	gather_how_to_projects()
	deploy_to_server()

main()