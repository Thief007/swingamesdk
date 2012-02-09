import subprocess
import shutil
import glob
import os

# This script is responsible for preparing the files for use by the parser and the bundler
# Steps:
# 1. Copy resources needed by the parser
#		- HowTos
#		- SwinGame library
# 2. Generate resource list
# 3. Parse HowTo files
#... and more!

def get_dist_directory():
	return "../../Dist"

def get_test_directory():
	return "../../CoreSDK/test"

def get_how_to_directory():
	return get_dist_directory() + "/HowTo"

def get_parser_directory():
	return "../SGWrapperGen/sg/pas_model/"

def prepare_directories():
	print " Cleaning HowTo folder:"
	shutil.rmtree(get_how_to_directory(), ignore_errors=True)
	print "  Making: 	HowTo folder"
	os.mkdir(get_how_to_directory())
	print "  Making:	Source_Code folder"
	os.mkdir(get_how_to_directory() + '/Source_Code/')
	os.mkdir(get_how_to_directory() + '/Source_Code/HowTos')
	os.mkdir(get_how_to_directory() + '/Source_Code/HowTos/lib/')

	print "  Copying Pascal library for parser"
	source = get_dist_directory() + "/Pascal/FPC/lib"
	destination = get_how_to_directory() + "/Source_Code/HowTos/lib/"

	print "	- Source:		", source
	print "	- Destination:	", destination
	
	# shutil.copytree(source)
	files_to_copy = glob.glob(os.path.join(source, "sg*.pas"))
	files_to_copy.append(source + "/SwinGame.pas")

	for fname in files_to_copy:
		print " Copying:	" + os.path.basename(fname)
		shutil.copy2(fname, destination)

def copy_how_tos():
	"""
	Copies all the How To documents from the source directory (CoreSDK/test)
	to Dist/HowTo/Source/HowTos

	How To files are identified by the regex HowTo*.pas
	"""

	source = get_test_directory()
	destination = get_how_to_directory() + "/Source_Code/HowTos"

	if not os.path.exists(destination):
		os.makedirs(destination)

	if not os.path.exists(source):
		print "Source path does not exist: %s" %source
		assert False
	print "*" * 70
	print "Copying How Tos:"
	print "-- from		%s" % source
	print "-- To 		%s" % destination
	print "*" * 70
	# all the HowTo's that start with HowTo and are a pascal file
	for fname in glob.glob(os.path.join(source, "HowTo*.pas")):
		shutil.copy2(fname, destination)
		print "Copied: %s" %os.path.basename(fname)
	print "*" * 70

def parse_how_tos():
	"""
	Calls the pascal parser to parse the how to files
	Files are moved into Dist/HowTo/Source_Code/%(Lang_name)
	"""
	print "*" * 70
	print "Parsing How Tos:"
	print "*" * 70
	if subprocess.call(["python", get_parser_directory() + "/pas_parser.py"]) != 0:
		print "Error parsing How To files."
		assert False

def generate_resource_list():
	"""
	This method produces a list of resources used to create the template files.
	The resource_list is put in Dist/HowTo/Source_Code
		- The list should be the same between languages
	"""
	print '*' * 70
	print " Generating resource list: "
	print '*' * 70
	if subprocess.call(["python", "run_How-To_Resources_Begin_script.py"]) == 0:
		resource_list = get_test_directory() + "/HowToResources.txt"
		shutil.copy2(resource_list, get_how_to_directory() + "/Source_Code")
	else:
		print " Error copying files..."
		quit()

def generate_templates():
	"""

	"""
	print '*' * 70
	print " Generating templates: "
	print '*' * 70
	if subprocess.call(["python", "package_how_tos.py"]) != 0:
		print " Error packaging how tos"
		quit()

def main():
	prepare_directories()
	generate_resource_list()
	copy_how_tos()
	parse_how_tos()
	generate_templates()
if __name__ == '__main__':
    main()
