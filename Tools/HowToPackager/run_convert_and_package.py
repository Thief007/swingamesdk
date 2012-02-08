import subprocess
import shutil
import glob
import os

# This script is responsible for preparing the files for use by the parser and the bundler
# Steps:
# 1. Copy resources needed by the parser
#		- HowTos
#		- SwinGame library
# 2. Parse HowTo files
#... and more!

def copy_how_tos():
	"""
	Copies all the How To documents from the source directory (CoreSDK/test)
	to Dist/HowTo/Source/HowTos

	How To files are identified by the regex HowTo*.pas
	"""
	source = "../../CoreSDK/test"
	destination = "../../Dist/HowTo/Source_Code/HowTos"

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
	"""
	print "*" * 70
	print "Parsing How Tos:"
	print "*" * 70
	if subprocess.call(["python", "../SGWrapperGen/sg/pas_model/pas_parser.py"]) != 0:
		print "Error parsing How To files."
		assert False
def main():
	copy_how_tos()
	parse_how_tos()

    #subprocess.call(["python", "run_How-To_Resources_Begin_script.py"])
    
if __name__ == '__main__':
    main()
