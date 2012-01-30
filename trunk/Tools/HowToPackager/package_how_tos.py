#
# Package How Tos
#
# Steps:
# 1: Create How Tos directory in dist
# 2: Copy in the Swingame Template
# 3: Delete GameMain.pas file
# 4: Copy in the required resources into /resources/sub-folder
# 5: Copy in the HowTo.pas file to /src
# 6: package folder to zip ready for upload to website
#

import os
import shutil
from shutil import make_archive

_lang = [ {"lang": "Pascal", "template": "../../Dist/Pascal/FPC", "main file": "GameMain.pas"} ]
_base_path = "../../Dist/HowTo"

def build_dir_structure():
    """Build the How To structure in Dist folder"""
    print "  Removing old HowTo folder"
    shutil.rmtree(_base_path)
    print "  Making new folder structure"
    os.mkdir(_base_path)
    
    for lang in _lang:
        print "  -  Adding language", lang["lang"]
        os.mkdir(_base_path + "/" + lang["lang"])
        os.mkdir (_base_path+ "/Source_Code")
def copy_how_to_template(how_to_name):
    print " - Adding", how_to_name
    
    # copy template to dir for each language
    for lang in _lang:
        path = "%s/%s/%s" % (_base_path, lang["lang"], how_to_name)
        print "   --", lang["lang"]
        # Copy template
        shutil.copytree(lang["template"], path)
        # Delete "main file" from "current_how_to_path"/src
        print "   -- Deleting GameMain file"
        os.remove(path + "/src/GameMain.pas")

def copy_how_to_resources(current_how_to, how_to_resource, lang):
    path = "%s/%s" % ("../../CoreSDK/test/Resources",how_to_resource)
    dest = "%s/%s/%s/%s/%s" % (_base_path, lang["lang"],current_how_to,"Resources",how_to_resource)
    print "   -- Copying ", path
    print "   -- To ",dest        
    shutil.copy2(path,dest)

def zip_how_to():
    f = open("../../CoreSDK/test/HowToResources.txt")
    how_to_file_lines = f.readlines()
    f.close()
    for line in how_to_file_lines:
           # if line starts with *... its a new how to...
           if line[0] == "*":
               current_how_to = line[1:].strip()
               for lang in _lang:
                    root_dir = os.path.expanduser(os.path.join(_base_path, lang["lang"], current_how_to))
                    path = "%s/%s/%s/%s" % (_base_path, lang["lang"], "Archive", current_how_to)
                    print "   -- Creating Archive "
                    make_archive(path, 'zip',root_dir)

def copy_current_how_to_pas_file():
    f = open("../../CoreSDK/test/HowToResources.txt")
    how_to_file_lines = f.readlines()
    f.close()
    for line in how_to_file_lines:
        # if line starts with *... its a new how to...
        if line[0] == "*":
            current_how_to = line[1:].strip()
            for lang in _lang:
                path = "%s/%s%s" % ("../../CoreSDK/test",current_how_to,".pas")
                if os.path.exists(path):                    
                    dest = "%s/%s/%s/%s/%s%s" % (_base_path, lang["lang"],current_how_to,"src",current_how_to,".pas")
                    print "   -- Copying ", path
                    print "   -- To ",dest        
                    shutil.copy2(path,dest)
                    sourcePath = "%s/%s%s" % ("../../CoreSDK/test",current_how_to,".pas")
                    sourceDest = "%s/%s/%s%s" % (_base_path,"Source_Code", current_how_to,".pas")
                    print "   -- Copying ", sourcePath
                    print "   -- To ",sourceDest        
                    shutil.copy2(sourcePath,sourceDest)
                else:
                    print "   WARNING -- "+ current_how_to +" file does not exist"
      
def copy_how_tos():
    """Copy the language templates to how tos folders for each how to document"""
    
    print "  Creating How To Templates"
    
    f = open("../../CoreSDK/test/HowToResources.txt")
    how_to_file_lines = f.readlines()
    f.close()
    
    current_how_to = None
    
    for line in how_to_file_lines:
        # if line starts with *... its a new how to...
        if line[0] == "*":
            current_how_to = line[1:].strip()
            copy_how_to_template(current_how_to)
        else:
            # its a resource for the current_how_to
            # copy from ../../CoreSDK/test/Resources/"from file" to _base_path/lang/how_toname/Resources/"from file"
            
            for lang in _lang:
                current_how_to_resource = line.strip()
                #only pascal available at this time 27 JAN 2012 so must alter to include language when other languages 
                #become available to copy resources WBUCHNER
                #path = "%s/%s/%s/%s" % (_base_path, lang["lang"],"CoreSDK/test/Resources",current_how_to_resource)
                copy_how_to_resources(current_how_to, current_how_to_resource, lang)

def main():
    build_dir_structure()
    copy_how_tos()
    copy_current_how_to_pas_file()
    zip_how_to()
if __name__ == '__main__':
    main()