#
# Test how tos
#
# Steps:
# 1: Opens a Swingame How To
# 2: Executes the how to
# 3: Asks tester if program executes correctly
# 4: Asks tester if program  performed as expected

import os
import shutil
import subprocess

_lang = [ {"lang": "Pascal", "template": "../../Dist/Pascal/FPC", "main file": "GameMain.pas"} ]
_enumLoad = [ {"load": "BitmapNamed",
_base_path = "../../Dist/HowTo"

def open_how_to():
    f = open("../../CoreSDK/test/HowToResources.txt")
    how_to_file_lines = f.readlines()
    f.close()
    
    current_how_to = None
    for line in how_to_file_lines:
        # if line starts with *... its a new how to...
        if line[0] == "*":
            current_how_to = line[1:].strip()
        for lang in _lang:
            current_how_to_resource = line.strip()      
            buildSH = "%s/%s/%s/%s" % (_base_path, lang["lang"],current_how_to,"build.sh")
            runSH = "%s/%s/%s/%s" % (_base_path, lang["lang"],current_how_to,"run.sh")
            subprocess.call([buildSH])
            subprocess.call([runSH])
    
def main():
    print "Hello Wayne"
    open_how_to();
if __name__ == '__main__':
    main()