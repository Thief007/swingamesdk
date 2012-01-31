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
import readline
import sys

_lang = [ {"lang": "Pascal", "template": "../../Dist/Pascal/FPC", "main file": "GameMain.pas"} ]
_enumLoad = [ {"load": "BitmapNamed"}]
_base_path = "../../Dist/HowTo"
_base_path_Source_Code = "../../Dist/HowTo/Source_Code"

def open_how_to():
    dirList = os.listdir(_base_path_Source_Code)
    for fname in dirList:
        current_how_to = fname.split('.')
        for lang in _lang:     
            buildSH = "%s/%s/%s/%s" % (_base_path, lang["lang"],current_how_to[0],"build.sh")
            print "path "+buildSH
            runSH = "%s/%s/%s/%s" % (_base_path, lang["lang"],current_how_to[0],"run.sh")
            subprocess.call([buildSH])
            # uncomment below to run every SwinGame
            #subprocess.call([runSH])
  
def main():
    open_how_to();
if __name__ == '__main__':
    main()