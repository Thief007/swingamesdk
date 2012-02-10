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
import sys
import glob
import platform

_lang = [ 
            {"lang": "Pascal", "template": "../../Dist/Pascal/FPC", "main file": "GameMain.pas", "extension": ".pas"}, 
            {"lang": "C", "template": "../../Dist/C/gcc", "main file": "main.c", "extension": ".c"} 
        ]
_enumLoad = [ {"load": "BitmapNamed"}]
_base_path = "../../Dist/HowTo/"
_base_path_Source_Code = "../../Dist/HowTo/Source_Code/"

#Getting OS Name
def get_os_name():
  osName = platform.system()
  if osName == "Darwin":
    return "Mac OS X"
  elif osName == "Linux":
    return "Linux"
  else:
    return "Windows"

def open_how_to():
    bash = ''
    if (get_os_name() == "Windows"):
      bash = 'bash ';

    for lang in _lang:     
        path = _base_path + lang['lang'] + '/'
        how_to_list = glob.glob(os.path.join(path, "HowTo*"))
        for how_to in how_to_list:
            build_sh = "%s/build.sh" % how_to
            run_sh = "%s/run.sh" % how_to
            print "Building %s" % build_sh #os.path.basename(how_to)
            subprocess.call("%s./%s" % (bash, build_sh))
            print "Running %s " % how_to #os.path.basename(how_to)
            subprocess.call("%s./%s" % (bash, run_sh))
  
def main():
    open_how_to();
if __name__ == '__main__':
    main()