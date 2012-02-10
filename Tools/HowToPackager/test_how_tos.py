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
import posixpath 

_lang = [ 
            {"lang": "Pascal", "template": "../../Dist/Pascal/FPC", "main file": "GameMain.pas", "extension": ".pas"}, 
            {"lang": "C", "template": "../../Dist/C/gpp", "main file": "main.c", "extension": ".c"} 
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
    failed_list = []
    bash = ''
    if (get_os_name() == "Windows"):
      bash = 'bash ';

    for lang in _lang:     
        path = _base_path + lang['lang']
        how_to_list = glob.glob((path + '//' + "HowTo*"))
        for how_to in how_to_list:
            print "*" * 70
            build_sh = posixpath.normpath("%s/build.sh" % how_to)
            build_sh = build_sh.replace("\\", "/")
            run_sh = posixpath.normpath("%s/run.sh" % how_to)
            run_sh = run_sh.replace("\\", "/")
            print " - Building %s" % build_sh #os.path.basename(how_to)
            print "     - ", "%s./%s" % (bash, build_sh)
            if subprocess.call("%s./%s" % (bash, build_sh)) == 0:
                pass
                # print " - Running %s " % run_sh #os.path.basename(how_to)
                # print "     - ", "%s./%s" % (bash, run_sh)
                # subprocess.call("%s./%s" % (bash, run_sh))
            else:
                print "     - Build failed."
                print "     - Removing %s" %how_to
                failed_list.append(os.path.basename(how_to))
                shutil.rmtree(how_to)
    print '*' * 70
    print ' - Failed builds:'
    for build in failed_list:
        print '     - ', build
    print '*' * 70


def main():
    open_how_to();
if __name__ == '__main__':
    main()