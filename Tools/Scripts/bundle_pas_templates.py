import os
import platform
import subprocess
import swin_shutil





#get Current path
def get_current_path():
  return os.getcwd()

def get_swin_game_dir():
  return "../../"
  
def get_python_script_dir():
  return "../SGWrapperGen/"


#Getting OS Name
def get_os_name():
  osName = platform.system()
  if osName == "Darwin":
    return "Mac OS X"
  elif osName == "Linux":
    return "Linux"
  else:
    return "Windows"


#generates swingame.pas

def create_swingame_pas():
  print("  Creating SwinGame Unit Code...");
  if(subprocess.call(["python",get_python_script_dir()+"lang_pas_unit.py"]) != 0):
    print ("Error Creating SwinGame Unit Code");
    quit();
  
  
#tree copy without svn and copy symbolic links.
def copy_without_svn(src,dest,overwrite = True):
  if(os.path.isdir(dest) and overwrite):
    print("    Cleaning %s" % dest)
    swin_shutil.rmtree(dest,ignore_errors = True)
  swin_shutil.copytree(src,dest,symlinks = True,ignore = swin_shutil.ignore_patterns(".svn"))
  print ("    Copied %s to %s" % (src,dest))
  
  
  
  ## name is the folder of the specific target i.e. FPC, Mono, etc





def assemble_dist(name, language, sgsdk):
  dist_folder = get_swin_game_dir()+"Dist/"
  lib_folder = get_swin_game_dir()+"CoreSDK/lib/"
  lib_src_folder = get_swin_game_dir()+"CoreSDK/libsrc/"
  src_folder = get_swin_game_dir()+"CoreSDK/src/"
  generated_folder = get_swin_game_dir()+"Generated/%s/lib/" % language
  template_folder = get_swin_game_dir()+"Templates/"
  common_template_folder = template_folder+"Common/"
  lang_template_folder = template_folder+ language+'/'
  common_lang_template_folder = lang_template_folder+"Common/"

  lang_dist_folder = dist_folder+language+'/'
  specific_dist_folder = lang_dist_folder+name+'/'
  specific_dist_lib_folder = specific_dist_folder+"lib/"
  specific_template_folder = lang_template_folder+name+'/'

#clean dist folder
  print("\n  Copying common files...")
  copy_without_svn(common_template_folder, specific_dist_folder)

  print("\n  Copying %s common files..." % language)
  copy_without_svn(common_lang_template_folder, specific_dist_folder,overwrite = False)

  print("\n  Copying %s specific files..." % name)
  copy_without_svn(specific_template_folder, specific_dist_folder, overwrite = False)
  
  print("\n  Copying %s generated files..." % language)
  copy_without_svn(generated_folder, specific_dist_lib_folder,overwrite = False)
  
  print("\n  Copying lib files...")
  copy_without_svn(lib_folder, specific_dist_lib_folder,overwrite = False)
  flat_copy_without_svn(lib_src_folder,specific_dist_lib_folder)
  flat_copy_without_svn(src_folder,specific_dist_lib_folder)
  
#copy every file not in svn into a flat destination.
def flat_copy_without_svn(src,dest):
  for root, dirs, files in os.walk(src):
    for f in files:
      if(root.find('.svn') == -1):
        fullpath = os.path.join(root, f)
        swin_shutil.copy(fullpath,dest)

  
def main():
  CopyList = [
    { 
      'target':     'FPC',
      'language':   'Pascal', 
      'sgsdk':      False
    },
  ]


  print("--------------------------------------------------")
  print("  Creating "+get_os_name()+" SwinGame Pascal Templates")
  print("--------------------------------------------------\n")
  
  
  create_swingame_pas();
  
  for build in CopyList:
    print("  Assembling Dist Folder for %s..." % build['target'])
    assemble_dist(build['target'],build['language'],build['sgsdk'])  
  print("\nFinished!");
  
  
  
if __name__ == '__main__':
    main()
