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




def CreateSwingamePas():
  print("  Creating SwinGame Unit Code...");
  if(subprocess.call(["python",get_python_script_dir()+"lang_pas_unit.py"]) != 0):
    print ("Error Creating SwinGame Unit Code");
    quit();
  
def CopyWithoutSvn(src,dest,overwrite = True):
  if(os.path.isdir(dest) and overwrite):
    print("    Cleaning %s" % dest)
    swin_shutil.rmtree(dest,ignore_errors = True)
  swin_shutil.copytree(src,dest,symlinks = True,ignore = swin_shutil.ignore_patterns(".svn"))
  print ("    Copied %s to %s" % (src,dest))
  
  
  
  ## name is the folder of the specific target i.e. FPC, Mono, etc



def copy_pascal_source_to_dist(core_sdk_folder, dist_source_folder):
  
  coresdk_libsrc = core_sdk_folder+"libsrc/"
  coresdk_src = core_sdk_folder+"src/"
  generated_source_src = get_swin_game_dir()+"Generated/Source/src"
  
  print("  Copying %s to %s" % (coresdk_libsrc,dist_source_folder))
  CopyWithoutSvn(coresdk_libsrc, dist_source_folder)
  
  print("  Copying %s to %s" % (coresdk_src,dist_source_folder))
  CopyWithoutSvn(coresdk_src, dist_source_folder,overwrite = False)
  
  print("  Copying %s to %s" % (generated_source_src,dist_source_folder))
  CopyWithoutSvn(generated_source_src, dist_source_folder,overwrite = False)

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
  print("  Copying common files...")
  CopyWithoutSvn(common_template_folder, specific_dist_folder)

  print("  Copying %s common files..." % language)
  CopyWithoutSvn(common_lang_template_folder, specific_dist_folder,overwrite = False)

  print("  Copying %s specific files..." % name)
  CopyWithoutSvn(specific_template_folder, specific_dist_folder, overwrite = False)
  
  print("  Copying %s generated files..." % language)
  CopyWithoutSvn(generated_folder, specific_dist_lib_folder,overwrite = False)
  
  print("  Copying lib files...")
  CopyWithoutSvn(lib_folder, specific_dist_lib_folder,overwrite = False)
  flat_copy(lib_src_folder,specific_dist_lib_folder)
  flat_copy(src_folder,specific_dist_lib_folder)
  
  
def flat_copy(src,dest):
  for root, dirs, files in os.walk(src):
    for f in files:
      if(root.find('.svn') == -1):
        fullpath = os.path.join(root, f)
        swin_shutil.copy(fullpath,dest)

  
def main():
  
  dist_dir_source_src = get_swin_game_dir()+"Dist/Source/src/"
  generated_dir_pascal = get_swin_game_dir()+"Generated/Pascal/lib/"
  template_folder = get_swin_game_dir()+"Templates/"
  pas_dist = get_swin_game_dir()+"Dist/Pascal/"

  CopyList = [
    { 
      'target':     'FPC',
      'language':   'Pascal', 
      'sgsdk':      False
    },
  ]


  print("--------------------------------------------------")
  print("  Creating "+get_os_name()+" SwinGame Pascal Templates")
  print("--------------------------------------------------")
  
  
  #CreateSwingamePas();
  
  #FLATTEN THIS!
  print("Copying pascal source to Dist folder...")
  #copy_pascal_source_to_dist(get_swin_game_dir()+"CoreSDK/", dist_dir_source_src)
  
  for build in CopyList:
    print("Assembling Dist Folder for %s..." % build['target'])
    assemble_dist(build['target'],build['language'],build['sgsdk'])  
  print("Finished!");
  
  
  
  
  
if __name__ == '__main__':
    main()
