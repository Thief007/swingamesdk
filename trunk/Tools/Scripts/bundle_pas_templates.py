import os
import platform
import subprocess
import swin_shutil



CopyList = [
  # { 
  #   'target':     'FPC',
  #   'language':   'Pascal', 
  #   'sgsdk':      False
  # },
  
  # {
  #   'target':     'iOS',
  #   'language':   'Pascal', 
  #   'sgsdk':      False
  # },

  # {
  #   'target':     'Mono',
  #   'language':   'CSharp', 
  #   'sgsdk':      True
  # },

  {
  'target':     'gcc',
  'language':   'C', 
  'sgsdk':      True
  },

  {
  'target':     'gpp',
  'language':   'C', 
  'sgsdk':      True
  },

]



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

def create_sgsdk_pas():
  print("    Creating SGSDK Code...");
  if(subprocess.call(["python",get_python_script_dir()+"create_pas_lib.py"]) != 0):
     print ("Error Creating SGSDK Code");
     quit();  


def create_swingame_pas():
  print("  Creating SwinGame Unit Code...");
  if(subprocess.call(["python",get_python_script_dir()+"lang_pas_unit.py"]) != 0):
    print ("Error Creating SwinGame Unit Code");
    quit();


def create_lang_lib(language):
  print("  Creating %s library..." % language);
  if(subprocess.call(["python",get_python_script_dir()+"create_%s_library.py" % language.lower()]) != 0):
    print ("Error Creating %s library..." % language);
    quit();
  
#tree copy without svn and copy symbolic links.
def copy_without_svn(src,dest,overwrite = True):
  if(os.path.isdir(dest) and overwrite):
    print("    Cleaning %s" % dest)
    swin_shutil.rmtree(dest,ignore_errors = True)
  print ("    Copying %s to %s" % (src,dest))
  swin_shutil.copytree(src,dest,symlinks = True,ignore = swin_shutil.ignore_patterns(".svn"))
  


#assembles the files for the dist folder
def assemble_dist(name, language, sgsdk):
  create_lang_lib(language)

  lib_folder = get_swin_game_dir()+"CoreSDK/lib/"
  lib_src_folder = get_swin_game_dir()+"CoreSDK/libsrc/"
  src_folder = get_swin_game_dir()+"CoreSDK/src/"
  generated_folder = get_swin_game_dir()+"Generated/%s/lib/" % language
  template_folder = get_swin_game_dir()+"Templates/"
  common_template_folder = template_folder+"Common/"
  lang_template_folder = template_folder+ language+'/'
  common_lang_template_folder = lang_template_folder+"Common/"

  lang_dist_folder = get_swin_game_dir()+"Dist/%s/" % language
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
  if (language == "Pascal"):
    flat_copy_without_svn(lib_src_folder,specific_dist_lib_folder)
    flat_copy_without_svn(src_folder,specific_dist_lib_folder)
    
  print("--------------------------------------------------")
  if(sgsdk):
    build_sgsdk()
    dist_source_folder = get_swin_game_dir()+"Dist/Source/"
    copy_without_svn(dist_source_folder+"bin/mac/SGSDK.framework", specific_dist_folder+"lib/mac/SGSDK.framework")
    copy_without_svn(dist_source_folder+"bin/mac/SGSDK.framework", specific_dist_folder+"lib/sdl13/mac/SGSDK.framework")
  
#copy every file not in svn into a flat destination.
def flat_copy_without_svn(src,dest):
  
  print("    Copying all files in %s to %s" % (src, dest))
  
  if(not os.path.isdir(dest)):
    os.mkdir(dest)
  for root, dirs, files in os.walk(src):
    for f in files:
      if(root.find('.svn') == -1):
        fullpath = os.path.join(root, f)
        swin_shutil.copy(fullpath,dest)


def build_sgsdk():
    bash = ""
    if (get_os_name() == "Windows"):
      bash = "bash ";
    dist_source_folder = get_swin_game_dir()+"Dist/Source/" 
    
    print "Building SGSDK..."
    
    print("\n  Copying required files to build SGSDK...")
    copy_coresdk_to_dist_source()
    print("\n--------------------------------------------------")
    
    print("Cleaning SGSDK...\n")
    
    if(subprocess.call(["./%sbuild.sh" % dist_source_folder, "-c"])!=0):
      print ("\n  Error Cleaning SGSDK");
      quit();
    
    if(os.listdir(get_swin_game_dir()+"Generated/Source/src/")==[]):
      print("  SGSDK code not found...\n")
      create_sgsdk_pas();
      
    print("Compiling SGSDK...\n")
    if(subprocess.call("%s./%sbuild.sh" % (bash, dist_source_folder))!=0):
      print ("\n  Error Compiling SGSDK");
      if (get_os_name() == "Windows"):
        print ("do you have msys/bin in your environment PATH variable?")
      quit();
    



# for compiling SGSDK
def copy_coresdk_to_dist_source():
  generated_source_folder = get_swin_game_dir()+"Generated/Source/src/"
  template_source_folder = get_swin_game_dir()+"Templates/Source"
  dist_source_folder = get_swin_game_dir()+"Dist/Source/"
  dist_source_src_folder = get_swin_game_dir()+"Dist/Source/src/"
  dist_source_lib_folder = get_swin_game_dir()+"Dist/Source/lib/"
  lib_src_folder = get_swin_game_dir()+"CoreSDK/libsrc/"
  src_folder = get_swin_game_dir()+"CoreSDK/src/"
  lib_folder = get_swin_game_dir()+"CoreSDK/lib/"
  
  
  copy_without_svn(template_source_folder,dist_source_folder)
  copy_without_svn(lib_folder, dist_source_lib_folder, overwrite = False)
  flat_copy_without_svn(generated_source_folder, dist_source_src_folder)
  flat_copy_without_svn(lib_src_folder, dist_source_src_folder)
  flat_copy_without_svn(src_folder,dist_source_src_folder)
  

  
def main():

  print("--------------------------------------------------")
  print("  Creating "+get_os_name()+" SwinGame Pascal Templates")
  print("--------------------------------------------------\n")
  
  
  
  create_swingame_pas();
  print("--------------------------------------------------")
  for build in CopyList:
    
    print("  Assembling Dist Folder for %s (%s)..." % (build['target'], build['language']))
    assemble_dist(build['target'],build['language'],build['sgsdk'])  
  print("\nFinished!");
  
  
  
if __name__ == '__main__':
    main()
