import os
import platform
import subprocess
import swin_shutil
from swin_template_utils import *

# ===================
# = Clear functions =
# ===================

def clear_generated():
    pass

# ==========================
# = Create SGSDK libraries =
# ==========================

# for compiling SGSDK
def copy_coresdk_to_dist_source():
    generated_source_folder =   generated_folder + "Source/src/"
    template_source_folder =    tempate_folder + "Source/"
    lib_src_folder =            swingame_path + "CoreSDK/libsrc/"
    src_folder =                swingame_path + "CoreSDK/src/"
    lib_folder =                swingame_path + "CoreSDK/lib/"
    
    dist_source_folder =        dist_folder + "Source/"
    dist_source_src_folder =    dist_source_folder + "src/"
    dist_source_lib_folder =    dist_source_folder + "lib/"
    
    copy_without_svn(template_source_folder, dist_source_folder)
    copy_without_svn(lib_folder, dist_source_lib_folder, overwrite = False)
    flat_copy_without_svn(generated_source_folder, dist_source_src_folder)
    flat_copy_without_svn(lib_src_folder, dist_source_src_folder)
    flat_copy_without_svn(src_folder, dist_source_src_folder)


_sgsdk_creation_script_options = {
    'Mac OS X': [['-badass','-static'], ['-godly','-static'], None],            # default must be last
    'Windows':  [None], #, '-badass', '-godly'],
}

def create_sgsdk_library():
    """ The first step in creating the SwinGame templates is to create the
        SGSDK framework/dll. This needs to be created for each of the various
        backend versions supported by the platform."""
    
    sgsdk_build_script = swingame_path + 'Dist/Source/build.sh'
    
    output_header(['Generating the code for SGSDK.pas'])
    run_python('create_sgsdk_code.py')
    
    output_line('Copying code to dist source directory')
    copy_coresdk_to_dist_source()
    
    output_header(['Compiling SGSDK framework/dll'])
    option_list = _sgsdk_creation_script_options[get_os_name()]
    
    for opt in option_list:
        run_bash(sgsdk_build_script, opt)


# =========================================
# = Create the various language libraries =
# =========================================

def create_lang_libraries():
  for key, lang_template_dict in template_details.items():
      output_header(['Creating code for %s' % key])
      
      run_python(lang_template_dict['script'])
      
      if lang_template_dict['pre_copy_script']:
          lang_template_dict['pre_copy_script']()
      
      for copy_dist in lang_template_dict['copy_dist']:
          if get_os_name() in copy_dist['os']:
              output_line("Packaging " + copy_dist["target"])
              assemble_dist(key, copy_dist, lang_template_dict['use_sgsdk'])
      

#assembles the files for the dist folder
def assemble_dist(language, dist_dict, use_sgsdk):
    coresdk_folder =            swingame_path + "CoreSDK/"
    lib_src_folder =            swingame_path + "CoreSDK/libsrc/"
    src_folder =                swingame_path + "CoreSDK/src/"
    generated_folder =          swingame_path + "Generated/%s/lib/" % language
    template_folder =           swingame_path + "Templates/"
    langdist_folder =          swingame_path + "Dist/%s/" % language
        
    common_template_folder =        template_folder + "Common/"
    lang_template_folder =          template_folder + language +'/'
    
    common_lang_template_folder =   lang_template_folder + "Common/"
    specific_template_folder =      lang_template_folder + dist_dict['target'] + '/'
    
    specificdist_folder =           langdist_folder + dist_dict['target'] + '/'
    specific_dist_lib_folder =      specificdist_folder + "lib/"
    
    copy_lib_dir =  dist_dict["lib"][get_os_name()]
    lib_folder =    coresdk_folder + copy_lib_dir
    
    
    #clean dist folder
    # print("\n  Copying common files...")
    copy_without_svn(common_template_folder, specificdist_folder)
    
    # print("\n  Copying %s common files..." % language)
    copy_without_svn(common_lang_template_folder, specificdist_folder, overwrite = False)
    
    # print("\n  Copying %s specific files..." % name)
    copy_without_svn(specific_template_folder, specificdist_folder, overwrite = False)
    
    # print("\n  Copying %s generated files..." % language)
    copy_without_svn(generated_folder, specific_dist_lib_folder, overwrite = False)
    
    # print("\n  Copying lib files...")
    copy_without_svn(lib_folder, specific_dist_lib_folder, overwrite = False)
    
    if language == "Pascal":
        flat_copy_without_svn(lib_src_folder, specific_dist_lib_folder)
        flat_copy_without_svn(src_folder, specific_dist_lib_folder)
    
    # print("--------------------------------------------------")
    
    if use_sgsdk:
        # print "copying library"
        dist_source_folder = swingame_path + "Dist/Source/"
        if (get_os_name() == "Windows"):
            copy_without_svn(dist_source_folder+"bin/win", specificdist_folder+"lib/win", overwrite = False)
        elif (get_os_name() == "Mac OS X"):
            if dist_dict['staticsgsdk']:
                # Copy staticlibs
                swin_shutil.copyfile(dist_source_folder+"bin/mac/sgsdk-sdl13.a", specificdist_folder+"lib/sdl13/mac/libSGSDK.a")
                swin_shutil.copyfile(dist_source_folder+"bin/mac/sgsdk-godly.a", specificdist_folder+"lib/godly/mac/libSGSDK.a")
            else:
                # Copy frameworks
                copy_without_svn(dist_source_folder+"bin/mac/SGSDK.framework", specificdist_folder+"lib/mac/SGSDK.framework")
                
                copy_without_svn(dist_source_folder+"bin/mac/SGSDK.framework", specificdist_folder+"lib/sdl13/mac/SGSDK.framework")
                cur = os.getcwd()
                os.chdir(specificdist_folder+"lib/sdl13/mac/SGSDK.framework/Versions")
                # print os.getcwd()
                os.remove('Current')
                os.symlink('./3.0badass', 'Current')
                
                copy_without_svn(dist_source_folder+"bin/mac/SGSDK.framework", specificdist_folder+"lib/godly/mac/SGSDK.framework")
                os.chdir(specificdist_folder+"lib/godly/mac/SGSDK.framework/Versions")
                # print os.getcwd()
                os.remove('Current')
                os.symlink('./3.0godly', 'Current')
                os.chdir(cur)

def main():
    output_header(['Packaging SwinGame Templates'])
    
    # Create the framework/dll
    create_sgsdk_library()
    create_lang_libraries()
    
    print("\nFinished!")
    
    
    
if __name__ == '__main__':
    main()
