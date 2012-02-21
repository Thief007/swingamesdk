import sys
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
          output_line("Packaging " + copy_dist["target"])
          assemble_dist(key, copy_dist, lang_template_dict['use_sgsdk'], None if not copy_dist.has_key('lang') else copy_dist['lang'])
      

#assembles the files for the dist folder
def assemble_dist(language, dist_dict, use_sgsdk, part_from):
    coresdk_folder =            swingame_path + "CoreSDK/"
    lib_src_folder =            swingame_path + "CoreSDK/libsrc/"
    src_folder =                swingame_path + "CoreSDK/src/"
    generated_folder =          swingame_path + "Generated/%s/lib/" % language
    template_folder =           swingame_path + "Templates/"
    langdist_folder =           swingame_path + "Dist/%s/" % (language if not part_from else part_from)
        
    common_template_folder =        template_folder + "Common/"
    lang_template_folder =          template_folder + (language if not part_from else part_from) +'/'
    
    common_lang_template_folder =   lang_template_folder + "Common/"
    specific_template_folder =      lang_template_folder + dist_dict['target'] + '/'
    
    specificdist_folder =           langdist_folder + dist_dict['target'] + '/'
    specific_dist_lib_folder =      specificdist_folder + "lib/"
    
    copy_lib_dir =  dist_dict["lib"]
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
        
        if "Windows" in dist_dict['os']:
            if not os.path.exists(dist_source_folder+"bin/win"):
                print >> sys.stderr, 'Missing Windows dll from', dist_source_folder+"bin/win"
            else:    
                copy_without_svn(dist_source_folder+"bin/win", specificdist_folder+"lib/win", overwrite = False)
            
        if "Mac OS X" in dist_dict['os']:
            if dist_dict['staticsgsdk']:
                # Copy staticlibs
                if not os.path.exists(dist_source_folder+"bin/mac/sgsdk-sdl13.a"):
                    print >> sys.stderr, 'Missing static libraries for mac'
                else:
                    swin_shutil.copyfile(dist_source_folder+"bin/mac/sgsdk-sdl13.a", specificdist_folder+"lib/sdl13/mac/libSGSDK.a")
                    swin_shutil.copyfile(dist_source_folder+"bin/mac/sgsdk-godly.a", specificdist_folder+"lib/godly/mac/libSGSDK.a")
            else:
                if not os.path.exists(dist_source_folder+"bin/mac/SGSDK.framework"):
                    print >> sys.stderr, 'Missing SwinGame framework for mac'
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
    
    create_lang_libraries()
    
    print("\nFinished!")
    
    
    
if __name__ == '__main__':
    main()
