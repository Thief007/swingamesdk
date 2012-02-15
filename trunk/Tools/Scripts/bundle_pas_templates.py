import os
import platform
import subprocess
import swin_shutil

_script_path        = os.path.dirname(os.path.realpath(__file__)) + '/'
_swingame_path      = os.path.realpath(_script_path + '../..') + '/'
_python_script_dir  = os.path.realpath(_script_path + '../SGWrapperGen') + '/'

_dist_folder        = _swingame_path + "Dist/"
_generated_folder   = _swingame_path + "Generated/"
_tempate_folder     = _swingame_path + "Templates/"


# ====================
# = Helper functions =
# ====================

def get_os_name():
    """ Returns the name of the Operating System."""
    osName = platform.system()
    if osName == "Darwin":
        return "Mac OS X"
    elif osName == "Linux":
        return "Linux"
    else:
        return "Windows"

_has_output = False

def output_header(msgs):
    global _has_output
    if _has_output: 
        print ""
    _has_output = True
    print "--------------------------------------------------"
    for line in msgs:
        print '  ', line
    print "--------------------------------------------------"

def output_line(msg):
    print '  * ', msg


def run_python(script_name):
    output_line('Running python script: ' + script_name)
    proc = subprocess.Popen(["python", _python_script_dir + script_name], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out,err = proc.communicate()
    
    if proc.returncode != 0:
        print "Error running script: ", script_name
        print out
        quit()

def run_bash(script_name, opts):
    if get_os_name() == "Windows":
        exec_list = ["bash", script_name]
    else:
        exec_list = [script_name]
    
    if opts:
        if isinstance(opts, list):
            exec_list.extend(opts)
        else:
            exec_list.append(opts)
    
    output_line('Running bash script: ' + str(exec_list))
    
    proc = subprocess.Popen(exec_list, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out,err = proc.communicate()
    
    if proc.returncode != 0:
        print "Error running script: ", script_name, " ", opts
        print out, err
        
        if (get_os_name() == "Windows"):
          print ("Make sure you have msys/bin in your environment PATH variable")
        
        quit()

def copy_without_svn(src,dest,overwrite = True):
    """tree copy without svn and copy symbolic links. Overwrite delete destination tree from root..."""

    if(os.path.isdir(dest) and overwrite):
        # print("    Cleaning %s" % dest)
        swin_shutil.rmtree(dest,ignore_errors = True)

    # print ("    Copying %s to %s" % (src,dest))
    swin_shutil.copytree(src,dest,symlinks = True,ignore = swin_shutil.ignore_patterns(".svn"))

def flat_copy_without_svn(src, dest):
    """copy every file not in svn into a flat destination."""
    # print("    Copying all files in %s to %s" % (src, dest))
    if(not os.path.isdir(dest)):
        os.mkdir(dest)

    for root, dirs, files in os.walk(src):
        for f in files:
            if(root.find('.svn') == -1):
                fullpath = os.path.join(root, f)
                swin_shutil.copy(fullpath,dest)

# ===================
# = Clear functions =
# ===================

def clear_generated():
    pass

# =================================================
# = Language specific functions called from above =
# =================================================

def build_csharp_lib():
    """The C# code has been generated, now turn it into a DLL"""
    output_line('Compiling .NET class library')
    
    dirs = {
        'cs_generated_lib_dir':     _swingame_path + 'Generated/CSharp/lib',
        'cs_generated_code_dir':    _swingame_path + 'Generated/CSharp/Code',
        'cs_lib_dir':               _swingame_path + 'Templates/CSharp/Library'
    }
    
    
    if get_os_name() == "Windows":
        csc = ['csc', '-t:library', '-r:System.dll', '-r:System.Drawing.dll', '-define:DEBUG', '-debug+', '-out:%(cs_generated_lib_dir)s/SwinGame.dll' % dirs, '%(cs_lib_dir)s/*.cs' % dirs, '%(cs_generated_code_dir)s/*.cs' % dirs]
    else:
        csc = ['gmcs', '-t:library', '-r:System.dll', '-r:System.Drawing.dll', '-define:DEBUG', '-debug+', '-out:%(cs_generated_lib_dir)s/SwinGame.dll' % dirs, '%(cs_lib_dir)s/*.cs' % dirs, '%(cs_generated_code_dir)s/*.cs' % dirs]
    
    proc = subprocess.Popen(csc, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out,err = proc.communicate()
    
    if proc.returncode != 0:
        print "Error compiling C# DLL"
        print out, err
        
        if (get_os_name() == "Windows"):
            print "Make sure you have csc in your environment PATH variable"
        else:
            print "Make sure you have gmcs in your path"
        quit()
        
    if dirs["cs_generated_code_dir"] != '':
        # path = dirs["cs_generated_code_dir"] + '/*.cs'
        [os.remove(os.path.join(dirs["cs_generated_code_dir"],f)) for f in os.listdir(dirs["cs_generated_code_dir"]) if f.endswith(".cs")]

# ==========================
# = Create SGSDK libraries =
# ==========================

# for compiling SGSDK
def copy_coresdk_to_dist_source():
    generated_source_folder =   _generated_folder + "Source/src/"
    template_source_folder =    _tempate_folder + "Source/"
    lib_src_folder =            _swingame_path + "CoreSDK/libsrc/"
    src_folder =                _swingame_path + "CoreSDK/src/"
    lib_folder =                _swingame_path + "CoreSDK/lib/"
    
    dist_source_folder =        _dist_folder + "Source/"
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
    
    sgsdk_build_script = _swingame_path + 'Dist/Source/build.sh'
    
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

_lang_generation_scripts = {
        'Pascal':   {
                'script':       'create_pascal_library.py',
                'use_sgsdk':    False,
                'copy_dist':    [
                    { 
                      'target':     'fpc',
                      'os':         ['Mac OS X', 'Windows', 'Linux'],
                      'lib':        {'Mac OS X': 'lib', 'Windows': 'lib', 'Linux': 'lib'},
                    },
                    {
                      'target':     'iOS',
                      'os':         ['Mac OS X'],
                      'lib':        {'Mac OS X': 'staticlib'},
                    },
                ],
                'pre_copy_script': None,
            },
        'C':    {
                'script':       'create_c_library.py',
                'use_sgsdk':    True,
                'copy_dist':    [
                    { 
                      'target':     'gpp',
                      'os':         ['Mac OS X', 'Windows', 'Linux'],
                      'lib':        {'Mac OS X': 'lib', 'Windows': 'lib', 'Linux': 'lib'},
                      'staticsgsdk':    False,
                    },
                    { 
                      'target':     'gcc',
                      'os':         ['Mac OS X', 'Windows', 'Linux'],
                      'lib':        {'Mac OS X': 'lib', 'Windows': 'lib', 'Linux': 'lib'},
                      'staticsgsdk':    False,
                    },
                    { 
                      'target':     'xcode 3',
                      'os':         ['Mac OS X'],
                      'lib':        {'Mac OS X': 'lib'},
                      'staticsgsdk':    False,
                    },
                    { 
                      'target':     'iOS',
                      'os':         ['Mac OS X'],
                      'lib':        {'Mac OS X': 'staticlib'},
                      'staticsgsdk':    True,
                    },
                ],
                'pre_copy_script': None,
            },
        #'ObjC':     'create_c_library.py',
        'CSharp':   {
                'script':       'create_csharp_library.py',
                'use_sgsdk':    True,
                'copy_dist':    [
                    { 
                      'target':     'mono',
                      'os':         ['Mac OS X', 'Linux'],
                      'lib':        {'Mac OS X': 'lib', 'Linux': 'lib'},
                      'staticsgsdk':    False,
                    },
                    { 
                      'target':     'vs08',
                      'os':         ['Windows'],
                      'lib':        {'Windows': 'lib/win'},
                      'staticsgsdk':    False,
                    },
                ],
                'pre_copy_script': build_csharp_lib,
            },
    } # end _lang_generation_scripts


def create_lang_libraries():
  for key, lang_template_dict in _lang_generation_scripts.items():
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
    coresdk_folder =            _swingame_path + "CoreSDK/"
    lib_src_folder =            _swingame_path + "CoreSDK/libsrc/"
    src_folder =                _swingame_path + "CoreSDK/src/"
    generated_folder =          _swingame_path + "Generated/%s/lib/" % language
    template_folder =           _swingame_path + "Templates/"
    lang_dist_folder =          _swingame_path + "Dist/%s/" % language
        
    common_template_folder =        template_folder + "Common/"
    lang_template_folder =          template_folder + language +'/'
    
    common_lang_template_folder =   lang_template_folder + "Common/"
    specific_template_folder =      lang_template_folder + dist_dict['target'] + '/'
    
    specific_dist_folder =          lang_dist_folder + dist_dict['target'] + '/'
    specific_dist_lib_folder =      specific_dist_folder + "lib/"
    
    copy_lib_dir =  dist_dict["lib"][get_os_name()]
    lib_folder =    coresdk_folder + copy_lib_dir
    
    
    #clean dist folder
    # print("\n  Copying common files...")
    copy_without_svn(common_template_folder, specific_dist_folder)
    
    # print("\n  Copying %s common files..." % language)
    copy_without_svn(common_lang_template_folder, specific_dist_folder, overwrite = False)
    
    # print("\n  Copying %s specific files..." % name)
    copy_without_svn(specific_template_folder, specific_dist_folder, overwrite = False)
    
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
        dist_source_folder = _swingame_path + "Dist/Source/"
        if (get_os_name() == "Windows"):
            copy_without_svn(dist_source_folder+"bin/win", specific_dist_folder+"lib/win", overwrite = False)
        elif (get_os_name() == "Mac OS X"):
            if dist_dict['staticsgsdk']:
                # Copy staticlibs
                swin_shutil.copyfile(dist_source_folder+"bin/mac/sgsdk-sdl13.a", specific_dist_folder+"lib/sdl13/mac/libSGSDK.a")
                swin_shutil.copyfile(dist_source_folder+"bin/mac/sgsdk-godly.a", specific_dist_folder+"lib/godly/mac/libSGSDK.a")
            else:
                # Copy frameworks
                copy_without_svn(dist_source_folder+"bin/mac/SGSDK.framework", specific_dist_folder+"lib/mac/SGSDK.framework")
                
                copy_without_svn(dist_source_folder+"bin/mac/SGSDK.framework", specific_dist_folder+"lib/sdl13/mac/SGSDK.framework")
                cur = os.getcwd()
                os.chdir(specific_dist_folder+"lib/sdl13/mac/SGSDK.framework/Versions")
                # print os.getcwd()
                os.remove('Current')
                os.symlink('./3.0badass', 'Current')
                
                copy_without_svn(dist_source_folder+"bin/mac/SGSDK.framework", specific_dist_folder+"lib/godly/mac/SGSDK.framework")
                os.chdir(specific_dist_folder+"lib/godly/mac/SGSDK.framework/Versions")
                # print os.getcwd()
                os.remove('Current')
                os.symlink('./3.0godly', 'Current')
                os.chdir(cur)

def main():
    output_header(['Creating SwinGame Templates'])
    
    # Create the framework/dll
    create_sgsdk_library()
    create_lang_libraries()
    
    print("\nFinished!")
    
    
    
if __name__ == '__main__':
    main()
