import os
import platform
import subprocess
import swin_shutil

sg_version         ="3.0 Beta"


script_path        = os.path.dirname(os.path.realpath(__file__)) + '/'
swingame_path      = os.path.realpath(script_path + '../..') + '/'
python_script_dir  = os.path.realpath(script_path + '../SGWrapperGen') + '/'

dist_folder        = swingame_path + "Dist/"
generated_folder   = swingame_path + "Generated/"
tempate_folder     = swingame_path + "Templates/"

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
    proc = subprocess.Popen(["python", python_script_dir + script_name], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
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

# ======================================================================
# = Language specific function called within the Template details dict =
# ======================================================================

def build_csharp_lib():
    """The C# code has been generated, now turn it into a DLL"""
    output_line('Compiling .NET class library')

    dirs = {
        'cs_generated_lib_dir':     os.path.join(swingame_path, 'Generated','CSharp','lib'),
        'cs_generated_code_dir':    os.path.join(swingame_path, 'Generated','CSharp','Code'),
        'cs_lib_dir':               os.path.join(swingame_path, 'Templates','CSharp','Library')
    }


    if get_os_name() == "Windows":
        csc = ['csc', '-t:library', '-r:System.dll', '-r:System.Drawing.dll', '-define:DEBUG', '-debug+', '-out:%(cs_generated_lib_dir)s\\SwinGame.dll' % dirs, '%(cs_lib_dir)s\\*.cs' % dirs, '%(cs_generated_code_dir)s\\*.cs' % dirs]
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

def pkg_csharp_installer(tmp_dir, to_dir):
    """Package up the SwinGame C# installer"""
    
    output_line('Creating Visual Studio Template Structure')
    
    # Replace 'MyGame' with '$safeprojectname$.src' in GameMain.cs
    o = open("NewGameMain.cs","a") #open for append
    game_main = os.path.join(to_dir, 'src', 'GameMain.cs')
    for line in open(game_main):
       line = line.replace("MyGame","$safeprojectname$.src")
       o.write(line) 
    o.close()
    run_bash('mv', ['NewGameMain.cs', game_main] )
    
    tmp_vs_dir = os.path.join(tmp_dir, 'Visual Studio') + '/'
    
    # Make the Visual Studio directory
    os.mkdir(tmp_vs_dir)
    # Copy in template files
    copy_without_svn(os.path.join(tempate_folder, 'Visual Studio', 'Express C# 08'), tmp_vs_dir)
    
    # Create the project zip
    os.chdir(to_dir)
    to_zip = tmp_vs_dir + 'SwinGame C# Project.zip'
    run_bash('zip', ['-q', '-r', '-y', to_zip, '.', '-x', '.DS_Store' ])
    
    # Zip it all together
    os.chdir(tmp_vs_dir)
    to_zip = dist_folder + 'SwinGame %s C# Template Installer.vsi' % sg_version
    output_line('Creating Template Installer for C#')
    run_bash('zip', ['-q', '-r', '-y', to_zip, '.', '-x', '.DS_Store' ])

# ===============================
# = Template details dictionary =
# ===============================


template_details = {
    'Pascal':   {
            'script':       'create_pascal_library.py',
            'use_sgsdk':    False,
            'copy_dist':    [
                { 
                  'target':         'fpc',
                  'os':             ['Mac OS X', 'Windows', 'Linux'],
                  'lib':            'lib',
                },
                {
                  'target':     'iOS',
                  'os':         ['Mac OS X'],
                  'lib':        'staticlib',
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
                  'lib':        'lib',
                  'staticsgsdk':    False,
                },
                { 
                  'target':     'gcc',
                  'os':         ['Mac OS X', 'Windows', 'Linux'],
                  'lib':        'lib',
                  'staticsgsdk':    False,
                },
                { 
                  'target':     'xcode 3',
                  'os':         ['Mac OS X'],
                  'lib':        'lib',
                  'staticsgsdk':    False,
                },
                { 
                  'target':     'iOS',
                  'os':         ['Mac OS X'],
                  'lib':        'staticlib',
                  'staticsgsdk':    True,
                },
            ],
            'pre_copy_script': None,
        },
    'ObjC': {
            'script':       'create_objc_library.py',
            'use_sgsdk':    True,
            'copy_dist':    [
                {
                    'target':       'gcc',
                    'os':           [ 'Mac OS X' ],
                    'lib':          'lib',
                    'staticsgsdk':  False,
                },
                {
                    'target':       'xcode 3',
                    'os':           [ 'Mac OS X' ],
                    'lib':          'lib',
                    'staticsgsdk':  False,
                },
            ],
            'pre_copy_script': None,
         },
    'CSharp':   {
            'script':       'create_csharp_library.py',
            'use_sgsdk':    True,
            'copy_dist':    [
                { 
                  'target':         'mono',
                  'os':             [ 'Mac OS X', 'Linux' ],
                  'lib':            'lib',
                  'staticsgsdk':    False,
                },
                { 
                  'target':         'vs08',
                  'os':             [ 'Windows' ],
                  'lib':            'lib/win',
                  'staticsgsdk':    False,
                  'pkg_script':     pkg_csharp_installer,
                },
            ],
            'pre_copy_script': build_csharp_lib,
        },
} # end _template_details

def deploy_list():
    """Returns a list of the files that need to be deployed to the server"""
    
    result = list()
    for key, lang_dict in template_details.items():
        for dist_dict in lang_dict['copy_dist']:
            result.append(dist_folder + dist_dict['template_path_name'] + '.zip')
    
    return result

def _setup_template_details():
    for key, lang_dict in template_details.items():
        for dist_dict in lang_dict['copy_dist']:
            dist_dict['template_name'] = '%s SwinGame %s %s' % (key, sg_version, str.upper(dist_dict['target']) )
            dist_dict['template_path_name'] = dist_dict['template_name'].replace(' ', '_').replace('.', '_')
    
    
    
_setup_template_details()

if __name__ == '__main__':
    print template_details
    
