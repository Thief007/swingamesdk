import os
import sys
import platform
import zipfile
import subprocess
import swin_shutil

from swin_template_utils import *
from bundle_templates import *

tmp_dir = dist_folder + 'tmp/'

def main():
    output_header(['Creating Template Files'])
    
    
    if os.path.exists(tmp_dir):
        swin_shutil.rmtree(tmp_dir)
    
    os.mkdir(tmp_dir)
    os.chdir(dist_folder)
    
    for key, lang_template_dict in template_details.items():
        output_header(['Packaging %s Templates' % key])
        
        for copy_dist in lang_template_dict['copy_dist']:
            output_line("Packaging " + copy_dist["target"])
            
            langdist_folder =       swingame_path + "Dist/%s/" % key
            specificdist_folder =   langdist_folder + copy_dist['target'] + '/'
            
            if not os.path.exists(specificdist_folder):
                print >> sys.stderr, ' !! Missing Template: ', specificdist_folder, ' !!'
                continue
            
            base_dir = tmp_dir + copy_dist['template_path_name'] + '/'
            to_dir = base_dir + 'ProjectTemplate'
            to_zip = dist_folder + copy_dist['template_path_name'] + '.zip'
            
            swin_shutil.copytree(specificdist_folder, to_dir, symlinks=True)
            
            os.chdir(base_dir)
            run_bash('zip', ['-q', '-r', '-y', to_zip, '.', '-x', '.DS_Store' ])
            
            if copy_dist.has_key('pkg_script'):
                copy_dist['pkg_script'](tmp_dir, to_dir)
    
    #swin_shutil.rmtree(tmp_dir)
    print("\nFinished!")
    
    
    
if __name__ == '__main__':
    main()
