import os
import sys
import platform
import zipfile
import subprocess
import swin_shutil

from swin_template_utils import *
from bundle_templates import *

tmp_dir = dist_folder + 'tmp/'

def zip_template(target, lang, template_path_name, pkg_script = None, part_from = None):
    """docstring for fname"""
    
    output_line("Packaging " + target)
    
    if lang:
        langdist_folder = dist_folder + "%s/" % (lang if not part_from else part_from)
    else:
        langdist_folder = dist_folder
    
    specificdist_folder =   langdist_folder + target + '/'
    
    if not os.path.exists(specificdist_folder):
        print >> sys.stderr, ' !! Missing Template: ', specificdist_folder, ' !!'
        return
    
    base_dir = tmp_dir + template_path_name + '/'
    to_dir = base_dir + 'ProjectTemplate'
    to_zip = dist_folder + template_path_name + '.zip'
    
    # print specificdist_folder, ' -> ', to_dir
    
    swin_shutil.copytree(specificdist_folder, to_dir, symlinks=True)
    
    os.chdir(base_dir)
    run_bash('zip', ['-q', '-r', '-y', to_zip, '.', '-x', '.DS_Store' ])
    
    if pkg_script:
        pkg_script(tmp_dir, to_dir)

def main():
    output_header(['Creating Template Files'])
    
    if os.path.exists(tmp_dir):
        swin_shutil.rmtree(tmp_dir)
    
    os.mkdir(tmp_dir)
    os.chdir(dist_folder)
    
    #hack...
    src_temp_path_name = 'Source of SwinGame %s' % sg_version
    src_temp_path_name = src_temp_path_name.replace(' ', '_').replace('.', '_')
    
    
    zip_template('Source', None, src_temp_path_name)
    
    for key, lang_template_dict in template_details.items():
        output_header(['Packaging %s Templates' % key])
        
        for copy_dist in lang_template_dict['copy_dist']:
            zip_template(copy_dist["target"], key, copy_dist['template_path_name'], 
                        copy_dist['pkg_script'] if copy_dist.has_key('pkg_script') else None, 
                        None if not copy_dist.has_key('lang') else copy_dist['lang'])
    
    #swin_shutil.rmtree(tmp_dir)
    print("\nFinished!")
    
    
    
if __name__ == '__main__':
    main()
