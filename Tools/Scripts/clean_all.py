import sys
import os
import platform
import subprocess
import swin_shutil
from swin_template_utils import *

def main():
    output_header(['Cleaning SwinGame Template Dist Locations'])
    
    clean_locs = [
            dist_folder + "Source/",
        ]
        
    for key, dist in template_details.items():
        clean_locs.append(dist_folder + key)
    
    for loc in clean_locs:
        run_bash('rm', ['-rf', loc] )
    
    print("\nFinished!")
    
    
    
if __name__ == '__main__':
    main()
