import os
import re
import shutil
_base_path = "../../CoreSDK/test"
_resource_base_path = "../../Dist/HowTo/"
_lang = [ {"lang": "Pascal", "template": "../../Dist/Pascal/FPC", "main file": "GameMain.pas"} ]
_load = ["LoadAnimationScriptNamed","LoadAnimationScript","LoadMusic","LoadMusicNamed","LoadSoundEffect","LoadSoundEffectNamed","LoadCharacter","LoadCharacterNamed","LoadBitmap","LoadBitmapWithTransparentColor","LoadBitmapNamed","LoadFont","LoadFontNamed","LoadResourceBundle","LoadResourceBundleWithProgress","LoadResourceBundleNamed","LoadTransparentBitmap","LoadTransparentBitmapNamed"]
_pkg  = {"TIMER":"timer","ANIM":"animations","LoadAnimationScriptNamed":"animations","LoadAnimationScript":"animations","LoadResourceBundle":"bundles","LoadResourceBundleWithProgress":"bundles","LoadResourceBundleNamed":"bundles","LoadResourceBundle":"bundles","LoadResourceBundleWithProgress":"bundles","LoadResourceBundleNamed":"bundles","LoadMusic":"sounds","LoadMusicNamed":"sounds","LoadSoundEffect":"sounds","BITMAP":"images","SOUND":"sounds","MUSIC":"sounds","LoadSoundEffectNamed":"sounds","LoadCharacter":"images","CHARACTER":"images","LoadCharacterNamed":"images","LoadBitmapNamed":"images","LoadBitmap":"images","LoadBitmapWithTransparentColor":"images","LoadTransparentBitmap":"images","LoadTransparentBitmapNamed":"images","FONT":"fonts","MAP":"maps","LoadFont":"fonts","LoadFontNamed":"fonts","PANEL":"panels","LoadPanel":"panels"}
_res_base_path = "../../CoreSDK/test/Resources/"

def get_Resources(word,f, _pkg):
    print "  --> "+_pkg[word[0]]+'/'+word[-1]
    f.write(_pkg[word[0]]+'/'+word[-1]+'\n')
    
    if _pkg[word[0]] == 'bundles':
        print "Bundled word "+_pkg[word[0]]
        load_Bundled_Resources(word, f, _pkg)

def get_Resources_Next(word, f, _pkg):
    print _pkg[word[1]]+'/'+word[-1]
    f.write(_pkg[word[1]]+'/'+word[-1]+'\n') 

def load_Bundled_Resources(word,f, _pkg):
    bundle_file =  _res_base_path+""+ _pkg[word[0]] +"/"+word[-1]
    bun = open(bundle_file,'r')
    lines_in_bundles_file = bun.readlines()
    for line in lines_in_bundles_file:
        word = re.findall(r"[A-Za-z_.-]+",line)
        if _pkg[word[0]] == "timer":
            print "TEST word"+_pkg[word[0]]
            print "TIMER FOUND -- passing"
        else:
            f.write(_pkg[word[0]].lower()+ "/" + word[-1]+'\n')
            print _pkg[word[0]].lower()+ "/" + word[-1]
        
             
def create_list():
    f = open("../../CoreSDK/test/HowToResources.txt",'w')
    dirList = os.listdir(_base_path)
    for fname in dirList:
        if fname == "HelloWorld.pas":continue
        if fname[0] == "H":
            newFileName = fname.split('.')
            fileName = "* "+newFileName[0]
            f.write(fileName+'\n')
            print " --Reading :"+fname
            how_to_file = open("../../CoreSDK/test/"+fname,'r')
            lines_in_pas_file = how_to_file.readlines()
            
            for line in lines_in_pas_file:
                word = re.findall(r"[A-Za-z_0-9.-]+",line)
                if len(word) == 0: continue        
                if word[0] in _load or (len(word) >= 2 and word[1] in _load):
                    if word[0] in _pkg:
                        get_Resources(word,f,_pkg)
                    elif (len(word) >= 2 and word[1] in _pkg):
                        get_Resources_Next(word,f,_pkg)
                        how_to_file.close()
                    
           
def main():
    create_list()
if __name__ == '__main__':
    main()