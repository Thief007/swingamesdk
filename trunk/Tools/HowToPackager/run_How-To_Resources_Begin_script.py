import os
import re
import shutil
_base_path = "../../CoreSDK/test"
_resource_base_path = "../../Dist/HowTo/"
_lang = [ {"lang": "Pascal", "template": "../../Dist/Pascal/FPC", "main file": "GameMain.pas"} ]
_load = ["LoadAnimationScriptNamed","LoadAnimationScript","LoadMusic","LoadMusicNamed","LoadSoundEffect","LoadSoundEffectNamed","LoadCharacter","LoadCharacterNamed","LoadBitmap","LoadBitmapWithTransparentColor","LoadFont","LoadFontNamed","LoadResourceBundle","LoadResourceBundleWithProgress","LoadResourceBundleNamed","LoadTransparentBitmap","LoadTransparentBitmapNamed"]
_pkg  = {"LoadAnimationScriptNamed":"animations","LoadAnimationScript":"animations","LoadResourceBundle":"bundles","LoadResourceBundleWithProgress":"bundles","LoadResourceBundleNamed":"bundles","LoadResourceBundle":"bundles","LoadResourceBundleWithProgress":"bundles","LoadResourceBundleNamed":"bundles","LoadMusic":"sounds","LoadMusicNamed":"sounds","LoadSoundEffect":"sounds","LoadSoundEffectNamed":"sounds","LoadCharacter":"images","LoadCharacterNamed":"images","LoadBitmap":"images","LoadBitmapWithTransparentColor":"images","LoadTransparentBitmap":"images","LoadTransparentBitmapNamed":"images","LoadFont":"fonts","LoadFontNamed":"fonts","LoadPanel":"panels",
}

def create_list():
    f = open("../../CoreSDK/test/HowToResources.txt",'w')
    dirList = os.listdir(_base_path)
    for fname in dirList:
        if fname[0] == "H":
            newFileName = fname.split('.')
            fileName = "* "+newFileName[0]
            f.write(fileName+'\n')
            print "  -- Opening "+fname
            how_to_file = open("../../CoreSDK/test/"+fname,'r')
            lines_in_pas_file = how_to_file.readlines()
            
            for line in lines_in_pas_file:
                word = re.findall(r"[A-Za-z_0-9.]+",line)
                if len(word) == 0: continue
                if word[0] in _load or (len(word) >= 2 and word[1] in _load):
                    print word
                    if word[0] in _pkg:
                        print _pkg[word[0]]+'/'+word[-1]
                        f.write(_pkg[word[0]]+'/'+word[-1]+'\n')
                    elif (len(word) >= 2 and word[1] in _pkg):
                        print _pkg[word[1]]+'/'+word[-1]
                        f.write(_pkg[word[1]]+'/'+word[-1]+'\n')
                        
    how_to_file.close()       
def main():
    create_list()
if __name__ == '__main__':
    main()