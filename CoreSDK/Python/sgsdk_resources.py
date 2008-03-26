''' Exposes utility methods to help with resource management. Also provides
    the SwinGame loading screen. '''

#    procedure LoadResources();
#    procedure FreeResources();
#    function GameFont(fontName): Font;
#    function GameImage(imageName): Bitmap;
#    function GameSound(soundName): SoundEffect;
#    function GameMusic(musicName): Music;
#    function GameMap(mapName): Map;


from sgsdk import *
from sgsdk_types import *

# note - sgsdk bases its path from the dll location..
import os
_base = os.path.abspath('') #+"/sgsdk/"
#print 'BASE IS ', _base


# Used by different apps to provide a list of resources they want managed
# using tuples (name,filename,...) as needed for each resource type 
sg_image_list = [] # (name,filename)
sg_font_list = [] # (name,filename,size)
sg_sound_list = [] # (name,filename)
sg_music_list = [] # (name,filename)
sg_map_list = [] # (filename)

# managed lists of resources - used to free them later.
_Images = [] # Array of Bitmap
_Fonts = [] # Array of Font
_Sounds = [] # Array of SoundEffect
_Music = [] # Array of Music
_Maps = [] # Array of Map




# small list of "known" colour names for use with GetColor('name') 
color_names = {'white':(255, 255, 255, 255), 
               'green':(0, 255, 0, 255), 
               'blue':(0, 0, 255, 255), 
               'black':(0, 0, 0, 255), 
               'red':(255, 0, 0, 255), 
               'yellow':(255, 255, 0, 255), 
               'pink':(255, 20, 147, 255), 
               'turquoise':(0, 206, 209, 255), 
               'grey':(128, 128, 128, 255), 
               'magenta':(255, 0, 255, 255), 
               'transparent':(0, 0, 0, 0) }

def GetColor(*args):
    ''' Gets a sgsdk color value using either a single parameter 'name' string 
        or the full set of (r,g,b,a) values. '''
    if len(args) == 1: # eg. 'red'
        return GetColourRGBA(*color_names[args[0]])
    else: # all 4 c_byte values used
        return GetColourRGBA(*args)

def LoadBitmap(filename):
    ''' Map to sgsdk function using default values.'''
    return LoadBitmapWithTransparentColor(filename, sgFalse, GetColor('transparent'))

def PlaySoundEffect(sound):
    ''' Map to sgsdk function. Play a sound once (does not loop). '''
    return PlaySoundEffectLoop(sound, 0)
    
def GetPathToResource(filename, restype):
    ''' Convenience method to access standard resource files based on type. '''
    return GetPathToResourceWithBaseAndKind(_base, filename, restype)

#------------------------------------------------------------------------------

def GameFont(fontName): # string, return Font
    ''' Get the Font you created with name font.
        PARAMETERS:
        - fontName: name of the font to get
        RETURNS:
        - the font
        EXCEPT:
        - if the font isn't found an error is returned '''
    for name, font in _Fonts:
        if name == fontName:
            return font
    # fall-though - no font by that name so raise
    raise KeyError, 'Font ' + fontName + ' does not exist...'


def GameImage(imageName): # string, return Bitmap
    ''' Get the image you created with name image.
        PARAMETERS:
        - image: name of the image to get
        RETURNS:
        - the image
        EXCEPT:
        - if the image isn't found an error is returned '''
    for name, image in _Images:
        if name == imageName:
            return image
    raise KeyError, 'Image ' + imageName + ' does not exist...'


def GameSound(soundName): # string, return SoundEffect
    ''' Get the soundeffect you created with name sound.
        PARAMETERS:
        - sound: name of the soundeffect to get
        RETURNS:
        - the soundeffect
        EXCEPT:
        - if the soundeffect isn't found an error is returned '''
    for name, sound in _Sounds:
        if name == soundName:
            return sound
    raise KeyError, 'Sound ' + soundName + ' does not exist...'

def GameMap(mapName): # String, returns Map
    ''' Get the map you created with name mapName.
        PARAMETERS:
        - mapName: name of the map to get
        RETURNS:
        - the map
        EXCEPT:
        - if the map isn't found an error is returned '''
    for name, map in _Maps:
        if name == mapName:
            return map
    raise KeyError, 'Map ' + mapName + ' does not exist...'

def GameMusic(musicName): # string, returns Music
    ''' Get the music you created with name music.
        PARAMETERS:
        - music: name of the music to get
        RETURNS:
        - the music
        EXCEPT:
        - if the music isn't found an error is returned '''
    for name, music in _Music:
        if name == musicName:
            return music
    raise KeyError, 'Music ' + musicName + ' does not exist...'

#------------------------------------------------------------------------------

def LoadResources():
    ''' Call this to load your resources and to
        display the loading screen.
        SIDE EFFECTS: '''
    oldW = ScreenWidth()
    oldH = ScreenHeight()

    ChangeScreenSize(800, 600)
    
    #Remove sleeps once "real" game resources are being loaded
    _ShowLoadingScreen()
    _ShowMessage('loading fonts', 0)
    _LoadManagedFonts()
    Sleep(100)

    _ShowMessage('loading images', 1)
    _LoadManagedImages()
    Sleep(100)

    _ShowMessage('loading sounds', 2)
    _LoadManagedSounds()
    Sleep(100)

    _ShowMessage('loading music', 3)
    _LoadManagedMusics()
    Sleep(100)

    _ShowMessage('loading maps', 4)
    _LoadManagedMaps()
    Sleep(100)

    _ShowMessage('SwinGame loaded', 5)
    Sleep(100)
    
    _EndLoadingScreen()

    ChangeScreenSize(oldW, oldH)


def FreeResources():
    ''' Free all of the resources that you have loaded for your game. This 
        should be called at the end of the program. 
        SIDE EFFECTS:
        - Frees all loaded Fonts, Images, Music, Sound, and Maps '''
    _FreeManagedFonts()
    _FreeManagedImages()
    _FreeManagedMusics()
    _FreeManagedSounds()
    _FreeManagedMaps()

#------------------------------------------------------------------------------

def _NewMap(mapName): # string
    ''' Creates a new Mappy Map from mapFile file.
        PARAMETERS:
         - mapName: The name of the map to load
        SIDE EFFECTS:
        - Loads the map from file into _Maps '''
    newMap = LoadMap(mapName)
    _Maps.append((mapName,newMap))

def _NewFont(fontName, fileName, size): # string, string, int
    ''' Creates a new font.
        PARAMETERS:
        - fontName: name to call font in _Fonts. Used when you access the font.
        - fileName: name of the font file to load. Must be in resources/fonts
        - size: Size of font to load
        SIDE EFFECTS:
        - Loads the font from file into _Fonts'''
    newFont = LoadFont(GetPathToResource(fileName, rkFont), size)
    _Fonts.append((fontName,newFont))

def _NewImage(imageName, fileName): # string string
    ''' Creates a new image.
        PARAMETERS:
        - imageName: name to call image in _images. Used when you access the image.
        - fileName: name of the image file to load. Must be in resources/images
        SIDE EFFECTS:
        - Loads the image from file into _Images '''        
    newImage = LoadBitmap(GetPathToResource(fileName, rkImage))
    _Images.append((imageName,newImage))

def _NewTransparentColourImage(imageName, fileName, transColour): # string string colour
    ''' Creates a new image with transparent key colour.
        PARAMETERS:
        - imageName: name to call image in _images. Used when you access the image.
        - fileName: name of the image file to load. Must be in resources/images
        - transColour: colour of a pixel to be transparent
        SIDE EFFECTS:
        - Loads the image from file into _Images with transparent key colour '''
    newImage = LoadBitmap(GetPathToResource(fileName, rkImage), true, transColour)
    _Images.append((imagename,newImage))

def _NewTransparentColorImage(imageName, fileName, transColor): # string string colour
    ''' Creates a new image with transparent key color.
        PARAMETERS:
        - imageName: name to call image in _images. Used when you access the image.
        - fileName: name of the image file to load. Must be in resources/images
        - transColor: color of a pixel to be transparent
        SIDE EFFECTS:
        - Loads the image from file into _Images with transparent key color '''
    _NewTransparentColourImage(imageName, fileName, transColor)


def _NewSound(soundName, fileName): # string string
    ''' Creates a new sound.
        PARAMETERS:
        - soundName: name to call sound in _sounds. Used when you access the sound.
        - fileName: name of the sound file to load. Must be in resources/sounds
        SIDE EFFECTS:
        - Loads the sound from file into _sounds '''
    newSound = LoadSoundEffect(GetPathToResource(fileName, rkSound))
    _Sounds.append((soundName,newSound))

def _NewMusic(musicName, fileName): # string string
    ''' Creates a new music.
        PARAMETERS:
        - musicName: name to call music in _musics. Used when you access the music.
        - fileName: name of the music file to load. Must be in resources/musics
        SIDE EFFECTS:
        - Loads the music from file into _musics '''
    newMusic = LoadMusic(GetPathToResource(fileName, rkSound))
    _Music.append((musicName,newMusic))

#------------------------------------------------------------------------------

def _LoadManagedFonts():
    ''' Load the fonts you need for your game as stored in sg_font_list
        [('name1','filename1',size)('name2','filename2',size) ... ]
        SIDE EFFECTS:
        - Loads the game's fonts '''
    _NewFont('ArialLarge', 'arial.ttf', 80)
    _NewFont('Courier', 'cour.ttf', 16)
    for args in sg_font_list:
        _NewFont(*args)

def _LoadManagedImages():
    ''' Load the images you need for your game  as stored in sg_image_list
        [('name1','filename1')('name2','filename2') ... ]
        SIDE EFFECTS:
        - Loads the game's images '''
    for args in sg_image_list:
        _NewImage(*args)

def _LoadManagedSounds():
    ''' Load the soundeffects you need for your game as stored in sg_sound_list
        [('name1','filename1')('name2','filename2') ... ]
        SIDE EFFECTS:
        - Loads the game's soundeffects '''
    for args in sg_sound_list:
        _NewSound(*args)

def _LoadManagedMusics():
    ''' Load the music you need for your game as stored in sg_music_list
        [('name1','filename1')('name2','filename2') ... ]
        SIDE EFFECTS:
        - Loads the game's music '''
    for args in sg_music_list:
        _NewMusic(*args)

def _LoadManagedMaps():
    ''' Load the maps you need for your game, as stored in sg_map_list
        [('filename'),('filename2'), ...]
        SIDE EFFECTS:
        - Loads the game's maps '''
    for args in sg_map_list:
        _NewMap(*args)

#------------------------------------------------------------------------------

def _FreeManagedFonts():
    ''' Frees the fonts that you have loaded.
        SIDE EFFECTS:
        - Frees the game's fonts '''
    global _Fonts
    for name, font in _Fonts:
        FreeFont(font)
    _Fonts = []

def _FreeManagedImages():
    ''' Frees the images that you have loaded.
        SIDE EFFECTS:
        - Frees the game's images '''
    global _Images
    for name, image in _Images:
        FreeBitmap(image)
    _Images = []

def _FreeManagedSounds():
    ''' Frees the images that you have loaded.
        SIDE EFFECTS:
        - Frees the game's images '''
    global _Sounds
    for name, sound in _Sounds:
        FreeSoundEffect(sound)
    _Sounds = []   

def _FreeManagedMusics():
    ''' Frees the music that you have loaded.
        SIDE EFFECTS:
        - Frees the game's music
        - Stops playing any music '''
    global _Music
    StopMusic()
    Sleep(100)
    for name, music in _Music:
        FreeMusic(music)
    _Music = []    

def _FreeManagedMaps():
    ''' Frees the maps that you have loaded.
        SIDE EFFECTS:
        - Frees the game's maps '''
    global _Maps
    for name, map in _Maps:
        FreeMap(map)
    _Maps = []   

#------------------------------------------------------------------------------

# used internally for the SwinGame loader screen
_Background = None # Bitmap
_Animation = None # Bitmap
_LoaderEmpty = None # Bitmap
_LoaderFull = None # Bitmap
_LoadingFont = None #: Font
_StartSound = None #: SoundEffect

def _PlaySwinGameIntro():
    ''' Plays the SwinGame intro. Please leave this in all SwinGames.
        SIDE EFFECTS:
        - Plays the starting sound
        - Draws the background and animation
        - Refreshes screen '''
    ANI_X, ANI_Y = 143, 134
    ANI_W, ANI_H = 546, 327
    ANI_V_CELL_COUNT = 6
    ANI_CELL_COUNT = 11
    PlaySoundEffect(_StartSound)
    Sleep(200)
    for i in range(ANI_CELL_COUNT):
        DrawBitmap(_Background, 0, 0)
        DrawBitmapPart(_Animation, 
                       (i // ANI_V_CELL_COUNT) * ANI_W, 
                       (i % ANI_V_CELL_COUNT) * ANI_H,
                       ANI_W, ANI_H, ANI_X, ANI_Y)
        RefreshScreen()
        ProcessEvents()
        Sleep(20)
    Sleep(1500)

def _ShowLoadingScreen():
    ''' Loads the resourced needed to show the loading screen,
        and plays the intro.
        SIDE EFFECTS:
        - Loads _Background, _Animation, _LoadingFont, and _StartSound
        - Plays Intro '''
    global _Background
    global _Animation, _LoaderEmpty, _LoaderFull, _LoadingFont, _StartSound
    
    _Background = LoadBitmap(GetPathToResource('SplashBack.png', rkImage))
    
    DrawBitmap(_Background, 0, 0)
    RefreshScreen(60)
    ProcessEvents()
    
    _Animation = LoadBitmap(GetPathToResource('SwinGameAni.png', rkImage))
    _LoaderEmpty = LoadBitmap(GetPathToResource('loader_empty.png', rkImage))
    _LoaderFull = LoadBitmap(GetPathToResource('loader_full.png', rkImage))
    _LoadingFont = LoadFont(GetPathToResource('arial.ttf', rkFont), 12)
    _StartSound = LoadSoundEffect(GetPathToResource('SwinGameStart.ogg', rkSound))

    _PlaySwinGameIntro()

def _ShowMessage(message, number): # string, int
    ''' This plays the "Loading ..." messages while your
        resources are being loaded.
        SIDE EFFECTS:
        - Draws text to the screen
        - Refreshes screen '''
    TX, TY = 310, 493 
    TW, TH = 200, 25
    STEPS = 5
    BG_X, BG_Y = 279, 453
    fullW = (260 * number) // STEPS # div
    
    DrawBitmap(_LoaderEmpty, BG_X, BG_Y)
    DrawBitmapPart(_LoaderFull, 0, 0, fullW, 66, BG_X, BG_Y)
    DrawTextLines(message, GetColor('white'), GetColor('transparent'), _LoadingFont, faCenter, TX, TY, TW, TH)
    RefreshScreen(60)
    ProcessEvents()    
    
def _EndLoadingScreen():
    ''' Ends the loading screen, and frees the set surfaces.
        SIDE EFFECTS:
        - Clears the screen
        - Frees _LoadingFont, _Background, _Animation, and _StartSound '''
    ProcessEvents()
    Sleep(500)
    ClearScreen(GetColor('black'))
    RefreshScreen(60)
    FreeFont(_LoadingFont)
    FreeBitmap(_Background)
    FreeBitmap(_Animation)
    FreeBitmap(_LoaderEmpty)
    FreeBitmap(_LoaderFull)
    FreeSoundEffect(_StartSound)


