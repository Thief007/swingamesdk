from sgsdk.sgsdk_types import *

options['debug'] = True # must be set before sgsdk import

from sgsdk.sgsdk import *
from sgsdk.sgsdk_resources import *

def SetGameResources():
    # sg_font/image/sound are globals in sgsdk_resources
    sg_font_list.append(("Courier", "cour.ttf", 16))
    sg_font_list.append(("CourierBigger", "cour.ttf", 25))
    sg_font_list.append(("ArialBig", "arial.ttf", 400))

    # sg_image_list.append(("ball", "ball.png"))

    # sg_sound_list.append(("Ball hit bat", "bosu06.wav"))

def Main():
    SetIcon('SwinGame.png')  
    OpenGraphicsWindow("Air Hockey", 800, 600)
    
    SetGameResources() # specific for this game
    LoadResources() # use sgsdk_resources to load/manage things

    clBlack = GetColor('black')
    clRed = GetColor('red')
    clGreen = GetColor('green')
    clBlue = GetColor('blue')
    clWhite = GetColor('white')

    while not IsKeyPressed(Keys.ESCAPE) and not WindowCloseRequested():
        ClearScreen(clBlack)
        DrawRectangle(clRed, -1, 20, 200, 200, 100)
        DrawRectangle(clGreen, -1, 220, 200, 200, 100)
        DrawRectangle(clBlue, -1, 420, 200, 200, 100)
        
        DrawText("Hello World", clRed, GameFont("Courier"), 20, 310)
        DrawText("Hello World", clGreen, GameFont("Courier"), 220, 310)
        DrawText("Hello World", clBlue, GameFont("Courier"), 420, 310)
        
        DrawFramerate(0, 0, GameFont("Courier"))
        
        DrawText("Hello World", clWhite, GameFont("ArialLarge"), 50, 50)
        
        RefreshScreenWithFrame(65)
        ProcessEvents()

    FreeResources()
    
    
#------------------------------------------------------------------------------

if __name__ == '__main__':    
    OpenAudio()    
    try:
        Main()
    finally:
        CloseAudio()
    
