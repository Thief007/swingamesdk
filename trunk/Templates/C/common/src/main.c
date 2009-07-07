#include <stdio.h>
#include <stdbool.h>
#include "Types.h"
#include "Audio.h"
#include "Graphics.h"
#include "Core.h"
#include "Text.h"
#include "Resources.h"

#define True 1

int main(int argc, char* argv[])
{
    SetAppPath(argv[0], True);
    OpenAudio();
    OpenGraphicsWindow("Hello World", 800, 600);
    //SoundEffect sound = LoadSoundEffect("./SwinGameStart.ogg");
    
    Color white = GetColor(255, 255, 255, 255);
    
    //PlaySoundEffect(sound);
    
    while (!WindowCloseRequested())
    {
        ClearScreen();
        
        //FillRectangle(white, 10, 10, 620, 460);
        DrawBitmap(GetBitmap("SplashBack"), 0, 0);
        
        ProcessEvents();
        RefreshScreen();
    }
    
    //FreeSoundEffect(&sound);
    
    ReleaseAllResources();
    
    CloseAudio();
    return 0;
}
