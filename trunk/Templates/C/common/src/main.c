#include "Types.h"
#include "Audio.h"
#include "Graphics.h"
#include "Core.h"
#include "Text.h"

int main()
{
    OpenAudio();
    OpenGraphicsWindow("Hello World", 640, 480);
    //SoundEffect sound = LoadSoundEffect("./SwinGameStart.ogg");
    
    Color white = GetColor(255, 255, 255, 255);
    
    //PlaySoundEffect(sound);
    
    while (!WindowCloseRequested())
    {
        ClearScreen();
        
        FillRectangle(white, 10, 10, 620, 460);
        
        ProcessEvents();
        RefreshScreen();
    }
    
    //FreeSoundEffect(&sound);
    
    CloseAudio();
    return 0;
}