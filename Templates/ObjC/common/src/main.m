#import <stdbool.h>
#import <Foundation/NSAutoreleasePool.h>
#import "SwinGame.h"

int main(int argc, const char* argv[])
{
    SetAppPath(argv[0], true);
    OpenAudio();
    OpenGraphicsWindow("Hello World", 800, 600);
    //SoundEffect sound = LoadSoundEffect("./SwinGameStart.ogg");
    
    Color white = RGBAColor(255, 255, 255, 255);
    
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
