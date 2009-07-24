#import <stdbool.h>
#import <Foundation/NSAutoreleasePool.h>
#import <Foundation/NSString.h>
#import "SwinGame.h"

int main(int argc, const char* argv[])
{
    [SGResources setAppPath:[NSString stringWithCString:argv[0] encoding:NSASCIIStringEncoding] : true];
    
    [SGAudio openAudio];
    [SGCore openGraphicsWindow:@"Hello World": 800: 600];
    [SGColors loadDefaultColors];
    
    while (![SGCore windowCloseRequested])
    {
        [SGCore processEvents];
        
        [SGGraphics clearScreen];
        
        //FillRectangle(white, 10, 10, 620, 460);
        [SGGraphics drawBitmap: [SGResources getBitmap:@"SplashBack"]: 0: 0];
        
        [SGCore refreshScreen];
    }
    
    [SGResources releaseAllResources];
    [SGAudio closeAudio];
    return 0;
}
