#import <stdbool.h>
#import <Foundation/NSAutoreleasePool.h>
#import <Foundation/NSString.h>
#import "SwinGame.h"

int main(int argc, const char* argv[])
{
    NSAutoreleasePool *appPool = [[NSAutoreleasePool alloc] init];
    [SGResources setAppPath:[NSString stringWithCString:argv[0] encoding:NSASCIIStringEncoding]];
    
    [SGAudio openAudio];
    [SGCore openGraphicsWindow:@"Hello World": 800: 600];
    [SGColors loadDefaultColors];
    
    while (![SGCore windowCloseRequested])
    {
        [SGCore processEvents];
        
        [SGGraphics clearScreen];
        
        [SGGraphics drawBitmap: [SGResources bitmapNamed:@"SplashBack"]: 0: 0];
        //FillRectangle(ColorWhite, 10, 10, 620, 460);
        
        [SGCore refreshScreen];
    }
    
    [SGAudio closeAudio];
    [SGResources releaseAllResources];
    [appPool drain];
    return 0;
}
