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
        NSAutoreleasePool *loopPool = [[NSAutoreleasePool alloc] init];
        
        //Update game...
        [SGCore processEvents];
        
        //Draw game...
        [SGGraphics clearScreen];
        [SGText drawFrameRateWithSimpleFont: 0 :0];
        [SGCore refreshScreen];
        
        [loopPool drain];
    }
    
    [SGAudio closeAudio];
    [SGResources releaseAllResources];
    [appPool drain];
    return 0;
}
