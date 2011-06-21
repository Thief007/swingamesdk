#import <stdbool.h>
#import <Foundation/Foundation.h>
#import "SwinGame.h"

int main()
{
    [SGAudio openAudio];
    [SGGraphics openGraphicsWindow:@"Hello World" 
                             width:800
                            height:600];
    [SGColors loadDefaultColors];
    
    while (![SGInput windowCloseRequested])
    {
        //Update game...
        [SGInput processEvents];
        
        //Draw game...
        [SGGraphics clearScreen];
        [SGText drawFramerateAtX:0 y:0];
        [SGGraphics refreshScreen];
    }
    
    [SGAudio closeAudio];
    [SGResources releaseAllResources];
    return 0;
}
