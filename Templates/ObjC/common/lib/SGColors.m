#import <Foundation/NSObject.h>

#import "SGColors.h"
#import "SGCore.h"

Color ColorBlue;
Color ColorGreen;
Color ColorRed;
Color ColorWhite;
Color ColorBlack;
Color ColorYellow;
Color ColorPink;
Color ColorTurquoise;
Color ColorGrey;
Color ColorMagenta;
Color ColorTransparent;
Color ColorLightGrey;

@implementation SGColors: NSObject

+ (void)loadDefaultColors
{
    ColorBlue =         [SGCore rGBAColor:0 :0 :255 :255];
    ColorGreen =        [SGCore rGBAColor:0 :255 :0 :255];
    ColorRed =          [SGCore rGBAColor:255 :0 :0 :255];
    ColorWhite =        [SGCore rGBAColor:255 :255 :255 :255];
    ColorBlack =        [SGCore rGBAColor:0 :0 :0 :255];
    ColorYellow =       [SGCore rGBAColor:255 :255 :0 :255];
    ColorPink =         [SGCore rGBAColor:255 :20 :147 :255];
    ColorTurquoise =    [SGCore rGBAColor:0 :206 :209 :255];
    ColorGrey =         [SGCore rGBAColor:128 :128 :128 :255];
    ColorMagenta =      [SGCore rGBAColor:255 :0 :255 :255];
    ColorTransparent =  [SGCore rGBAColor:0 :0 :0 :0];
    ColorLightGrey =    [SGCore rGBAColor:200 :200 :200 :255];
}

@end
