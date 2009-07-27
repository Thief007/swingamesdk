#include "Colors.h"
#include "Core.h"

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

void loadDefaultColors()
{
    ColorBlue =         RGBAColor(0, 0, 255, 255);
    ColorGreen =        RGBAColor(0, 255, 0, 255);
    ColorRed =          RGBAColor(255, 0, 0, 255);
    ColorWhite =        RGBAColor(255, 255, 255, 255);
    ColorBlack =        RGBAColor(0, 0, 0, 255);
    ColorYellow =       RGBAColor(255, 255, 0, 255);
    ColorPink =         RGBAColor(255, 20, 147, 255);
    ColorTurquoise =    RGBAColor(0, 206, 209, 255);
    ColorGrey =         RGBAColor(128, 128, 128, 255);
    ColorMagenta =      RGBAColor(255, 0, 255, 255);
    ColorTransparent =  RGBAColor(0, 0, 0, 0);
    ColorLightGrey =    RGBAColor(200, 200, 200, 255);
}
