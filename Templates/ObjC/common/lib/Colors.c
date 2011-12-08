#include "Colors.h"
#include "Graphics.h"

color ColorBlue;
color ColorGreen;
color ColorRed;
color ColorWhite;
color ColorBlack;
color ColorYellow;
color ColorPink;
color ColorTurquoise;
color ColorGrey;
color ColorMagenta;
color ColorTransparent;
color ColorLightGrey;

void load_default_colors()
{
    ColorBlue =         rgbacolor(0, 0, 255, 255);
    ColorGreen =        rgbacolor(0, 255, 0, 255);
    ColorRed =          rgbacolor(255, 0, 0, 255);
    ColorWhite =        rgbacolor(255, 255, 255, 255);
    ColorBlack =        rgbacolor(0, 0, 0, 255);
    ColorYellow =       rgbacolor(255, 255, 0, 255);
    ColorPink =         rgbacolor(255, 20, 147, 255);
    ColorTurquoise =    rgbacolor(0, 206, 209, 255);
    ColorGrey =         rgbacolor(128, 128, 128, 255);
    ColorMagenta =      rgbacolor(255, 0, 255, 255);
    ColorTransparent =  rgbacolor(0, 0, 0, 0);
    ColorLightGrey =    rgbacolor(200, 200, 200, 255);
}
