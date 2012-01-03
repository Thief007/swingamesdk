#include "SwinGame.h"

int main(int argc, char* argv[])
{
    open_audio();
    open_graphics_window("Primitive shapes", 800, 600);
    load_default_colors();
	
	clear_screen();
	
	draw_rectangle(ColorGreen, 25, 100, 200, 100 );
	fill_rectangle(ColorGreen, 25, 400, 200, 100 );
	
	draw_triangle(ColorYellow, 275, 200, 475, 200, 375, 100);
	fill_triangle(ColorYellow, 275, 500, 475, 500, 375, 400);
	
	draw_circle(ColorWhite, 625, 150, 100);
	
	refresh_screen();
    
    delay(5000);

    release_all_resources();
    close_audio();
    return 0;
}
