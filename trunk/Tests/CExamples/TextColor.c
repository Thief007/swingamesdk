#include "SwinGame.h"

int main(int argc, char* argv[])
{
    open_audio();
    open_graphics_window("Colored text", 800, 600);
    load_default_colors();
	
	clear_screen();
	
	draw_text("Hello World!", ColorRed, 380, 280);
	draw_text("I'm Here!!!", ColorGreen, 380, 320);
	
	refresh_screen();
    
    delay(5000);

    release_all_resources();
    close_audio();
    return 0;
}
