#include "SwinGame.h"

int main(int argc, char* argv[])
{
    open_audio();
    open_graphics_window("Import Bitmap", 800, 600);
    load_default_colors();
	
	clear_screen();
	
	load_bitmap_named("predator", "corpolupo1.png");
	draw_bitmap ("predator", 350, 100);
	
	refresh_screen();
    
    delay(5000);

    release_all_resources();
    close_audio();
    return 0;
}
