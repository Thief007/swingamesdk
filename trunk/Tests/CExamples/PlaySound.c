#include "SwinGame.h"

int main(int argc, char* argv[])
{
    open_audio();
    open_graphics_window("Boom Boom", 800, 600);
    load_default_colors();
	
	clear_screen();
	
	load_sound_effect_named("boom", "boom.wav");
	
	play_sound_effect("boom");
	delay(500);	
	
	refresh_screen();
    
	play_sound_effect("boom");
    delay(500);

    release_all_resources();
    close_audio();
    return 0;
}
