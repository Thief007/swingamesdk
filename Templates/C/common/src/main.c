#include <stdio.h>
#include <stdbool.h>
#include "SwinGame.h"

int main(int argc, char* argv[])
{
    set_app_path(argv[0]);
    open_audio();
    open_graphics_window("Hello World", 800, 600);
    load_default_colors();
    
    do
    {
        process_events();
        
        clear_screen();
        
        //FillRectangle(ColorWhite, 10, 10, 620, 460);
        draw_bitmap(bitmap_named("SplashBack"), 0, 0);
        
        refresh_screen();
    } while ( ! window_close_requested());
    
    close_audio();
    
    release_all_resources();
    return 0;
}
