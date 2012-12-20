#include <stdio.h>
#include "SwinGame.h"

int main()
{
    open_audio();
    open_graphics_window("Hello World", 800, 600);
    load_default_colors();
    show_swin_game_splash_screen();
    
    do
    {
        process_events();
        
        clear_screen(ColorWhite);
        
        draw_framerate(0,0);
        
        refresh_screen();
    } while ( ! window_close_requested() );
    
    close_audio();
    
    release_all_resources();
    return 0;
}
