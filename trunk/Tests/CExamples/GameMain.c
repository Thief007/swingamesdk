#include "SwinGame.h"

int main(int argc, char* argv[])
{
    open_audio();
    open_graphics_window("Put Project Title Here...", 800, 600);
    load_default_colors();
    
    delay(5000);

    release_all_resources();
    close_audio();
    return 0;
}
