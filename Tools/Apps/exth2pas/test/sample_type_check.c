#include <stdio.h>
#include "SDL/SDL.h"

void PascalCheckEnum_SDL_EventType(int *ptr);


void c_check_enum_SDL_EventType()
{
  int data[38];
  data[0] = sizeof(SDL_EventType);
  data[1] = SDL_FIRSTEVENT;
  data[2] = SDL_QUIT;
  data[3] = SDL_WINDOWEVENT;
  data[4] = SDL_SYSWMEVENT;
  data[5] = SDL_KEYDOWN;
  data[6] = SDL_KEYUP;
  data[7] = SDL_TEXTEDITING;
  data[8] = SDL_TEXTINPUT;
  data[9] = SDL_MOUSEMOTION;
  data[10] = SDL_MOUSEBUTTONDOWN;
  data[11] = SDL_MOUSEBUTTONUP;
  data[12] = SDL_MOUSEWHEEL;
  data[13] = SDL_INPUTMOTION;
  data[14] = SDL_INPUTBUTTONDOWN;
  data[15] = SDL_INPUTBUTTONUP;
  data[16] = SDL_INPUTWHEEL;
  data[17] = SDL_INPUTPROXIMITYIN;
  data[18] = SDL_INPUTPROXIMITYOUT ;
  data[19] = SDL_JOYAXISMOTION ;
  data[20] = SDL_JOYBALLMOTION ;
  data[21] = SDL_JOYHATMOTION ;
  data[22] = SDL_JOYBUTTONDOWN ;
  data[23] = SDL_JOYBUTTONUP ;
  data[24] = SDL_FINGERDOWN ;
  data[25] = SDL_FINGERUP ;
  data[26] = SDL_FINGERMOTION ;
  data[27] = SDL_TOUCHBUTTONDOWN ;
  data[28] = SDL_TOUCHBUTTONUP ;
  data[29] = SDL_DOLLARGESTURE ;
  data[30] = SDL_DOLLARRECORD ;
  data[31] = SDL_MULTIGESTURE ;
  data[32] = SDL_CLIPBOARDUPDATE ;
  data[33] = SDL_EVENT_COMPAT1 ;
  data[34] = SDL_EVENT_COMPAT2 ;
  data[35] = SDL_EVENT_COMPAT3 ;
  data[36] = SDL_USEREVENT ;
  data[37] = SDL_LASTEVENT;
  printf("Checking SDL_EventType...\n");
  PascalCheckEnum_SDL_EventType(data);
  printf("    Done... SDL_EventType\n");
}

int main()
{
  c_check_enum_SDL_EventType();
}