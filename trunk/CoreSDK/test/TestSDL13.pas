program TestSDL13;
uses SDL13, SDL_Net;

procedure Main();
var
  win : PSDL_Window;
  renderer : PSDL_Renderer;
  srcRect : SDL_Rect;
begin      
 { srcRect.x := 0;
  srcRect.y := 0;
  srcRect.w := 100;
  srcRect.h :=100;
  
  //SDL_Init(SDL_INIT_VIDEO);
  
  win := SDL_CreateWindow('Test', SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 800, 600, Uint32(SDL_WINDOW_SHOWN));
  
  renderer := nil;
  renderer := SDL_CreateRenderer(win, -1, LongWord(SDL_RENDERER_ACCELERATED) or LongWord(SDL_RENDERER_PRESENTVSYNC));
  
 SDL_SetRenderDrawColor(renderer, 255, 0, 0, 255);
 SDL_RenderClear(renderer);
  
  repeat
      SDL_RenderFillRect(renderer, @srcRect);
      SDL_RenderPresent(renderer);
  until False;
  
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(win);}
    
	//ReleaseAllResources();
end;

begin
	main();
end.