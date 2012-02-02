// fpc test.pas -k"-rpath @loader_path/../Frameworks" -k"-F../Frameworks -framework SDL" -k"-macosx_version_min 10.7 -no_pie"

program Test;
uses SDL, Math;

var
  win : PSDL_Window;
  renderer : PSDL_Renderer;
  srcRect : SDL_Rect;
begin      
  srcRect.x := 0;
  srcRect.y := 0;
  srcRect.w := 100;
  srcRect.h :=100;
  
  //SDL_Init(SDL_INIT_VIDEO);
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  
  win := SDL_CreateWindow('Test', 100, 100, 800, 600, Uint32(SDL_WINDOW_SHOWN));
  
  renderer := nil;
  renderer := SDL_CreateRenderer(win, -1, LongWord(SDL_RENDERER_ACCELERATED) or LongWord(SDL_RENDERER_PRESENTVSYNC));
  WriteLn(HexStr(renderer));
  
 SDL_SetRenderDrawColor(renderer, 0, 255, 0, 255);
 SDL_RenderClear(renderer);
 SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255); 
  repeat
{      srcRect.x +=1;
      srcRect.y +=1;
}      SDL_RenderFillRect(renderer, @srcRect);
      SDL_RenderPresent(renderer);
  until False;
  
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(win);
    
	//ReleaseAllResources();
end.