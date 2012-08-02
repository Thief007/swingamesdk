// Add shared util functions for SDL2 here.
//
unit sgSDL13Utils;

interface
uses sgTypes, sdl13;

	procedure RecreateTexture(bmp: Bitmap);
	procedure RenderTempSurface(bmp: Bitmap; srcRect, destRect: PSDL_Rect);

implementation
uses sgShared, sgDriverSDL13;

	procedure RecreateTexture(bmp: Bitmap);
	var
		bmpData: PSDL13Surface;
	begin
		bmpData := PSDL13Surface(bmp^.surface);
		if not Assigned(bmpData) then exit;

		if Assigned(bmpData^.texture) then
		begin
			SDL_DestroyTexture(bmpData^.texture);
		end;
	    
	    bmpData^.texture := SDL_CreateTextureFromSurface(PSDL13Screen(_screen)^.renderer, bmpData^.surface);
	end;

	procedure RenderTempSurface(bmp: Bitmap; srcRect, destRect: PSDL_Rect); 
	var
		texture : PSDL_Texture;
		renderer : PSDL_Renderer;
	begin
		renderer := PSDL13Screen(_screen)^.renderer;
	    texture := SDL_CreateTextureFromSurface(renderer, GetSurface(bmp));
		
		SDL_RenderCopy(renderer, texture, srcRect, destRect);
		
		SDL_DestroyTexture(texture);
	end;

end.