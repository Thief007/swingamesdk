unit sgDriverTextSDL;

//=============================================================================
// sgDriverTextSDL.pas
//=============================================================================
//
// This file contains SDL 1.2 specific implementation of text functions that is called by the text driver.
//
// Change History:
//	2012-01-05: Aaron : Created File.
//	
// Notes:
//		-
// TODO:
// 		- 
//=============================================================================
interface
	procedure LoadSDLTextDriver();
		
//=============================================================================		
implementation
	uses sgDriverText, sdl_ttf, sgTypes, sgShared, sdl, sgGraphics, sgImages;
	
	const EOL = LineEnding; // from sgShared

	
	function LoadFontProcedure(fontName, filename: String; size: Longint): Font;
	begin
		New(result);
	    if result = nil then
			begin
				RaiseException('LoadFont to allocate space.');
	        	exit;
	      	end;

	      result^.fptr := TTF_OpenFont(PChar(filename), size);
	      if result^.fptr = nil then
	      begin
	        Dispose(result);
	        RaiseException('LoadFont failed: ' + TTF_GetError());
	        exit;
	      end;

	      result^.name := fontName;
	  
	end;
	
	procedure CloseFontProcedure(fontToClose : font);
	begin
		TTF_CloseFont(fontToClose^.fptr);
	end;
	
	function IsSet(toCheck, checkFor: FontAlignment): Boolean; overload;
  	begin
    	result := (Longint(toCheck) and Longint(checkFor)) = Longint(checkFor);
  	end;
	
	procedure PrintStringsProcedure(dest: Bitmap; font: Font; str: String; rc: Rectangle; clrFg, clrBg:Color; flags:FontAlignment);
	var
	
	    sText: Bitmap;
	    temp: PSDL_Surface;
	    lineSkip, width, height: Longint;
	    lines: Array of String;
	    subStr: String;
	    n, w, h, i: Longint;
	    rect: TSDL_Rect;
		SDLrc: PSDL_Rect;
	    colorFG: TSDL_Color;
	    bgTransparent: Boolean;
	begin
	  if Length(str) = 0 then exit;
	  if dest^.surface = nil then begin RaiseWarning('Error Printing Strings: There was no surface to draw onto.'); exit; end;
    
	  colorFG := ToSDLColor(clrFg);
	  bgTransparent := TransparencyOf(clrBg) < 255;
    
	  // If there's nothing to draw, return NULL
	  if (Length(str) = 0) or (font = nil) then exit;
    
	  // This is the surface that everything is printed to.
	  lineSkip  := TTF_FontLineSkip( font^.fptr );
	  width    := rc.width;
	  height    := 10;
	  SetLength(lines, 1);
    
	  // Break the String into its lines:
	  n := -1; i := 0;
	  while n <> 0 do
	  begin
	    // Get until either "\n" or "\0":
	    n := Pos(eol, str);
    
	    //Copy all except EOL
	    if n = 0 then subStr := str
	    else if n = 1 then subStr := ' '
	    else subStr := Copy(str, 1, n - 1);
    
	    if Length(subStr) < 1 then subStr := ' ';
    
	    //Remove the line from the original string
	    if n <> 0 then
	    begin
	      str := Copy( str, n + Length(eol), Length(str)); //excess not copied...
	    end;
    
	    i := i + 1;
	    SetLength(lines, i);
	    lines[i - 1] := subStr;
    
	    w := 0;
	    // Get the size of the rendered text.
	    if Length(subStr) > 0 then TTF_SizeText(font^.fptr, PChar(subStr), w, height);
    
	    if w > width then width := w;
	  end;
    
	  if (width = 0) or (height = 0) then exit;
    
	  // Length(lines) = Number of Lines.
	  // we assume that height is the same for all lines.
	  height := (Length(lines) - 1) * lineSkip + height;
    
	  sText := CreateBitmap(width, height);
	  ClearSurface(sText, clrBg);
    
	  // Actually render the text:
	  for i := 0 to High(lines) do
	  begin
	    // The rendered text:
	    if length(lines[i]) = 0 then continue;
    
	    temp := TTF_RenderText_Blended(font^.fptr, PChar(lines[i]), colorFG);
	    //temp := TTF_RenderUNICODE_Blended(font^.fptr, PUint16(lines[i]), colorFG);
    
	    // Put it on the surface:
	    if IsSet(flags, AlignLeft) or
	       (not (IsSet(flags, AlignCenter) or
	             IsSet(flags, AlignRight))) then
	    begin
	      // If it's specifically LEFT or none of the others:
	      rect := NewSDLRect(0,i*lineSkip,0,0);
	    end
	    else if IsSet(flags, AlignCenter) then
	    begin
	      w := 0;
	      h := 0;
    
	      TTF_SizeText(font^.fptr, PChar(lines[i]), w, h);
	      rect := NewSDLRect(width div 2 - w div 2, i * lineSkip, 0, 0)
	    end
	    else if IsSet(flags, AlignRight) then
	    begin
	      // Get w and h from the size of the text...
	      w := 0; h := 0;
	      TTF_SizeText(font^.fptr, PChar(lines[i]), w, h);
	      rect := NewSDLRect(rc.width - w, i * lineSkip, 0, 0);
	    end
	    else begin RaiseWarning('Invalid font alignment'); exit; end;
    
	    // Render the current line. Ignore alpha in this draw
	    if bgTransparent then SDL_SetAlpha(temp, 0, SDL_ALPHA_TRANSPARENT);
	    SDL_BlitSurface(temp, nil, sText^.surface, @rect);
    
	    // Clean up:
	    SDL_FreeSurface(temp);
	  end;
    
	  // Draw the text on top of that:
	  rect.x := 0; rect.y := 0; rect.w := rc.width; rect.h := rc.height;
	  if (not bgTransparent) then SDL_SetAlpha(sText^.surface, 0, SDL_ALPHA_TRANSPARENT);
	  new(SDLrc);
	  SDLrc^ := NewSDLRect(trunc(rc.x),trunc(rc.y),rc.width,rc.height);
	  SDL_BlitSurface(sText^.surface, @rect, dest^.surface, SDLrc );
    
	  FreeBitmap(sText);
	end;
	
	
	procedure LoadSDLTextDriver();
	begin
		TextDriver.LoadFont := @LoadFontProcedure;
		TextDriver.CloseFont := @CloseFontProcedure;
		TextDriver.PrintStrings := @PrintStringsProcedure;
	end;
	

end.